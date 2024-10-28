{- |
Module      : GeniusYield.OrderBot
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OrderBot (
  AssetInfo (..),
  PriceProviderConfig (..),
  OrderBot (..),
  ExecutionStrategy (..),
  runOrderBot,
) where

import Control.Arrow (second, (&&&))
import Control.Concurrent (threadDelay)
import Control.Exception (
  AsyncException (UserInterrupt),
  Exception,
  SomeException,
  bracket,
  displayException,
  fromException,
  handle,
  throwIO,
  try,
 )
import Control.Monad (
  filterM,
  forever,
  unless,
  when,
  (<=<),
 )
import Control.Monad.Reader (runReaderT)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (foldl', foldlM, toList)
import Data.Functor ((<&>))
import Data.List (find)
import qualified Data.List.NonEmpty as NE (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Txt
import Data.Word (Word64)
import Deriving.Aeson
import GeniusYield.Api.Dex.Constants (DEXInfo (..))
import GeniusYield.GYConfig (
  Confidential (..),
  GYCoreConfig (cfgNetworkId),
  withCfgProviders,
 )
import GeniusYield.Imports (coerce)
import GeniusYield.OrderBot.DataSource (closeDB, connectDB)
import GeniusYield.OrderBot.MatchingStrategy (
  IndependentStrategy,
  MatchExecutionInfo (..),
  MatchResult,
  executionSkeleton,
  matchExecutionInfoUtxoRef,
 )
import GeniusYield.OrderBot.OrderBook (
  OrderBook,
  buyOrders,
  foldrOrders,
  maOrderBookToList,
  populateOrderBook,
  sellOrders,
  withEachAsset,
 )
import GeniusYield.OrderBot.Types (
  OrderAssetPair (..),
  assetInfo,
 )
import GeniusYield.Providers.Common (SubmitTxException)
import GeniusYield.Providers.Maestro (networkIdToMaestroEnv)
import GeniusYield.Server.Ctx (TapToolsEnv (..))
import GeniusYield.Server.Dex.HistoricalPrices.TapTools.Client hiding (handleTapToolsError)
import GeniusYield.Transaction (GYCoinSelectionStrategy (GYLegacy))
import GeniusYield.TxBuilder (
  GYTxBuildResult (..),
  GYTxBuilderMonadIO,
  GYTxSkeleton,
  buildTxBodyParallelWithStrategy,
  runGYTxBuilderMonadIO,
  runGYTxQueryMonadIO,
  utxosAtAddresses,
  utxosAtTxOutRefs,
 )
import GeniusYield.TxBuilder.Errors (GYTxMonadException)
import GeniusYield.Types
import qualified Maestro.Client.V1 as Maestro
import qualified Maestro.Types.V1 as Maestro
import System.Exit (exitSuccess)
import Web.HttpApiData (ToHttpApiData (..))

data AssetInfo = AssetInfo
  { assetTicker :: Text
  , assetDecimals :: Word64
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "asset", Maestro.LowerFirst]] AssetInfo

data MaestroConfig = MaestroConfig
  { mcApiKey :: !(Confidential Text)
  , mcResolution :: !Maestro.Resolution
  , mcDex :: !Maestro.Dex
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "mc", Maestro.LowerFirst]] MaestroConfig

newtype TapToolsConfig = TapToolsConfig
  { ttcApiKey :: Confidential Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "ttc", Maestro.LowerFirst]] TapToolsConfig

-- | Price provider to get ADA price of a token.
data PriceProviderConfig
  = TapToolsPriceProviderConfig !TapToolsConfig
  | MaestroPriceProviderConfig !MaestroConfig
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[Rename "TapToolsPriceProviderConfig" "tapTools", Rename "MaestroPriceProviderConfig" "maestro"]] PriceProviderConfig

data MaestroPP = MaestroPP
  { mppEnv :: !(Maestro.MaestroEnv 'Maestro.V1)
  , mppResolution :: !Maestro.Resolution
  , mppDex :: !Maestro.Dex
  }

newtype TapToolsPP = TapToolsPP
  { ttppEnv :: TapToolsEnv
  }

data PriceProvider
  = TapToolsPriceProvider !TapToolsPP
  | MaestroPriceProvider !MaestroPP

buildTapToolsPP :: TapToolsConfig -> IO TapToolsPP
buildTapToolsPP TapToolsConfig {..} = do
  tcenv <- tapToolsClientEnv
  let tenv = TapToolsEnv tcenv (coerce ttcApiKey)
  pure
    TapToolsPP
      { ttppEnv = tenv
      }

buildMaestroPP :: MaestroConfig -> IO MaestroPP
buildMaestroPP MaestroConfig {..} = do
  env <- networkIdToMaestroEnv (coerce mcApiKey) GYMainnet
  pure
    MaestroPP
      { mppEnv = env
      , mppResolution = mcResolution
      , mppDex = mcDex
      }

buildPP :: PriceProviderConfig -> IO PriceProvider
buildPP (TapToolsPriceProviderConfig ttpc) = TapToolsPriceProvider <$> buildTapToolsPP ttpc
buildPP (MaestroPriceProviderConfig mpc) = MaestroPriceProvider <$> buildMaestroPP mpc

-- | The order bot is product type between bot info and "execution strategies".
data OrderBot = OrderBot
  { botSkey :: !GYPaymentSigningKey
  -- ^ Signing key of the bot.
  , botStakeAddress :: !(Maybe GYStakeAddressBech32)
  -- ^ Optional bech32 encoded stake address.
  , botCollateral :: !(Maybe (GYTxOutRef, Bool))
  -- ^ UTxO ref of the collateral UTxO in the bot's wallet.
  --
  --          NOTE: If collateral is Nothing, then Atlas will choose some UTxO to
  --          function as collateral. If a TxOutRef is given, the bool indicates whether
  --          the collateral can be spent in the tx.
  , botExecutionStrat :: !ExecutionStrategy
  -- ^ The execution strategy, which includes and governs the matching strategy.
  , botAssetPairFilter :: [OrderAssetPair]
  -- ^ List that can be used to filter out uninteresting orders/pools.
  --          The multiasset order book is created only with the existing pairs on
  --          the list.
  , botRescanDelay :: Int
  -- ^ How many microseconds to wait after a tx submission before rescanning
  --          the chain for orders.
  , botTakeMatches :: [MatchResult] -> IO [MatchResult]
  -- ^ How and how many matching results do the bot takes to build, sign and
  --          submit every iteration.
  , botLovelaceWarningThreshold :: Maybe Natural
  -- ^ See 'botCLovelaceWarningThreshold'.
  , botPriceProvider :: Maybe PriceProviderConfig
  -- ^ The price provider for the bot, used in case arbitrage is in non-ada token & we need to decide if the arbitraged tokens compensate the ada lost due to transaction fees.
  , botTokenInfos :: Map GYAssetClass AssetInfo
  }

{- | Currently, we only have the parallel execution strategy: @MultiAssetTraverse@,
     where each order book for each unique asset pair (see: "GeniusYield.OrderBot.Types.equivalentAssetPair")
     is processed independently.
-}
newtype ExecutionStrategy = MultiAssetTraverse IndependentStrategy

sorNS :: GYLogNamespace
sorNS = "SOR"

runOrderBot ::
  -- | Path to the config file for the GY framework.
  GYCoreConfig ->
  -- | Complete DEX information.
  DEXInfo ->
  -- | OrderBot configuration.
  OrderBot ->
  IO ()
runOrderBot
  cfg
  di
  OrderBot
    { botSkey
    , botStakeAddress
    , botCollateral
    , botExecutionStrat = MultiAssetTraverse strat
    , botAssetPairFilter
    , botRescanDelay
    , botTakeMatches
    , botLovelaceWarningThreshold
    , botPriceProvider
    , botTokenInfos
    } = do
    withCfgProviders cfg "" $ \providers -> do
      let logInfo = gyLogInfo providers sorNS
          logDebug = gyLogDebug providers sorNS
          logWarn = gyLogWarning providers sorNS

          netId = cfgNetworkId cfg
          botPkh = paymentKeyHash $ paymentVerificationKey botSkey
          botChangeAddr = addressFromCredential netId (GYPaymentCredentialByKey botPkh) (stakeAddressToCredential . stakeAddressFromBech32 <$> botStakeAddress)
          botAddrs = [botChangeAddr]
      logInfo $
        unlines
          [ ""
          , "Starting bot with given credentials"
          , "  Payment key hash: " ++ show (paymentKeyHashToPlutus botPkh)
          , "  Wallet Addresses: " ++ show (Txt.unpack . addressToText <$> botAddrs)
          , "  Change Address: " ++ (Txt.unpack . addressToText $ botChangeAddr)
          , "  Collateral: " ++ show botCollateral
          , "  Lovelace balance warning threshold: " ++ show botLovelaceWarningThreshold
          , "  Scan delay (µs): " ++ show botRescanDelay
          , "  Bot price configuration: " ++ show botPriceProvider
          , "  Bot token infos: " ++ show botTokenInfos
          , "  Token Pairs to scan:"
          , unlines (map (("\t - " ++) . show) botAssetPairFilter)
          , ""
          ]
      mpp <- maybe (pure Nothing) (fmap Just . buildPP) botPriceProvider
      bracket (connectDB netId providers) closeDB $ \conn -> forever $
        handle (handleAnyException providers) $ do
          logInfo "Rescanning for orders..."
          botUtxos <- runGYTxQueryMonadIO netId providers $ utxosAtAddresses botAddrs
          let botBalance = foldMapUTxOs utxoValue botUtxos
              botLovelaceBalance = valueAssetClass botBalance GYLovelace
          logInfo $
            unwords
              [ "Bot balance:"
              , show botBalance
              ]
          when (botLovelaceBalance < maybe 0 fromIntegral botLovelaceWarningThreshold) $
            logWarn $
              unwords
                [ "Bot lovelace balance is below the warning threshold. Threshold:"
                , show botLovelaceWarningThreshold
                , ", bot lovelace balance:"
                , show botLovelaceBalance
                ]
          -- First we populate the multi asset orderbook, using the provided
          -- @populateOrderBook@.
          book <- populateOrderBook conn di botAssetPairFilter

          let bookList = maOrderBookToList book
          logInfo $
            unwords
              [ "MultiAsset Order Book Info:"
              , unwords $ jsonBookInfo bookList
              ]
          logDebug $
            unwords
              [ "MultiAsset Order Book:"
              , jsonPrint bookList
              ]

          -- Now we pass each asset pair's orderbook to the provided execution strategy.
          let matchesFound = withEachAsset strat book

          logDebug $
            unwords
              [ "Matches Found:"
              , jsonPrint matchesFound
              ]
          logInfo $
            unwords
              [ "Total matches found:"
              , jsonPrint $ M.toList $ matchingsPerOrderAssetPair botAssetPairFilter matchesFound
              ]

          {- This part builds and submits the transactions from the returned matches.
             This part has the highest chances of throwing exceptions, as it's extremely
             stateful. The user provided exception handler is used to wrap this flow.
          -}
          unless (all null matchesFound) $ do
            matchesToExecute <- botTakeMatches matchesFound

            logDebug $
              unwords
                [ "Matches To Execute:"
                , jsonPrint matchesToExecute
                ]

            logInfo $
              unwords
                [ "Number Of Matches To Execute:"
                , jsonPrint $ M.toList $ matchingsPerOrderAssetPair botAssetPairFilter matchesToExecute
                ]

            -- We first build all the tx Bodies from the matches
            txs <- buildTransactions matchesToExecute di netId providers (botAddrs, botChangeAddr) botCollateral

            logInfo $
              unwords
                [ "Number Of Matches Built:"
                , show $ length txs
                ]

            -- We filter the txs that are not losing tokens
            profitableTxs <-
              filterM
                (notLosingTokensCheck netId providers botAddrs botAssetPairFilter mpp botTokenInfos)
                txs

            logInfo $
              unwords
                [ "Transactions are losing money:"
                , show (length txs - length profitableTxs)
                ]

            {- We submit the txs sequentially. It's important to do it this way
            because a utxo used as collateral in tx 1 can be used as input in tx2.
            If we submit those txs concurrently, it can fail -}

            mapM_ (\(tx, _) -> signAndSubmitTx tx providers botSkey) profitableTxs

          {- Block production on the chain takes time. One has to wait for some amount
             of time before the blockchain state properly changes and another transaction
             can be submitted.
          -}
          logInfo "Waiting to rescan for orders..."
          threadDelay botRescanDelay
   where
    handleAnyException :: GYProviders -> SomeException -> IO ()
    handleAnyException _ (fromException -> Just UserInterrupt) =
      putStrLn "Gracefully stopping..." >> exitSuccess
    handleAnyException providers err =
      let logErr = gyLogError providers sorNS
       in logErr (show err) >> threadDelay botRescanDelay

signAndSubmitTx :: GYTxBody -> GYProviders -> GYPaymentSigningKey -> IO ()
signAndSubmitTx txBody providers botSkey = handle handlerSubmit $ do
  let tx = signGYTxBody txBody [botSkey]
  logDebug $ unwords ["Transaction to submit:", show txBody]
  tid <- gySubmitTx providers tx
  logInfo $ unwords ["Submitted order matching transaction with id:", show tid]
 where
  logInfo, logDebug, logWarn :: String -> IO ()
  logInfo = gyLogInfo providers sorNS
  logDebug = gyLogDebug providers sorNS
  logWarn = gyLogWarning providers sorNS

  handlerSubmit :: SubmitTxException -> IO ()
  handlerSubmit ex = logWarn $ unwords ["SubmitTxException:", show ex]

buildTransactions ::
  [MatchResult] ->
  DEXInfo ->
  GYNetworkId ->
  GYProviders ->
  ([GYAddress], GYAddress) ->
  Maybe (GYTxOutRef, Bool) ->
  IO [(GYTxBody, MatchResult)]
buildTransactions
  matchesToExecute
  di
  netId
  providers
  (botAddrs, botChangeAddr)
  botCollateral = handle handlerBuildTx $ do
    res <-
      runGYTxMonadNodeParallelWithStrategy
        GYLegacy
        netId
        providers
        botAddrs
        botChangeAddr
        botCollateral
        $ traverse resultToSkeleton matchesToExecute

    case res of
      -- Successful cases
      GYTxBuildSuccess txs -> return $ zip (getBodies txs) matchesToExecute
      GYTxBuildPartialSuccess _ txs ->
        return $
          mapMaybe
            (findBody (getBodies txs))
            matchesToExecute
      -- Failure cases
      GYTxBuildFailure v ->
        logWarn (unwords ["Insufficient funds:", show v])
          >> return []
      GYTxBuildNoInputs -> logWarn "No Inputs" >> return []
   where
    logWarn :: String -> IO ()
    logWarn = gyLogWarning providers sorNS

    findBody :: [GYTxBody] -> MatchResult -> Maybe (GYTxBody, MatchResult)
    findBody bs mr =
      let ref = matchExecutionInfoUtxoRef $ head mr
       in find (elem ref . txBodyTxIns) bs <&> (,mr)

    getBodies = NE.toList

    resultToSkeleton :: MatchResult -> GYTxBuilderMonadIO (GYTxSkeleton 'PlutusV2)
    resultToSkeleton mResult = runReaderT (executionSkeleton (dexPORefs di) mResult) di

    handlerBuildTx :: GYTxMonadException -> IO [(GYTxBody, MatchResult)]
    handlerBuildTx ex =
      logWarn (unwords ["GYTxMonadException:", show ex])
        >> return []

notLosingTokensCheck ::
  GYNetworkId ->
  GYProviders ->
  [GYAddress] ->
  [OrderAssetPair] ->
  Maybe PriceProvider ->
  Map GYAssetClass AssetInfo ->
  (GYTxBody, MatchResult) ->
  IO Bool
notLosingTokensCheck netId providers botAddrs oapFilter mpp assetInfos (txBody, matchesToExecute) = do
  let logDebug = gyLogDebug providers sorNS
      logWarn = gyLogWarning providers sorNS
      logErr = gyLogError providers sorNS
      matchesRefs = map matchExecutionInfoUtxoRef matchesToExecute
      botInputs = filter (`notElem` matchesRefs) $ txBodyTxIns txBody

  inputs <- runGYTxQueryMonadIO netId providers $ utxosAtTxOutRefs botInputs

  let (inputLovelace, filteredACInput) =
        utxosLovelaceAndFilteredValueAtAddr inputs
      (outputLovelace, filteredACOutput) =
        utxosLovelaceAndFilteredValueAtAddr $ txBodyUTxOs txBody
      botAssets = valueAssets filteredACInput
      fees = txBodyFee txBody
      nonAdaTokenArbitrage = M.fromList $ filter ((/= 0) . snd) $ map (\ac -> (ac, valueAssetClass filteredACOutput ac - valueAssetClass filteredACInput ac)) $ toList botAssets
      filteredACCheck = all (> 0) $ M.elems nonAdaTokenArbitrage -- We already filtered for zero values.
  lovelaceCheck <-
    if all currencyIsLovelace oapFilter
      then pure (outputLovelace >= inputLovelace)
      else case mpp of
        Nothing -> pure $ inputLovelace - outputLovelace <= fees -- Should include flat taker fee here as well.
        Just pp -> do
          let tokensWithInfos = M.restrictKeys assetInfos (M.keysSet nonAdaTokenArbitrage)
          accLovelace <- do
            priceInfos <- case pp of
              MaestroPriceProvider mpp -> do
                foldlM'
                  ( \acc (ac, assetInfo) -> do
                      res <- getLovelacePriceOfAssetMaestro mpp ac assetInfo
                      pure $! M.insert ac res acc
                  )
                  (M.empty :: Map GYAssetClass (Either PricesProviderException Rational))
                  $ M.toList tokensWithInfos
              TapToolsPriceProvider tpp -> do
                getLovelacePriceOfAssetsTapTools tpp tokensWithInfos
            foldlM'
              ( \accLovelace (ac, amt) -> do
                  if not (M.member ac assetInfos)
                    then do
                      logWarn $ "AssetInfo not found for: " ++ show ac
                      pure accLovelace
                    else do
                      let lovelacePriceOfAssetE = priceInfos M.! ac
                      case lovelacePriceOfAssetE of
                        Left e -> do
                          logErr $ "Failed to get lovelace price of asset: " ++ show ac ++ ", with error: " ++ show e
                          pure accLovelace
                        Right lovelacePriceOfAsset -> do
                          pure $ accLovelace + floor (lovelacePriceOfAsset * fromIntegral amt) -- TODO: Unit test this part!
              )
              0
              $ M.toList nonAdaTokenArbitrage
          pure $ outputLovelace + accLovelace >= inputLovelace
  let completeCheck = lovelaceCheck && filteredACCheck
  unless lovelaceCheck $
    logWarn $
      unwords
        [ "Transaction losing lovelaces: "
        , "Expected ADA total amount at least: " ++ show (inputLovelace - fees)
        , "Actual ADA total amount: " ++ show outputLovelace
        ]
  unless filteredACCheck $
    logWarn $
      unwords
        [ "Transaction losing tokens: "
        , "Expected Tokens total amount: " ++ show filteredACOutput
        , "Actual Tokens total amount: " ++ show filteredACInput
        ]
  unless completeCheck $ do
    logDebug $
      unwords
        [ "CompleteChecks:"
        , jsonPrint matchesToExecute
        , "Tx: " ++ show txBody
        ]

  return completeCheck
 where
  botAssetFilter :: GYAssetClass -> Bool
  botAssetFilter ac =
    any
      (\oap -> currencyAsset oap == ac || commodityAsset oap == ac)
      oapFilter

  utxosValueAtAddr :: GYUTxOs -> GYValue
  utxosValueAtAddr =
    mconcat
      . map utxoValue
      . filter ((`elem` botAddrs) . utxoAddress)
      . utxosToList

  utxosLovelaceAndFilteredValueAtAddr ::
    GYUTxOs ->
    (Integer, GYValue)
  utxosLovelaceAndFilteredValueAtAddr utxos =
    second (valueFromList . filter (botAssetFilter . fst) . valueToList) $
      valueSplitAda $
        utxosValueAtAddr utxos

  currencyIsLovelace :: OrderAssetPair -> Bool
  currencyIsLovelace oap = currencyAsset oap == GYLovelace

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

jsonPrint :: ToJSON a => a -> String
jsonPrint = B.unpack . BL.toStrict . encode

jsonBookInfo :: [(OrderAssetPair, OrderBook)] -> [String]
jsonBookInfo = map (jsonPrint . second (totalSellOrders &&& totalBuyOrders))

totalSellOrders :: OrderBook -> Int
totalSellOrders = foldrOrders (const (+ 1)) 0 . sellOrders

totalBuyOrders :: OrderBook -> Int
totalBuyOrders = foldrOrders (const (+ 1)) 0 . buyOrders

matchingsPerOrderAssetPair :: [OrderAssetPair] -> [MatchResult] -> Map OrderAssetPair Int
matchingsPerOrderAssetPair oaps = foldl' succOAP (M.fromList $ map (,0) oaps)
 where
  succOAP :: Map OrderAssetPair Int -> MatchResult -> Map OrderAssetPair Int
  succOAP m (OrderExecutionInfo _ oi : _) = M.insertWith (+) (assetInfo oi) 1 m
  succOAP m _ = m

runGYTxMonadNodeParallelWithStrategy :: GYCoinSelectionStrategy -> GYNetworkId -> GYProviders -> [GYAddress] -> GYAddress -> Maybe (GYTxOutRef, Bool) -> GYTxBuilderMonadIO [GYTxSkeleton v] -> IO GYTxBuildResult
runGYTxMonadNodeParallelWithStrategy strat nid providers addrs change collateral act = runGYTxBuilderMonadIO nid providers addrs change collateral $ act >>= buildTxBodyParallelWithStrategy strat

foldlM' :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM' f = foldlM (\ !acc -> f acc)

data MaestroPriceException = MaestroApiError !Text !Maestro.MaestroError
  deriving stock Show
  deriving anyclass Exception

data TapToolsPriceException
  = TapToolsApiError !Text !TapToolsException
  | TapToolsOtherError !Text !Text
  deriving stock (Eq, Show)
  deriving anyclass Exception

data PricesProviderException
  = PPMaestroErr MaestroPriceException
  | PPTapToolsErr TapToolsPriceException
  deriving stock Show

instance Exception PricesProviderException where
  displayException (PPMaestroErr err) = "Maestro fail: " ++ displayException err
  displayException (PPTapToolsErr err) = "TapTools fail: " ++ displayException err

throwMspvApiError :: Text -> Maestro.MaestroError -> IO a
throwMspvApiError locationInfo =
  throwIO . MaestroApiError locationInfo

handleMaestroError :: Text -> Either Maestro.MaestroError a -> IO a
handleMaestroError locationInfo = either (throwMspvApiError locationInfo) pure

handleMaestroSourceFail :: MaestroPriceException -> IO (Either PricesProviderException a)
handleMaestroSourceFail = pure . Left . PPMaestroErr

-- | Assumption: None of the tokens is ADA.
getLovelacePriceOfAssetsTapTools :: TapToolsPP -> Map GYAssetClass AssetInfo -> IO (Map GYAssetClass (Either PricesProviderException Rational))
getLovelacePriceOfAssetsTapTools (TapToolsPP {..}) assetInfos = do
  let units :: [TapToolsUnit] = coerce $ M.keys assetInfos
      adaPrecision :: Int = 6 -- We cast to @Int@ so as to handle overflows when performing subtraction later.
  priceInfosE <- try $ tapToolsPrices ttppEnv units
  case priceInfosE of
    Left (e :: TapToolsException) ->
      pure $ M.map (\_ -> Left (PPTapToolsErr $ TapToolsApiError functionLocationIdent e)) assetInfos
    Right priceInfos ->
      pure $
        M.mapWithKey
          ( \ac _ -> do
              let unit :: TapToolsUnit = coerce ac
              case M.lookup unit priceInfos of
                Nothing -> Left $ PPTapToolsErr $ TapToolsOtherError functionLocationIdent ("Price not found for given unit: " <> toUrlPiece unit)
                Just price -> do
                  let AssetInfo {..} = assetInfos M.! ac
                      tokenPrecision :: Int = fromIntegral assetDecimals
                      precisionDiff = 10 ** fromIntegral (adaPrecision - tokenPrecision)
                      adjustedPrice = price * precisionDiff
                  Right . toRational $ adjustedPrice
          )
          assetInfos
 where
  functionLocationIdent = "getLovelacePriceOfAssetsTapTools"

-- | Assumption: Provided token is not ADA.
getLovelacePriceOfAssetMaestro :: MaestroPP -> GYAssetClass -> AssetInfo -> IO (Either PricesProviderException Rational)
getLovelacePriceOfAssetMaestro MaestroPP {..} _ac AssetInfo {..} = do
  handle handleMaestroSourceFail $ do
    let pairName = "ADA-" <> assetTicker
        pair = Maestro.TaggedText pairName

    ohlInfo <-
      handleMaestroError (functionLocationIdent <> " - fetching price from pair") <=< try $
        -- TODO: Should limit to 1?
        Maestro.pricesFromDex mppEnv mppDex pair (Just mppResolution) Nothing Nothing Nothing (Just Maestro.Descending)

    let info = head ohlInfo
        adaPrecision :: Int = 6 -- We cast to @Int@ so as to handle overflows when performing subtraction later.
        tokenPrecision :: Int = fromIntegral assetDecimals
        precisionDiff = 10 ** fromIntegral (adaPrecision - tokenPrecision)

        price = Maestro.ohlcCandleInfoCoinAClose info

        adjustedPrice = price * precisionDiff

    return . Right . toRational $ adjustedPrice
 where
  functionLocationIdent = "getLovelacePriceOfAssetMaestro"