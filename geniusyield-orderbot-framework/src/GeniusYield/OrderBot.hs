{- |
Module      : GeniusYield.OrderBot
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OrderBot (
  OrderBot (..),
  ExecutionStrategy (..),
  runOrderBot,
) where

import Control.Arrow (second, (&&&))
import Control.Concurrent (threadDelay)
import Control.Exception (
  AsyncException (UserInterrupt),
  SomeException,
  bracket,
  fromException,
  handle,
 )
import Control.Monad (
  filterM,
  forever,
  unless,
 )
import Control.Monad.Reader (runReaderT)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (foldl', toList)
import Data.Functor ((<&>))
import Data.List (find)
import qualified Data.List.NonEmpty as NE (toList)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as Txt
import GeniusYield.Api.Dex.Constants (DEXInfo (..))
import GeniusYield.GYConfig (
  GYCoreConfig (cfgNetworkId),
  withCfgProviders,
 )
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
import GeniusYield.Transaction (GYCoinSelectionStrategy (GYLegacy))
import GeniusYield.TxBuilder (
  GYTxBuildResult (..),
  GYTxBuilderMonadIO,
  GYTxSkeleton,
  buildTxBodyParallelWithStrategy,
  runGYTxBuilderMonadIO,
  runGYTxQueryMonadIO,
  utxosAtTxOutRefs,
 )
import GeniusYield.TxBuilder.Errors (GYTxMonadException)
import GeniusYield.Types
import System.Exit (exitSuccess)

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
  }

{- | Currently, we only have the parallel execution strategy: @MultiAssetTraverse@,
     where each order book for each unique asset pair (see: "GeniusYield.OrderBot.Types.equivalentAssetPair")
     is processed independently.
-}
newtype ExecutionStrategy = MultiAssetTraverse IndependentStrategy

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
    } = do
    withCfgProviders cfg "" $ \providers -> do
      let logInfo = gyLogInfo providers "SOR"
          logDebug = gyLogDebug providers "SOR"

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
          , "  Token Pairs to scan:"
          , unlines (map (("\t - " ++) . show) botAssetPairFilter)
          , ""
          ]

      bracket (connectDB netId providers) closeDB $ \conn -> forever $
        handle (handleAnyException providers) $ do
          logInfo "Rescanning for orders..."

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
                (notLosingTokensCheck netId providers botAddrs botAssetPairFilter)
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
      let logErr = gyLogError providers "SOR"
       in logErr (show err) >> threadDelay botRescanDelay

signAndSubmitTx :: GYTxBody -> GYProviders -> GYPaymentSigningKey -> IO ()
signAndSubmitTx txBody providers botSkey = handle handlerSubmit $ do
  let tx = signGYTxBody txBody [botSkey]
  logDebug $ unwords ["Transaction to submit:", show txBody]
  tid <- gySubmitTx providers tx
  logInfo $ unwords ["Submitted order matching transaction with id:", show tid]
 where
  logInfo, logDebug, logWarn :: String -> IO ()
  logInfo = gyLogInfo providers "SOR"
  logDebug = gyLogDebug providers "SOR"
  logWarn = gyLogWarning providers "SOR"

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
    logWarn = gyLogWarning providers "SOR"

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
  (GYTxBody, MatchResult) ->
  IO Bool
notLosingTokensCheck netId providers botAddrs oapFilter (txBody, matchesToExecute) = do
  let logDebug = gyLogDebug providers "SOR"
      logWarn = gyLogWarning providers "SOR"
      matchesRefs = map matchExecutionInfoUtxoRef matchesToExecute
      botInputs = filter (`notElem` matchesRefs) $ txBodyTxIns txBody

  inputs <- runGYTxQueryMonadIO netId providers $ utxosAtTxOutRefs botInputs

  let (inputLovelace, filteredACInput) =
        utxosLovelaceAndFilteredValueAtAddr inputs
      (outputLovelace, filteredACOutput) =
        utxosLovelaceAndFilteredValueAtAddr $ txBodyUTxOs txBody

      fees = txBodyFee txBody
      lovelaceCheck = if all currencyIsLovelace oapFilter then outputLovelace >= inputLovelace else inputLovelace - outputLovelace <= fees

      filteredACCheck =
        all
          ( \ac ->
              valueAssetClass filteredACInput ac
                <= valueAssetClass filteredACOutput ac
          )
          $ toList
          $ valueAssets filteredACInput

      completeCheck = lovelaceCheck && filteredACCheck

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

matchingsPerOrderAssetPair :: [OrderAssetPair] -> [MatchResult] -> M.Map OrderAssetPair Int
matchingsPerOrderAssetPair oaps = foldl' succOAP (M.fromList $ map (,0) oaps)
 where
  succOAP :: M.Map OrderAssetPair Int -> MatchResult -> M.Map OrderAssetPair Int
  succOAP m (OrderExecutionInfo _ oi : _) = M.insertWith (+) (assetInfo oi) 1 m
  succOAP m _ = m

runGYTxMonadNodeParallelWithStrategy :: GYCoinSelectionStrategy -> GYNetworkId -> GYProviders -> [GYAddress] -> GYAddress -> Maybe (GYTxOutRef, Bool) -> GYTxBuilderMonadIO [GYTxSkeleton v] -> IO GYTxBuildResult
runGYTxMonadNodeParallelWithStrategy strat nid providers addrs change collateral act = runGYTxBuilderMonadIO nid providers addrs change collateral $ act >>= buildTxBodyParallelWithStrategy strat
