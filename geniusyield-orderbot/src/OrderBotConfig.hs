{-|
Module      : OrderBotConfig
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module OrderBotConfig where

import           Control.Exception ( throwIO )
import           Control.Monad ( (<=<) )
import           Control.Monad.Reader ( runReaderT )
import           Control.Monad.Error.Class ( throwError )
import           Data.Aeson ( eitherDecodeFileStrict
                            , (.:), (.:?)
                            , withArray, withObject
                            , FromJSON(parseJSON)
                            , Array
                            , Value(Object), eitherDecodeStrict
                            )
import qualified Data.Aeson.Types as Aeson
import           Data.Bifunctor ( first )
import           Data.String ( IsString(..) )
import           Data.Random ( shuffle, sample )
import qualified Data.Vector as V
import           Data.List ( nub )
import           GHC.Generics ( Generic )
import           System.Envy ( FromEnv (fromEnv), Var, Parser, envMaybe, env
                             , decodeEnv
                             )
import           System.Random.MWC (fromSeed, initialize, createSystemSeed)

import           Ply (readTypedScript, TypedScript, ScriptRole (..))
import           PlutusLedgerApi.V1         ( Address )
import           PlutusLedgerApi.V1.Scripts ( ScriptHash )
import           PlutusLedgerApi.V1.Value   ( AssetClass )

import           GeniusYield.OrderBot
import           GeniusYield.OrderBot.Types ( OrderAssetPair(..)
                                            , equivalentAssetPair
                                            )
import           GeniusYield.OrderBot.MatchingStrategy ( MatchResult )
import           GeniusYield.Types
import           Cardano.Api ( AsType (AsSigningKey, AsPaymentKey)
                             , deserialiseFromTextEnvelope
                             )

import           Strategies ( BotStrategy(..), allStrategies, mkIndependentStrategy )
import           GeniusYield.DEX.Api.Types

-- | Order bot vanilla config.
data OrderBotConfig =
    OrderBotConfig
    { botCSkey                  :: Either FilePath GYPaymentSigningKey
    -- ^ Signing key of the bot.
    , botCCollateral            :: Maybe GYTxOutRef
    {- ^ UTxO ref of the collateral UTxO in the bot's wallet.

         NOTE: If collateral is Nothing, then Atlas will choose some UTxO to
          function as collateral. If a TxOutRef is given, the bool indicates wheter
          the collateral can be spent in the tx.
    -}
    , botCExecutionStrat        :: BotStrategy
    -- ^ Name of the running strategy.
    , botCAssetFilter           :: [OrderAssetPair]
    -- ^ List of asset pairs to scan.
    , botCRescanDelay           :: Int
    {- ^ The duration (microseconds) of time we wait before re-initiating a
         complete iteration for the bot.
    -}
    , botCFPNftPolicy           :: FilePath
    -- ^ FilePath of the partial order minting policy.
    , botCFPOrderValidator      :: FilePath
    -- ^ FilePath of the partial order validator.
    , botCMaxOrderMatches       :: Int
    -- ^ The maximum amount of orders to be matched into a single transaction.
    , botCMaxTxsPerIteration    :: Int
    {- ^ The maximum amount of transactions that the bot will build, sign and
         submit in each iteration.
    -}
    , botCRandomizeMatchesFound :: Bool
    {- ^ A boolean that dictates whether the bot chooses the tx to submit at
         random (to decrease collisions), or not (to maximize profit)
    -}
    , botCPORConfig             :: PORConfig
    {- ^ UTxO reference of the UTxO storing the NFT that is placed in each order.
         Address of the previous UTxO, together with a "must have" NFT to officially
         identification.

         UTxO reference of the partial order minting policy.
         UTxO reference of the partial order validator.
    -}
    }
    deriving stock (Show, Eq, Generic)

instance FromEnv OrderBotConfig where
    fromEnv _ =
        OrderBotConfig
        <$> (Right . parseCBORSKey <$> env "BOTC_SKEY")
        <*> (fmap fromString <$> envMaybe "BOTC_COLLATERAL")
        <*> envWithMsg ("Invalid Strategy. Must be one of: " ++ show allStrategies) "BOTC_EXECUTION_STRAT"
        <*> (parseArray <$> env "BOTC_ASSET_FILTER")
        <*> envIntWithMsg "BOTC_RESCAN_DELAY"
        <*> env "BOTC_FP_NFT_POLICY"
        <*> env "BOTC_FP_ORDER_VALIDATOR"
        <*> envIntWithMsg "BOTC_MAX_ORDERS_MATCHES"
        <*> envIntWithMsg "BOTC_MAX_TXS_PER_ITERATION"
        <*> envWithMsg "Must be either 'True' or 'False'" "BOTC_RANDOMIZE_MATCHES_FOUND"
        <*> (parsePORDict <$> env "BOTC_POREFS")
      where
        parseCBORSKey :: String -> GYPaymentSigningKey
        parseCBORSKey s =
            either (error . ("Error parsing 'BOTC_SKEY': " ++)) paymentSigningKeyFromApi $
            eitherDecodeStrict (fromString s) >>=
            first show . deserialiseFromTextEnvelope (AsSigningKey AsPaymentKey)

        parsePORDict :: String -> PORConfig
        parsePORDict = either (error . ("Error parsing 'BOTC_POREFS': " ++)) id
                        . eitherDecodeStrict . fromString

        parseArray :: String -> [OrderAssetPair]
        parseArray s = either (error . ("Error parsing 'BOTC_ASSET_FILTER': " ++) ) id $
            eitherDecodeStrict (fromString s) >>=
            Aeson.parseEither parseScanTokenPairs

envIntWithMsg :: Var a => String -> Parser a
envIntWithMsg = envWithMsg "Not a number"

envWithMsg :: Var a => String -> String -> Parser a
envWithMsg msg name = maybe (throwError $ unwords ["Error parsing enviroment variable", name ++ ":", msg]) return =<< envMaybe name

instance FromJSON OrderBotConfig where
    parseJSON (Object obj) = OrderBotConfig
        <$> (Left <$> obj .: "signingKeyFP")
        <*> obj .:? "collateral"
        <*> obj .: "strategy"
        <*> (parseScanTokenPairs =<< obj .: "scanTokens")
        <*> obj .:  "scanDelay"
        <*> obj .:  "nftMintingPolicyFP"
        <*> obj .:  "orderValidatorFP"
        <*> obj .:  "maxOrderMatches"
        <*> obj .:  "maxTxsPerIteration"
        <*> obj .:  "randomizeMatchesFound"
        <*> obj .:  "validatorRefs"

    parseJSON _ = fail "Expecting object value"

parseScanTokenPairs :: Value -> Aeson.Parser [OrderAssetPair]
parseScanTokenPairs = withArray "parseScanTokenPairs" parseArrayTokenPairs

parseArrayTokenPairs :: Array -> Aeson.Parser [OrderAssetPair]
parseArrayTokenPairs = mapM parseObjectTokenPair . V.toList

parseObjectTokenPair :: Value -> Aeson.Parser OrderAssetPair
parseObjectTokenPair = withObject "OrderAssetPair" $ \v -> OAssetPair
    <$> v .: "currencyAsset"
    <*> v .: "commodityAsset"

data PORConfig =
    PORConfig
    { botCRefAddr      :: GYAddress
    , botCRefNft       :: GYAssetClass
    , botCScriptRef    :: Maybe GYTxOutRef
    , botCNftPolicyRef :: Maybe GYTxOutRef
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON PORConfig where
    parseJSON (Object obj) =
        PORConfig
        <$> (addressFromBech32 <$> obj .: "refAddr")
        <*> obj .: "refNftAC"
        <*> obj .:? "scriptRef"
        <*> obj .:? "nftPolicyRef"
    parseJSON _ = fail "Expecting object value"

-- | Given a vanilla order bot configuration, builds a complete order bot setup.
buildOrderBot :: OrderBotConfig -> IO OrderBot
buildOrderBot OrderBotConfig
    { botCSkey
    , botCCollateral
    , botCExecutionStrat
    , botCAssetFilter
    , botCRescanDelay
    , botCMaxOrderMatches
    , botCMaxTxsPerIteration
    , botCRandomizeMatchesFound
    } = do
    skey <- either readPaymentSigningKey return botCSkey
    maxOrderMatch <- intToNatural "Max Order matches amount" botCMaxOrderMatches
    maxTxPerIter <- intToNatural "Max Tx per iteration" botCMaxTxsPerIteration
    oneEquivalentAssetPair <-
        if hasNoneEquivalentAssetPair botCAssetFilter
        then return $ nub botCAssetFilter
        else throwIO $ userError "Can't have equivalent order asset pairs scanTokens"
    return $ OrderBot
        { botSkey            = skey
        , botCollateral      = buildCollateral
        , botExecutionStrat  =
            MultiAssetTraverse $ mkIndependentStrategy botCExecutionStrat maxOrderMatch
        , botAssetPairFilter = nub oneEquivalentAssetPair
        , botRescanDelay     = botCRescanDelay
        , botTakeMatches     = takeMatches botCRandomizeMatchesFound maxTxPerIter
        }
    where
      buildCollateral :: Maybe (GYTxOutRef, Bool)
      buildCollateral = (,False) <$> botCCollateral

      hasNoneEquivalentAssetPair :: [OrderAssetPair] -> Bool
      hasNoneEquivalentAssetPair [] = True
      hasNoneEquivalentAssetPair (oap:oaps) =
          not (any (equivalentAssetPair oap) oaps)
          && hasNoneEquivalentAssetPair oaps

readBotConfig :: Maybe FilePath -> IO OrderBotConfig
readBotConfig = either (throwIO . userError) return <=<
                maybe decodeEnv eitherDecodeFileStrict

intToNatural :: String -> Int -> IO Natural
intToNatural _ i | i > 0 = return $ fromInteger $ toInteger i
intToNatural msg _ = throwIO $ userError $ msg ++ " is negative or zero"

takeMatches :: Bool -> Natural -> [MatchResult] -> IO [MatchResult]
takeMatches r (fromIntegral -> maxTxPerIter) matches =
    take maxTxPerIter <$> if r then shuffleList matches else return matches

shuffleList :: [a] -> IO [a]
shuffleList xs = createSystemSeed
               >>= initialize . fromSeed
               >>= runReaderT (sample (shuffle xs))

-- | Read the compiled scripts from the paths included in the OrderBotConfig
getDexInfo :: OrderBotConfig -> IO DEXInfo
getDexInfo OrderBotConfig{ botCFPNftPolicy
                         , botCFPOrderValidator
                         , botCPORConfig
                         } = do
    dexPolicyRaw    <- readNftPolicy
    dexValidatorRaw <- readOrderValidator

    let partialOrderValidator = mkDEXValidator dexValidatorRaw
                                               (addressToPlutus $ botCRefAddr botCPORConfig)
                                               (botCRefNft botCPORConfig)
        nftPolicy             = mkDEXMintingPolicy dexPolicyRaw partialOrderValidator (addressToPlutus $ botCRefAddr botCPORConfig) (botCRefNft botCPORConfig)
        porefs = PORefs { porRefAddr      = botCRefAddr botCPORConfig
                        , porRefNft       = botCRefNft botCPORConfig
                        , porValidatorRef = botCScriptRef botCPORConfig
                        , porNftPolicyRef = botCNftPolicyRef botCPORConfig
                        }
    return DEXInfo
        { dexPartialOrderValidator = partialOrderValidator
        , dexNftPolicy = nftPolicy
        , dexPORefs = porefs
        }

  where
    readNftPolicy
        :: IO (TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass])
    readNftPolicy = readTypedScript botCFPNftPolicy

    readOrderValidator
        :: IO (TypedScript 'ValidatorRole '[Address, AssetClass])
    readOrderValidator = readTypedScript botCFPOrderValidator
