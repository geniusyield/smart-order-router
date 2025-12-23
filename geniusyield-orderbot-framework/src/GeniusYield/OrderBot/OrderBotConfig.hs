{- |
Module      : GeniusYield.OrderBot.OrderBotConfig
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OrderBot.OrderBotConfig where

import Cardano.Api (
  AsType (AsPaymentKey, AsSigningKey),
  deserialiseFromTextEnvelope,
 )
import Control.Exception (throwIO)
import Control.Monad ((<=<))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (
  Array,
  FromJSON (parseJSON),
  Value (Object),
  eitherDecodeFileStrict,
  eitherDecodeStrict,
  withArray,
  withObject,
  (.:),
  (.:?),
 )
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (first)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Random (sample, shuffle)
import Data.String (IsString (..))
import qualified Data.Vector as V
import Data.Word (Word64)
import GHC.Generics (Generic)
import GeniusYield.OrderBot
import GeniusYield.OrderBot.MatchingStrategy (MatchResult)
import GeniusYield.OrderBot.Strategies (BotStrategy, allStrategies, mkIndependentStrategy)
import GeniusYield.OrderBot.Types (
  OrderAssetPair (..),
  equivalentAssetPair,
 )
import GeniusYield.Types
import System.Envy (
  FromEnv (fromEnv),
  Parser,
  Var,
  decodeEnv,
  env,
  envMaybe,
 )
import System.Random.MWC (createSystemSeed, fromSeed, initialize)

-- | Order bot vanilla config.
data OrderBotConfig
  = OrderBotConfig
  { botCSkey :: Either FilePath GYPaymentSigningKey
  -- ^ Signing key of the bot.
  , botCStakeAddress :: Maybe GYStakeAddressBech32
  -- ^ Optional bech32 encoded stake address.
  , botCCollateral :: Maybe GYTxOutRef
  {- ^ UTxO ref of the collateral UTxO in the bot's wallet.

         NOTE: If collateral is Nothing, then Atlas will choose some UTxO to
          function as collateral. If a TxOutRef is given, the bool indicates wheter
          the collateral can be spent in the tx.
  -}
  , botCExecutionStrat :: BotStrategy
  -- ^ Name of the running strategy.
  , botCAssetFilter :: [OrderAssetPair]
  -- ^ List of asset pairs to scan.
  , botCRescanDelay :: Int
  {- ^ The duration (microseconds) of time we wait before re-initiating a
         complete iteration for the bot.
  -}
  , botCMaxOrderMatches :: Int
  -- ^ The maximum amount of orders to be matched into a single transaction.
  , botCMaxTxsPerIteration :: Int
  {- ^ The maximum amount of transactions that the bot will build, sign and
         submit in each iteration.
  -}
  , botCRandomizeMatchesFound :: Bool
  {- ^ A boolean that dictates whether the bot chooses the tx to submit at
         random (to decrease collisions), or not (to maximize profit)
  -}
  , botCLovelaceWarningThreshold :: Maybe Natural
  -- ^ If bot's lovelace balance falls below this value, bot would log warning logs.
  , botCTokenInfos :: Maybe (Map GYAssetClass AssetInfo)
  -- ^ Token registry information. Since prices given by provider are usually in display units, we need information such as registered decimal places to know lovelace value per indivisible token unit.
  }
  deriving stock (Show, Generic)

instance FromEnv OrderBotConfig where
  fromEnv _ =
    (OrderBotConfig . Right . parseCBORSKey <$> env "BOTC_SKEY")
      <*> (fmap fromString <$> envMaybe "BOTC_STAKE_ADDRESS")
      <*> (fmap fromString <$> envMaybe "BOTC_COLLATERAL")
      <*> envWithMsg ("Invalid Strategy. Must be one of: " ++ show allStrategies) "BOTC_EXECUTION_STRAT"
      <*> (parseArray <$> env "BOTC_ASSET_FILTER")
      <*> envIntWithMsg "BOTC_RESCAN_DELAY"
      <*> envIntWithMsg "BOTC_MAX_ORDERS_MATCHES"
      <*> envIntWithMsg "BOTC_MAX_TXS_PER_ITERATION"
      <*> envWithMsg "Must be either 'True' or 'False'" "BOTC_RANDOMIZE_MATCHES_FOUND"
      -- Apparently, there is no `Var` instance for `Natural` in `System.Envy`.
      <*> (fmap (fromIntegral @Word64 @Natural) <$> envMaybe "BOTC_LOVELACE_WARNING_THRESHOLD")
      <*> (fmap forceFromJson <$> envMaybe "BOTC_TOKEN_INFOS")
   where
    parseCBORSKey :: String -> GYPaymentSigningKey
    parseCBORSKey s =
      either (error . ("Error parsing 'BOTC_SKEY': " ++)) paymentSigningKeyFromApi $
        eitherDecodeStrict (fromString s)
          >>= first show . deserialiseFromTextEnvelope (AsSigningKey AsPaymentKey)

    parseArray :: String -> [OrderAssetPair]
    parseArray s =
      either (error . ("Error parsing 'BOTC_ASSET_FILTER': " ++)) id $
        eitherDecodeStrict (fromString s)
          >>= Aeson.parseEither parseScanTokenPairs

    forceFromJson :: FromJSON a => String -> a
    forceFromJson = either error id . eitherDecodeStrict . fromString

envIntWithMsg :: Var a => String -> Parser a
envIntWithMsg = envWithMsg "Not a number"

envWithMsg :: Var a => String -> String -> Parser a
envWithMsg msg name = maybe (throwError $ unwords ["Error parsing enviroment variable", name ++ ":", msg]) return =<< envMaybe name

instance FromJSON OrderBotConfig where
  parseJSON (Object obj) =
    (OrderBotConfig . Left <$> (obj .: "signingKeyFP"))
      <*> obj .:? "stakeAddress"
      <*> obj .:? "collateral"
      <*> obj .: "strategy"
      <*> (parseScanTokenPairs =<< obj .: "scanTokens")
      <*> obj .: "scanDelay"
      <*> obj .: "maxOrderMatches"
      <*> obj .: "maxTxsPerIteration"
      <*> obj .: "randomizeMatchesFound"
      <*> obj .:? "lovelaceWarningThreshold"
      <*> obj .:? "tokenInfos"
  parseJSON _ = fail "Expecting object value"

parseScanTokenPairs :: Value -> Aeson.Parser [OrderAssetPair]
parseScanTokenPairs = withArray "parseScanTokenPairs" parseArrayTokenPairs

parseArrayTokenPairs :: Array -> Aeson.Parser [OrderAssetPair]
parseArrayTokenPairs = mapM parseObjectTokenPair . V.toList

parseObjectTokenPair :: Value -> Aeson.Parser OrderAssetPair
parseObjectTokenPair = withObject "OrderAssetPair" $ \v ->
  OAssetPair
    <$> v .: "currencyAsset"
    <*> v .: "commodityAsset"

-- | Given a vanilla order bot configuration, builds a complete order bot setup.
buildOrderBot :: OrderBotConfig -> IO OrderBot
buildOrderBot OrderBotConfig {..} = do
  skey <- either readPaymentSigningKey return botCSkey
  maxOrderMatch <- intToNatural "Max Order matches amount" botCMaxOrderMatches
  maxTxPerIter <- intToNatural "Max Tx per iteration" botCMaxTxsPerIteration
  oneEquivalentAssetPair <-
    if hasNoneEquivalentAssetPair botCAssetFilter
      then return $ nub botCAssetFilter
      else throwIO $ userError "Can't have equivalent order asset pairs scanTokens"
  return $
    OrderBot
      { botSkey = skey
      , botStakeAddress = botCStakeAddress
      , botCollateral = buildCollateral
      , botExecutionStrat =
          MultiAssetTraverse $ mkIndependentStrategy botCExecutionStrat maxOrderMatch
      , botAssetPairFilter = nub oneEquivalentAssetPair
      , botRescanDelay = botCRescanDelay
      , botTakeMatches = takeMatches botCRandomizeMatchesFound maxTxPerIter
      , botLovelaceWarningThreshold = botCLovelaceWarningThreshold
      , botTokenInfos = fromMaybe mempty botCTokenInfos
      }
 where
  buildCollateral :: Maybe (GYTxOutRef, Bool)
  buildCollateral = (,False) <$> botCCollateral

  hasNoneEquivalentAssetPair :: [OrderAssetPair] -> Bool
  hasNoneEquivalentAssetPair [] = True
  hasNoneEquivalentAssetPair (oap : oaps) =
    not (any (equivalentAssetPair oap) oaps)
      && hasNoneEquivalentAssetPair oaps

readBotConfig :: Maybe FilePath -> IO OrderBotConfig
readBotConfig =
  either (throwIO . userError) return
    <=< maybe decodeEnv eitherDecodeFileStrict

intToNatural :: String -> Int -> IO Natural
intToNatural _ i | i > 0 = return $ fromInteger $ toInteger i
intToNatural msg _ = throwIO $ userError $ msg ++ " is negative or zero"

takeMatches :: Bool -> Natural -> [MatchResult] -> IO [MatchResult]
takeMatches r (fromIntegral -> maxTxPerIter) matches =
  take maxTxPerIter <$> if r then shuffleList matches else return matches

shuffleList :: [a] -> IO [a]
shuffleList xs =
  createSystemSeed
    >>= initialize . fromSeed
    >>= runReaderT (sample (shuffle xs))
