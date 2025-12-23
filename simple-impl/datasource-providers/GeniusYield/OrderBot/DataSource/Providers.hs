{- |
Module      : GeniusYield.OrderBot.DataSource.Providers
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OrderBot.DataSource.Providers (
  Connection,
  connectDB,
  closeDB,
  withEachAssetOrders,
) where

import Control.Monad
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.List (foldl')
import Data.Map.Strict (Map, unionWith)
import qualified Data.Map.Strict as Map
import GeniusYield.Api.DEX.Constants (DEXInfo (..))
import GeniusYield.Api.DEX.PartialOrder
import GeniusYield.Api.DEX.TwoWayOrder
import GeniusYield.OrderBot.Types
import GeniusYield.TxBuilder
import GeniusYield.Types

data Connection = Connection !GYNetworkId {-# UNPACK #-} !GYProviders

type OrderData = (# OrderAssetPair, [OrderInfo 'BuyOrder], [OrderInfo 'SellOrder] #)

connectDB :: GYNetworkId -> GYProviders -> IO Connection
connectDB netId providers = pure $ Connection netId providers

closeDB :: Connection -> IO ()
closeDB = const $ return ()

withEachAssetOrders ::
  Connection ->
  DEXInfo ->
  [OrderAssetPair] ->
  (a -> OrderData -> a) ->
  a ->
  IO a
withEachAssetOrders c dex assetFilter f acc = do
  infoMap <- allOrderInfos c dex assetFilter
  pure $
    Map.foldlWithKey'
      ( \acc oaip someOrderInfos ->
          let (buys, sells) =
                foldl'
                  ( \(!buys, !sells) (SomeOrderInfo oInf@OrderInfo {orderType}) -> case orderType of
                      SBuyOrder -> (oInf : buys, sells)
                      SSellOrder -> (buys, oInf : sells)
                  )
                  ([], [])
                  someOrderInfos
           in f acc (# oaip, buys, sells #)
      )
      acc
      infoMap

runQuery :: Connection -> GYTxQueryMonadIO a -> IO a
runQuery (Connection nid providers) = runGYTxQueryMonadIO nid providers

allOrderInfos ::
  Connection ->
  DEXInfo ->
  [OrderAssetPair] ->
  IO (Map OrderAssetPair [SomeOrderInfo])
allOrderInfos c dex assetPairs = do
  cTime <- getCurrentGYTime
  partialOrderInfos <- runQuery c $ runReaderT (partialOrdersWithTransformerPredicate (dexPORefs dex) (partialOrderFilter cTime)) (dexScripts dex)
  twoWayOrderInfos <- runQuery c $ runReaderT (twoWayOrdersWithTransformerPredicate (dexTWORefs dex) (twoWayOrderFilter cTime)) (dexScripts dex)
  let m1 = foldl' f Map.empty partialOrderInfos
  m2 <- foldM g Map.empty twoWayOrderInfos
  forM_ (Map.toList m2) (\(i, sois) -> (forM_ sois (\(SomeOrderInfo oi) -> print i >> ppOrderInfo oi)))
  return $ unionWith (++) m1 m2
 where
  f m (partialOrderInfoToOrderInfo -> info@(SomeOrderInfo OrderInfo {orderAsset})) = Map.insertWith (++) orderAsset [info] m
  g m x = do
    twois <- twoWayOrderInfoToOrderInfo x
    case twois of
      [info@(SomeOrderInfo OrderInfo {orderAsset})] -> do
        return $ Map.insertWith (++) orderAsset [info] m
      [info1@(SomeOrderInfo oi1), info2@(SomeOrderInfo oi2)] -> do
        return $ Map.insertWith (++) (orderAsset oi2) [info2] (Map.insertWith (++) (orderAsset oi1) [info1] m)
      _ -> error "Two Way Orders with wrong token pair matched"

  partialOrderFilter :: GYTime -> PartialOrderInfo -> Maybe (OrderAssetPair, PartialOrderInfo)
  partialOrderFilter cTime poi = if inTimeOrderPartial cTime poi then filterTokenPairPartial poi else Nothing

  twoWayOrderFilter :: GYTime -> TwoWayOrderInfo -> Maybe (OrderAssetPair, TwoWayOrderInfo)
  twoWayOrderFilter cTime twoi = if inTimeOrderTwoWay cTime twoi then filterTokenPairTwoWay twoi else Nothing

  filterTokenPairPartial :: PartialOrderInfo -> Maybe (OrderAssetPair, PartialOrderInfo)
  filterTokenPairPartial poi@PartialOrderInfo {poiOfferedAsset, poiAskedAsset}
    | assetPair1 `elem` assetPairs = Just (assetPair1, poi)
    | assetPair2 `elem` assetPairs = Just (assetPair2, poi)
    | otherwise = Nothing
   where
    assetPair1 = mkOrderAssetPair poiOfferedAsset poiAskedAsset
    assetPair2 = mkOrderAssetPair poiAskedAsset poiOfferedAsset

  filterTokenPairTwoWay :: TwoWayOrderInfo -> Maybe (OrderAssetPair, TwoWayOrderInfo)
  filterTokenPairTwoWay twoi
    | assetPair1 `elem` assetPairs = Just (assetPair1, twoi)
    | assetPair2 `elem` assetPairs = Just (assetPair2, twoi)
    | otherwise = Nothing
   where
    twoiOfferedAsset = oaStraightAsset (extractOrderAssets twoi)
    twoiAskedAsset = oaReverseAsset (extractOrderAssets twoi)
    assetPair1 = mkOrderAssetPair twoiOfferedAsset twoiAskedAsset
    assetPair2 = mkOrderAssetPair twoiAskedAsset twoiOfferedAsset

  inTimeOrderPartial :: GYTime -> PartialOrderInfo -> Bool
  inTimeOrderPartial time poi = isAfterStart time (poiStart poi) && isBeforeEnd time (poiEnd poi)

  inTimeOrderTwoWay :: GYTime -> TwoWayOrderInfo -> Bool
  inTimeOrderTwoWay time twoi = isAfterStart time (twoiStart twoi) && isBeforeEnd time (twoiEnd twoi)

partialOrderInfoToOrderInfo :: (OrderAssetPair, PartialOrderInfo) -> SomeOrderInfo
partialOrderInfoToOrderInfo = uncurry mkOrderInfoPO

twoWayOrderInfoToOrderInfo :: (OrderAssetPair, TwoWayOrderInfo) -> IO [SomeOrderInfo]
twoWayOrderInfoToOrderInfo (oap, twoi) = do
  soi <- mkOrderInfoTWO oap twoi
  case soi of
    Left os -> pure [os]
    Right (soi1@(SomeOrderInfo oi1), soi2@(SomeOrderInfo oi2)) -> do
      let p = getPrice (orderPrice oi1) * getPrice (orderPrice oi2)
      pure $ if p > 1 then [] else [soi1, soi2]

isAfterStart :: GYTime -> Maybe GYTime -> Bool
isAfterStart current = maybe True (current >=)

isBeforeEnd :: GYTime -> Maybe GYTime -> Bool
isBeforeEnd current = maybe True (current <=)
