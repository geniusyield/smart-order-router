{-|
Module      : GeniusYield.OrderBot.DataSource.Providers
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.OrderBot.DataSource.Providers
    ( Connection
    , connectDB
    , closeDB
    , withEachAssetOrders
    ) where

import           Data.List                        (foldl')
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map

import           Control.Monad.Reader             (ReaderT (runReaderT))
import           GeniusYield.Api.Dex.Constants    (DEXInfo (..))
import           GeniusYield.Api.Dex.PartialOrder
import           GeniusYield.OrderBot.Types
import           GeniusYield.TxBuilder
import           GeniusYield.Types

data Connection = Connection !GYNetworkId {-# UNPACK #-} !GYProviders

type OrderData = (# OrderAssetPair, [OrderInfo 'BuyOrder], [OrderInfo 'SellOrder] #)

connectDB :: GYNetworkId -> GYProviders -> IO Connection
connectDB netId providers = pure $ Connection netId providers

closeDB :: Connection -> IO ()
closeDB = const $ return ()

withEachAssetOrders
    :: Connection
    -> DEXInfo
    -> [OrderAssetPair]
    -> (a -> OrderData -> a)
    -> a
    -> IO a
withEachAssetOrders c dex assetFilter f acc = do
  infoMap <- allOrderInfos c dex assetFilter
  pure $
    Map.foldlWithKey'
      ( \acc oaip someOrderInfos ->
          let (buys, sells) =
                foldl'
                  ( \(!buys, !sells) (SomeOrderInfo oInf@OrderInfo {orderType}) -> case orderType of
                      SBuyOrder  -> (oInf : buys, sells)
                      SSellOrder -> (buys, oInf : sells)
                  )
                  ([], [])
                  someOrderInfos
           in f acc (# oaip, buys, sells #)
      )
      acc
      infoMap

runQuery :: Connection -> GYTxQueryMonadNode a -> IO a
runQuery (Connection nid providers) = runGYTxQueryMonadNode nid providers

allOrderInfos
    :: Connection
    -> DEXInfo
    -> [OrderAssetPair]
    -> IO (Map OrderAssetPair [SomeOrderInfo])
allOrderInfos c dex assetPairs = do
    cTime <- getCurrentGYTime

    partialOrderInfos <- runQuery c $
                         runReaderT (partialOrdersWithTransformerPredicate (dexPORefs dex) $ partialOrderFilter cTime) dex

    return $ foldl' f Map.empty partialOrderInfos
  where
    f m (partialOrderInfoToOrderInfo -> info@(SomeOrderInfo OrderInfo {assetInfo})) =
      Map.insertWith (++) assetInfo [info] m

    partialOrderFilter :: GYTime -> PartialOrderInfo -> Maybe (OrderAssetPair, PartialOrderInfo)
    partialOrderFilter cTime poi = if inTimeOrder cTime poi
                                   then filterTokenPair poi
                                   else Nothing

    filterTokenPair :: PartialOrderInfo -> Maybe (OrderAssetPair, PartialOrderInfo)
    filterTokenPair poi@PartialOrderInfo { poiOfferedAsset, poiAskedAsset }
        | assetPair1 `elem` assetPairs = Just (assetPair1, poi)
        | assetPair2 `elem` assetPairs = Just (assetPair2, poi)
        | otherwise = Nothing
      where
        assetPair1 = mkOrderAssetPair poiOfferedAsset poiAskedAsset
        assetPair2 = mkOrderAssetPair poiAskedAsset poiOfferedAsset

    inTimeOrder :: GYTime -> PartialOrderInfo -> Bool
    inTimeOrder time poi = isAfterStart time (poiStart poi)
                           &&
                           isBeforeEnd time (poiEnd poi)

partialOrderInfoToOrderInfo :: (OrderAssetPair, PartialOrderInfo) -> SomeOrderInfo
partialOrderInfoToOrderInfo = uncurry mkOrderInfo

isAfterStart :: GYTime -> Maybe GYTime -> Bool
isAfterStart current = maybe True (current >=)

isBeforeEnd :: GYTime -> Maybe GYTime -> Bool
isBeforeEnd current = maybe True (current <=)
