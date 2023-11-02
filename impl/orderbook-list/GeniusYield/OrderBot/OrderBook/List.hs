{-|
Module      : GeniusYield.OrderBot.OrderBook.List
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.OrderBot.OrderBook.List (
  -- * Core Order book types
  MultiAssetOrderBook,
  mkMultiAssetOrderBook,
  maOrderBookToList,
  OrderBook (..),

  -- * Order book components
  Orders (..),

  -- * Order book construction
  populateOrderBook,
  buildOrderBookList,

  -- * Order book queries
  lowestSell,
  highestBuy,
  withoutTip,
  foldlOrders,
  foldrOrders,
  ordersLTPrice,
  ordersLTEPrice,
  ordersGTPrice,
  ordersGTEPrice,
  volumeLTPrice,
  volumeLTEPrice,
  volumeGTPrice,
  volumeGTEPrice,

  -- * MultiAssetOrderBook reading utilities
  withEachAsset,
) where

import Data.Aeson (ToJSON, toJSON, object)
import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord (Down (Down))

import GeniusYield.OrderBot.DataSource (Connection, DEX, withEachAssetOrders)
import GeniusYield.OrderBot.Types

type MultiAssetOrderBook = Map OrderAssetPair OrderBook

mkMultiAssetOrderBook :: [(OrderAssetPair, OrderBook)] -> MultiAssetOrderBook
mkMultiAssetOrderBook = M.fromList

maOrderBookToList :: MultiAssetOrderBook -> [(OrderAssetPair, OrderBook)]
maOrderBookToList = M.toList

newtype Orders t = Orders {unOrders :: [OrderInfo t]}
  deriving newtype (Eq, Show)

data OrderBook = OrderBook
  { sellOrders :: Orders 'SellOrder
  , buyOrders :: Orders 'BuyOrder
  }
  deriving stock (Show, Eq)

instance ToJSON OrderBook where
    toJSON _ = object []

populateOrderBook
    :: Connection
    -> DEX
    -> [OrderAssetPair]
    -> IO MultiAssetOrderBook
populateOrderBook conn dex f = do
  multiAssetBookL <-
    withEachAssetOrders
      conn
      dex
      f
      buildOrderBookList
      []
  pure $ mkMultiAssetOrderBook multiAssetBookL

buildOrderBookList
  :: [(OrderAssetPair, OrderBook)]
  -> (# OrderAssetPair, [OrderInfo 'BuyOrder], [OrderInfo 'SellOrder] #)
  -> [(OrderAssetPair, OrderBook)]
buildOrderBookList acc (# _, _, [] #) = acc
buildOrderBookList acc (# _, [], _ #) = acc
buildOrderBookList acc (# oap, buyOrders, sellOrders #) =
  (oap, OrderBook (Orders $ sortOn price sellOrders)
                  (Orders $ sortOn (Down . price) buyOrders)) : acc

lowestSell :: Orders 'SellOrder -> OrderInfo 'SellOrder
lowestSell = head . unOrders

highestBuy :: Orders 'BuyOrder -> OrderInfo 'BuyOrder
highestBuy = head . unOrders

withoutTip :: Orders t -> Orders t
withoutTip = Orders . drop 1 . unOrders

foldlOrders :: forall a t. (a -> OrderInfo t -> a) -> a -> Orders t -> a
foldlOrders f e = foldl' f e . unOrders

foldrOrders :: forall a t. (OrderInfo t -> a -> a) -> a -> Orders t -> a
foldrOrders f e = foldr f e . unOrders

ordersLTPrice :: Price -> Orders t -> Orders t
ordersLTPrice maxPrice = Orders . filter (\oi -> price oi < maxPrice) . unOrders

ordersLTEPrice :: Price -> Orders t -> Orders t
ordersLTEPrice maxPrice = Orders . filter (\oi -> price oi <= maxPrice) . unOrders

ordersGTPrice :: Price -> Orders t -> Orders t
ordersGTPrice maxPrice = Orders . filter (\oi -> price oi > maxPrice) . unOrders

ordersGTEPrice :: Price -> Orders t -> Orders t
ordersGTEPrice maxPrice = Orders . filter (\oi -> price oi >= maxPrice) . unOrders

sumVolumes :: [OrderInfo t] -> Volume
sumVolumes = foldMap volume

volumeLTPrice :: Price -> Orders t -> Volume
volumeLTPrice p = sumVolumes . unOrders . ordersLTPrice p

volumeLTEPrice :: Price -> Orders t -> Volume
volumeLTEPrice p = sumVolumes . unOrders . ordersLTEPrice p

volumeGTPrice :: Price -> Orders t -> Volume
volumeGTPrice p = sumVolumes . unOrders . ordersGTPrice p

volumeGTEPrice :: Price -> Orders t -> Volume
volumeGTEPrice p = sumVolumes . unOrders . ordersGTEPrice p

withEachAsset :: (OrderAssetPair -> OrderBook -> [a]) -> MultiAssetOrderBook -> [a]
withEachAsset f = M.foldrWithKey (\p b acc -> f p b ++ acc) mempty
