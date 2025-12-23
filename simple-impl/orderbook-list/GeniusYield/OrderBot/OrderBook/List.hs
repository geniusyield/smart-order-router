{- |
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
  emptyOrders,
  unconsOrders,
  insertOrder,
  deleteOrder,

  -- * Order book queries
  lowestSell,
  lowestSellMaybe,
  highestBuy,
  highestBuyMaybe,
  withoutTip,
  foldlOrders,
  foldrOrders,
  foldlMOrders,
  filterOrders,
  ordersLTPrice,
  ordersLTEPrice,
  ordersGTPrice,
  ordersGTEPrice,
  volumeLTPrice,
  volumeLTEPrice,
  volumeGTPrice,
  volumeGTEPrice,
  nullOrders,

  -- * MultiAssetOrderBook reading utilities
  withEachAsset,
) where

import Data.Aeson (ToJSON, object, toJSON)
import Data.Foldable (foldl', foldlM)
import Data.List (delete, insertBy, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Ord (Down (Down))
import GeniusYield.Api.DEX.Constants (DEXInfo)
import GeniusYield.OrderBot.DataSource (
  Connection,
  withEachAssetOrders,
 )
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

populateOrderBook ::
  Connection ->
  DEXInfo ->
  [OrderAssetPair] ->
  IO MultiAssetOrderBook
populateOrderBook conn dex f = do
  multiAssetBookL <- withEachAssetOrders conn dex f buildOrderBookList []
  pure $ mkMultiAssetOrderBook multiAssetBookL

buildOrderBookList ::
  [(OrderAssetPair, OrderBook)] ->
  (# OrderAssetPair, [OrderInfo 'BuyOrder], [OrderInfo 'SellOrder] #) ->
  [(OrderAssetPair, OrderBook)]
buildOrderBookList acc (# _, _, [] #) = acc
buildOrderBookList acc (# _, [], _ #) = acc
buildOrderBookList acc (# oap, buyOrders, sellOrders #) =
  ( oap
  , OrderBook
      (Orders $ sortOn orderPrice sellOrders)
      (Orders $ sortOn (Down . orderPrice) buyOrders)
  )
    : acc

emptyOrders :: Orders t
emptyOrders = Orders []

unconsOrders :: Orders t -> Maybe (OrderInfo t, Orders t)
unconsOrders (Orders []) = Nothing
unconsOrders (Orders (x : xs)) = Just (x, Orders xs)

insertOrder :: OrderInfo t -> Orders t -> Orders t
insertOrder oi (Orders os) = Orders $
  case orderType oi of
    SBuyOrder -> insertBy (\oadd opresent -> compare (orderPrice opresent) (orderPrice oadd)) oi os
    SSellOrder -> insertBy (\oadd opresent -> compare (orderPrice oadd) (orderPrice opresent)) oi os

deleteOrder :: OrderInfo t -> Orders t -> Orders t
deleteOrder oi (Orders os) = Orders $ delete oi os

lowestSell :: Orders 'SellOrder -> OrderInfo 'SellOrder
lowestSell = head . unOrders

lowestSellMaybe :: Orders 'SellOrder -> Maybe (OrderInfo 'SellOrder)
lowestSellMaybe = listToMaybe . unOrders

highestBuy :: Orders 'BuyOrder -> OrderInfo 'BuyOrder
highestBuy = head . unOrders

highestBuyMaybe :: Orders 'BuyOrder -> Maybe (OrderInfo 'BuyOrder)
highestBuyMaybe = listToMaybe . unOrders

withoutTip :: Orders t -> Orders t
withoutTip = Orders . drop 1 . unOrders

foldlOrders :: forall a t. (a -> OrderInfo t -> a) -> a -> Orders t -> a
foldlOrders f e = foldl' f e . unOrders

foldlMOrders :: forall a t m. Monad m => (a -> OrderInfo t -> m a) -> a -> Orders t -> m a
foldlMOrders f e = foldlM f e . unOrders

foldrOrders :: forall a t. (OrderInfo t -> a -> a) -> a -> Orders t -> a
foldrOrders f e = foldr f e . unOrders

filterOrders :: (OrderInfo t -> Bool) -> Orders t -> Orders t
filterOrders f = Orders . filter f . unOrders

nullOrders :: Orders t -> Bool
nullOrders = null . unOrders

ordersLTPrice :: Price -> Orders t -> Orders t
ordersLTPrice maxPrice = Orders . filter (\oi -> orderPrice oi < maxPrice) . unOrders

ordersLTEPrice :: Price -> Orders t -> Orders t
ordersLTEPrice maxPrice = Orders . filter (\oi -> orderPrice oi <= maxPrice) . unOrders

ordersGTPrice :: Price -> Orders t -> Orders t
ordersGTPrice maxPrice = Orders . filter (\oi -> orderPrice oi > maxPrice) . unOrders

ordersGTEPrice :: Price -> Orders t -> Orders t
ordersGTEPrice maxPrice = Orders . filter (\oi -> orderPrice oi >= maxPrice) . unOrders

sumVolumes :: [OrderInfo t] -> Volume
sumVolumes = foldMap orderVolume

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
