{-# LANGUAGE RecordWildCards #-}

module GeniusYield.OrderBot.OrderBook.AnnSet (
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

import Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Foldable (foldl', foldlM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified GeniusYield.AnnSet.Internal as AnnSet
import GeniusYield.Api.DEX.Constants (DEXInfo)
import GeniusYield.OrderBot.DataSource (Connection, withEachAssetOrders)
import GeniusYield.OrderBot.Types
import GeniusYield.Types (showTxOutRef)

type MultiAssetOrderBook = Map OrderAssetPair OrderBook

mkMultiAssetOrderBook :: [(OrderAssetPair, OrderBook)] -> MultiAssetOrderBook
mkMultiAssetOrderBook = M.fromList

maOrderBookToList :: MultiAssetOrderBook -> [(OrderAssetPair, OrderBook)]
maOrderBookToList = M.toList

{- Why can't this be a type synonym?

In general, types declared as 'data' in the signature can be type synonyms in the actual impl
_only if they are not parameterized_. Since 'Orders' is declared as a parameterized 'data' type
in the signature - it cannot be a type synonym in the implementation.

The reasoning is of course obvious: the signature expects a type that satisfied the generativity
axiom, i.e a type that can be partially applied. However, type synonyms violate this rule - and thus,
an implementation may not use a type synonym in place of a parameterized data type.
-}
newtype Orders t = Orders {unOrders :: AnnSet.Orders t}
  deriving newtype (Eq, Show)

data OrderBook = OrderBook
  { sellOrders :: Orders 'SellOrder
  , buyOrders :: Orders 'BuyOrder
  }
  deriving stock (Show, Eq)

instance ToJSON OrderBook where
  toJSON OrderBook {sellOrders, buyOrders} =
    Aeson.object
      [ "sellOrders" .= sellOrders
      , "buyOrders" .= buyOrders
      ]

instance ToJSON (Orders t) where
  toJSON = Aeson.listValue sOrderToValue . AnnSet.toAscList . unOrders
   where
    sOrderToValue :: AnnSet.OrderView t -> Aeson.Value
    sOrderToValue AnnSet.OrderView {orderInfo} =
      let v = orderVolume orderInfo
       in Aeson.object
            [ "orderRef" .= showTxOutRef (orderRef orderInfo)
            , "orderType" .= show (orderType orderInfo)
            , "orderAssets" .= orderAsset orderInfo
            , "volume"
                .= Aeson.object
                  [ "min" .= volumeMin v
                  , "max" .= volumeMax v
                  ]
            , "price" .= show (getPrice $ orderPrice orderInfo)
            ]

populateOrderBook ::
  Connection ->
  DEXInfo ->
  [OrderAssetPair] ->
  IO MultiAssetOrderBook
populateOrderBook conn dex f = do
  multiAssetBookL <-
    withEachAssetOrders
      conn
      dex
      f
      buildOrderBookList
      []
  pure $ mkMultiAssetOrderBook multiAssetBookL

buildOrderBookList ::
  [(OrderAssetPair, OrderBook)] ->
  (# OrderAssetPair, [OrderInfo 'BuyOrder], [OrderInfo 'SellOrder] #) ->
  [(OrderAssetPair, OrderBook)]
buildOrderBookList acc (# _, _, [] #) = acc
buildOrderBookList acc (# _, [], _ #) = acc
buildOrderBookList acc (# oap, buyOrders, sellOrders #) =
  let buyOrders' = Orders $ AnnSet.mkOrders buyOrders
      sellOrders' = Orders $ AnnSet.mkOrders sellOrders
   in (oap, OrderBook sellOrders' buyOrders') : acc

emptyOrders :: Orders t
emptyOrders = Orders AnnSet.empty

unconsOrders :: Orders t -> Maybe (OrderInfo t, Orders t)
unconsOrders (Orders AnnSet.Empty) = Nothing
unconsOrders (Orders (AnnSet.Entry _ _ a l r)) = Just (AnnSet.orderInfo a, Orders $ AnnSet.glue l r)

insertOrder :: OrderInfo t -> Orders t -> Orders t
insertOrder oi = case (orderType oi) of
  SSellOrder -> Orders . AnnSet.insert (AnnSet.mkOrderView oi) . unOrders
  SBuyOrder -> Orders . AnnSet.insert (AnnSet.mkOrderView oi) . unOrders

deleteOrder :: OrderInfo t -> Orders t -> Orders t
deleteOrder oi = case (orderType oi) of
  SSellOrder -> Orders . AnnSet.delete (AnnSet.mkOrderView oi) . unOrders
  SBuyOrder -> Orders . AnnSet.delete (AnnSet.mkOrderView oi) . unOrders

lowestSell :: Orders 'SellOrder -> OrderInfo 'SellOrder
lowestSell = AnnSet.orderInfo . AnnSet.findMin . unOrders

lowestSellMaybe :: Orders 'SellOrder -> Maybe (OrderInfo 'SellOrder)
lowestSellMaybe = fmap AnnSet.orderInfo . AnnSet.lookupMin . unOrders

highestBuy :: Orders 'BuyOrder -> OrderInfo 'BuyOrder
highestBuy = AnnSet.orderInfo . AnnSet.findMin . unOrders

highestBuyMaybe :: Orders 'BuyOrder -> Maybe (OrderInfo 'BuyOrder)
highestBuyMaybe = fmap AnnSet.orderInfo . AnnSet.lookupMin . unOrders

withoutTip :: Orders t -> Orders t
withoutTip (Orders AnnSet.Empty) = Orders AnnSet.Empty
withoutTip (Orders (AnnSet.Entry _ _ _ l r)) = Orders (AnnSet.glue l r)

foldlOrders :: forall a t. (a -> OrderInfo t -> a) -> a -> Orders t -> a
foldlOrders f e (Orders os) = foldl' g e os
 where
  g :: (a -> AnnSet.OrderView t -> a)
  g a AnnSet.OrderView {..} = f a orderInfo

foldrOrders :: forall a t. (OrderInfo t -> a -> a) -> a -> Orders t -> a
foldrOrders f e (Orders os) = foldr (f . AnnSet.orderInfo) e os

foldlMOrders :: forall a t m. Monad m => (a -> OrderInfo t -> m a) -> a -> Orders t -> m a
foldlMOrders f e (Orders os) = foldlM g e os
 where
  g :: (a -> AnnSet.OrderView t -> m a)
  g a AnnSet.OrderView {..} = f a orderInfo

filterOrders :: (OrderInfo t -> Bool) -> Orders t -> Orders t
filterOrders p = Orders . AnnSet.filter (p . AnnSet.orderInfo) . unOrders

ordersLTPrice :: Price -> Orders t -> Orders t
ordersLTPrice maxPrice = Orders . AnnSet.takeWhileAntitone ((< maxPrice) . AnnSet.orderPrice) . unOrders

ordersLTEPrice :: Price -> Orders t -> Orders t
ordersLTEPrice maxPrice = Orders . AnnSet.takeWhileAntitone ((<= maxPrice) . AnnSet.orderPrice) . unOrders

ordersGTPrice :: Price -> Orders t -> Orders t
ordersGTPrice minPrice = Orders . AnnSet.takeWhileAntitone ((> minPrice) . AnnSet.orderPrice) . unOrders

ordersGTEPrice :: Price -> Orders t -> Orders t
ordersGTEPrice minPrice = Orders . AnnSet.takeWhileAntitone ((>= minPrice) . AnnSet.orderPrice) . unOrders

volumeLTPrice :: Price -> Orders t -> Volume
volumeLTPrice maxPrice = AnnSet.totalVolume . AnnSet.measure . unOrders . ordersLTPrice maxPrice

volumeLTEPrice :: Price -> Orders t -> Volume
volumeLTEPrice maxPrice = AnnSet.totalVolume . AnnSet.measure . unOrders . ordersLTEPrice maxPrice

volumeGTPrice :: Price -> Orders t -> Volume
volumeGTPrice minPrice = AnnSet.totalVolume . AnnSet.measure . unOrders . ordersGTPrice minPrice

volumeGTEPrice :: Price -> Orders t -> Volume
volumeGTEPrice minPrice = AnnSet.totalVolume . AnnSet.measure . unOrders . ordersGTEPrice minPrice

nullOrders :: Orders t -> Bool
nullOrders = null . unOrders

withEachAsset :: (OrderAssetPair -> OrderBook -> [a]) -> MultiAssetOrderBook -> [a]
withEachAsset f = M.foldrWithKey (\p b acc -> f p b ++ acc) mempty
