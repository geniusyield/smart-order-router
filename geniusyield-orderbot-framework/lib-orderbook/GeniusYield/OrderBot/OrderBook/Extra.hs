{-|
Module      : GeniusYield.OrderBot.OrderBook.Extra
Synopsis    : Extra utilities when working with order books.
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OrderBot.OrderBook.Extra (
  foldlMOrders',
  mapMOrders_,
  lookupBest,
) where

import           Prelude                         (Maybe, Monad, (*>), pure)
import           GeniusYield.OrderBot.Types      (OrderInfo, SOrderTypeI (..), SOrderType (..), OrderType)
import           GeniusYield.OrderBot.OrderBook

-- | @foldlM'@ variant for 'Orders' which is strict in accumulator.
foldlMOrders' :: forall a t m. Monad m => (a -> OrderInfo t -> m a) -> a -> Orders t -> m a
foldlMOrders' f = foldlMOrders (\(!acc) -> f acc)

-- | @mapM_@ variant for 'Orders'.
mapMOrders_ :: forall a t m. Monad m => (OrderInfo t -> m a) -> Orders t -> m ()
mapMOrders_ f os = foldlMOrders' (\_ oi -> f oi *> pure ()) () os

-- | In case we have buy orders, return the best buy order (highest price). And in case we have sell orders, return the best sell order (lowest price).
lookupBest :: forall (t :: OrderType). SOrderTypeI t => Orders t -> Maybe (OrderInfo t)
lookupBest os = case (sOrderType @t) of
  SBuyOrder  -> highestBuyMaybe os
  SSellOrder -> lowestSellMaybe os