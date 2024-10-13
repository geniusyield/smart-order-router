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
) where

import           Prelude                         (Monad, (*>), pure)
import           GeniusYield.OrderBot.Types      (OrderInfo)
import           GeniusYield.OrderBot.OrderBook

-- | @foldlM'@ variant for 'Orders' which is strict in accumulator.
foldlMOrders' :: forall a t m. Monad m => (a -> OrderInfo t -> m a) -> a -> Orders t -> m a
foldlMOrders' f = foldlMOrders (\(!acc) -> f acc)

-- | @mapM_@ variant for 'Orders'.
mapMOrders_ :: forall a t m. Monad m => (OrderInfo t -> m a) -> Orders t -> m ()
mapMOrders_ f os = foldlMOrders' (\_ oi -> f oi *> pure ()) () os