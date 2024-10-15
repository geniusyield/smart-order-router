module Tests.Prop.Orderbook where

import Control.Monad.Identity (Identity (..))
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as M

import GeniusYield.OrderBot.OrderBook.List
import GeniusYield.OrderBot.Types
import Tests.Prop.Utils

{- | This property checks that the sell order with the lowest price (lowestSell)
    reported by the OrderBook is the same as the lowest sell generated.
-}
propLowestSell :: Property
propLowestSell = forAllShrink genOrderInfos shrinkTuple $
  \(oap, bOrders, sOrders) -> M.monadic (\(Identity p) -> p) $ do
    let book = buildOrderBookList [] (# oap, bOrders, sOrders #)
    M.monitor (counterexample (unlines ["BOOK:", show book, "Selected Lowest: ", show $ map lowSell book]))
      >> M.assert (all (\b -> price (lowSell b) == foldl minSellOrder (price $ head sOrders) sOrders) book)
 where
  lowSell b = lowestSell $ sellOrders (snd b)
  minSellOrder acc x = min acc (price x)

{- | This property checks that the buy order with the highest price (highestBuy)
    reported by the OrderBook is the same as the highest buy generated.
-}
propHighestBuy :: Property
propHighestBuy = forAllShrink genOrderInfos shrinkTuple $
  \(oap, bOrders, sOrders) -> M.monadic (\(Identity p) -> p) $ do
    let book = buildOrderBookList [] (# oap, bOrders, sOrders #)
    M.monitor (counterexample (unlines ["BOOK:", show book, "Selected Highest: ", show $ map highBuy book]))
      >> M.assert (all (\b -> price (highBuy b) == foldl maxBuyOrder (price $ head bOrders) bOrders) book)
 where
  highBuy b = highestBuy $ buyOrders (snd b)
  maxBuyOrder acc x = max acc (price x)

{- | This property checks that the sell orders are reported in increasing order
    by foldlOrders
-}
propSellsAreInOrder :: Property
propSellsAreInOrder = forAllShrink genOrderInfos shrinkTuple $
  \(oap, bOrders, sOrders) -> M.monadic (\(Identity p) -> p) $ do
    let book = buildOrderBookList [] (# oap, bOrders, sOrders #)
    M.monitor (counterexample (unlines ["BOOK:", show book]))
      >> M.assert (all sellsAreOrdered book)
 where
  sellsAreOrdered b = snd $ foldlOrders (\(o', isOrdered) o -> (o, isOrdered && price o' <= price o)) (lowSell b, True) (sellOrders (snd b))
  lowSell b = lowestSell $ sellOrders (snd b)

{- | This property checks that the buy orders are reported in decreasing order
    by foldlOrders
-}
propBuysAreInOrder :: Property
propBuysAreInOrder = forAllShrink genOrderInfos shrinkTuple $
  \(oap, bOrders, sOrders) -> M.monadic (\(Identity p) -> p) $ do
    let book = buildOrderBookList [] (# oap, bOrders, sOrders #)
    M.monitor (counterexample (unlines ["BOOK:", show book]))
      >> M.assert (all buysAreOrdered book)
 where
  buysAreOrdered b = snd $ foldlOrders (\(o', isOrdered) o -> (o, isOrdered && price o' >= price o)) (highBuy b, True) (buyOrders (snd b))
  highBuy b = highestBuy $ buyOrders (snd b)
