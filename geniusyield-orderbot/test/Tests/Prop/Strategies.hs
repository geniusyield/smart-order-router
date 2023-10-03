module Tests.Prop.Strategies where

import Control.Monad.Identity (Identity(..))
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as M

import Data.Ratio
import qualified Data.ByteString.Lazy.Char8 as LBS

import GeniusYield.OrderBot.Types
import GeniusYield.OrderBot.MatchingStrategy
import GeniusYield.OrderBot.OrderBook.List
import GeniusYield.Types
import Data.Aeson (encode)
import Tests.Prop.Utils


{- | Function that creates the boilerplate for the properties.

    Given the strategy and a property over the matches generated by the strategy:
        * Calls the generator
        * Sets up the shrink function
        * Builds the OrderBook
        * Runs the strategy
        * Sets up the counterexample and label
        * Runs the property over the result of running the strategy
-}
mkStrategyTest
    :: IndependentStrategy
    -> ([MatchExecutionInfo] -> Bool)
    -> Property
mkStrategyTest strat prop = forAllShrink genOrderInfos shrinkTuple $
    \(oap, buyOrders, sellOrders) -> M.monadic (\(Identity p) -> p) $ do
        let book = buildOrderBookList [] (# oap, buyOrders, sellOrders #)
        M.pre $ length book == 1
        let meis = uncurry strat $ head book
        M.monitor (counterexample (unlines ["MEIS:", LBS.unpack $ encode meis])) >>
            M.monitor (label (getLabel meis)) >>
                M.assert (all prop meis)
  where
    getLabel :: [MatchResult] -> String
    getLabel meis
        | null meis = "No matches found"
        | length meis == 1 = "1 match found"
        | length meis <= 10 = "2-10 matches found"
        | otherwise = "11+ matches found"

{- | Property that checks if the strategy can find a match if one exists.

    This property expects a generator that generates orderbooks with no
    matches, an extra buyOrder and an extra sellOrder that should make a match.

    Given the strategy, and generator:
        * Calls the generator
        * Sets up the shrink function
        * Builds the OrderBook
        * Runs the strategy
        * Checks that there were no matches
        * Build the OrderBook by adding the new sell and buy Orders
        * Runs the strategy again
        * Sets up the counterexample and label
        * Runs the property over the result of running the strategy the second time
-}
propCanFindOnlyMatching
    :: IndependentStrategy
    -> Gen (OrderAssetPair, [OrderInfo 'BuyOrder], [OrderInfo 'SellOrder], OrderInfo 'BuyOrder, OrderInfo 'SellOrder)
    -> Property
propCanFindOnlyMatching strat gen = forAllShrink gen shrinkTuple' $
    \(oap, buyOrders, sellOrders, nBuyOrder, nSellOrder) -> M.monadic (\(Identity p) -> p) $ do
        let book = buildOrderBookList [] (# oap, buyOrders, sellOrders #)
        M.pre $ length book == 1
        let meis = uncurry strat $ head book
        M.pre $ all null meis
        let book' = buildOrderBookList [] (# oap, nBuyOrder : buyOrders, nSellOrder : sellOrders #)
            meis' = uncurry strat $ head book'
        M.monitor (counterexample (unlines ["","MEIS:", LBS.unpack $ encode meis', "BOOK:", show book'])) >>
            M.monitor (label (if null meis then "No matches" else "Matches found")) >>
                M.assert (any (\mr -> length mr >= 2) meis')

{- | Generates a fixes OrderAssetPair, a list of buy and sell orders that
    don't generate any matches because they don't line up on price.
    And an extra buy and sell orders that can be matched togheter.
-}
genOrderInfosWrongPrices :: Gen (OrderAssetPair, [OrderInfo 'BuyOrder], [OrderInfo 'SellOrder], OrderInfo 'BuyOrder, OrderInfo 'SellOrder)
genOrderInfosWrongPrices = do
    buyOrders <- listOf1 $ genBuyOrder' oap
    sellOrders <- listOf1 $ genSellOrder' oap
    newBuyOrder <- genBuyOrder oap
    newSellOrder <- genSellOrder oap `suchThat` sellOrderIsProfitable newBuyOrder
    return (oap, buyOrders, sellOrders, newBuyOrder, newSellOrder)
  where
    goldPolicyId = "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef"
    oap = mkOrderAssetPair GYLovelace (GYToken goldPolicyId "GOLD")

    sellOrderIsProfitable :: OrderInfo 'BuyOrder -> OrderInfo 'SellOrder -> Bool
    sellOrderIsProfitable bOrder sOrder =  price sOrder <= price bOrder
                                        && volumeMin (volume sOrder) <= volumeMax (volume bOrder)
                                        && volumeMin (volume bOrder) <= volumeMax (volume sOrder)

    genBuyOrder' :: OrderAssetPair -> Gen (OrderInfo 'BuyOrder)
    genBuyOrder' oap = OrderInfo <$> genGYTxOutRef
                                <*> pure SBuyOrder
                                <*> pure oap
                                <*> genVolume
                                <*> genPrice `suchThat` ((< (50%1)) . getPrice)
                                <*> pure Nothing

    genSellOrder' :: OrderAssetPair -> Gen (OrderInfo 'SellOrder)
    genSellOrder' oap = OrderInfo <$> genGYTxOutRef
                                <*> pure SSellOrder
                                <*> pure oap
                                <*> genVolume
                                <*> genPrice `suchThat` ((> (50%1)) . getPrice)
                                <*> pure Nothing

{- | Generates a fixes OrderAssetPair, a list of buy and sell orders that
    don't generate any matches because the volume of the buy orders
    can't fill any sell order.

    And an extra buy and sell orders that can be matched togheter.
-}
genOrderInfosWrongVolumes :: Gen (OrderAssetPair, [OrderInfo 'BuyOrder], [OrderInfo 'SellOrder], OrderInfo 'BuyOrder, OrderInfo 'SellOrder)
genOrderInfosWrongVolumes = do
    sellOrders <- listOf1 $ genSellOrder oap
    let minMinVolume = minimum $ map (volumeMin . volume) sellOrders
    buyOrders <- genBuys [] minMinVolume

    newBuyOrder <- genBuyOrder oap
    newSellOrder <- genSellOrder oap `suchThat` sellOrderIsProfitable newBuyOrder
    return (oap, buyOrders, sellOrders, newBuyOrder, newSellOrder)
  where
    goldPolicyId = "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef"
    oap = mkOrderAssetPair GYLovelace (GYToken goldPolicyId "GOLD")

    sellOrderIsProfitable :: OrderInfo 'BuyOrder -> OrderInfo 'SellOrder -> Bool
    sellOrderIsProfitable bOrder sOrder =  price sOrder <= price bOrder
                                        && volumeMin (volume sOrder) <= volumeMax (volume bOrder)
                                        && volumeMin (volume bOrder) <= volumeMax (volume sOrder)

    genBuyOrder' :: Natural -> Gen (OrderInfo 'BuyOrder)
    genBuyOrder' max = OrderInfo <$> genGYTxOutRef
                                     <*> pure SBuyOrder
                                     <*> pure oap
                                     <*> genVolume' 1 (fromIntegral max)
                                     <*> genPrice
                                     <*> pure Nothing

    genBuys :: [OrderInfo 'BuyOrder] -> Natural -> Gen [OrderInfo 'BuyOrder]
    genBuys acc 0 = return acc
    genBuys acc max = do
        size <- getSize
        order <- genBuyOrder' max
        let nMax = max - volumeMax (volume order)
        if size < length acc
        then return acc
        else oneof [genBuys (order : acc) nMax, genBuys acc nMax]

{- | Property that checks if the sum of the offered tokens in the buy orders is
    less than or equal to the sum of offered tokens in the sell orders.
-}
propOffered :: [MatchExecutionInfo] -> Bool
propOffered [] = True
propOffered xs = let buys = filter isBuyOrder xs
                     sells = filter isSellOrder xs
                 in sumOfOffered buys <= sumOfOffered sells

{- | Property that checks if the sum of the price tokens in the buy orders is
    greater than or equal to the sum of price tokens in the sell orders.

    This means that the bot is not using price tokens from it's own wallet to
    make matches.
-}
propPrice :: [MatchExecutionInfo] -> Bool
propPrice [] = True
propPrice xs = let buys = filter isBuyOrder xs
                   sells = filter isSellOrder xs
               in sumOfPrice buys >= sumOfPrice sells

{- | Property that checks if the matches generated by the strategy can be done
   Complete fill can always be performed and partial fills need to be
    between the min and max volume
-}
propCanExecuteFill :: [MatchExecutionInfo] -> Bool
propCanExecuteFill = all canFill
  where
    canFill :: MatchExecutionInfo -> Bool
    canFill (OrderExecutionInfo CompleteFill _) = True
    canFill (OrderExecutionInfo (PartialFill n) OrderInfo {volume}) =
        n >= volumeMin volume
        &&
        n < volumeMax volume

--------------------------------------------------
-- | UTILS
--------------------------------------------------

-- | Checks if a MatchExecutionInfo is a sell order
isSellOrder :: MatchExecutionInfo -> Bool
isSellOrder (OrderExecutionInfo _ OrderInfo {orderType = SSellOrder}) = True
isSellOrder _ = False

-- | Checks if a MatchExecutionInfo is a buy order
isBuyOrder :: MatchExecutionInfo -> Bool
isBuyOrder (OrderExecutionInfo _ OrderInfo {orderType = SBuyOrder}) = True
isBuyOrder _ = False

-- | Given a list of MatchExecutionInfo, sums the offered tokens filled
sumOfOffered :: [MatchExecutionInfo] -> Natural
sumOfOffered = foldl (\acc -> (+) acc . eiOffered) 0
  where
    eiOffered :: MatchExecutionInfo -> Natural
    eiOffered (OrderExecutionInfo CompleteFill OrderInfo {volume}) = volumeMax volume
    eiOffered (OrderExecutionInfo (PartialFill n) _) = n

-- | Given a list of MatchExecutionInfo, sums the price tokens neccesary for the fills
sumOfPrice :: [MatchExecutionInfo] -> Natural
sumOfPrice = foldl (\acc -> (+) acc . eiOfferedByPrice) 0
  where
    eiOfferedByPrice :: MatchExecutionInfo -> Natural
    eiOfferedByPrice (OrderExecutionInfo CompleteFill OrderInfo {volume,price}) =
        ceiling $ (toInteger (volumeMax volume) % 1) * getPrice price
    eiOfferedByPrice (OrderExecutionInfo (PartialFill n) OrderInfo {price}) =
        ceiling $ (toInteger n % 1) * getPrice price

{- | Shrink function for the CanFindOnlyMatching property.

    Shrinks, in order:
        * Buy Orders
        * Sell Orders
        * The extra buy order
        * The extra sell order
-}
shrinkTuple'
    :: (OrderAssetPair, [OrderInfo 'BuyOrder], [OrderInfo 'SellOrder], OrderInfo 'BuyOrder, OrderInfo 'SellOrder)
    -> [(OrderAssetPair, [OrderInfo 'BuyOrder], [OrderInfo 'SellOrder], OrderInfo 'BuyOrder, OrderInfo 'SellOrder)]
shrinkTuple' (oap, xs, ys, bo, so) =
    [ (oap, xs', ys, bo, so) | xs' <- shrinkList shrinkOrderInfo xs ] ++
    [ (oap, xs, ys', bo, so) | ys' <- shrinkList shrinkOrderInfo ys ] ++
    [ (oap, xs, ys, bo', so) | bo' <- shrinkOrderInfo bo
                             , volumeMin (volume so) < volumeMax (volume bo')
                             , price so < price bo' ] ++
    [ (oap, xs, ys, bo, so') | so' <- shrinkOrderInfo so
                             , volumeMin (volume bo) < volumeMax (volume so')]

