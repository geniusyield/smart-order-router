module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty)

import Strategies
import Tests.Prop.Strategies
import Tests.Prop.Orderbook

{- | All strategies are exiting when a match is found.
     so the interesting logic in the ComplexOneToManyPartial is not being tested
-}
main :: IO ()
main =  defaultMain $ testGroup "QC"
    [ testGroup "Orderbook Tests"
        [ testProperty "lowestSell" propLowestSell
        , testProperty "highestBuy" propHighestBuy
        , testProperty "sellsAreInOrder" propSellsAreInOrder
        , testProperty "buysAreInOrder" propBuysAreInOrder
        ]
    , testGroup "Strategies tests" $ map qcTestsForStrategy
        -- Every strategy property is ran over each element of this list. So adding
        -- a new BotStrategy constructor to the list is enough to include the new
        -- strategy into the complete tests suit.
        [ OneSellToManyBuy ]
    ]

qcTestsForStrategy :: BotStrategy -> TestTree
qcTestsForStrategy strat = testGroup (show strat)
                           [ testProperty "Price" $
                               mkStrategyTest iStrat propPrice
                           , testProperty "Offer" $
                               mkStrategyTest iStrat propOffered
                           , testProperty "Can Fill" $
                               mkStrategyTest iStrat propCanExecuteFill
                           , testProperty "Can find only Match - Price" $
                               propCanFindOnlyMatching iStrat genOrderInfosWrongPrices
                            , testProperty "Can find only Match - Volume" $
                               propCanFindOnlyMatching iStrat genOrderInfosWrongVolumes
                           ]
  where
    iStrat = mkIndependentStrategy strat 10
