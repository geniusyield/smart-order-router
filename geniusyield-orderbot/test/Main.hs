module Main where

import GeniusYield.OrderBot.Strategies.Impl
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Tests.Prop.Orderbook
import Tests.Prop.Strategies

{- | All strategies are exiting when a match is found.
     so the interesting logic in the ComplexOneToManyPartial is not being tested
-}
main :: IO ()
main =
  defaultMain $
    testGroup
      "QC"
      [ testGroup
          "Orderbook Tests"
          [ testProperty "lowestSell" propLowestSell
          , testProperty "highestBuy" propHighestBuy
          , testProperty "sellsAreInOrder" propSellsAreInOrder
          , testProperty "buysAreInOrder" propBuysAreInOrder
          ]
      , testGroup "Strategies tests" $ map qcTestsForStrategy allStrategies
      ]

qcTestsForStrategy :: BotStrategy -> TestTree
qcTestsForStrategy strat =
  testGroup
    (show strat)
    [ testProperty "Price" $
        mkStrategyTest iStrat propPrice
    , testProperty "Offer" $
        mkStrategyTest iStrat propOffered
    , testProperty "Can Fill" $
        mkStrategyTest iStrat propCanExecuteFill
    , testProperty "Can find only Match - Price" $
        propCanFindOnlyMatching iStrat genOrderInfosWrongPrices
    ]
 where
  iStrat = mkIndependentStrategy strat 10
