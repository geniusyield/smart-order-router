{-|
Module      : Strategies
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

Dummy change to test CI.

-}
module Strategies
    ( BotStrategy(..)
    , mkIndependentStrategy
    ) where

import Control.Monad.State.Strict (State, execState, modify')
import Data.Aeson                 (FromJSON, ToJSON, parseJSON, withText)
import Data.Aeson.Types           (Parser)
import Data.Text                  (Text)
import Data.Data                  (Typeable)
import GHC.Generics               (Generic)
import GHC.Natural                (Natural)
import System.Envy                (Var (..))

import GeniusYield.OrderBot.MatchingStrategy ( IndependentStrategy
                                             , MatchResult
                                             , completeFill
                                             , partialFill
                                             )
import GeniusYield.OrderBot.OrderBook.List (OrderBook(..), unOrders)
import GeniusYield.OrderBot.Types ( OrderInfo (..)
                                  , OrderType (BuyOrder, SellOrder)
                                  , Volume (..)
                                  , Price
                                  )

-- | Every bot strategy must be named here.
data BotStrategy = OneSellToManyBuy
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, Typeable)

instance FromJSON BotStrategy where
    parseJSON = withText "BotStrategy" parse
      where
        parse :: Text -> Parser BotStrategy
        parse "OneSellToManyBuy" = return OneSellToManyBuy
        parse _ = fail "Undefined strategy name"

instance Var BotStrategy where
    fromVar s = case s of
        "OneSellToManyBuy" -> Just OneSellToManyBuy
        _                  -> Nothing
    toVar = show

{- | Given a bot strategy and a max amount of orders per transaction, creates
     an independent strategy.
-}
mkIndependentStrategy :: BotStrategy -> Natural -> IndependentStrategy
mkIndependentStrategy bs maxOrders _ bk =
    case bs of
      OneSellToManyBuy -> oneSellToManyBuy maxOrders bk

-- | Strategy state containing the matchings found and the remaining buy orders.
data StrategyState = StrategyState
                     { matchResults    :: ![MatchResult]
                     , remainingOrders :: ![OrderInfo 'BuyOrder]
                     }

-- | Utility function for updating the state, after one run of the strategy.
updateStrategyState
    :: MatchResult
    -> [OrderInfo 'BuyOrder]
    -> StrategyState
    -> StrategyState
updateStrategyState [] bos' ss = ss { remainingOrders = bos' }
updateStrategyState mr' bos' StrategyState { matchResults = mr } =
    StrategyState { matchResults = mr ++ [mr']
                  , remainingOrders = bos'
                  }

{- | Strategy matching: Picking one sell order and matching it with many (up to
     `maxOrders`) buy orders.
-}
oneSellToManyBuy :: Natural -> OrderBook -> [MatchResult]
oneSellToManyBuy maxOrders OrderBook{sellOrders, buyOrders} =
    matchResults
    $ execState (mapM_ go $ unOrders sellOrders)
    $ StrategyState {matchResults = [], remainingOrders = unOrders buyOrders}
  where
    go :: OrderInfo 'SellOrder
       -> State StrategyState ()
    go order = modify' $
               \st -> uncurry updateStrategyState
                      (multiFill (maxOrders - 1) (<=) order (remainingOrders st)) st

-- | General matching orders function.
multiFill
    :: forall b b'
    .  Natural
    -> (Price -> Price -> Bool)
    -> OrderInfo b
    -> [OrderInfo b']
    -> (MatchResult, [OrderInfo b'])
multiFill maxOrders checkPrices order = go (maxOrders - 1) vh
  where
    (Volume vl vh) = volume order
    checkPrice = checkPrices $ price order

    go :: Natural -> Natural -> [OrderInfo b'] -> (MatchResult, [OrderInfo b'])
    go _ 0 os = ([completeFill order], os)
    go 0 v os
        | (vh - v) >= vl = ([partialFill order (vh - v)], os)
        | otherwise = ([], os)
    go _ v []
        | (vh - v) > vl = ([partialFill order (vh - v)], [])
        | otherwise = ([], [])
    go limitO remVol (o : os)
        | remVol == maxFillX && checkPrice xP =
              let !b = completeFill o
              in ([completeFill order, b], os)
        | remVol > maxFillX && remVol >= minFillX && checkPrice xP =
              case go (limitO - 1) (remVol - maxFillX) os of
                  ([], _) -> updateRemaining o $ go limitO remVol os
                  (bs, s) -> (completeFill o : bs, s)
        | remVol < maxFillX
          && remVol >= minFillX
          && checkPrice xP =
              ([completeFill order, partialFill o remVol], os)
        | otherwise = updateRemaining o $ go limitO remVol os
      where
        xP = price o
        (Volume minFillX maxFillX) = volume o

        updateRemaining x (a, b) = (a, x : b)
