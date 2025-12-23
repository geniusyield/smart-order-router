{-# LANGUAGE MultiWayIf #-}

{- |
Module      : GeniusYield.OrderBot.Strategies.Impl
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OrderBot.Strategies.Impl (
  BotStrategy (..),
  allStrategies,
  MatchResult,
  IndependentStrategy,
  mkIndependentStrategy,
) where

import Control.Monad.State.Strict (State, execState, modify')
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Data (Typeable)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GeniusYield.OrderBot.OrderBook
import GeniusYield.OrderBot.OrderBook.Extra
import GeniusYield.OrderBot.Types
import System.Envy (Var (..))

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
    _ -> Nothing
  toVar = show

allStrategies :: [BotStrategy]
allStrategies = [OneSellToManyBuy]

type MatchResult = [MatchExecutionInfo]

type IndependentStrategy = (OrderAssetPair -> OrderBook -> [MatchResult])

mkIndependentStrategy :: BotStrategy -> Natural -> IndependentStrategy
mkIndependentStrategy bs maxOrders _ bk =
  case bs of
    OneSellToManyBuy -> oneSellToManyBuy maxOrders bk

-- | Strategy state containing the matchings found and the remaining buy orders.
data StrategyState = StrategyState
  { matchResults :: ![MatchResult]
  , remainingOrders :: !(Orders 'BuyOrder)
  }

-- | Utility function for updating the state, after one run of the strategy.
updateStrategyState :: (MatchResult, Orders 'BuyOrder) -> StrategyState -> StrategyState
updateStrategyState ([], bos') ss = ss {remainingOrders = bos'}
updateStrategyState (mr', bos') StrategyState {matchResults = mr} =
  StrategyState
    { matchResults = mr ++ [mr']
    , remainingOrders = bos'
    }

{- | Strategy matching: Picking one sell order and matching it with many (up to
`maxOrders`) buy orders.
-}
oneSellToManyBuy :: Natural -> OrderBook -> [MatchResult]
oneSellToManyBuy maxOrders ob =
  matchResults $
    execState (mapMOrders_ go $ sellOrders ob) $
      StrategyState {matchResults = [], remainingOrders = buyOrders ob}
 where
  go :: OrderInfo 'SellOrder -> State StrategyState ()
  go order = modify' $
    \st ->
      updateStrategyState
        (multiFill (maxOrders - 1) (<=) order (remainingOrders st))
        st

-- | General matching orders function.
multiFill ::
  forall b b'.
  Natural ->
  (Price -> Price -> Bool) ->
  OrderInfo b ->
  Orders b' ->
  (MatchResult, Orders b')
multiFill maxOrders checkPrices order = go (maxOrders - 1) vh
 where
  (Volume vl vh) = orderVolume order
  checkPrice = checkPrices $ orderPrice order

  go :: Natural -> Natural -> Orders b' -> (MatchResult, Orders b')
  go _ 0 os = ([completeFill order], os)
  go 0 v os
    | (vh - v) >= vl = ([partialFill order (vh - v)], os)
    | otherwise = ([], os)
  go limitO remVol os' =
    case unconsOrders os' of
      Nothing ->
        if
          | (vh - remVol) > vl -> ([partialFill order (vh - remVol)], emptyOrders)
          | otherwise -> ([], emptyOrders)
      Just (o, os) ->
        if
          | remVol == maxFillX && checkPrice xP ->
              let !b = completeFill o
               in ([completeFill order, b], os)
          | remVol > maxFillX && remVol >= minFillX && checkPrice xP ->
              case go (limitO - 1) (remVol - maxFillX) os of
                ([], _) -> updateRemaining o $ go limitO remVol os
                (bs, s) -> (completeFill o : bs, s)
          | remVol < maxFillX
              && remVol >= minFillX
              && checkPrice xP ->
              ([completeFill order, partialFill o remVol], os)
          | otherwise -> updateRemaining o $ go limitO remVol os
       where
        xP = orderPrice o
        (Volume minFillX maxFillX) = orderVolume o

        updateRemaining x (a, b) = (a, insertOrder x b)
