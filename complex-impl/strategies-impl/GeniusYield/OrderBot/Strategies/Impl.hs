{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module GeniusYield.OrderBot.Strategies.Impl (
  BotStrategy (..),
  allStrategies,
  MatchResult,
  IndependentStrategy,
  mkIndependentStrategy,
) where

import Control.Monad (when)
import Control.Monad.State.Strict
import Data.Aeson
import Data.Data (Typeable)
import Data.Function ((&))
import qualified Data.Map as M
import GHC.Generics
import GeniusYield.OrderBot.OrderBook
import GeniusYield.OrderBot.OrderBook.Extra
import GeniusYield.OrderBot.Types
import GeniusYield.Types
import System.Envy (Var (..))

data BotStrategy
  = ComplexOneToManyPartial
  | SimpleOneSellToManyBuyPartial
  | SimpleOneBuyToManySellPartial
  | SimpleOneToManyPartial
  | SimpleOneToOneExact
  | MixedSellBuyPartial
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Typeable)

instance Var BotStrategy where
  fromVar s = case s of
    "ComplexOneToManyPartial" -> Just ComplexOneToManyPartial
    "SimpleOneBuyToManySellPartial" -> Just SimpleOneBuyToManySellPartial
    "SimpleOneSellToManyBuyPartial" -> Just SimpleOneSellToManyBuyPartial
    "SimpleOneToManyPartial" -> Just SimpleOneToManyPartial
    "SimpleOneToOneExact" -> Just SimpleOneToOneExact
    "MixedSellBuyPartial" -> Just MixedSellBuyPartial
    _ -> Nothing
  toVar = show

allStrategies :: [BotStrategy]
allStrategies = [ComplexOneToManyPartial, SimpleOneSellToManyBuyPartial, SimpleOneBuyToManySellPartial, SimpleOneToManyPartial, SimpleOneToOneExact, MixedSellBuyPartial]

type MatchResult = [MatchExecutionInfo]

type IndependentStrategy = (OrderAssetPair -> OrderBook -> [MatchResult])

mkIndependentStrategy :: BotStrategy -> Natural -> IndependentStrategy
mkIndependentStrategy bs maxOrders _ bk =
  case bs of
    ComplexOneToManyPartial -> complexOneToManyPartial maxOrders bk
    SimpleOneBuyToManySellPartial -> simpleOneBuyToManySellPartial maxOrders bk
    SimpleOneSellToManyBuyPartial -> simpleOneSellToManyBuyPartial maxOrders bk
    SimpleOneToManyPartial -> simpleOneToManyPartial maxOrders bk
    SimpleOneToOneExact -> simpleOneToOneExact bk
    MixedSellBuyPartial -> mixedSellBuyPartial maxOrders bk

{- Note: Each of the strategies below is meant to be used with `withIntersection`. It's more efficient to get the intersection then
         run the strategy than it is to check the price during every iteration when folding through the order book.

         (Efficiently calculating the intersection while facilitating O(1) access to the total volume in an order book is the
          reason that AnnSet exists.)
-}

{- The simplest possible strategy. Matches one order to another exactly, always find the best match.-}
simpleOneToOneExact :: OrderBook -> [MatchResult]
simpleOneToOneExact ob = evalState (foldlMOrders' go [] $ sellOrders ob) $ buyOrders ob
 where
  go ::
    [MatchResult] ->
    OrderInfo 'SellOrder ->
    State (Orders 'BuyOrder) [MatchResult]
  go acc sell =
    get >>= \bs ->
      fillExact sell bs & \case
        ([], _) -> pure acc
        (oeis, s) -> modify' (const s) >> pure (oeis : acc)

  fillExact :: OrderInfo 'SellOrder -> Orders 'BuyOrder -> (MatchResult, Orders 'BuyOrder)
  fillExact ox os = case lookupBest . filterOrders (\o -> volumeMax (orderVolume o) == volumeMax (orderVolume ox)) $ os of
    Nothing -> ([], os)
    Just match ->
      let !oeis = [completeFill ox, completeFill match]
          !os' = deleteOrder match os -- we could fake O(1) delete with a mutable hashmap that tracks deleted orders if we need to
       in (oeis, os')

{- Find the order with the volume nearest the maximum fill volume of each sell order. Does not partially consume buy orders.

NOTE: The current implementation of this strategy is not working as intended.

The actual implementation is looking for buy orders that are greater than the sell order. But this doesn't look right
Also, when a buy order is selected, the logic that checks if we need to do a
  partial or a complete fill of the buy and sell order is incorrect.
  Often trying to do a partial fill with the total size of the order and raising an exception

To make the quickcheck tests pass, the strategy is commented until we can fix the issues.
-}

-- simpleOneToOnePartial :: OrderBook -> [MatchExecutionInfo]
-- simpleOneToOnePartial OrderBook {..} = evalState (foldM go [] $ unOrders sellOrders) $ unOrders buyOrders
--   where
--     go ::
--       [MatchExecutionInfo] ->
--       AnnSet.OrderView 'SellOrder ->
--       State (AnnSet.Orders 'BuyOrder) [MatchExecutionInfo]
--     go acc sell =
--       get >>= \bs ->
--         fillPartial (orderInfo sell) bs & \case
--           ([], _) -> pure acc
--           (oeis, s) -> modify' (const s) >> pure (oeis <> acc)

--     fillPartial :: Ord (OrderView b') => OrderInfo b -> AnnSet.Orders b' -> ([MatchExecutionInfo], AnnSet.Orders b')
--     fillPartial ox@OrderInfo {volume = Volume _ maxVol} os =
--       foldl' highestMatchingVolume Nothing os & \case
--         Nothing -> ([], os)
--         Just oi ->
--           let !(Volume _ h) = AnnSet.orderVolume oi
--               filled = min h maxVol
--               !oei = if h == maxVol then completeFill (orderInfo oi) else partialFill (orderInfo oi) filled
--               !soei = if h == maxVol then completeFill ox else partialFill ox filled
--               !annSet = AnnSet.delete oi os
--            in ([soei, oei], annSet)
--       where
--         highestMatchingVolume :: Maybe (OrderView b') -> OrderView b' -> Maybe (OrderView b')
--         highestMatchingVolume Nothing ovx
--           | maxVol >= minVolX && maxVol <= maxVolX = Just ovx
--           | otherwise = Nothing
--           where
--             Volume minVolX maxVolX = AnnSet.orderVolume ovx
--         highestMatchingVolume (Just acc) ovx
--           | maxVol >= minVolX
--               && maxVol <= maxVolX
--               && volumeMax (AnnSet.orderVolume acc) < maxVolX =
--             Just ovx
--           | otherwise = Just acc
--           where
--             Volume minVolX maxVolX = AnnSet.orderVolume ovx

{- This is a group of naive one-to-many partial order matching strategies. In this strategy, partially filled orders are *not* "recycled".
   I.e., when we partially fill a buy order, we remove it from the set of candidate matches for the remaining sell orders.

   We have two versions, one that takes a single sell order and matches it agains many buy orders and viceversa.
   We also have 'simpleOneToManyPartial'. This strategy tries using oneSellManyBuy and if it doesn't find any, it tries using oneBuyManySell.
-}
simpleOneBuyToManySellPartial :: Natural -> OrderBook -> [MatchResult]
simpleOneBuyToManySellPartial maxOrders ob =
  evalState (foldlMOrders' (goMatchOneToManyPartial maxOrders (>=)) [] $ buyOrders ob) (sellOrders ob)

simpleOneSellToManyBuyPartial :: Natural -> OrderBook -> [MatchResult]
simpleOneSellToManyBuyPartial maxOrders ob =
  evalState (foldlMOrders' (goMatchOneToManyPartial maxOrders (<=)) [] $ sellOrders ob) (buyOrders ob)

simpleOneToManyPartial :: Natural -> OrderBook -> [MatchResult]
simpleOneToManyPartial maxOrders ob =
  case simpleOneSellToManyBuyPartial maxOrders ob of
    [] -> simpleOneBuyToManySellPartial maxOrders ob
    xs -> xs

goMatchOneToManyPartial ::
  forall b b'.
  Natural ->
  (Price -> Price -> Bool) ->
  [MatchResult] ->
  OrderInfo b ->
  State (Orders b') [MatchResult]
goMatchOneToManyPartial maxOrders priceCompare acc (order) =
  get
    >>= \bs -> case multiFillPartial maxOrders order (priceCompare $ orderPrice order) bs of
      ([], _) -> pure acc
      (matches', bs') -> do
        modify' $ const bs'
        pure $ matches' : acc

multiFillPartial ::
  forall b b'.
  Natural ->
  OrderInfo b ->
  (Price -> Bool) ->
  Orders b' ->
  (MatchResult, Orders b')
multiFillPartial maxOrders order checkPrice = go (maxOrders - 1) vh
 where
  (Volume vl vh) = orderVolume order

  go :: Natural -> Natural -> Orders b' -> (MatchResult, Orders b')
  go _ 0 os = ([completeFill order], os)
  go 0 v os
    | (vh - v) >= vl = ([partialFill order (vh - v)], os)
    | otherwise = ([], os)
  go limitO remVol os = case unconsOrders os of
    Nothing ->
      if
        | (vh - remVol) > vl -> ([partialFill order (vh - remVol)], emptyOrders)
        | otherwise -> ([], emptyOrders)
    -- TODO: Rename this annset & xOI and at other places too!
    Just (xOI, annSet) ->
      if
        | remVol == maxFillX && checkPrice xP ->
            let !b = completeFill xOI
             in ([completeFill order, b], annSet)
        | remVol > maxFillX && remVol >= minFillX && checkPrice xP ->
            case go (limitO - 1) (remVol - maxFillX) annSet of
              ([], _) -> updateRemainingAnnSet xOI $ go limitO remVol annSet
              (bs, s) -> (completeFill xOI : bs, s)
        | remVol < maxFillX
            && remVol >= minFillX
            && checkPrice xP ->
            ([completeFill order, partialFill xOI remVol], annSet)
        | otherwise -> updateRemainingAnnSet xOI $ go limitO remVol annSet
     where
      xP = orderPrice xOI
      (Volume minFillX maxFillX) = orderVolume xOI

      updateRemainingAnnSet e (a, b) = (a, insertOrder e b)

{- Helper data type used to implement complexOneToManyPartial.

   The general idea behind complexOneToManyPartial is that, when we come across a buy order that we can
   use to partially fill the sell order we're attempting to match, we do not generate the MatchExecutionInfo
   at that point, but instead subtract the remaining volume needed to fill the sell order from the volume of the buy order
   and re-insert the buy order into the set so that its remaining volume can be used to match remaining sell orders.

   To do that, we need to retain the original volume of the buy order (so that we can restore the volume to its original value when
   we eventually do construct the MatchExecutionInfo). We also need to keep track of the amount of the buy order which has been filled.

   Could use a tuple but, uh, this is a lot prettier and easier to understand.
-}
data PartialBuy = PartialBuy
  { origVolume :: Volume
  , amountFilled :: Natural
  }

-- this should definitely be a hashmap (b/c bytestring keys, essentially)
type PartialFills = M.Map GYTxOutRef PartialBuy

{-  This strategy does "partial consumption" of buy orders, i.e., when a buy order is partially filled
    in the course of finding matches to a sell order, we subtract the filled volume from the buy order and
    insert it back into the set so that it can possibly be matched to one of the remaining sell orders.

    LEAVE THE TRACES IN FOR NOW. It's very hard to tell whether these strategies work the way they are supposed to, the traces greatly help
    in locating errors or bugs.

    Note to Chase: Because of the way this has to work, buy and sell orders may not necessarily be grouped together
                   in the printed list of matches.

    Note to future me #1: If we *really* wanted to find *the best* matches, we would keep track of partially filled sell orders as well
                          and re-run the matching strategy repeatedly until we've reached a certain number of iterations or exhausted the
                          sell orders.

    Note to future me #2: We should be able to use the Summary to substantially reduce the average time complexity of this strategy. -}
complexOneToManyPartial :: Natural -> OrderBook -> [MatchResult]
complexOneToManyPartial maxOrders ob =
  case foldlOrders goMatch (M.empty, [], buyOrders ob) (sellOrders ob) of
    (pfs, oeis, bos) -> buildOrders pfs oeis bos
 where
  goMatch ::
    (PartialFills, [MatchExecutionInfo], Orders 'BuyOrder) ->
    OrderInfo 'SellOrder ->
    (PartialFills, [MatchExecutionInfo], Orders 'BuyOrder)
  goMatch (pfs, oeis, bos) so = case multiFillPartial' pfs bos so of
    (pfs', oeis', bos') -> (pfs', oeis <> oeis', bos')

  buildOrders ::
    PartialFills ->
    [MatchExecutionInfo] ->
    Orders 'BuyOrder ->
    [MatchResult]
  buildOrders pfs oeis bos = [foldlOrders go oeis bos]
   where
    go :: [MatchExecutionInfo] -> OrderInfo 'BuyOrder -> [MatchExecutionInfo]
    go oeis' o = case M.lookup (orderRef o) pfs of
      Nothing -> oeis'
      Just PartialBuy {..} -> partialFill (restore pfs o) amountFilled : oeis'

  restore :: PartialFills -> OrderInfo 'BuyOrder -> OrderInfo 'BuyOrder
  restore pfs oi = case M.lookup (orderRef oi) pfs of
    Just PartialBuy {..} -> oi {orderVolume = origVolume}
    Nothing -> oi

  multiFillPartial' ::
    PartialFills ->
    Orders 'BuyOrder ->
    OrderInfo 'SellOrder ->
    (PartialFills, [MatchExecutionInfo], Orders 'BuyOrder)
  multiFillPartial' pfs orders order = go (maxOrders - 1) pfs vh orders
   where
    (Volume vl vh) = orderVolume order
    checkPrice = (orderPrice order <=)

    go ::
      Natural ->
      PartialFills ->
      Natural ->
      Orders 'BuyOrder ->
      (PartialFills, [MatchExecutionInfo], Orders 'BuyOrder)
    go _ m 0 os = (m, [completeFill order], os)
    go 0 m v os
      | (vh - v) >= vl = (m, [partialFill order (vh - v)], emptyOrders)
      | otherwise = (m, [], os)
    go limitO m remVol os =
      case unconsOrders os of
        Nothing ->
          if
            | (vh - remVol) >= vl -> (m, [partialFill order (vh - remVol)], emptyOrders)
            | otherwise -> (m, [], emptyOrders)
        Just (xOI, annSet) ->
          if
            | remVol == maxFillX && checkPrice xP ->
                let !m' = M.delete (orderRef xOI) m -- though, maybe we don't actually need to delete?
                    !b = completeFill $ restore m xOI
                 in (m', [completeFill order, b], annSet)
            | remVol > maxFillX && remVol >= minFillX && checkPrice xP ->
                let !xOI' = completeFill . restore m $ xOI
                 in case go (limitO - 1) m (remVol - maxFillX) annSet of
                      (_, [], _) -> updateRemainingAnnSet xOI $ go limitO m remVol annSet
                      (m', bs, s) -> (m', xOI' : bs, s)
            -- this is the only interesting branch for the partial fill map stuff.
            | remVol < maxFillX
                && remVol >= minFillX
                && checkPrice xP ->
                let !annSet' = insertOrder x' $ annSet
                    !m' = updatePFs xOI remVol m
                    -- I might need a check that maxFillX - remVol > minFillX here \/
                    !x' = xOI {orderVolume = Volume 0 (maxFillX - remVol)}
                 in -- 0 should be fine here, if remVol >= minFillX then we can fill the remainder as we please in subsequent passes w/o regard for the minimum volume
                    (m', [completeFill order], annSet')
            | otherwise -> updateRemainingAnnSet xOI $ go limitO m remVol annSet
         where
          -- should use M.adjust
          updatePFs :: OrderInfo 'BuyOrder -> Natural -> PartialFills -> PartialFills
          updatePFs o fill pfs' = case M.lookup ref pfs' of
            Just PartialBuy {..} -> M.insert ref PartialBuy {amountFilled = amountFilled + fill, ..} pfs'
            Nothing -> M.insert ref newFill pfs'
           where
            newFill =
              PartialBuy
                { origVolume = xv
                , amountFilled = fill
                }
            ref = orderRef o

          updateRemainingAnnSet e (a, b, c) = (a, b, insertOrder e c)

          xv@(Volume minFillX maxFillX) = orderVolume xOI
          xP = orderPrice xOI

{- | Strategy state containing the matchings found and the remaining sell and
  buy orders.
-}
data StrategyState = StrategyState
  { matchResults :: ![MatchResult]
  , remBuyOrders :: !(Orders 'BuyOrder)
  , remSellOrders :: !(Orders 'SellOrder)
  }

mixedSellBuyPartial :: Natural -> OrderBook -> [MatchResult]
mixedSellBuyPartial maxOrders ob =
  matchResults $
    execState (goMatchSellBuyPartial maxOrders) $
      StrategyState
        { matchResults = []
        , remBuyOrders = buyOrders ob
        , remSellOrders = sellOrders ob
        }

-- | Utility function for updating the state, after one run of the strategy.
updateStrategyState ::
  MatchResult ->
  Orders 'BuyOrder ->
  Orders 'SellOrder ->
  StrategyState ->
  StrategyState
updateStrategyState [] buys sells ss =
  ss
    { remBuyOrders = buys
    , remSellOrders = sells
    }
updateStrategyState mr buys sells StrategyState {matchResults = mrs} =
  StrategyState
    { matchResults = mrs ++ [mr]
    , remBuyOrders = buys
    , remSellOrders = sells
    }

-- | Given a type order getter, we get the best order and the remaining orders.
getOrder ::
  forall b.
  SOrderTypeI b =>
  (StrategyState -> Orders b) ->
  State StrategyState (Maybe (OrderInfo b, Orders b))
getOrder orderTypeGetter = do
  orders <- gets orderTypeGetter
  -- Looking up for the minimum on the set works for any of the order types
  -- because the buy orders are sorted in the inverse order than the sell
  -- orders.
  case lookupBest orders of
    Nothing -> return Nothing
    Just o -> return $ Just (o, deleteOrder o orders)

tryRemoveOneOrder :: State StrategyState Bool
tryRemoveOneOrder = do
  mSells <- getOrder remSellOrders
  mBuys <- getOrder remBuyOrders
  case (mSells, mBuys) of
    (Nothing, Nothing) -> return False
    (Just (_, sells'), Nothing) ->
      modify' (\s -> s {remSellOrders = sells'})
        >> return True
    (Nothing, Just (_, buys')) ->
      modify' (\s -> s {remBuyOrders = buys'})
        >> return True
    (Just (sell, sells'), Just (buy, buys')) ->
      let (Volume svl _) = orderVolume sell
          (Volume bvl _) = orderVolume buy
          remOrder s =
            if svl < bvl
              then s {remSellOrders = sells'}
              else s {remBuyOrders = buys'}
       in modify' remOrder >> return True

goMatchSellBuyPartial ::
  Natural ->
  State StrategyState ()
goMatchSellBuyPartial maxOrders = go
 where
  go :: State StrategyState ()
  go = do
    foundSellMatch <- multiSellFillPartial
    foundeBuyMatch <- multiBuyFillPartial
    if foundSellMatch || foundeBuyMatch
      then go
      else tryRemoveOneOrder >>= flip when go

  multiSellFillPartial :: State StrategyState Bool
  multiSellFillPartial =
    multiOrderFillPartial remSellOrders remBuyOrders (<=) (flip . updateStrategyState)

  multiBuyFillPartial :: State StrategyState Bool
  multiBuyFillPartial =
    multiOrderFillPartial remBuyOrders remSellOrders (>=) updateStrategyState

  multiOrderFillPartial ::
    forall b b'.
    (SOrderTypeI b, SOrderTypeI b') =>
    (StrategyState -> Orders b) ->
    (StrategyState -> Orders b') ->
    (Price -> Price -> Bool) ->
    (MatchResult -> Orders b -> Orders b' -> StrategyState -> StrategyState) ->
    State StrategyState Bool
  multiOrderFillPartial ordersGetter reverseOrdersGetter priceCompare updSt = do
    mBestOrderOrders <- getOrder ordersGetter
    case mBestOrderOrders of
      Nothing -> return False
      Just (bestOrder, remOrders) -> do
        let bestOrderPriceCheck = priceCompare $ orderPrice bestOrder
        rOrders <- gets reverseOrdersGetter
        case multiFillPartial maxOrders bestOrder bestOrderPriceCheck rOrders of
          ([], _) -> return False
          (mr, rOrders') ->
            modify' (updSt mr remOrders rOrders')
              >> return True
