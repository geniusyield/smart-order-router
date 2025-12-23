{- |
Module      : GeniusYield.OrderBot.MatchingStrategy
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OrderBot.MatchingStrategy (
  IndependentStrategy,
  FillType (..),
  MatchExecutionInfo (..),
  MatchResult,
  completeFill,
  partialFill,
  executionSkeleton,
  matchExecutionInfoUtxoRef,
) where

import Data.Either (lefts, rights)
import Data.Maybe (fromJust)
import Data.Ratio (denominator, numerator, (%))
import GeniusYield.Api.DEX.PartialOrder (
  PORefs,
  PartialOrderInfo (..),
 )
import GeniusYield.Api.DEX.TwoWayOrder (TWFillSpec (..), TwoWayOrderInfo (..), fillTwoWayAndLegacyPartialOrders)
import GeniusYield.Api.DEX.TwoWayOrderConfig (RefTWOCD (..), TWORef (..))
import GeniusYield.Api.Types (GYApiMonad)
import GeniusYield.OrderBot.Strategies (
  IndependentStrategy,
  MatchResult,
 )
import GeniusYield.OrderBot.Types
import GeniusYield.TxBuilder (GYTxSkeleton)
import GeniusYield.TxBuilder.Class (GYTxQueryMonad, GYTxUserQueryMonad)
import GeniusYield.Types (rationalToGHC)
import GeniusYield.Types.Address (GYAddress)
import GeniusYield.Types.PlutusVersion (PlutusVersion (PlutusV3))
import GeniusYield.Types.TxOutRef (GYTxOutRef)

executionSkeleton ::
  (GYApiMonad m, GYTxQueryMonad m, GYTxUserQueryMonad m) =>
  (TWORef, PORefs) ->
  GYAddress ->
  RefTWOCD ->
  MatchResult ->
  m (GYTxSkeleton 'PlutusV3)
executionSkeleton (twor, pors) recpAddr refTwo mr = do
  fillTwoWayAndLegacyPartialOrders twor tspecs refTwo pors pspecs Nothing
 where
  (pspecs, tspecs) = let specs = (map f mr) in (lefts specs, rights specs)

  f (OrderExecutionInfo ft o@(OrderInfo {orderData = Just (DEXOrderDataPartial poi)})) =
    let getAmountPartial CompleteFill = orderAmount o
        getAmountPartial (PartialFill n) = if isBuyOrder o then floor $ fromIntegral n * getPrice (orderPrice o) else n
     in Left (poiRef poi, getAmountPartial ft)
  f (OrderExecutionInfo ft o@OrderInfo {orderData = Just (DEXOrderDataTwoWay twoi)}) =
    Right $
      TWFillSpec
        { twfsOrderRef = (twoiRef twoi)
        , twfsDirection = fromJust (orderDir o)
        , twfsAmount = getAmountTwo ft
        , twfsOracleCertificate = orderCert o
        , twfsRecipient = recpAddr
        }
   where
    calcNet o = ceiling $ (fromIntegral o :: Rational) * (q % (p + q))
     where
      f = rationalToGHC (twoiMakerFeeRatio twoi)
      p = numerator f
      q = denominator f
    getAmountTwo CompleteFill = calcNet (orderAmount o)
    getAmountTwo (PartialFill n) = calcNet (if isBuyOrder o then floor $ fromIntegral n * getPrice (orderPrice o) else n)
  f _ = error "DEX order data not found"

matchExecutionInfoUtxoRef :: MatchExecutionInfo -> GYTxOutRef
matchExecutionInfoUtxoRef (OrderExecutionInfo _ (OrderInfo {orderData = Just (DEXOrderDataPartial p)})) = poiRef p
matchExecutionInfoUtxoRef (OrderExecutionInfo _ (OrderInfo {orderData = Just (DEXOrderDataTwoWay t)})) = twoiRef t
matchExecutionInfoUtxoRef _ = error "DEX order data not found"
