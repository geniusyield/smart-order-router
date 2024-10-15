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

import Data.Maybe (fromJust)
import GeniusYield.Api.Dex.PartialOrder (
  PORefs,
  PartialOrderInfo (poiOfferedAmount),
  fillMultiplePartialOrders',
 )
import GeniusYield.Api.Dex.Types (GYDexApiMonad)
import GeniusYield.OrderBot.Strategies (
  IndependentStrategy,
  MatchResult,
 )
import GeniusYield.OrderBot.Types
import GeniusYield.TxBuilder (GYTxSkeleton)
import GeniusYield.Types.PlutusVersion (PlutusVersion (PlutusV2))
import GeniusYield.Types.TxOutRef (GYTxOutRef)

executionSkeleton ::
  GYDexApiMonad m a =>
  PORefs ->
  MatchResult ->
  m (GYTxSkeleton 'PlutusV2)
executionSkeleton pors mr = fillMultiplePartialOrders' pors (map f mr) Nothing mempty
 where
  f (OrderExecutionInfo ft o) =
    let oi = fromJust $ mPoi o -- It's always under `Just` constructor in our code, but we aren't able to get rid of `Maybe` type for now since that would require significant changes in the test-suite.
     in ( oi
        , case ft of
            CompleteFill -> poiOfferedAmount oi
            PartialFill n ->
              if isBuyOrder o
                then
                  floor $ fromIntegral n * getPrice (price o)
                else
                  n
        )

matchExecutionInfoUtxoRef :: MatchExecutionInfo -> GYTxOutRef
matchExecutionInfoUtxoRef (OrderExecutionInfo CompleteFill OrderInfo {orderRef}) = orderRef
matchExecutionInfoUtxoRef (OrderExecutionInfo (PartialFill _) OrderInfo {orderRef}) = orderRef
