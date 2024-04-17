{-|
Module      : GeniusYield.OrderBot.MatchingStrategy
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.OrderBot.MatchingStrategy
    ( IndependentStrategy
    , FillType (..)
    , MatchExecutionInfo (..)
    , MatchResult
    , completeFill
    , partialFill
    , executionSkeleton
    , matchExecutionInfoUtxoRef
    ) where

import           Data.Aeson                       (ToJSON (toJSON), (.=))
import qualified Data.Aeson                       as Aeson
import           Data.Maybe                       (fromJust)
import           Data.Text                        (Text)
import           Numeric.Natural                  (Natural)

import           GeniusYield.Api.Dex.PartialOrder (PORefs,
                                                   PartialOrderInfo (poiOfferedAmount),
                                                   fillMultiplePartialOrders')
import           GeniusYield.Api.Dex.Types        (GYDexApiMonad)
import           GeniusYield.OrderBot.OrderBook   (OrderBook)
import           GeniusYield.OrderBot.Types
import           GeniusYield.TxBuilder            (GYTxSkeleton)
import           GeniusYield.Types.PlutusVersion  (PlutusVersion (PlutusV2))
import           GeniusYield.Types.TxOutRef       (GYTxOutRef, showTxOutRef)

{- | A matching strategy has access to the 'OrderBook' for a single asset pair,
alongside all its relevant query functions. It must produce a 'MatchResult' which
has information on how to execute the order matching transaction.
-}
type IndependentStrategy = (OrderAssetPair -> OrderBook -> [MatchResult])

data MatchExecutionInfo
    = forall t. OrderExecutionInfo !FillType {-# UNPACK #-} !(OrderInfo t)

-- Smart Constructors
completeFill :: OrderInfo t -> MatchExecutionInfo
completeFill = OrderExecutionInfo CompleteFill

partialFill :: OrderInfo t -> Natural -> MatchExecutionInfo
partialFill o n = OrderExecutionInfo (PartialFill n) o

instance ToJSON MatchExecutionInfo where
  toJSON (OrderExecutionInfo fillT OrderInfo { orderRef, orderType, assetInfo
                                             , volume
                                             , price = Price {getPrice = x}
                                             }) =
      Aeson.object
      [ "utxoRef"   .= showTxOutRef orderRef
      , "volumeMin" .= volumeMin volume
      , "volumeMax" .= volumeMax volume
      , "price"     .= x
      , "commodity" .= commodityAsset assetInfo
      , "currency"  .= currencyAsset assetInfo
      , "type"      .= prettySOrderType orderType
      , "fillType"  .= show fillT
      ]
    where
      prettySOrderType :: SOrderType t -> Text
      prettySOrderType SBuyOrder  = "Buy"
      prettySOrderType SSellOrder = "Sell"

{- | The result of order matching - should contain information to perform execute order and LP transactions.

Essentially, all orders (and pool swaps) in a list of 'MatchExecutionInfo's are matched with each other.

All of their tokens are put into one big transaction bucket, which is then auto balanced to pay each other.
Any extra tokens are returned to the bot wallet - this is known as arbitrage profit.
-}
type MatchResult = [MatchExecutionInfo]

{- | "Fill" refers to the _volume_ of the order filled. Therefore, its unit is always the 'commodityAsset'.

Of course, 'CompleteFill' just means the whole order is filled, whether it's buy or sell.

'PartialFill' means slightly different things for the two order types. But the 'Natural' field within
always designates the 'commodityAsset'.

For sell orders, `PartialFill n` indicates that n amount of commodity tokens will be sold from the order,
and the respective payment will be made in the currency asset.

For buy orders, `PartialFill n` indicates that n amount of
commodity tokens should be bought, and the corresponding price (orderPrice * n), _floored_ if necessary,
must be paid by the order.

**NOTE**: The 'n' in 'PartialFill n' must not be the max volume of the order. Use 'CompleteFill' in those scenarios.
-}
data FillType = CompleteFill | PartialFill Natural deriving stock (Eq, Show)

executionSkeleton
  :: GYDexApiMonad m a
  => PORefs
  -> MatchResult
  -> m (GYTxSkeleton 'PlutusV2)
executionSkeleton pors mr = fillMultiplePartialOrders' pors (map f mr) Nothing mempty
  where
    f (OrderExecutionInfo ft o) =
      let oi = fromJust $ mPoi o in  -- It's always under `Just` constructor in our code, but we aren't able to get rid of `Maybe` type for now since that would require significant changes in the test-suite.
      (oi
      , case ft of
          CompleteFill -> poiOfferedAmount oi
          PartialFill n ->
            if isBuyOrder o then
              floor $ fromIntegral n * getPrice (price o)
            else
              n
      )


matchExecutionInfoUtxoRef :: MatchExecutionInfo -> GYTxOutRef
matchExecutionInfoUtxoRef (OrderExecutionInfo CompleteFill OrderInfo {orderRef}) = orderRef
matchExecutionInfoUtxoRef (OrderExecutionInfo (PartialFill _) OrderInfo {orderRef}) = orderRef
