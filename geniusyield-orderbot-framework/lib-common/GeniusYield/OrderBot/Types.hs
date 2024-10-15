{- |
Module      : GeniusYield.OrderBot.Types
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OrderBot.Types (
  OrderInfo (OrderInfo, orderRef, orderType, assetInfo, volume, price, mPoi),
  SomeOrderInfo (SomeOrderInfo),
  OrderAssetPair (OAssetPair, currencyAsset, commodityAsset),
  OrderType (..),
  SOrderType (..),
  SOrderTypeI (..),
  Volume (..),
  Price (..),
  mkOrderInfo,
  isSellOrder,
  isBuyOrder,
  mkOrderAssetPair,
  equivalentAssetPair,
  mkEquivalentAssetPair,
  FillType (..),
  MatchExecutionInfo (..),
  completeFill,
  partialFill,
) where

import Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as Aeson
import Data.Kind (Type)
import Data.Ratio (denominator, numerator, (%))
import Data.Text (Text)
import Numeric.Natural (Natural)

import GeniusYield.Types.TxOutRef (GYTxOutRef, showTxOutRef)
import GeniusYield.Types.Value (GYAssetClass (..))

import GeniusYield.Api.Dex.PartialOrder (PartialOrderInfo (..))
import GeniusYield.Types (rationalToGHC)

-------------------------------------------------------------------------------
-- Information on DEX orders relevant to a matching strategy
-------------------------------------------------------------------------------

{- | For each unique 'OrderAssetPair' (see: 'equivalentAssetPair'): there are 2
'OrderInfo' types: 'OrderInfo BuyOrder', and 'OrderInfo SellOrder'.

For a buy order, the volume indicates the amount of 'commodityAsset' being bought.
Whereas the price indicates the requested price, in 'currencyAsset', of each
'commodityAsset' being bought.

For a sell order, the volume indicates the amount of 'commodityAsset' being sold.
Whereas the price indicates the requested price, in 'currencyAsset', for each
'commodityAsset' being sold.

See: 'mkOrderInfo'.
-}
type OrderInfo :: OrderType -> Type
data OrderInfo t = OrderInfo
  { orderRef :: !GYTxOutRef
  , orderType :: !(SOrderType t)
  , assetInfo :: !OrderAssetPair
  , volume :: !Volume
  -- ^ Volume of the 'commodityAsset', either being bought or sold.
  , price :: !Price
  -- ^ Price of each 'commodityAsset', in 'currencyAsset'.
  , mPoi :: !(Maybe PartialOrderInfo)
  -- ^ The complete PartialOrderInfo. To avoid quering it again when filling the order
  }
  deriving stock (Eq, Show)

-- | Existential that can encapsulate both buy and sell orders.
data SomeOrderInfo = forall t. SomeOrderInfo (OrderInfo t)

deriving stock instance Show SomeOrderInfo

{- | Given a partialOrderInfo from a DEX order on the chain, and the
OrderAssetPair that determines the commodity and currency asset. Determine the
type of the order (buy/sell) and yield a 'exists t. OrderInfo t'.

== How does one deal with converting volume and price?

Inside an 'OrderInfo', the 'volume' always uses the 'commodityAsset' as its unit,
whereas the 'price' uses the 'currencyAsset' as its unit. In the DEX however, all
orders are really sell orders. They are offering some amount of some asset, and
asking for another.

For sell orders, where the offered asset in the DEX order is deemed to be a
'commodityAsset', there is no conversion necessary. 'volume' is simply in terms
of the offered asset amount and the minFill is simply 1.
Similarly, 'price' is the same as the DEX order's price.

But what about buy orders? These are the orders that are offering an asset which
is deemed to be a 'currencyAsset'. And they are asking for an asset which is deemed
to be a 'commodityAsset'.

In that case, the price is simply the DEX order's price but flipped (e.g x % y -> y % x).

The volume conversion is slightly more involved, the max volume is the DEX order's
price multiplied by the DEX order's offered amount. If the result is not a whole
number, it is ceiled - because more payment is always accepted, but less is not.
The min volume is just the ceiling of the price, because that's the amount of
commodity assets you would need to pay to access 1 of the offered currencyAssets.
-}
mkOrderInfo ::
  -- | The order token Pair with currency and commodity assets
  OrderAssetPair ->
  -- | The partialOrderInfo to use when building this OrderInfo.
  PartialOrderInfo ->
  SomeOrderInfo
mkOrderInfo oap poi@PartialOrderInfo {..} = case orderType of
  BuyOrder ->
    let maxVolume = ceiling $ (toInteger poiOfferedAmount % 1) * askedPrice
        minVolume = ceiling askedPrice
     in builder
          SBuyOrder
          (Volume minVolume maxVolume)
          $ Price (denominator askedPrice % numerator askedPrice)
  SellOrder ->
    builder
      SSellOrder
      (Volume 1 poiOfferedAmount)
      $ Price askedPrice
 where
  orderType = mkOrderType poiAskedAsset oap
  askedPrice = rationalToGHC poiPrice
  builder :: SOrderType t -> Volume -> Price -> SomeOrderInfo
  builder t vol price = SomeOrderInfo $ OrderInfo poiRef t oap vol price (Just poi)

isSellOrder :: OrderInfo t -> Bool
isSellOrder OrderInfo {orderType = SSellOrder} = True
isSellOrder _ = False

isBuyOrder :: OrderInfo t -> Bool
isBuyOrder OrderInfo {orderType = SBuyOrder} = True
isBuyOrder _ = False

-------------------------------------------------------------------------------
-- Order classification components.
-------------------------------------------------------------------------------

data OrderType = BuyOrder | SellOrder deriving stock (Eq, Show)

data SOrderType (t :: OrderType) where
  SBuyOrder :: SOrderType 'BuyOrder
  SSellOrder :: SOrderType 'SellOrder

deriving stock instance Eq (SOrderType t)
deriving stock instance Show (SOrderType t)

class SOrderTypeI (t :: OrderType) where
  sOrderType :: SOrderType t

instance SOrderTypeI 'BuyOrder where
  sOrderType = SBuyOrder

instance SOrderTypeI 'SellOrder where
  sOrderType = SSellOrder

-------------------------------------------------------------------------------
-- Order components
-------------------------------------------------------------------------------

{- | The amount of the commodity asset (being brought or sold), represented as
a closed interval.

Although the contract permits fills as low a 1 indivisible token,
the @volumeMin@ field is still needed, because Buy orders are normalized and you
can't always fill it for 1. The amount depends on the price of the order.

@volumeMin@ should always be @<= volumeMax@. Users are responsible for maintaining
this invariant.
-}
data Volume = Volume
  { volumeMin :: !Natural
  -- ^ Minimum bound of the Order volume interval.
  , volumeMax :: !Natural
  -- ^ Maximum bound of the Order volume interval.
  }
  deriving stock (Eq, Show, Ord)

instance Semigroup Volume where
  (Volume minV1 maxV1) <> (Volume minV2 maxV2) = Volume (minV1 + minV2) (maxV1 + maxV2)
  {-# INLINEABLE (<>) #-}

instance Monoid Volume where
  mempty = Volume 0 0
  {-# INLINEABLE mempty #-}

-- | The amount of currency asset (per commodity asset) offered or asked for in an order.
newtype Price = Price {getPrice :: Rational} deriving stock (Show, Eq, Ord)

instance Semigroup Price where
  p1 <> p2 = Price $ getPrice p1 + getPrice p2
  {-# INLINEABLE (<>) #-}

instance Monoid Price where
  mempty = Price 0
  {-# INLINEABLE mempty #-}

{- | The asset pair in a DEX Order.

All 'OrderAssetPair's constructed out of equivalent raw asset pairs, must compare
equal. See: 'equivalentAssetPair'.

For each unique asset pair (see: 'mkAssetPair'), one asset is chosen as the
"commodity" (being sold), and the other is chosen as the "currency" - this makes
it simpler to perform order matching.
-}
data OrderAssetPair = OAssetPair
  { currencyAsset :: !GYAssetClass
  , commodityAsset :: !GYAssetClass
  }
  deriving stock (Eq, Ord, Show)

instance ToJSON OrderAssetPair where
  toJSON OAssetPair {currencyAsset, commodityAsset} =
    Aeson.object
      [ "currencyAsset" .= currencyAsset
      , "commodityAsset" .= commodityAsset
      ]

{- | Two order asset pairs are considered "equivalent" (but not strictly equal, as in 'Eq'),
     if they contain the same 2 assets irrespective of order.
     i.e {currencyAsset = A, commodityAsset = B} and
         {currencyAsset = B, commodityAsset = A} are equivalent.
-}
equivalentAssetPair :: OrderAssetPair -> OrderAssetPair -> Bool
equivalentAssetPair oap oap' = oap == oap' || oap == mkEquivalentAssetPair oap'

mkEquivalentAssetPair :: OrderAssetPair -> OrderAssetPair
mkEquivalentAssetPair oap =
  OAssetPair
    { commodityAsset = currencyAsset oap
    , currencyAsset = commodityAsset oap
    }

mkOrderAssetPair ::
  -- | Asset class of the currency asset in the order.
  GYAssetClass ->
  -- | Asset class of the commodity asset in the order.
  GYAssetClass ->
  OrderAssetPair
mkOrderAssetPair curAsset comAsset =
  OAssetPair
    { commodityAsset = comAsset
    , currencyAsset = curAsset
    }

mkOrderType ::
  -- | Asset class of the asked asset in the order.
  GYAssetClass ->
  -- | Order Asset Pair with commodity and currency assets
  OrderAssetPair ->
  OrderType
mkOrderType asked oap
  | commodityAsset oap == asked = BuyOrder
  | otherwise = SellOrder

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

data MatchExecutionInfo
  = forall t. OrderExecutionInfo !FillType {-# UNPACK #-} !(OrderInfo t)

instance ToJSON MatchExecutionInfo where
  toJSON
    ( OrderExecutionInfo
        fillT
        OrderInfo
          { orderRef
          , orderType
          , assetInfo
          , volume
          , price = Price {getPrice = x}
          }
      ) =
      Aeson.object
        [ "utxoRef" .= showTxOutRef orderRef
        , "volumeMin" .= volumeMin volume
        , "volumeMax" .= volumeMax volume
        , "price" .= x
        , "commodity" .= commodityAsset assetInfo
        , "currency" .= currencyAsset assetInfo
        , "type" .= prettySOrderType orderType
        , "fillType" .= show fillT
        ]
     where
      prettySOrderType :: SOrderType t -> Text
      prettySOrderType SBuyOrder = "Buy"
      prettySOrderType SSellOrder = "Sell"

completeFill :: OrderInfo t -> MatchExecutionInfo
completeFill = OrderExecutionInfo CompleteFill

partialFill :: OrderInfo t -> Natural -> MatchExecutionInfo
partialFill o n = OrderExecutionInfo (PartialFill n) o
