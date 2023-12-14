{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : GeniusYield.DEX.Api.PartialOrder
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.DEX.Api.PartialOrder
    ( PartialOrderDatum (..)
    , PartialOrderAction (..)
    , PartialOrderInfo (..)
    , partialOrders
    , fillMultiplePartialOrders
    , getPartialOrderInfo
    , getPartialOrdersInfos
    ) where

import           Control.Monad.Except                   (ExceptT (..),
                                                         runExceptT)
import qualified Data.Map.Merge.Strict                  as Map
import qualified Data.Map.Strict                        as Map
import qualified Data.Set                               as Set
import qualified PlutusLedgerApi.V1                     as Plutus
import qualified PlutusLedgerApi.V1.Value               as Plutus
import qualified PlutusTx
import qualified PlutusTx.AssocMap                      as PlutusTx
import qualified PlutusTx.Prelude                       as PlutusTx
import qualified PlutusTx.Ratio                         as PlutusTx

import           Control.Monad.Error.Class              (liftEither)
import           Control.Monad.Reader
import           Data.Foldable                          (foldlM)
import           Data.Maybe                             (fromJust)
import           GeniusYield.DEX.Api.PartialOrderConfig (PartialOrderConfigInfoF (..),
                                                         fetchPartialOrderConfig)
import           GeniusYield.DEX.Api.Types
import           GeniusYield.HTTP.Errors                (IsGYApiError (..))
import           GeniusYield.Imports
import           GeniusYield.TxBuilder.Class
import           GeniusYield.TxBuilder.Errors           (GYTxMonadException,
                                                         throwAppError)
import           GeniusYield.Types

-------------------------------------------------------------------------------
-- Partial Order Datum
-------------------------------------------------------------------------------

-- | Representation of total fees contained in the order.
data PartialOrderContainedFee = PartialOrderContainedFee
    { pocfLovelaces     :: Integer
    -- ^ Fees explicitly charged in lovelaces, like flat lovelace fee collected
    --    from maker and taker(s).
    , pocfOfferedTokens :: Integer
    -- ^ Fees explicitly collected as percentage of offered tokens from maker.
    , pocfAskedTokens   :: Integer
    -- ^ Fees explicitly collected as percentage of asked tokens from taker.
    } deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''PartialOrderContainedFee


instance Semigroup PartialOrderContainedFee where
  (<>) a b =
    PartialOrderContainedFee
      { pocfLovelaces     = pocfLovelaces a + pocfLovelaces b
      , pocfOfferedTokens = pocfOfferedTokens a + pocfOfferedTokens b
      , pocfAskedTokens   = pocfAskedTokens a + pocfAskedTokens b
      }

instance Monoid PartialOrderContainedFee where mempty = PartialOrderContainedFee 0 0 0

-- | Datum of the fee output.
data PartialOrderFeeOutput = PartialOrderFeeOutput
    { pofdMentionedFees :: PlutusTx.Map Plutus.TxOutRef Plutus.Value
      -- ^ Map, mapping order being consumed to the collected fees.
    , pofdReservedValue :: Plutus.Value
      -- ^ Value reserved in this UTxO which is not to be considered as fees.
    , pofdSpentUTxORef  :: Maybe Plutus.TxOutRef
      -- ^ If not @Nothing@, it mentions the UTxO being consumed, whose value is used to provide for UTxOs minimum ada requirement.
    } deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''PartialOrderFeeOutput

data PartialOrderDatum = PartialOrderDatum
    { podOwnerKey              :: Plutus.PubKeyHash
    -- ^ Public key hash of the owner. Order cancellations must be signed by this.
    , podOwnerAddr             :: Plutus.Address
    -- ^ Address of the owner. Payments must be made to this address.
    , podOfferedAsset          :: Plutus.AssetClass
    -- ^ The asset being offered.
    , podOfferedOriginalAmount :: Integer
    -- ^ Original number of units being offered. Initially, this would be same as `podOfferedAmount`.
    , podOfferedAmount         :: Integer
    -- ^ The number of units being offered.
    , podAskedAsset            :: Plutus.AssetClass
    -- ^ The asset being asked for as payment.
    , podPrice                 :: PlutusTx.Rational
    -- ^ The price for one unit of the offered asset.
    , podNFT                   :: Plutus.TokenName
    -- ^ Token name of the NFT identifying this order.
    , podStart                 :: Maybe Plutus.POSIXTime
    -- ^ The time when the order can earliest be filled (optional).
    , podEnd                   :: Maybe Plutus.POSIXTime
    -- ^ The time when the order can latest be filled (optional).
    , podPartialFills          :: Integer
    -- ^ Number of partial fills order has undergone, initially would be 0.
    , podMakerLovelaceFlatFee  :: Integer
    -- ^ Flat fee (in lovelace) paid by the maker.
    , podTakerLovelaceFlatFee  :: Integer
    -- ^ Flat fee (in lovelace) paid by the taker.
    , podContainedFee          :: PartialOrderContainedFee
    -- ^ Total fees contained in the order.
    , podContainedPayment      :: Integer
    -- ^ Payment (in asked asset) contained in the order.
    } deriving stock (Generic, Show)

PlutusTx.makeIsDataIndexed ''PartialOrderDatum [('PartialOrderDatum, 0)]

-- | Exceptions raised while (partially) filling (partial) orders.
data FillOrderException
    -- | Attempt to (partially) fill an order too early.
    = TooEarlyFill { foeStart :: !GYSlot, foeNow :: !GYSlot }
    -- | Attempt to (partially) fill an order too late.
    | TooLateFill { foeEnd :: !GYSlot, foeNow :: !GYSlot }
    deriving stock Show
    deriving anyclass (Exception, IsGYApiError)

data PodException = PodNftNotAvailable
                  | PodNonPositiveAmount !Integer
                  | PodNonPositivePrice !GYRational
                  | PodRequestedAmountGreaterOrEqualToOfferedAmount
                    { poeReqAmt        :: !Natural
                    , poeOfferedAmount :: !Natural
                    }
                  | PodRequestedAmountGreaterThanOfferedAmount {poeReqAmt:: !Natural, poeOfferedAmount :: !Natural}
                  | PodNonDifferentAssets !GYAssetClass
                    -- ^ Offered asset is same as asked asset.
                  | PodEndEarlierThanStart
                      !GYTime  -- ^ Start time.
                      !GYTime  -- ^ End time.
                  | PodNegativeFrontendFee !GYValue
                  | PodNotAllOrderRefsPresent  -- ^ We couldn't fetch information for some of the given `GYTxOutRef`s. Note that this does not relate to UTxO being spent as depending upon provider, we would fetch information for even those `GYTxOutRef` which have been spent.
                      !(Set.Set GYTxOutRef)  -- ^ Missing output refs.
    deriving stock Show
    deriving anyclass (Exception, IsGYApiError)

-------------------------------------------------------------------------------
-- Partial Order action
-------------------------------------------------------------------------------

data PartialOrderAction
    = PartialCancel
    | PartialFill !Integer
    | CompleteFill
    deriving stock (Show, Eq, Ord)

PlutusTx.makeIsDataIndexed ''PartialOrderAction [ ('PartialCancel, 0)
                                                , ('PartialFill, 1)
                                                , ('CompleteFill, 2)
                                                ]

-------------------------------------------------------------------------------
-- Partial Order info
-------------------------------------------------------------------------------

data POIContainedFee = POIContainedFee
    { poifLovelaces     :: !Natural
    , poifOfferedTokens :: !Natural
    , poifAskedTokens   :: !Natural
    } deriving stock (Show, Eq, Generic)

instance Semigroup POIContainedFee where
  (<>) a b =
    POIContainedFee
      { poifLovelaces     = poifLovelaces a + poifLovelaces b
      , poifOfferedTokens = poifOfferedTokens a + poifOfferedTokens b
      , poifAskedTokens   = poifAskedTokens a + poifAskedTokens b
      }

instance Monoid POIContainedFee where mempty = POIContainedFee 0 0 0

data PartialOrderInfo = PartialOrderInfo
    { poiRef                   :: !GYTxOutRef         -- ^ Reference to the partial order.
    , poiOwnerKey              :: !GYPubKeyHash       -- ^ Public key hash of the owner.
    , poiOwnerAddr             :: !GYAddress          -- ^ Address of the owner.
    , poiOfferedAsset          :: !GYAssetClass       -- ^ The asset being offered.
    , poiOfferedOriginalAmount :: !Natural            -- ^ The number of units originally offered.
    , poiOfferedAmount         :: !Natural            -- ^ The number of units being offered.
    , poiAskedAsset            :: !GYAssetClass       -- ^ The asset being asked for as payment.
    , poiPrice                 :: !GYRational         -- ^ The price for one unit of the offered asset.
    , poiNFT                   :: !GYTokenName        -- ^ Token name of the NFT identifying this partial order.
    , poiStart                 :: !(Maybe GYTime)     -- ^ The time when the order can earliest be filled (optional).
    , poiEnd                   :: !(Maybe GYTime)     -- ^ The time when the order can latest be filled (optional).
    , poiPartialFills          :: !Natural            -- ^ The number of past partial fills.
    , poiMakerLovelaceFlatFee  :: !Natural            -- ^ Flat fee (in lovelace) paid by the maker.
    , poiTakerLovelaceFlatFee  :: !Natural            -- ^ Flat fee (in lovelace) paid by the taker.
    , poiContainedFee          :: !POIContainedFee    -- ^ Fee contained in the order.
    , poiContainedPayment      :: !Natural            -- ^ Payment (in asked asset) contained in the order.
    , poiUTxOValue             :: !GYValue            -- ^ Total value in the UTxO.
    , poiUTxOAddr              :: !GYAddress          -- ^ Address of the order UTxO.
    , poiNFTCS                 :: !GYMintingPolicyId  -- ^ Caching the CS to avoid recalculating for it.
    } deriving stock (Show, Eq, Generic)

poiContainedFeeToPlutus :: POIContainedFee -> PartialOrderContainedFee
poiContainedFeeToPlutus POIContainedFee {..} =
  PartialOrderContainedFee
    { pocfLovelaces     = fromIntegral poifLovelaces
    , pocfOfferedTokens = fromIntegral poifOfferedTokens
    , pocfAskedTokens   = fromIntegral poifAskedTokens
    }

poiContainedFeeFromPlutus :: PartialOrderContainedFee -> POIContainedFee
poiContainedFeeFromPlutus PartialOrderContainedFee {..} =
  POIContainedFee
    { poifLovelaces     = fromIntegral pocfLovelaces
    , poifOfferedTokens = fromIntegral pocfOfferedTokens
    , poifAskedTokens   = fromIntegral pocfAskedTokens
    }

-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------

-- | List and transform all the partial orders for the given function.
partialOrders
    :: forall a m
    . GYApiQueryMonad m
    => (PartialOrderInfo -> Maybe a)
    -- ^ Filter + Transformer function. Nothing means ignore that partial order.
    -> m [a]
partialOrders pOrderPredicate = do
    DEXInfo {dexPartialOrderValidator} <- ask

    addr <- scriptAddress dexPartialOrderValidator
    let paymentCred = addressToPaymentCredential addr & fromJust
    foldM mkPOrderInfo [] =<< utxosAtPaymentCredentialWithDatums paymentCred

  where
    mkPOrderInfo
        :: GYApiQueryMonad m
        => [a]
        -> (GYUTxO, Maybe GYDatum)
        -> m [a]
    mkPOrderInfo acc (_, Nothing) = return acc
    mkPOrderInfo acc (GYUTxO {utxoRef, utxoAddress, utxoValue}, Just gyDatum) =
        case Plutus.fromBuiltinData $ datumToPlutus' gyDatum of
            Nothing -> return acc
            Just d -> either (const acc) (addPartialOrderInfo acc) <$>
                      runExceptT
                      (makePartialOrderInfo utxoRef (utxoAddress, utxoValue, d))

    addPartialOrderInfo
        :: [a]
        -> PartialOrderInfo
        -> [a]
    addPartialOrderInfo pois = maybe pois (:pois) . pOrderPredicate

-------------------------------------------------------------------------------
-- Tx building
-------------------------------------------------------------------------------

{- | Fills multiple orders. If the provided amount of offered tokens to buy in an order is equal to the offered amount, then we completely fill the order. Otherwise, it gets partially filled.
-}
fillMultiplePartialOrders
    :: (HasCallStack, GYApiMonad m)
    => [(Either GYTxOutRef PartialOrderInfo, Natural)]
    -> m (GYTxSkeleton PlutusV2)
fillMultiplePartialOrders eOrders = do
    di <- ask

    -- This machinery is needed to accomodate the fact that MatchExecutionInfos may not have the PartialOrderInfo. In that case we must query it from the ref.
    let separateOrders :: ([(GYTxOutRef, Natural)], [(PartialOrderInfo, Natural)])
                       -> (Either GYTxOutRef PartialOrderInfo, Natural)
                       -> ([(GYTxOutRef, Natural)], [(PartialOrderInfo, Natural)])
        separateOrders (!refOrders, !poiOrders) (Left ref, n)  = ((ref, n) : refOrders, poiOrders)
        separateOrders (!refOrders, !poiOrders) (Right poi, n) = (refOrders, (poi, n) : poiOrders)

        (!ordersWithRefAndAmount, !ordersWithPoiAndAmount) = foldl' separateOrders ([],[]) eOrders
        ordersWithRefAndAmount' = Map.fromList ordersWithRefAndAmount
    queriedOrders <- getPartialOrdersInfos $ Map.keys ordersWithRefAndAmount'
    -- Even though we use `dropMissing`, `getPartialOrdersInfos` verify that all entries are present.
    let otherOrdersWithPoiAndAmount = Map.elems $ Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMatched (\_ poi amt -> (poi, amt))) queriedOrders ordersWithRefAndAmount'
        orders = otherOrdersWithPoiAndAmount ++ ordersWithPoiAndAmount
        por = dexPORefs di

    (cfgRef, poci) <- fetchPartialOrderConfig (porRefAddr por) (porRefNft por)

    let buildWithFeeOutput = do
          let (!feeOutputMap, !totalContainedFee, !maxTakerFee) =
                foldl'
                  (\(!mapAcc, !feeAcc, !prevMaxTakerFee) (PartialOrderInfo {..}, amtToFill) ->
                    let curMaxTakerFee = max prevMaxTakerFee poiTakerLovelaceFlatFee in
                    if amtToFill == poiOfferedAmount then
                      let orderContainedFee = poiContainedFeeToValue poiContainedFee poiOfferedAsset poiAskedAsset
                      in (PlutusTx.unionWith (<>) mapAcc (PlutusTx.singleton (txOutRefToPlutus poiRef) (valueToPlutus orderContainedFee)), feeAcc <> orderContainedFee, curMaxTakerFee)
                    else (mapAcc, feeAcc, curMaxTakerFee)
                  )
                  (PlutusTx.empty, mempty, 0)
                  orders
              fee = totalContainedFee <> valueFromLovelace (fromIntegral maxTakerFee)
              feeOutput
                  | fee == mempty = mempty
                  | otherwise     =
                      mustHaveOutput $ mkGYTxOut (pociFeeAddr poci) fee $ datumFromPlutusData $ PartialOrderFeeOutput feeOutputMap mempty Nothing
          foldlM
            (\(!prevSkel) (poi@PartialOrderInfo {..}, amt) -> do
              commonCheck amt poiOfferedAmount
              cs <- validFillRangeConstraints poiStart poiEnd
              let skel =
                    if amt == poiOfferedAmount then

                      let expectedValueOut = expectedPaymentWithDeposit poi True

                      in
                           mustHaveInput (partialOrderInfoToIn poi CompleteFill di)
                        <> mustHaveOutput (partialOrderInfoToPayment poi expectedValueOut)
                        <> mustMint (mintingScript di) nothingRedeemer poiNFT (-1)
                        <> cs

                    else

                      let price' = partialOrderPrice poi amt
                          od = partialOrderInfoToPartialOrderDatum poi
                                { poiOfferedAmount    = poiOfferedAmount - amt
                                , poiPartialFills     = poiPartialFills + 1
                                , poiContainedPayment = poiContainedPayment + fromIntegral (valueAssetClass price' poiAskedAsset)
                                }

                          expectedValueOut = poiUTxOValue <> price' `valueMinus` valueSingleton poiOfferedAsset (toInteger amt)
                          o = mkGYTxOut poiUTxOAddr expectedValueOut (datumFromPlutusData od)

                      in
                           mustHaveInput (partialOrderInfoToIn poi (PartialFill $ toInteger amt) di)
                        <> mustHaveOutput o
                        <> cs

              pure $! prevSkel <> skel
            )
            (mustHaveRefInput cfgRef <> feeOutput)
            orders


    let buildWithoutFeeOutput = do
          let maxTakerFee = foldl' (\prevMaxTakerFee (PartialOrderInfo {..}, _) -> max prevMaxTakerFee poiTakerLovelaceFlatFee) 0 orders
          foldlM
            (\(!prevSkel) (idx, (poi@PartialOrderInfo {..}, amt)) -> do
              commonCheck amt poiOfferedAmount
              let price' = partialOrderPrice poi amt
                  tf = if idx == 1 then mempty { poifLovelaces = fromIntegral maxTakerFee } else mempty
                  od = partialOrderInfoToPartialOrderDatum poi
                        { poiOfferedAmount    = poiOfferedAmount - amt
                        , poiPartialFills     = poiPartialFills + 1
                        , poiContainedFee     = poiContainedFee <> tf
                        , poiContainedPayment = poiContainedPayment + fromIntegral (valueAssetClass price' poiAskedAsset)
                        }

                  expectedValueOut = poiUTxOValue <> price' <> poiContainedFeeToValue tf poiOfferedAsset poiAskedAsset `valueMinus` valueSingleton poiOfferedAsset (toInteger amt)
                  o = mkGYTxOut poiUTxOAddr expectedValueOut (datumFromPlutusData od)

              cs   <- validFillRangeConstraints poiStart poiEnd

              pure $!
                  prevSkel
                <> mustHaveInput (partialOrderInfoToIn poi (PartialFill $ toInteger amt) di)
                <> mustHaveOutput o
                <> cs
            )
            (mustHaveRefInput cfgRef)
            (zip [(1 :: Natural).. ] orders)

    if isJust $ find (\(PartialOrderInfo {..}, amt) -> amt == poiOfferedAmount) orders then buildWithFeeOutput
    else buildWithoutFeeOutput
  where
    commonCheck amt poiOfferedAmount = do
       when (amt == 0) . throwAppError $ PodNonPositiveAmount $ toInteger amt
       when (amt > poiOfferedAmount) . throwAppError $ PodRequestedAmountGreaterThanOfferedAmount amt poiOfferedAmount

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

partialOrderInfoToPartialOrderDatum :: PartialOrderInfo -> PartialOrderDatum
partialOrderInfoToPartialOrderDatum PartialOrderInfo {..} = PartialOrderDatum
    { podOwnerKey              = pubKeyHashToPlutus poiOwnerKey
    , podOwnerAddr             = addressToPlutus poiOwnerAddr
    , podOfferedAsset          = assetClassToPlutus poiOfferedAsset
    , podOfferedOriginalAmount = fromIntegral poiOfferedOriginalAmount
    , podOfferedAmount         = fromIntegral poiOfferedAmount
    , podAskedAsset            = assetClassToPlutus poiAskedAsset
    , podPrice                 = PlutusTx.fromGHC $ toRational poiPrice
    , podNFT                   = tokenNameToPlutus poiNFT
    , podStart                 = timeToPlutus <$> poiStart
    , podEnd                   = timeToPlutus <$> poiEnd
    , podPartialFills          = fromIntegral poiPartialFills
    , podMakerLovelaceFlatFee  = fromIntegral poiMakerLovelaceFlatFee
    , podTakerLovelaceFlatFee  = toInteger poiTakerLovelaceFlatFee
    , podContainedFee          = poiContainedFeeToPlutus poiContainedFee
    , podContainedPayment      = toInteger poiContainedPayment
    }

poiGetContainedFeeValue :: PartialOrderInfo -> GYValue
poiGetContainedFeeValue PartialOrderInfo {..} = poiContainedFeeToValue poiContainedFee poiOfferedAsset poiAskedAsset

poiContainedFeeToValue :: POIContainedFee -> GYAssetClass -> GYAssetClass -> GYValue
poiContainedFeeToValue POIContainedFee {..} offAC askAC =
  valueSingleton GYLovelace (fromIntegral poifLovelaces)
  <> valueSingleton offAC (fromIntegral poifOfferedTokens)
  <> valueSingleton askAC (fromIntegral poifAskedTokens)

-- | Builds an input consuming the given order for a particular action.
partialOrderInfoToIn :: PartialOrderInfo
                     -> PartialOrderAction
                     -> DEXInfo
                     -> GYTxIn PlutusV2
partialOrderInfoToIn oi@PartialOrderInfo {..} oa DEXInfo {..} = GYTxIn
    { gyTxInTxOutRef = poiRef
    , gyTxInWitness  = GYTxInWitnessScript
        script
        (datumFromPlutusData $ partialOrderInfoToPartialOrderDatum oi)
        $ redeemerFromPlutusData oa
    }
  where
    script = case porValidatorRef dexPORefs of
        Nothing -> GYInScript dexPartialOrderValidator
        Just ref -> GYInReference ref (validatorToScript dexPartialOrderValidator)


mintingScript :: DEXInfo -> GYMintScript PlutusV2
mintingScript DEXInfo{dexNftPolicy, dexPORefs} = case porNftPolicyRef dexPORefs of
  Nothing  -> GYMintScript dexNftPolicy
  Just ref -> GYMintReference ref (mintingPolicyToScript dexNftPolicy)

-- | Builds an output paying some value to the order owner, also adds the UTxO
--   reference of the order to the datum.
partialOrderInfoToPayment :: PartialOrderInfo -> GYValue -> GYTxOut 'PlutusV2
partialOrderInfoToPayment oi v = mkGYTxOut (poiOwnerAddr oi) v (datumFromPlutusData $ txOutRefToPlutus $ poiRef oi)

-- | Given an order and the bought amount from that order, returns the total
--   price.
partialOrderPrice :: PartialOrderInfo -> Natural -> GYValue
partialOrderPrice PartialOrderInfo {..} amt = valueSingleton poiAskedAsset $ ceiling $ rationalToGHC poiPrice * toRational amt

{- | Note that at any moment, an order UTxO contains:-
        * An NFT.
        * Remaining offered tokens.
        * Payment for tokens consumed.
        * Initial deposit.
        * Collected fees.
-}
expectedPaymentWithDeposit :: PartialOrderInfo -> Bool -> GYValue
expectedPaymentWithDeposit poi@PartialOrderInfo {..} isCompleteFill =
  let toSubtract = valueSingleton (GYToken poiNFTCS poiNFT) 1 <> valueSingleton poiOfferedAsset (toInteger poiOfferedAmount) <> poiGetContainedFeeValue poi
      toAdd = if isCompleteFill then partialOrderPrice poi poiOfferedAmount else mempty
  in poiUTxOValue <> toAdd `valueMinus` toSubtract

-- | Given a UTxO reference of an order, returns the complete information.
getPartialOrderInfo :: GYApiQueryMonad m
                    => GYTxOutRef
                    -> m PartialOrderInfo
getPartialOrderInfo orderRef = do
    utxoWithDatum <- utxoAtTxOutRefWithDatum' orderRef
    vod           <- utxoDatumPure' utxoWithDatum

    runExceptT (makePartialOrderInfo orderRef vod) >>= liftEither

getPartialOrdersInfos :: GYApiQueryMonad m
                      => [GYTxOutRef]
                      -> m (Map.Map GYTxOutRef PartialOrderInfo)
getPartialOrdersInfos orderRefs = do
    utxosWithDatums <- utxosAtTxOutRefsWithDatums orderRefs
    let vod = utxosDatumsPure utxosWithDatums
    when (Map.size vod /= length orderRefs) $ throwAppError $ PodNotAllOrderRefsPresent $ Set.fromList orderRefs `Set.difference` Map.keysSet vod
    runExceptT (Map.traverseWithKey makePartialOrderInfo vod) >>= liftEither

-- | Given the UTxO reference of an order, returns the complete information.
--   Checking the validity of the order.
makePartialOrderInfo :: GYApiQueryMonad m
                     => GYTxOutRef
                     -> (GYAddress, GYValue, PartialOrderDatum)
                     -> ExceptT GYTxMonadException m PartialOrderInfo
makePartialOrderInfo orderRef (utxoAddr, v, PartialOrderDatum {..}) = do
    DEXInfo{dexNftPolicy} <- ask
    addr         <- addressFromPlutus' podOwnerAddr

    key          <- pubKeyHashFromPlutus' podOwnerKey

    offeredAsset <- assetClassFromPlutus' podOfferedAsset
    nft          <- tokenNameFromPlutus' podNFT
    askedAsset   <- assetClassFromPlutus' podAskedAsset

    when (valueAssetClass v (GYToken (mintingPolicyId dexNftPolicy) nft) /= 1) $
        throwAppError PodNftNotAvailable

    return PartialOrderInfo
        { poiRef                    = orderRef
        , poiOwnerKey               = key
        , poiOwnerAddr              = addr
        , poiOfferedAsset           = offeredAsset
        , poiOfferedOriginalAmount  = fromInteger podOfferedOriginalAmount
        , poiOfferedAmount          = fromInteger podOfferedAmount
        , poiAskedAsset             = askedAsset
        , poiPrice                  = rationalFromPlutus podPrice
        , poiNFT                    = nft
        , poiStart                  = timeFromPlutus <$> podStart
        , poiEnd                    = timeFromPlutus <$> podEnd
        , poiPartialFills           = fromInteger podPartialFills
        , poiMakerLovelaceFlatFee   = fromIntegral podMakerLovelaceFlatFee
        , poiTakerLovelaceFlatFee   = fromInteger podTakerLovelaceFlatFee
        , poiContainedFee           = poiContainedFeeFromPlutus podContainedFee
        , poiContainedPayment       = fromInteger podContainedPayment
        , poiUTxOValue              = v
        , poiUTxOAddr               = utxoAddr
        , poiNFTCS                  = mintingPolicyId dexNftPolicy
        }

validFillRangeConstraints
    :: forall m
    .  GYTxQueryMonad m
    => Maybe GYTime
    -> Maybe GYTime
    -> m (GYTxSkeleton PlutusV2)
validFillRangeConstraints Nothing Nothing = return mempty
validFillRangeConstraints mstart mend = do
    now <- slotOfCurrentBlock
    sc <- maybe (return mempty) (startConstraint now) mstart
    ec <- maybe (return mempty) (endConstraint now) mend
    return $ sc <> ec
  where
    startConstraint :: GYSlot -> GYTime -> m (GYTxSkeleton PlutusV2)
    startConstraint now start = do
        startSlot <- enclosingSlotFromTime' start
        if now >= startSlot
            then return $ isInvalidBefore now
            else throwAppError $ TooEarlyFill {foeStart = startSlot, foeNow = now}

    endConstraint :: GYSlot -> GYTime -> m (GYTxSkeleton PlutusV2)
    endConstraint now end = do
        endSlot <- enclosingSlotFromTime' end
        if now <= endSlot
            then return $ isInvalidAfter $ min endSlot $ unsafeAdvanceSlot now 120
            else throwAppError $ TooLateFill {foeEnd = endSlot, foeNow = now}
