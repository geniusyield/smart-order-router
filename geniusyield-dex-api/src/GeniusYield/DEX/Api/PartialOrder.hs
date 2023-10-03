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
    , minDeposit
    , partialOrders
    , completelyFillPartialOrder
    , partiallyFillPartialOrder
    , getPartialOrderInfo
    ) where

import           Control.Monad.Except (ExceptT (..), runExceptT)
import qualified PlutusLedgerApi.V1 as Plutus
import qualified PlutusLedgerApi.V1.Value as Plutus
import qualified PlutusTx
import qualified PlutusTx.Prelude as PlutusTx
import qualified PlutusTx.Ratio as PlutusTx

import           GeniusYield.DEX.Api.Constants
import           GeniusYield.DEX.Api.Types
import           GeniusYield.Imports
import           GeniusYield.TxBuilder.Class
import           GeniusYield.Types
import           GeniusYield.TxBuilder.Errors (throwAppError, GYTxMonadException)
import           GeniusYield.HTTP.Errors (IsGYApiError (..))
import           Control.Monad.Error.Class (liftEither)
import           Control.Monad.Reader

-------------------------------------------------------------------------------
-- Partial Order datum
-------------------------------------------------------------------------------

data PartialOrderDatum = PartialOrderDatum
    { podOwnerKey              :: !Plutus.PubKeyHash
    -- ^ Public key hash of the owner. Order cancellations must be signed by this.
    , podOwnerAddr             :: !Plutus.Address
    -- ^ Address of the owner. Payments must be made to this address.
    , podOfferedAsset          :: !Plutus.AssetClass
    -- ^ The asset being offered.
    , podOfferedOriginalAmount :: !Integer
    -- ^ Original number of units being offered. Initially, this would be same as `podOfferedAmount`.
    ,  podOfferedAmount        :: !Integer
    -- ^ The number of units being offered.
    ,  podAskedAsset           :: !Plutus.AssetClass
    -- ^ The asset being asked for as payment.
    ,  podPrice                :: !PlutusTx.Rational
    -- ^ The price for one unit of the offered asset.
    ,  podMinFilling           :: !Integer
    -- ^ Minimal number of units of the offered asset that must be paid for in a partial filling.
    ,  podNFT                  :: !Plutus.TokenName
    -- ^ Token name of the NFT identifying this order.
    ,  podStart                :: !(Maybe Plutus.POSIXTime)
    -- ^ The time when the order can earliest be filled (optional).
    ,  podEnd                  :: !(Maybe Plutus.POSIXTime)
    -- ^ The time when the order can latest be filled (optional).
    , podFee                   :: Integer
    -- ^ Fee the filler is entitled to take.
    , podPartialFills          :: Integer
    -- ^ Number of partial fills order has undergone, initially would be 0.
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''PartialOrderDatum [ ('PartialOrderDatum, 0) ]

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
                  | PodNonPositiveMinFilling !Integer
                  | PodRequestedAmountGreaterOrEqualToOfferedAmount
                    { poeReqAmt:: !Natural
                    , poeOfferedAmount :: !Natural
                    }
                  | PodRequestedAmountLessThanMinFilling
                    { poeReqAmt:: !Natural
                    , poeMinFilling :: !Natural
                    }
                  | PodFeeNotEnough !Integer
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

data PartialOrderInfo = PartialOrderInfo
    { poiRef                   :: !GYTxOutRef
    -- ^ Reference to the partial order.
    , poiOwnerKey              :: !GYPubKeyHash
    -- ^ Public key hash of the owner.
    , poiOwnerAddr             :: !GYAddress
    -- ^ Address of the owner.
    , poiOfferedAsset          :: !GYAssetClass
    -- ^ The asset being offered.
    , poiOfferedOriginalAmount :: !Natural
    -- ^ The number of units originally offered.
    , poiOfferedAmount         :: !Natural
    -- ^ The number of units being offered.
    , poiAskedAsset            :: !GYAssetClass
    -- ^ The asset being asked for as payment.
    , poiPrice                 :: !GYRational
    -- ^ The price for one unit of the offered asset.
    , poiMinFilling            :: !Natural
    -- ^ Minimal number of units of the asked-for asset that must be paid in a partial filling.
    , poiNFT                   :: !GYTokenName
    -- ^ Token name of the NFT identifying this partial order.
    , poiFeesDeposits          :: !Natural
    -- ^ Number of lovelace included for fees and deposits.
    , poiStart                 :: !(Maybe GYTime)
    -- ^ The time when the order can earliest be filled (optional).
    , poiEnd                   :: !(Maybe GYTime)
    -- ^ The time when the order can latest be filled (optional).
    , poiFee                   :: !Natural
    -- ^ The fee for each filling.
    , poiPartialFills          :: !Natural
    -- ^ The number of past partial fills.
    } deriving stock (Show, Eq, Generic)

-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------

-- | List and transform all the partial orders for the given function.
partialOrders
    :: forall a m
    .  GYApiQueryMonad m
    => (PartialOrderInfo -> Maybe a)
    -- ^ Filter + Transformer function. Nothing means ignore that partial order.
    -> m [a]
partialOrders pOrderPredicate = do
    DEXInfo {dexPartialOrderValidator} <- ask

    addr <- scriptAddress dexPartialOrderValidator
    foldM mkPOrderInfo [] =<< utxosAtAddressesWithDatums [addr]

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

    addPartialOrderInfo :: [a] -> PartialOrderInfo -> [a]
    addPartialOrderInfo pois = maybe pois (:pois) . pOrderPredicate

-------------------------------------------------------------------------------
-- Tx building
-------------------------------------------------------------------------------

-- | Completely fill a partially-fillable order.
completelyFillPartialOrder
    :: (HasCallStack, GYApiMonad m)
    => Either GYTxOutRef PartialOrderInfo
    -- ^ The order reference.
    -> m (GYTxSkeleton PlutusV2)
completelyFillPartialOrder poiSource = do
    di@DEXInfo{dexPORefs} <- ask

    oi@PartialOrderInfo {..} <- case poiSource of
        Left orderRef -> getPartialOrderInfo orderRef
        Right poi     -> return poi

    let price = partialOrderPrice oi poiOfferedAmount
        feesAndDeposits = valueFromLovelace
                          $ max 0
                          $ toInteger poiFeesDeposits - toInteger poiFee
        refScript  = maybe mempty mustHaveRefInput (porValidatorRef dexPORefs)
        refMinting = maybe mempty mustHaveRefInput (porNftPolicyRef dexPORefs)

    cs <- validFillRangeConstraints poiStart poiEnd
    return $ mconcat
        [ mustHaveInput (partialOrderInfoToIn oi CompleteFill di)
        , mustHaveOutput (partialOrderInfoToPayment oi $ price <> feesAndDeposits)
        , mustMint (mintingScript di) nothingRedeemer poiNFT (-1)
        , cs
        , refScript
        , refMinting
        , mustHaveRefInput (porRefNftRef dexPORefs)
        ]

-- | Partially fill a partially-fillable order.
partiallyFillPartialOrder
    :: (HasCallStack, GYApiMonad m)
    => Either GYTxOutRef PartialOrderInfo
    -- ^ The order reference.
    -> Natural
    -- ^ The amount of offered tokens to buy.
    -> m (GYTxSkeleton PlutusV2)
partiallyFillPartialOrder poiSource amt = do
    di@DEXInfo{dexNftPolicy, dexPartialOrderValidator, dexPORefs} <- ask

    oi@PartialOrderInfo {..} <- case poiSource of
        Left orderRef -> getPartialOrderInfo orderRef
        Right poi -> return poi

    outAddr <- scriptAddress dexPartialOrderValidator

    when (amt == 0) . throwAppError
        $ PodNonPositiveAmount $ toInteger amt
    when (amt >= poiOfferedAmount) . throwAppError
        $ PodRequestedAmountGreaterOrEqualToOfferedAmount amt poiOfferedAmount
    when (amt < poiMinFilling) . throwAppError
        $ PodRequestedAmountLessThanMinFilling amt poiMinFilling

    let od = partialOrderInfoToPartialOrderDatum oi
                { poiOfferedAmount = poiOfferedAmount - amt
                , poiPartialFills  = poiPartialFills + 1
                }
        price = partialOrderPrice oi amt
        feesAndDeposits = valueFromLovelace
                          $ max 0
                          $ toInteger poiFeesDeposits -
                            toInteger (poiFee + minDeposit)
        v = mconcat
            [ valueSingleton poiOfferedAsset (toInteger $ poiOfferedAmount - amt)
            , feesAndDeposits
            , valueSingleton (GYToken (mintingPolicyId dexNftPolicy) poiNFT) 1
            ]
        payment   = price <> valueFromLovelace (toInteger minDeposit)

        o         = mkGYTxOut outAddr v (datumFromPlutusData od)
        refScript = maybe mempty mustHaveRefInput (porValidatorRef dexPORefs)

    cs <- validFillRangeConstraints poiStart poiEnd
    return $ mconcat
        [ mustHaveInput (partialOrderInfoToIn oi (PartialFill $ toInteger amt) di)
        , mustHaveOutput o
        , mustHaveOutput (partialOrderInfoToPayment oi payment)
        , cs
        , refScript
        , mustHaveRefInput (porRefNftRef dexPORefs)
        ]

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Builds an input consuming the given order for a particular action.
partialOrderInfoToIn
    :: PartialOrderInfo
    -> PartialOrderAction
    -> DEXInfo
    -> GYTxIn PlutusV2
partialOrderInfoToIn oi@PartialOrderInfo {..} oa DEXInfo {..} =
    GYTxIn
    { gyTxInTxOutRef = poiRef
    , gyTxInWitness = GYTxInWitnessScript
                      script
                      (datumFromPlutusData $ partialOrderInfoToPartialOrderDatum oi)
                      $ redeemerFromPlutusData oa
    }
  where
    script = case porValidatorRef dexPORefs of
        Nothing -> GYInScript dexPartialOrderValidator
        Just ref -> GYInReference ref (validatorToScript dexPartialOrderValidator)

-- | Builds an output paying some value to the order owner, also adds the UTxO
--   reference of the order to the datum.
partialOrderInfoToPayment :: PartialOrderInfo -> GYValue -> GYTxOut PlutusV2
partialOrderInfoToPayment oi v =
    mkGYTxOut (poiOwnerAddr oi) v
    (datumFromPlutusData $ txOutRefToPlutus $ poiRef oi)

-- | Given a UTxO reference of an order, returns the complete information.
getPartialOrderInfo :: GYApiQueryMonad m => GYTxOutRef -> m PartialOrderInfo
getPartialOrderInfo oref = do
    utxo <- utxoAtTxOutRef' oref
    vod <- utxoDatum' utxo
    runExceptT (makePartialOrderInfo oref vod) >>= liftEither

-- | Given an order and the bought amount from that order, returns the total
--   price.
partialOrderPrice :: PartialOrderInfo -> Natural -> GYValue
partialOrderPrice PartialOrderInfo {..} amt =
    valueSingleton poiAskedAsset $
    ceiling $ rationalToGHC poiPrice * toRational amt

mintingScript :: DEXInfo -> GYMintScript PlutusV2
mintingScript DEXInfo{dexNftPolicy, dexPORefs} =
    case porNftPolicyRef dexPORefs of
        Nothing -> GYMintScript dexNftPolicy
        Just ref -> GYMintReference ref (mintingPolicyToScript dexNftPolicy)

partialOrderInfoToPartialOrderDatum :: PartialOrderInfo -> PartialOrderDatum
partialOrderInfoToPartialOrderDatum PartialOrderInfo {..} =
    PartialOrderDatum
    { podOwnerKey              = pubKeyHashToPlutus poiOwnerKey
    , podOwnerAddr             = addressToPlutus poiOwnerAddr
    , podOfferedAsset          = assetClassToPlutus poiOfferedAsset
    , podOfferedOriginalAmount = fromIntegral poiOfferedOriginalAmount
    , podOfferedAmount         = fromIntegral poiOfferedAmount
    , podAskedAsset            = assetClassToPlutus poiAskedAsset
    , podPrice                 = PlutusTx.fromGHC $ toRational poiPrice
    , podMinFilling            = fromIntegral poiMinFilling
    , podNFT                   = tokenNameToPlutus poiNFT
    , podStart                 = timeToPlutus <$> poiStart
    , podEnd                   = timeToPlutus <$> poiEnd
    , podFee                   = fromIntegral poiFee
    , podPartialFills          = fromIntegral poiPartialFills
    }

-- | Given the UTxO reference of an order, returns the complete information.
--   Checking the validity of the order.
makePartialOrderInfo
    :: GYApiQueryMonad m
    => GYTxOutRef
    -> (GYAddress, GYValue, PartialOrderDatum)
    -> ExceptT GYTxMonadException m PartialOrderInfo
makePartialOrderInfo orderRef (_, v, PartialOrderDatum {..}) = do
    DEXInfo{dexNftPolicy} <- ask
    addr <- addressFromPlutus' podOwnerAddr
    key  <- pubKeyHashFromPlutus' podOwnerKey

    offeredAsset <- assetClassFromPlutus' podOfferedAsset
    nft          <- tokenNameFromPlutus' podNFT
    askedAsset   <- assetClassFromPlutus' podAskedAsset

    let price        = rationalFromPlutus podPrice
        feesDeposits = flip valueAssetClass GYLovelace $ v `valueMinus`
                       valueSingleton offeredAsset podOfferedAmount

    when (price <= 0) $
        throwAppError (PodNonPositivePrice price)

    when (valueAssetClass v (GYToken (mintingPolicyId dexNftPolicy) nft) /= 1) $
        throwAppError PodNftNotAvailable

    when (podFee < 200_000) $
        throwAppError (PodFeeNotEnough podFee)

    return PartialOrderInfo
        { poiRef                   = orderRef
        , poiOwnerKey              = key
        , poiOwnerAddr             = addr
        , poiOfferedAsset          = offeredAsset
        , poiOfferedOriginalAmount = fromInteger podOfferedOriginalAmount
        , poiOfferedAmount         = fromInteger podOfferedAmount
        , poiAskedAsset            = askedAsset
        , poiPrice                 = price
        , poiMinFilling            = fromInteger podMinFilling
        , poiNFT                   = nft
        , poiFeesDeposits          = fromInteger feesDeposits
        , poiStart                 = timeFromPlutus <$> podStart
        , poiEnd                   = timeFromPlutus <$> podEnd
        , poiFee                   = fromInteger podFee
        , poiPartialFills          = fromInteger podPartialFills
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
