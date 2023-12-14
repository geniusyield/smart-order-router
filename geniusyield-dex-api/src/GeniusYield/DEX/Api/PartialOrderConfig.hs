{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : GeniusYield.DEX.Api.PartialOrderConfig
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}

module GeniusYield.DEX.Api.PartialOrderConfig
    ( PocdException (..)
    , PartialOrderConfigInfoF (..)
    , PartialOrderConfigInfo
    , fetchPartialOrderConfig
    ) where

import qualified Cardano.Api                as Api
import           Data.Text                  (pack)
import           Network.HTTP.Types         (status400)

import           GeniusYield.HTTP.Errors    (GYApiError (..), IsGYApiError (..))
import           GeniusYield.Imports
import           GeniusYield.TxBuilder      (GYTxQueryMonad (utxosAtAddressWithDatums),
                                             addressFromPlutus', throwAppError,
                                             utxoDatumPure')
import           GeniusYield.Types
import qualified PlutusLedgerApi.V1         as Plutus
import           PlutusTx                   (BuiltinData,
                                             FromData (fromBuiltinData))
import qualified PlutusTx
import           PlutusTx.Builtins.Internal (BuiltinByteString (..))
import           PlutusTx.Ratio             as PlutusTx (Rational)

data PartialOrderConfigDatum = PartialOrderConfigDatum
    { pocdSignatories    :: [Plutus.PubKeyHash]
    -- ^ Public key hashes of the potential signatories.
    , pocdReqSignatories :: Integer
    -- ^ Number of required signatures.
    , pocdNftSymbol      :: Plutus.CurrencySymbol
    -- ^ Currency symbol of the partial order Nft.
    , pocdFeeAddr        :: Plutus.Address
    -- ^ Address to which fees are paid.
    , pocdMakerFeeFlat   :: Integer
    -- ^ Flat fee (in lovelace) paid by the maker.
    , pocdMakerFeeRatio  :: PlutusTx.Rational
    -- ^ Proportional fee (in the offered token) paid by the maker.
    , pocdTakerFee       :: Integer
    -- ^ Flat fee (in lovelace) paid by the taker.
    , pocdMinDeposit     :: Integer
    -- ^ Minimum required deposit (in lovelace).
    } deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''PartialOrderConfigDatum

data PartialOrderConfigInfoF addr = PartialOrderConfigInfo
    { pociSignatories    :: ![GYPubKeyHash]
    -- ^ Public key hashes of the potential signatories.
    , pociReqSignatories :: !Integer
    -- ^ Number of required signatures.
    , pociNftSymbol      :: !GYMintingPolicyId
    -- ^ Minting Policy Id of the partial order Nft.
    , pociFeeAddr        :: !addr
    -- ^ Address to which fees are paid.
    , pociMakerFeeFlat   :: !Integer
    -- ^ Flat fee (in lovelace) paid by the maker.
    , pociMakerFeeRatio  :: !GYRational
    -- ^ Proportional fee (in the offered token) paid by the maker.
    , pociTakerFee       :: !Integer
    -- ^ Flat fee (in lovelace) paid by the taker.
    , pociMinDeposit     :: !Integer
    -- ^ Minimum required deposit (in lovelace).
    } deriving stock (Show, Generic, Functor)

type PartialOrderConfigInfo = PartialOrderConfigInfoF GYAddress

instance FromData (PartialOrderConfigInfoF Plutus.Address) where
    fromBuiltinData :: BuiltinData -> Maybe (PartialOrderConfigInfoF Plutus.Address)
    fromBuiltinData d = do
            PartialOrderConfigDatum{..} <- fromBuiltinData d
            signatories <- fromEither $ mapM pubKeyHashFromPlutus pocdSignatories
            nftSymbol   <- fromEither $ mintingPolicyIdFromCurrencySymbol pocdNftSymbol
            pure PartialOrderConfigInfo
                { pociSignatories    = signatories
                , pociReqSignatories = pocdReqSignatories
                , pociNftSymbol      = nftSymbol
                , pociFeeAddr        = pocdFeeAddr
                , pociMakerFeeFlat   = pocdMakerFeeFlat
                , pociMakerFeeRatio  = rationalFromPlutus pocdMakerFeeRatio
                , pociTakerFee       = pocdTakerFee
                , pociMinDeposit     = pocdMinDeposit
                }
        where
            fromEither :: Either e a -> Maybe a
            fromEither = either (const Nothing) Just

newtype PocdException = PocdException GYAssetClass
  deriving stock Show
  deriving anyclass Exception

instance IsGYApiError PocdException where
    toApiError (PocdException nftToken) = GYApiError
        { gaeErrorCode  = "PARTIAL_ORDER_CONFIG_NOT_FOUND"
        , gaeHttpStatus = status400
        , gaeMsg = pack $ printf "Partial order config not found for NFT: %s"  nftToken
        }

fetchPartialOrderConfig :: GYTxQueryMonad m => GYAddress -> GYAssetClass -> m (GYTxOutRef, PartialOrderConfigInfo)
fetchPartialOrderConfig addr nftToken = do
    utxos <- utxosAtAddressWithDatums addr $ Just nftToken
    case utxos of
        [p@(utxo, Just _)] -> do
            (_, _, d') <- utxoDatumPure' p
            feeAddr    <- addressFromPlutus' $ pociFeeAddr d'
            pure (utxoRef utxo, feeAddr <$ d')
        _                  -> throwAppError $ PocdException nftToken

mintingPolicyIdFromCurrencySymbol :: Plutus.CurrencySymbol -> Either PlutusToCardanoError GYMintingPolicyId
mintingPolicyIdFromCurrencySymbol cs =
  let
    BuiltinByteString bs = Plutus.unCurrencySymbol cs
  in
    case Api.deserialiseFromRawBytes Api.AsPolicyId bs of
        Left e    -> Left $ DeserialiseRawBytesError $ pack $
            "mintingPolicyIdFromCurrencySymbol: " <> show cs <> ", error: " <> show e
        Right pid -> Right $ mintingPolicyIdFromApi pid
