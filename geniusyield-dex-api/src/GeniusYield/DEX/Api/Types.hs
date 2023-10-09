{-|
Module      : GeniusYield.DEX.Api.Types
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.DEX.Api.Types
    ( GYApiQueryMonad
    , GYApiMonad
    , DEXInfo (..)
    , PORefs (..)
    , mkDEXValidator
    , mkDEXMintingPolicy
    , mkPORefs
    ) where

import           Control.Monad.Reader          (MonadReader)

import           PlutusLedgerApi.V1            (Address)
import           PlutusLedgerApi.V1.Scripts    (ScriptHash)
import           PlutusLedgerApi.V1.Value      (AssetClass)
import           Ply                           (ScriptRole (..), TypedScript)
import           Ply.Core.Apply                ((#))

import           GeniusYield.DEX.Api.Constants (minDeposit)
import           GeniusYield.DEX.Api.Utils     (mintingPolicyFromPly,
                                                validatorFromPly)
import           GeniusYield.TxBuilder.Class   (GYTxMonad, GYTxQueryMonad)
import           GeniusYield.Types             (GYAddress, GYAssetClass,
                                                GYMintingPolicy, GYTxOutRef,
                                                GYValidator,
                                                PlutusVersion (PlutusV2),
                                                assetClassToPlutus,
                                                scriptPlutusHash,
                                                validatorToScript)

type GYApiQueryMonad m = (MonadReader DEXInfo m, GYTxQueryMonad m)

type GYApiMonad m = (GYApiQueryMonad m, GYTxMonad m)

-- | Type that encapsulates the scripts needed for the dex api.
data DEXInfo = DEXInfo
               { dexPartialOrderValidator :: !(GYValidator PlutusV2)
               , dexNftPolicy             :: !(GYMintingPolicy PlutusV2)
               , dexPORefs                :: !PORefs
               }

-- | Type that encapsulates the nescesary information for the partial order contract.
data PORefs = PORefs
    { porRefAddr      :: !GYAddress
    -- ^ The address where the reference NFT will be placed.
    , porRefNft       :: !GYAssetClass
    -- ^ The reference NFT.
    , porRefNftRef    :: !GYTxOutRef
    -- ^ The location of the reference NFT.
    , porValidatorRef :: !(Maybe GYTxOutRef)
    -- ^ The reference for the partial order validator.
    , porNftPolicyRef :: !(Maybe GYTxOutRef)
    -- ^ The reference for the partial order minting policy.
    } deriving stock Show

-- Smart Constructors

mkDEXMintingPolicy
    :: TypedScript 'MintingPolicyRole '[ScriptHash, Integer]
    -> GYValidator PlutusV2
    -> GYMintingPolicy PlutusV2
mkDEXMintingPolicy mintingPolicyRaw v = mintingPolicyFromPly $ mintingPolicyRaw
                                        # scriptPlutusHash (validatorToScript v)
                                        # toInteger minDeposit

mkDEXValidator
    :: TypedScript 'ValidatorRole '[Address, AssetClass, Integer]
    -> Address
    -> GYAssetClass
    -> GYValidator PlutusV2
mkDEXValidator validatorRaw addr ac = validatorFromPly $ validatorRaw
                                      # addr
                                      # assetClassToPlutus ac
                                      # fromIntegral minDeposit
mkPORefs
    :: GYAddress
    -> GYAssetClass
    -> GYTxOutRef
    -> Maybe GYTxOutRef
    -> Maybe GYTxOutRef
    -> PORefs
mkPORefs porAddr porAC porRef mVRef mNPRef =
    PORefs
    { porRefAddr      = porAddr
    , porRefNft       = porAC
    , porRefNftRef    = porRef
    , porValidatorRef = mVRef
    , porNftPolicyRef = mNPRef
    }
