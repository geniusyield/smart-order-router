{-|
Module      : GeniusYield.DEX.Api.Utils
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.DEX.Api.Utils
    ( maybeToExceptT
    , expectedTokenName
    , gyExpectedTokenName
    , validatorFromPly
    , mintingPolicyFromPly
    ) where

import           Control.Monad.Except            (ExceptT (..))
import           Data.Maybe                      (fromJust)

import qualified PlutusLedgerApi.V1              as Plutus (TokenName (..),
                                                            TxId (..),
                                                            TxOutRef (..),
                                                            serialiseUPLC)
import qualified PlutusTx.Builtins               as Plutus (BuiltinByteString,
                                                            consByteString,
                                                            sha2_256)

import           GeniusYield.Types               (GYMintingPolicy, GYTokenName,
                                                  GYTxOutRef, GYValidator,
                                                  SingPlutusVersion (..),
                                                  mintingPolicyFromSerialisedScript,
                                                  tokenNameFromPlutus,
                                                  txOutRefToPlutus,
                                                  validatorFromSerialisedScript)
import           GeniusYield.Types.PlutusVersion (PlutusVersion (..),
                                                  SingPlutusVersionI (singPlutusVersion))
import           Ply                             (ScriptRole (..),
                                                  TypedScript (..))
import qualified Ply

maybeToExceptT :: Functor m => e -> m (Maybe a) -> ExceptT e m a
maybeToExceptT err m = ExceptT $ fmap (maybe (Left err) Right) m

expectedTokenName :: Plutus.TxOutRef -> Plutus.TokenName
expectedTokenName (Plutus.TxOutRef (Plutus.TxId tid) ix) = Plutus.TokenName s
  where
    s :: Plutus.BuiltinByteString
    s = Plutus.sha2_256 (Plutus.consByteString ix tid)

gyExpectedTokenName :: GYTxOutRef -> GYTokenName
gyExpectedTokenName = fromJust . tokenNameFromPlutus . expectedTokenName .
                      txOutRefToPlutus

validatorFromPly
    :: forall v
    . SingPlutusVersionI v
    => TypedScript 'ValidatorRole '[]
    -> GYValidator v
validatorFromPly ts = case singPlutusVersion @v of
    SingPlutusV1 -> if Ply.getPlutusVersion ts == Ply.ScriptV1
      then validatorFromSerialisedScript @PlutusV1 $ toSerialisedValidator ts
      else error "validatorFromPly: Invalid script version"
    SingPlutusV2 -> if Ply.getPlutusVersion ts == Ply.ScriptV2
      then validatorFromSerialisedScript @PlutusV2 $ toSerialisedValidator ts
      else error "validatorFromPly: Invalid script version"
  where
    toSerialisedValidator (TypedScript _ s) = Plutus.serialiseUPLC s

mintingPolicyFromPly
    :: forall v
    . SingPlutusVersionI v
    => TypedScript 'MintingPolicyRole '[]
    -> GYMintingPolicy v
mintingPolicyFromPly ts = case singPlutusVersion @v of
    SingPlutusV1 -> if Ply.getPlutusVersion ts == Ply.ScriptV1
      then mintingPolicyFromSerialisedScript @PlutusV1 $ toSerialisedMintingPolicy ts
      else error "mintingPolicyFromPly: Invalid script version"
    SingPlutusV2 -> if Ply.getPlutusVersion ts == Ply.ScriptV2
      then mintingPolicyFromSerialisedScript @PlutusV2 $ toSerialisedMintingPolicy ts
      else error "mintingPolicyFromPly: Invalid script version"
  where
    toSerialisedMintingPolicy (TypedScript _ s) = Plutus.serialiseUPLC s
