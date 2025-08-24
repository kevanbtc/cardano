{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}

module UnykornLife.LifeToken where

import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Ledger
import Ledger.Contexts
import Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import Codec.Serialise (serialise)

-- | Very simple minting policy: always succeeds.
{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy _ _ = True

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy ||])

plutusScript :: Script
plutusScript = unMintingPolicyScript policy

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise plutusScript

script :: PlutusScript PlutusScriptV1
script = PlutusScriptSerialised scriptSBS
