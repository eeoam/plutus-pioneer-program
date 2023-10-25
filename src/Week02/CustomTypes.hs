

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CustomTypes where


import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx (BuiltinData, compile, unstableMakeIsData)
import PlutusTx.Builtins as Builtins (mkI)
import PlutusTx.Prelude (otherwise, traceError, Bool(..), Eq((==)), Integer, traceIfFalse, ($))
import Prelude (IO)
import Utilities (wrapValidator, writeValidatorToFile)



newtype MyRedeemer = MkMyRedeemer Integer
PlutusTx.unstableMakeIsData ''MyRedeemer

-- | This validator succeeds if the redeemer is equal to `MkMyRedeemer 42`.
{-# INLINABLE #-}
mkCTValidator :: () -> MyRedeemer -> PlutusV2.ScriptContext -> Bool
mkCTValidator datum (MkMyRedeemer r) scriptContext = traceIfFalse "expected 42" $ r == 42

{-# INLINABLE wrappedMkVal #-}
wrappedMkVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedMkVal = wrapValidator mkCTValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile[|| wrappedMkVal ||])

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/customtypes.plutus" validator

