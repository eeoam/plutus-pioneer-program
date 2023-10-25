

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CustomTypes where

newtype MyRedeemer = MkMyRedeemer Integer
PlutusTx.unstableMakeIsData ''MyRedeemer

-- | This validator succeeds if the redeemer is equal to `MkMyRedeemer 42`.
{-# INLINABLE #-}
mkCTValidator :: () -> MyRedeemer -> PlutusV2.ScriptContext -> Bool
mkCTValidator datum (MkMyRedeemer r) scriptContext = traceIfFalse "expected 42" $ r == 42

{=# INLINABLE wrappedMkVal #-}
wrappedMkVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedMkVal = wrapValidator mkCTValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile[|| wrappedMkVal ||])

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/customtypes.plutus" validator

