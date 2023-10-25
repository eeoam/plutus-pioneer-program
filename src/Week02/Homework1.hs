

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Homework1 where


import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx (BuiltinData, compile, unstableMakeIsData)
import PlutusTx.Builtins as Builtins (mkI)
import PlutusTx.Prelude (otherwise, traceError, Bool(..), Eq((==)), Integer, traceIfFalse, ($))
import Prelude (IO)
import Utilities (wrapValidator, writeValidatorToFile)



-- | This should validate if and only if the two booleans in the redeemer are True!
{-# INLINABLE mkValidator #-}
mkValidator :: () -> (Bool, Bool) -> PlutusV2.ScriptContext -> Bool
mkValidator _ (a,b) _ = a && b

{-# INLINABLE wrappedMkVal #-}
wrappedVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedVal = wrapValidator mkValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile[|| wrappedVal ||])



