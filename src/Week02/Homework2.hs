

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Homework2 where


import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx (BuiltinData, compile, unstableMakeIsData)
import PlutusTx.Builtins as Builtins (mkI)
import PlutusTx.Prelude (otherwise, traceError, Bool(..), Eq((==)), Integer, traceIfFalse, ($))
import Prelude (IO)
import Utilities (wrapValidator, writeValidatorToFile)



data MyRedeemer = MyRedeemer
    { flag1 :: Bool
    , flag2 :: Bool
    }
PlutusTx.unstableMakeIsData ''MyRedeemer

-- | This should validate if and only if the two booleans in the redeemer are different.
{-# INLINABLE mkValidator #-}
mkValidator :: () -> MyRedeemer -> PlutusV2.ScriptContext -> Bool
mkValidator _ (MyRedeemer a b) _ = a /= b

{-# INLINABLE wrappedMkVal #-}
wrappedVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedVal = wrapValidator mkValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile[|| wrappedVal ||])

