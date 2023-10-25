

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module FortyTwoTyped where


import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx (BuiltinData, compile)
import PlutusTx.Builtins as Builtins (mkI)
import PlutusTx.Prelude (otherwise, traceError, Bool, Eq((==)), Integer, traceIfFalse, ($))
import Prelude (IO)
import Utilities (wrapValidator, writeValidatorToFile)



-- | This validator only succeeds if the redeemer is equal to 42.
{-# INLINABLE mk42Validator #-}
mk42Validator :: () -> Integer -> PlutusV2.ScriptContext -> Bool
mk42Validator datum redeemer scriptContext = traceIfFalse "expected 42" $  redeemer == 42

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile[|| wrapValidator mk42Validator ||])

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/fortytwotyped.plutus" validator

