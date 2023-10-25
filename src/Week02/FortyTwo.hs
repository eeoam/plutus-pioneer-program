

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module FortyTwo where


import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx (BuiltinData, compile)
import PlutusTx.Builtins as Builtins (mkI)
import PlutusTx.Prelude (otherwise, traceError, (==))
import Prelude (IO)
import Utilities (writeValidatorToFile)



-- | This validator only succeeds if the redeemer is equal to 42.
{-# INLINABLE mk42Validator #-}
mk42Validator :: BuiltinData -> BuiltinData -> BuiltinData
mk42Validator datum redeemer scriptContext 
    | redeemer == Builtins.mkI 42 = ()
    | otherwise                   = traceError "expected 42"

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile[|| mk42Validator ||])

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/fortytwo.plutus" validator

