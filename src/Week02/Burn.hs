

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}



module Burn where

import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx (BuiltinData, compile)
import PlutusTx.Prelude (traceError)
import Prelude (IO)
import Utilities (writeValidatorToFile)

-- | This validator always fails.
{-# INLINABLE mkBurnValidator #-}
mkBurnValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkBurnValidator datum redeemer scriptContext = traceError "it burns!!!"

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile[|| mkBurnValidator ||])

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/burn.plutus" validator