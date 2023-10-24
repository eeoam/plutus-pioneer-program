
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Gift where

import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx (BuiltinData, compile)
import Prelude (IO)
import Utilities (writeValidatorToFile)

-- | This validator always succeeds.
{-# INLINABLE mkGiftValidator #-}
mkGiftValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkGiftValidator datum redeemer scriptContext = ()

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkGiftValidator ||])

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/gift.plutus" validator