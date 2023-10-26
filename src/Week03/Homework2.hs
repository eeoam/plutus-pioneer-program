

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}




module Week03.Homework2 where

import Data.Maybe (fromJust)
import Plutus.V1.Ledger.Interval (contains)
import Plutus.V2.Ledger.Api (POSIXTime, PubKeyHash,
                             ScriptContext (scriptContextTxInfo),
                             TxInfo (txInfoValidRange),
                             Validator, from, to, mkValidatorScript)
import Plutus.V2.Ledger.Api qualified as PlutusV2
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx (BuiltinData, compile, unstableMakeIsData
                 applyCode, liftCode, makeLift)
import PlutusTx.Builtins as Builtins (mkI)
import PlutusTx.Prelude (otherwise, traceError, Bool(..), Eq((==)), Integer, traceIfFalse, ($), (&&), (.))
import Prelude (IO, String)
import Utilities (wrapValidator, writeValidatorToFile
                  Network, posixTimeFromIso8601,
                  printDataToJSON,
                  validatorAddressBech32)



{-# INLINABLE mkValidator #-}
-- | This should validate if 
-- | the transaction has a signature from the parameterized beneficiary
-- | and the deadline has passed.
mkValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkValidator beneficiaryPkh deadline () ctx = 
    traceIfFalse "no beneficiary signature" beneficiarySigned &&
    traceIfFalse "deadline not reached"     afterDeadline
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        beneficiarySigned :: Bool
        beneficiarySigned = txSignedBy info beneficiaryPkh

        afterDeadline :: Bool
        afterDeadline = (from deadline) `contains` (txInfoValidRange info)

{-# INLINABLE mkWrappedValidator #-}
mkWrappedValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator . mkValidator

validator :: PubKeyHash -> PlutusV2.Validator
validator = mkValidatorScript $$(compile [|| mkWrappedValidator ||])

saveVal :: PubKeyHash -> IO ()
saveVal = writeValidatorToFile "./assets/parameterized-vesting.plutus" . validator

