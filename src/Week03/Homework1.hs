

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




module Week03.Homework1 where

import Data.Maybe (fromJust)
import Plutus.V1.Ledger.Interval (contains)
import Plutus.V2.Ledger.Api (POSIXTime, PubKeyHash,
                             ScriptContext (scriptContextTxInfo),
                             TxInfo (txInfoValidRange),
                             Validator, from, mkValidatorScript)
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



data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE mkValidator #-}
-- | This should validate if either beneficiary1 has signed the transaction
-- | and the current slot is before or at the deadline
-- | or if beneficiary2 has signed the transaction and the deadline has passed.
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = 
    (beneficiary1Signed' && beforeOrAtDeadline') || 
    (beneficiary2Signed' && afterDeadline')
    where
        beneficiary1Signed' = traceIfFalse "beneficiary1's signature missing" beneficiary1Signed
        beforeOrAtDeadline' = traceIfFalse "deadline passed" beforeOrAtDeadline
        beneficiary2Signed' = traceIfFalse "beneficiary2's signature missing" beneficiary2Signed
        afterDeadline' = traceIfFalse "deadline not reached" afterDeadline

        info :: TxInfo
        info = scriptContextTxInfo ctx

        beneficiary1Signed :: Bool
        beneficiary1Signed = txSignedBy info $ beneficiary1 dat

        beforeOrAtDeadline :: Bool
        beforeOrAtDeadline = t `after` txInfoValidRange info

        beneficiary2Signed :: Bool
        beneficiary2Signed = txSignedBy info $ beneficiary2 dat

        afterDeadline :: Bool
        afterDeadline = t `before` txInfoValidRange info

{-# INLINABLE mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator

validator :: PlutusV2.Validator
validator = mkValidatorScript $$(compile [|| mkWrappedValidator ||])

