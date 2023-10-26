

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}


module Week03.ParameterizedVesting where

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



data VestingParams = VestingParams
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    }
PlutusTx.makeLift ''VestingParams

{-# mkValidator #-}
mkValidator :: VestingParams -> () -> () -> ScriptContext -> Bool
mkValidator params () () ctx = 
        traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
        traceIfFalse "deadline not reached" deadlineReached
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        signedByBeneficiary :: Bool
        signedByBeneficiary = txSignedBy info $ beneficiary params

        deadlineReached :: Bool
        deadlineReached = contains (from $ deadline params) $ txInfoValidRange info

{-# INLINABLE mkWrappedValidator #-}
mkWrappedValidator :: VestingParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator . mkValidator

validator :: VestingParams -> PlutusV2.Validator
validator params = mkValidatorScript ($$(compile [|| mkWrappedValidator ||]) `applyCode` liftCode params)

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/parameterized-vesting.plutus" validator

vestingAddressBech32 :: Network -> String
vestingAddressBech32 network = validatorAddressBech32 network validator




