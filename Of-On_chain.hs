On-Chain Code (Validator Script)
The on-chain code for the land acquisition contract will focus on validating the land purchase transactions.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Ledger
import Ledger.Typed.Scripts
import PlutusTx
import PlutusTx.Prelude

-- Define the Datum and Redeemer
newtype LandDatum = LandDatum (PubKeyHash, Integer) -- Example data
PlutusTx.makeLift ''LandDatum

data LandAction = ConfirmPurchase | CancelPurchase
PlutusTx.makeLift ''LandAction

-- Validator logic
{-# INLINABLE mkValidator #-}
mkValidator :: LandDatum -> LandAction -> ScriptContext -> Bool
mkValidator datum action ctx = case action of
    ConfirmPurchase -> -- Logic to confirm land purchase
    CancelPurchase  -> -- Logic to cancel the purchase
    _               -> False

-- Boilerplate code
data Land
instance Scripts.ValidatorTypes Land where
    type instance DatumType Land = LandDatum
    type instance RedeemerType Land = LandAction

typedValidator :: Scripts.TypedValidator Land
typedValidator = Scripts.mkTypedValidator @Land
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @LandDatum @LandAction

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```

Off-Chain Code (Plutus Application Backend)
The off-chain code handles user interactions, gathers inputs, and submits transactions.

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import Plutus.Contract
import qualified PlutusTx
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Ada as Ada

-- Contract endpoint
type LandAcquisitionSchema = Endpoint "acquireLand" LandDatum

acquireLand :: AsContractError e => LandDatum -> Contract w LandAcquisitionSchema e ()
acquireLand datum = do
    let tx = Constraints.mustPayToTheScript datum (Ada.lovelaceValueOf 1_000_000) -- Example amount
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "Land acquired for " ++ show datum

endpoints :: Contract () LandAcquisitionSchema Text ()
endpoints = forever $ handleError logError $ awaitPromise $ acquireLand' >> logInfo @String "Running land acquisition contract."
  where
    acquireLand' = endpoint @"acquireLand" acquireLand

-- Compile contract
mkSchemaDefinitions ''LandAcquisitionSchema
mkKnownCurrencies []
