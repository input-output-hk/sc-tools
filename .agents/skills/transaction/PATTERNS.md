# Patterns (sc-tools transactions)

Copy/paste recipes for building transactions with sc-tools (`Convex.BuildTx` + `Convex.CoinSelection`).

These examples focus on the *transaction-building* step (constructing an unbalanced tx). For a
function index, see [TRANSACTION.md](TRANSACTION.md). For errors, see
[TROUBLESHOOTING.md](TROUBLESHOOTING.md).

## Table of Contents

- [Spend a pubkey UTxO and pay to an address](#spend-a-pubkey-utxo-and-pay-to-an-address)
- [Pay to a script with inline datum](#pay-to-a-script-with-inline-datum)
- [Spend a Plutus UTxO (inline datum)](#spend-a-plutus-utxo-inline-datum)
- [Spend a Plutus UTxO using a reference script](#spend-a-plutus-utxo-using-a-reference-script)
- [Index-dependent redeemers (lookahead)](#index-dependent-redeemers-lookahead)
- [Mint with an inline Plutus minting policy](#mint-with-an-inline-plutus-minting-policy)
- [Mint with a reference script](#mint-with-a-reference-script)
- [Burn tokens (negative quantity)](#burn-tokens-negative-quantity)
- [Create a reference script output](#create-a-reference-script-output)
- [Require extra signatures](#require-extra-signatures)
- [Set a validity interval](#set-a-validity-interval)
- [Ensure outputs meet min-UTxO](#ensure-outputs-meet-min-utxo)
- [Balance, sign, submit (end-to-end skeleton)](#balance-sign-submit-end-to-end-skeleton)
- [Real-world references](#real-world-references)

## Spend a pubkey UTxO and pay to an address

```haskell
import Cardano.Api qualified as C
import Convex.BuildTx (MonadBuildTx)
import Convex.BuildTx qualified as BuildTx

payFromWallet
  :: (MonadBuildTx era m, C.IsMaryBasedEra era)
  => C.TxIn
  -> C.AddressInEra era
  -> C.Value
  -> m ()
payFromWallet input recipient value = do
  BuildTx.spendPublicKeyOutput input
  BuildTx.payToAddress recipient value
```

## Pay to a script with inline datum

```haskell
import Cardano.Api qualified as C
import Convex.BuildTx qualified as BuildTx
import PlutusLedgerApi.V1 qualified as Plutus

lockInlineDatum
  :: (BuildTx.MonadBuildTx era m, C.IsBabbageBasedEra era, Plutus.ToData datum)
  => C.NetworkId
  -> C.ScriptHash
  -> datum
  -> C.Value
  -> m ()
lockInlineDatum networkId validatorHash datum value =
  BuildTx.payToScriptInlineDatum networkId validatorHash datum C.NoStakeAddress value
```

## Spend a Plutus UTxO (inline datum)

Use this when the spent UTxO carries an inline datum.

```haskell
import Cardano.Api qualified as C
import Convex.BuildTx qualified as BuildTx
import PlutusLedgerApi.V1 qualified as Plutus

spendInlineDatum
  :: ( BuildTx.MonadBuildTx era m
     , C.IsAlonzoBasedEra era
     , C.HasScriptLanguageInEra lang era
     , C.IsPlutusScriptLanguage lang
     , Plutus.ToData redeemer
     )
  => C.TxIn
  -> C.PlutusScript lang
  -> redeemer
  -> m ()
spendInlineDatum txIn validator redeemer =
  BuildTx.spendPlutusInlineDatum txIn validator redeemer
```

## Spend a Plutus UTxO using a reference script

Use this when the validator lives in a reference script UTxO (`refScriptTxIn`).

```haskell
import Cardano.Api qualified as C
import Convex.BuildTx qualified as BuildTx
import PlutusLedgerApi.V1 qualified as Plutus

spendWithRefScript
  :: ( BuildTx.MonadBuildTx era m
     , C.IsBabbageBasedEra era
     , C.HasScriptLanguageInEra lang era
     , C.IsPlutusScriptLanguage lang
     , Plutus.ToData redeemer
     )
  => C.TxIn
  -- ^ Script-locked input being spent
  -> C.TxIn
  -- ^ Reference script input
  -> C.PlutusScriptVersion lang
  -> redeemer
  -> m ()
spendWithRefScript scriptTxIn refScriptTxIn plutusVer redeemer =
  -- This variant automatically adds `refScriptTxIn` as a reference input.
  BuildTx.spendPlutusRefWithInlineDatum scriptTxIn refScriptTxIn plutusVer redeemer
```

If the reference script UTxO is *also* being spent in the same transaction, use
`spendPlutusRefWithoutInRef*` to avoid putting the same `TxIn` in both the spending set and the
reference set.

## Index-dependent redeemers (lookahead)

Some scripts need redeemers that depend on **the final tx body ordering** (e.g. “my redeemer is my input index”).
Use `addInputWithTxBody` / `addMintWithTxBody` together with `findIndexSpending` / `findIndexMinted`.

Important:

- Indices are **ledger-ordered** (not insertion order).
- Use `BuildTx.buildScriptWitness`/`buildRefScriptWitness` so ex-units are placeholders (`C.ExecutionUnits 0 0`) that balancing can replace.

```haskell
import Cardano.Api qualified as C
import Convex.BuildTx qualified as BuildTx
import Convex.Scripts (toHashableScriptData)

-- Spend: redeemer depends on input index.
spendWithIndexRedeemer
  :: forall era m
   . ( BuildTx.MonadBuildTx era m
     , C.IsAlonzoBasedEra era
     , C.HasScriptLanguageInEra C.PlutusScriptV3 era
     )
  => C.TxIn
  -> C.PlutusScript C.PlutusScriptV3
  -> m ()
spendWithIndexRedeemer txIn validator =
  let witness txBody =
        C.ScriptWitness C.ScriptWitnessForSpending $
          BuildTx.buildScriptWitness
            validator
            (C.ScriptDatumForTxIn $ Just $ toHashableScriptData ())
            (fromIntegral @Int @Integer $ BuildTx.findIndexSpending txIn txBody)
   in BuildTx.setScriptsValid >> BuildTx.addInputWithTxBody txIn witness

-- Mint: redeemer depends on minting policy index.
mintWithPolicyIndexRedeemer
  :: forall era m
   . ( BuildTx.MonadBuildTx era m
     , C.IsAlonzoBasedEra era
     , C.HasScriptLanguageInEra C.PlutusScriptV3 era
     )
  => C.PolicyId
  -> C.PlutusScript C.PlutusScriptV3
  -> C.AssetName
  -> C.Quantity
  -> m ()
mintWithPolicyIndexRedeemer policyId mintingPolicy assetName qty =
  let witness txBody =
        BuildTx.buildScriptWitness
          mintingPolicy
          C.NoScriptDatumForMint
          (fromIntegral @Int @Integer $ BuildTx.findIndexMinted policyId txBody)
   in BuildTx.setScriptsValid >> BuildTx.addMintWithTxBody policyId assetName qty witness
```

## Mint with an inline Plutus minting policy

```haskell
import Cardano.Api qualified as C
import Convex.BuildTx qualified as BuildTx
import PlutusLedgerApi.V1 qualified as Plutus

mintAndPay
  :: ( BuildTx.MonadBuildTx era m
     , C.IsAlonzoBasedEra era
     , C.IsMaryBasedEra era
     , C.HasScriptLanguageInEra lang era
     , C.IsPlutusScriptLanguage lang
     , Plutus.ToData redeemer
     )
  => C.PlutusScript lang
  -> redeemer
  -> C.AssetName
  -> C.Quantity
  -> C.AddressInEra era
  -> m ()
mintAndPay policyScript redeemer assetName qty recipient = do
  BuildTx.mintPlutus policyScript redeemer assetName qty

  let policyHash = C.hashScript (C.PlutusScript C.plutusScriptVersion policyScript)
      mintedValue = BuildTx.assetValue policyHash assetName qty

  -- Include lovelace for min-UTxO when paying multi-assets.
  -- For robust handling, prefer `setMinAdaDepositAll` with protocol params.
  BuildTx.payToAddress recipient (mintedValue <> C.lovelaceToValue 3_000_000)
```

## Mint with a reference script

```haskell
import Cardano.Api qualified as C
import Convex.BuildTx qualified as BuildTx
import PlutusLedgerApi.V1 qualified as Plutus

mintWithRefScript
  :: ( BuildTx.MonadBuildTx era m
     , C.IsBabbageBasedEra era
     , C.HasScriptLanguageInEra lang era
     , C.IsPlutusScriptLanguage lang
     , Plutus.ToData redeemer
     )
  => C.TxIn
  -- ^ Reference script input
  -> C.PlutusScriptVersion lang
  -> C.ScriptHash
  -- ^ Minting policy hash
  -> redeemer
  -> C.AssetName
  -> C.Quantity
  -> m ()
mintWithRefScript refScriptTxIn plutusVer policyHash redeemer assetName qty =
  -- Also adds `refScriptTxIn` as a reference input.
  BuildTx.mintPlutusRef refScriptTxIn plutusVer policyHash redeemer assetName qty
```

## Burn tokens (negative quantity)

Minting and burning are the same API: use a **negative** `C.Quantity` to burn.

```haskell
import Cardano.Api qualified as C
import Convex.BuildTx qualified as BuildTx
import PlutusLedgerApi.V1 qualified as Plutus

burn
  :: ( BuildTx.MonadBuildTx era m
     , C.IsAlonzoBasedEra era
     , C.HasScriptLanguageInEra lang era
     , C.IsPlutusScriptLanguage lang
     , Plutus.ToData redeemer
     )
  => C.PlutusScript lang
  -> redeemer
  -> C.AssetName
  -> C.Quantity
  -> m ()
burn policyScript redeemer assetName (C.Quantity q) =
  BuildTx.mintPlutus policyScript redeemer assetName (C.Quantity (-q))
```

## Create a reference script output

Creates an output that carries a reference script (Babbage+).

```haskell
import Cardano.Api qualified as C
import Convex.BuildTx qualified as BuildTx

publishRefScript
  :: (BuildTx.MonadBuildTx era m, C.IsBabbageBasedEra era)
  => C.AddressInEra era
  -> C.PlutusScript C.PlutusScriptV2
  -> C.Value
  -> m ()
publishRefScript ownerAddr validator minAda =
  BuildTx.createRefScriptNoDatum ownerAddr (C.PlutusScript C.PlutusScriptV2 validator) minAda
```

## Require extra signatures

Plutus scripts often require extra signers (beyond “spend key witnesses”).

```haskell
import Cardano.Api qualified as C
import Convex.BuildTx qualified as BuildTx

requireSigner :: (BuildTx.MonadBuildTx era m, C.IsAlonzoBasedEra era) => C.Hash C.PaymentKey -> m ()
requireSigner = BuildTx.addRequiredSignature
```

## Set a validity interval

Use lenses for `TxBodyContent` fields that don’t have a dedicated helper.

```haskell
import Cardano.Api qualified as C
import Control.Lens (set)
import Convex.BuildTx qualified as BuildTx
import Convex.CardanoApi.Lenses qualified as L

setUpperBound :: (BuildTx.MonadBuildTx era m, C.IsShelleyBasedEra era) => C.SlotNo -> m ()
setUpperBound upperSlot =
  BuildTx.addBtx $
    set L.txValidityUpperBound (C.TxValidityUpperBound C.shelleyBasedEra (Just upperSlot))
```

## Ensure outputs meet min-UTxO

When balancing complains about min-UTxO, either add lovelace manually, or apply `setMinAdaDeposit*`
using protocol parameters.

```haskell
import Cardano.Api qualified as C
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain(queryProtocolParameters))

ensureMinAda :: (MonadBlockchain era m, BuildTx.MonadBuildTx era m, C.IsMaryBasedEra era) => m ()
ensureMinAda = do
  params <- queryProtocolParameters
  BuildTx.setMinAdaDepositAll params
```

## Balance, sign, submit (end-to-end skeleton)

```haskell
import Cardano.Api qualified as C
import Convex.BuildTx qualified as BuildTx
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadBlockchain, ValidationError, sendTx)
import Convex.CoinSelection qualified as CoinSelection
import Convex.Utxos (UtxoSet)
import Convex.Wallet (Wallet)
import Control.Monad.Except (MonadError)
import Control.Tracer (nullTracer)

-- Variant A: you have a `Convex.Wallet.Wallet` (coin selection will also sign with it).
mkAndSubmitWithWallet
  :: forall era err m a
   . ( MonadBlockchain era m
     , MonadError err m
     , C.IsBabbageBasedEra era
     , CoinSelection.AsBalancingError err era
     , CoinSelection.AsCoinSelectionError err
     )
  => Wallet
  -> UtxoSet C.CtxUTxO a
  -> BuildTx.TxBuilder era
  -> m (Either (ValidationError era) C.TxId)
mkAndSubmitWithWallet wallet walletUtxos txb = do
  (tx, _changes) <- CoinSelection.balanceForWallet nullTracer wallet walletUtxos txb CoinSelection.TrailingChange
  sendTx tx

-- Variant B: you only have an address (custom signing keys, custom return output, etc).
mkAndSubmit
  :: forall era err m a
   . ( MonadBlockchain era m
     , MonadError err m
     , C.IsBabbageBasedEra era
     , CoinSelection.AsBalancingError err era
     , CoinSelection.AsCoinSelectionError err
     )
  => C.AddressInEra era
  -- ^ Wallet address (for change)
  -> UtxoSet C.CtxUTxO a
  -- ^ Wallet UTxOs
  -> BuildTx.TxBuilder era
  -- ^ Unbalanced builder
  -> [C.ShelleyWitnessSigningKey]
  -- ^ Keys to sign with
  -> m (Either (ValidationError era) C.TxId)
mkAndSubmit walletAddr walletUtxos txb signingKeys = do
  let changeOut = L.emptyTxOut walletAddr
  (balancedBody, _changes) <-
    CoinSelection.balanceTx nullTracer changeOut walletUtxos txb CoinSelection.TrailingChange

  let tx = CoinSelection.signBalancedTxBody signingKeys balancedBody
  sendTx tx
```

Note: the exact wallet/UTxO types in your app may differ; treat this as a wiring template.

## Real-world references

- `src/base/lib/Convex/BuildTx.hs` (core API)
- `src/coin-selection/lib/Convex/CoinSelection.hs` (balancing/signing + error types)
- `src/coin-selection/lib/Convex/Query.hs` (higher-level “balance using payment credentials” helpers)
- `src/coin-selection/test/Spec.hs` (working end-to-end examples: reference scripts, burning via negative quantity, matching-index scripts)
- External (local) examples using sc-tools in practice:
  - `/home/gumbo/iohk/usdcx-backend/usdcx-offchain/src/Cardano/Iris/OffChain/UpgradableParams/BuildTx.hs`
  - `/home/gumbo/iohk/usdcx-backend/usdcx-offchain/src/Cardano/Iris/OffChain/USDCXMinting/BuildTx.hs`
