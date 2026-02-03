# sc-tools Transaction Building (`Convex.BuildTx`)

This file is an API-oriented guide to building Cardano transactions with sc-tools. It focuses on
`Convex.BuildTx` (constructing unbalanced tx bodies) and points to the balancing/signing layer
(`Convex.CoinSelection`) when relevant.

For end-to-end recipes, see [PATTERNS.md](PATTERNS.md). For debugging, see
[TROUBLESHOOTING.md](TROUBLESHOOTING.md).

## Table of Contents

- [Core concepts](#core-concepts)
- [Era constraints cheat sheet](#era-constraints-cheat-sheet)
- [Choosing helpers](#choosing-helpers)
- [Building and running](#building-and-running)
- [Inputs](#inputs)
- [Outputs](#outputs)
- [Minting](#minting)
- [Staking](#staking)
- [Witness helpers](#witness-helpers)
- [Min-Ada / min-UTxO](#min-ada--min-utxo)
- [Inspecting indices](#inspecting-indices)
- [Related modules](#related-modules)

## Core concepts

- **`TxBuilder era`**: a composable builder that (lazily) produces a `C.TxBodyContent C.BuildTx era`.
  - It is a `Monoid` (composition), and can optionally inspect the final tx body to compute witnesses.
  - Avoid circular dependencies: a witness builder must not depend on itself.
- **`MonadBuildTx era m`**: an effect for accumulating `TxBuilder`s (a writer-like interface).
- **Unbalanced vs balanced**:
  - `Convex.BuildTx` helps you build an *unbalanced* tx body (inputs/outputs/mints/etc).
  - `Convex.CoinSelection` turns that into a *balanced* tx (adds missing inputs, fees, collateral, ex-units).

Throughout this doc, “tx body content” means `C.TxBodyContent C.BuildTx era` (the `cardano-api` type used
to build transactions).

## Era constraints cheat sheet

Most functions are polymorphic in `era` but constrained by the minimum era needed:

- `C.IsShelleyBasedEra era`: basic tx construction
- `C.IsMaryBasedEra era`: multi-asset `Value`, minting, most `payTo*` helpers
- `C.IsAlonzoBasedEra era`: Plutus scripts, collateral, required signers, script validity
- `C.IsBabbageBasedEra era`: inline datums, reference inputs, reference scripts
- `C.IsConwayBasedEra era`: Conway-era stake credential certificate helpers

If you see “Could not deduce …” type errors, you’re usually missing one of these constraints.

## Choosing helpers

### Spending: pick the right `spend*`

| Scenario | Use | Notes |
|---|---|---|
| Spend a pubkey-controlled UTxO | `spendPublicKeyOutput` | Adds a key witness for spending. |
| Spend a Plutus script UTxO (script in tx, datum provided) | `spendPlutus` | Use when the spent output has a datum hash and you provide the datum value. |
| Spend a Plutus script UTxO (script in tx, inline datum) | `spendPlutusInlineDatum` | Use when the spent output carries an inline datum. |
| Spend with a reference script (datum provided) | `spendPlutusRef` | Also adds the reference script input to `txInsReference`. |
| Spend with a reference script (inline datum) | `spendPlutusRefWithInlineDatum` | Also adds the reference script input to `txInsReference`. |
| Reference-script UTxO is also being spent | `spendPlutusRefWithoutInRef*` | Prevents the same `TxIn` from appearing in both spending and reference sets. |
| Spend with a native `SimpleScript` | `spendSimpleScript` | For timelock/multisig scripts. |

### Outputs: datum hash vs inline datum

| Output kind | Use | Notes |
|---|---|---|
| Pay to an address | `payToAddress` / `payToPublicKey` | Simple payments. |
| Pay to a script with **datum hash** | `payToScriptDatumHash` | Stores only the datum hash in the output. |
| Pay to a script with **inline datum** | `payToScriptInlineDatum` | Stores the datum in the output (Babbage+). |
| Advanced datum-hash output | `payToScriptHash` | Takes a `HashableScriptData` directly. |

### Reference script outputs (Babbage+)

- `createRefScriptNoDatum`: reference script with no datum.
- `createRefScriptInlineDatum`: reference script with an inline datum.
- `createRefScriptDatumHash`: **currently uses an inline datum in this repo** (same behavior as `createRefScriptInlineDatum`). If you truly need a datum hash, build it manually via `createRefScriptBase`:

```haskell
import Cardano.Api qualified as C
import Convex.BuildTx qualified as BuildTx
import Convex.Scripts (toHashableScriptData)

let dat = C.TxOutDatumHash C.alonzoBasedEra (C.hashScriptDataBytes (toHashableScriptData datum))
BuildTx.createRefScriptBase addr script dat value
```

### Minting: mint vs burn

- Mint by using a **positive** `C.Quantity`.
- Burn by using a **negative** `C.Quantity` (see `coin-selection/test/Spec.hs` for an example).

## Building and running

### Core types / combinators

- `TxBuilder(..)`: the underlying builder type.
- `liftTxBodyEndo :: (C.TxBodyContent C.BuildTx era -> C.TxBodyContent C.BuildTx era) -> TxBuilder era`: lift a simple endomorphism.
- `buildTx :: TxBuilder era -> C.TxBodyContent C.BuildTx era`: run the builder (starting from an empty body).
- `buildTxWith :: TxBuilder era -> C.TxBodyContent C.BuildTx era -> C.TxBodyContent C.BuildTx era`: run from a seed body.

### Writer-style effect (`MonadBuildTx`)

- `class MonadBuildTx era m | m -> era where addTxBuilder :: TxBuilder era -> m ()`
- `addBtx :: MonadBuildTx era m => (C.TxBodyContent C.BuildTx era -> C.TxBodyContent C.BuildTx era) -> m ()`: add a “no-lookahead” tx-body modification.
- `BuildTxT era m a`: transformer implementing `MonadBuildTx`.
  - `runBuildTxT`, `execBuildTxT`, `evalBuildTxT`
  - Pure helpers: `runBuildTx`, `execBuildTx`, `execBuildTx'`

### “Look at the final tx body” builders

These are useful when a witness/redeemer needs an input/mint/withdrawal index:

- `addInputWithTxBody`
- `addMintWithTxBody`
- `addWithdrawalWithTxBody`

Use with the index helpers in [Inspecting indices](#inspecting-indices).

## Inputs

### Spending inputs

- `spendPublicKeyOutput :: MonadBuildTx era m => C.TxIn -> m ()`
- Plutus (inline script):
  - `spendPlutus :: ... => C.TxIn -> C.PlutusScript lang -> datum -> redeemer -> m ()`
  - `spendPlutusInlineDatum :: ... => C.TxIn -> C.PlutusScript lang -> redeemer -> m ()`
- Plutus (reference script):
  - `spendPlutusRef :: ... => C.TxIn -> C.TxIn -> C.PlutusScriptVersion lang -> datum -> redeemer -> m ()`
  - `spendPlutusRefWithInlineDatum :: ... => C.TxIn -> C.TxIn -> C.PlutusScriptVersion lang -> redeemer -> m ()`
  - `spendPlutusRefWithoutInRef` / `spendPlutusRefWithoutInRefInlineDatum` (does *not* add the reference input)
- Simple scripts:
  - `spendSimpleScript :: ... => C.TxIn -> C.SimpleScript -> m ()`

### Reference inputs / collateral / scripts

- `addReference :: (C.IsBabbageBasedEra era, MonadBuildTx era m) => C.TxIn -> m ()`
- `addCollateral :: (C.IsAlonzoBasedEra era, MonadBuildTx era m) => C.TxIn -> m ()`
- `addAuxScript :: (C.IsAllegraBasedEra era, MonadBuildTx era m) => C.ScriptInEra era -> m ()`
- `setScriptsValid :: (C.IsAlonzoBasedEra era, MonadBuildTx era m) => m ()`
- `addRequiredSignature :: (C.IsAlonzoBasedEra era, MonadBuildTx era m) => C.Hash C.PaymentKey -> m ()`

## Outputs

Low-level:

- `prependTxOut :: MonadBuildTx era m => C.TxOut C.CtxTx era -> m ()`
- `addOutput :: MonadBuildTx era m => C.TxOut C.CtxTx era -> m ()`

Payments:

- `payToAddress :: (C.IsMaryBasedEra era, MonadBuildTx era m) => C.AddressInEra era -> C.Value -> m ()`
- `payToAddressTxOut :: C.AddressInEra era -> C.Value -> C.TxOut C.CtxTx era`
- `payToPublicKey :: ... => C.NetworkId -> C.Hash C.PaymentKey -> C.Value -> m ()`

Script-locked outputs (datum hash vs inline datum):

- `payToScriptHash :: ... => C.NetworkId -> C.ScriptHash -> C.HashableScriptData -> C.StakeAddressReference -> C.Value -> m ()`
- `payToScriptDatumHash :: ... => C.NetworkId -> C.Script lang -> datum -> C.StakeAddressReference -> C.Value -> m ()`
- `payToScriptInlineDatum :: ... => C.NetworkId -> C.ScriptHash -> datum -> C.StakeAddressReference -> C.Value -> m ()`

Reference scripts (create an output that carries a reference script):

- `createRefScriptBase`
- `createRefScriptNoDatum`
- `createRefScriptDatumHash`
- `createRefScriptInlineDatum`

## Minting

- `mintPlutus :: ... => C.PlutusScript lang -> redeemer -> C.AssetName -> C.Quantity -> m ()`
- `mintPlutusRef :: ... => C.TxIn -> C.PlutusScriptVersion lang -> C.ScriptHash -> redeemer -> C.AssetName -> C.Quantity -> m ()`
- `mintSimpleScriptAssets :: ... => C.SimpleScript -> [(C.AssetName, C.Quantity)] -> m ()`
- `assetValue :: C.ScriptHash -> C.AssetName -> C.Quantity -> C.Value`

## Staking

Withdrawals:

- `addWithdrawal :: ... => C.StakeAddress -> C.Quantity -> C.Witness C.WitCtxStake era -> m ()`
- `addScriptWithdrawal :: ... => C.ScriptHash -> C.Quantity -> C.ScriptWitness C.WitCtxStake era -> m ()`
- `addWithdrawZeroPlutusV2InTransaction`
- `addWithdrawZeroPlutusV2Reference`

Certificates:

- `addCertificate :: ... => C.Certificate era -> m ()`
- Conway-era helpers:
  - `mkConwayStakeCredentialRegistrationCertificate`
  - `mkConwayStakeCredentialDelegationCertificate`
  - `mkConwayStakeCredentialRegistrationAndDelegationCertificate`
  - `mkConwayStakeCredentialUnRegistrationCertificate`
- Stake script witnesses:
  - `addStakeScriptWitness`
  - `addStakeScriptWitnessRef`
  - `addStakeWitnessWithTxBody` (advanced)

## Witness helpers

Utility constructors for script witnesses:

- `buildScriptWitness :: ... => C.PlutusScript lang -> C.ScriptDatum witctx -> redeemer -> C.ScriptWitness witctx era`
- `buildRefScriptWitness :: ... => C.TxIn -> C.PlutusScriptVersion lang -> C.ScriptDatum witctx -> redeemer -> C.ScriptWitness witctx era`

## Min-Ada / min-UTxO

- `minAdaDeposit :: C.LedgerProtocolParameters era -> C.TxOut C.CtxTx era -> C.Quantity`
- `setMinAdaDeposit :: C.LedgerProtocolParameters era -> C.TxOut C.CtxTx era -> C.TxOut C.CtxTx era`
- `setMinAdaDepositAll :: MonadBuildTx era m => C.LedgerProtocolParameters era -> m ()`

If balancing fails with a min-UTxO error, this section is usually the fix.

## Inspecting indices

These help compute on-chain indices for redeemers that depend on input/mint/withdrawal positions.

- Indices are **ledger-ordered**, not insertion-ordered (e.g. inputs are ordered by `TxIn`, withdrawals by stake address, minting by policy ID). Use the helpers below instead of guessing.
- Spending inputs: `lookupIndexSpending`, `findIndexSpending`
- Reference inputs: `lookupIndexReference`, `findIndexReference`
- Mint policies: `lookupIndexMinted`, `findIndexMinted`
- Withdrawals: `lookupIndexWithdrawal`, `findIndexWithdrawal`

Use them with `addInputWithTxBody` / `addMintWithTxBody` / `addWithdrawalWithTxBody`.

## Related modules

- `Convex.CoinSelection`: `balanceTx`, `balanceForWallet`, `signBalancedTxBody`, error types (`BalanceTxError`, `CoinSelectionError`, `BalancingError`).
- `Convex.CoinSelection.Class`: `MonadBalance` (useful in tests or when you want to intercept/trace balancing).
- `Convex.CardanoApi.Lenses`: lenses/prisms for `TxBodyContent` fields; use with `addBtx` for “escape hatch” mutations.
- `Convex.Class`: `MonadBlockchain` (`queryNetworkId`, `queryProtocolParameters`, `sendTx`), plus mockchain support.
- `Convex.Query`: higher-level helpers that fetch UTxOs via `MonadUtxoQuery` (e.g. balance using payment credentials, operators).
- `Convex.MockChain.CoinSelection`: “balance + sign + submit” helpers for tests on the mockchain.
- `Convex.Blockfrost.MonadBlockchain` / `Convex.Blockfrost.Types`: Blockfrost-backed queries and conversions when you’re not talking to a local node.
