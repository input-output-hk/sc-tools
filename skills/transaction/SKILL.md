---
name: transaction
description: Build, balance, and sign Cardano transactions in Haskell using sc-tools (`Convex.BuildTx` for constructing unbalanced transactions; `Convex.CoinSelection` for coin selection/balancing/signing; `Convex.Query`/Blockfrost/node/mockchain backends for fetching inputs). Use when writing or debugging off-chain code that adds inputs/outputs/mints/withdrawals/collateral/reference scripts, sets validity intervals and required signers, or fixes balancing/script execution/submission failures.
---

# Transaction building (sc-tools)

This skill documents the sc-tools transaction-building stack centered on `Convex.BuildTx`.

## Workflow

1. Build an **unbalanced** `TxBuilder era` using `MonadBuildTx` helpers (`spendPublicKeyOutput`, `spendPlutus*`, `payTo*`, `mint*`, `addWithdrawal`, …).
2. Make the body **balancing-friendly**:
   - If you create outputs with tokens, inline datums, or reference scripts, apply `setMinAdaDepositAll` with protocol parameters.
   - If you build custom script witnesses, use `buildScriptWitness`/`buildRefScriptWitness` (they set placeholder ex-units that balancing will replace).
3. Balance it with `Convex.CoinSelection` (adds missing inputs, fees, collateral, execution units, checks min-UTxO, etc).
4. Sign and submit the resulting transaction.

## Minimal skeleton

```haskell
import Cardano.Api qualified as C
import Convex.BuildTx qualified as BuildTx
import Convex.CoinSelection qualified as CoinSelection
import Control.Tracer (nullTracer)

type Era = C.ConwayEra

mkTxBuilder
  :: C.TxIn
  -> C.AddressInEra Era
  -> C.Value
  -> BuildTx.TxBuilder Era
mkTxBuilder input recipient value =
  BuildTx.execBuildTx $ do
    BuildTx.spendPublicKeyOutput input
    BuildTx.payToAddress recipient value

-- Later (in a MonadBlockchain/MonadError context):
--
-- (tx, _changes) <-
--   CoinSelection.balanceForWallet nullTracer wallet walletUtxos (mkTxBuilder input recipient value) CoinSelection.TrailingChange
-- _txIdOrErr <- sendTx tx
```

### Make outputs min-UTxO safe (recommended)

If you can query protocol parameters (e.g. in mockchain or a node-backed environment), apply:

```haskell
txb <- BuildTx.execBuildTxT $ do
  ...
  pp <- queryProtocolParameters
  BuildTx.setMinAdaDepositAll pp
```

This avoids common `CheckMinUtxoValueError` failures during balancing.

If the era becomes ambiguous, prefer a local type alias/annotation (e.g. `type Era = C.ConwayEra`) rather than visible type applications on `execBuildTxT`.

## Inputs to collect (when writing a tx)

- `era`: usually `C.ConwayEra` (specialize if you can).
- `NetworkId`: required for address construction in many helpers (`payToPublicKey`, `payToScriptInlineDatum`, etc).
- Spending inputs: `C.TxIn` for each UTxO you will spend.
- Script inputs:
  - Validator/minting policy as `C.PlutusScript lang` (inline) or a reference script `C.TxIn` + `C.PlutusScriptVersion lang`.
  - Datum mode (inline datum vs datum hash) and redeemers (must have `Plutus.ToData`).
- Outputs: recipient addresses, script hashes, datums, and the `C.Value` for each output.
- Balancing context:
  - Either a `Convex.Wallet.Wallet` + its `UtxoSet`, or
  - Payment credentials + `MonadUtxoQuery` (see `Convex.Query`).
- Extra requirements: collateral UTxO availability (ADA-only), required signers (`addRequiredSignature`), validity bounds, min-UTxO.

## Use lenses when a helper doesn’t exist

`Convex.BuildTx` intentionally doesn’t wrap every `TxBodyContent` field. When you need to set
something not covered (validity interval, metadata, governance fields, …), use
`Convex.CardanoApi.Lenses` + `addBtx`:

```haskell
import Control.Lens (set)
import Convex.CardanoApi.Lenses qualified as L

BuildTx.addBtx $
  set L.txValidityUpperBound (C.TxValidityUpperBound C.shelleyBasedEra (Just upperSlot))
```

## Gotchas (high-signal)

- **Era constraints matter**: inline datums, reference inputs, and reference scripts require `C.IsBabbageBasedEra era`.
- **`TxBuilder` can observe the final tx body**: functions like `addInputWithTxBody` are powerful but can loop if you make the witness depend on itself.
- **Order**: `TxBuilder`’s `Semigroup` instance is intentionally reversed so that `do a; b` applies `a` before `b`.
- **Indices are ledger-ordered**: inputs are ordered by `TxIn`, withdrawals by stake address, minting by policy ID. Use `lookupIndex*` / `findIndex*` helpers; don’t assume insertion order.
- **Lookahead requires placeholders**: when constructing Plutus witnesses manually, use `BuildTx.buildScriptWitness` / `BuildTx.buildRefScriptWitness` (they use `C.ExecutionUnits 0 0`); balancing will substitute real ex-units.

## Navigation

- API index: [TRANSACTION.md](TRANSACTION.md)
- Recipes: [PATTERNS.md](PATTERNS.md)
- Debugging: [TROUBLESHOOTING.md](TROUBLESHOOTING.md)

Key code modules (in this repo):

- `src/base/lib/Convex/BuildTx.hs`
- `src/coin-selection/lib/Convex/CoinSelection.hs`
- `src/optics/lib/Convex/CardanoApi/Lenses.hs`
