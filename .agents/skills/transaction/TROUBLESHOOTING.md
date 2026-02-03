# Troubleshooting (sc-tools transactions)

Common errors and solutions when building/balancing/submitting transactions with:

- `Convex.BuildTx` (construct unbalanced tx bodies)
- `Convex.CoinSelection` (coin selection + balancing + signing)
- `Convex.Class` (submit/query via `MonadBlockchain`)

## Table of Contents

- [Build Errors](#build-errors)
- [Script Errors](#script-errors)
- [Coin Selection Errors](#coin-selection-errors)
- [Submission Errors](#submission-errors)
- [Common Mistakes](#common-mistakes)
- [Debug Checklist](#debug-checklist)

---

## Build Errors

### Missing era constraints (compile-time)

**Symptom:** GHC errors like:

- `Could not deduce (C.IsBabbageBasedEra era) ...`
- `Could not deduce (C.IsAlonzoBasedEra era) ...`

**Cause:** Many helpers require a minimum era (inline datums, reference scripts, Plutus, etc).

**Fix:**

- Add the missing constraint(s) to your function signature, or
- Specialize `era` (commonly to `C.ConwayEra` in current-era code).

Example:

```haskell
myTx
  :: (BuildTx.MonadBuildTx era m, C.IsBabbageBasedEra era)
  => ...
  -> m ()
myTx = ...
```

### Missing `ToData` instance (compile-time)

**Symptom:** GHC errors like `No instance for (Plutus.ToData MyDatum)`.

**Cause:** `spendPlutus*`, `mintPlutus*`, `payToScript*` convert datums/redeemers with `Plutus.ToData`.

**Fix:** Use a datum/redeemer type that has `ToData` (often a PlutusTx-derived type), or add the instance.

### “Hangs forever” when building a tx body

**Symptom:** Evaluating `buildTx txb` never finishes.

**Cause:** You created a circular dependency by making a witness/redeemer depend on itself via `TxBuilder` lookahead
(e.g. using `addInputWithTxBody` and then fully forcing the final body).

**Fix:** Ensure lookahead functions only read *independent* parts of the final body (often just an index via
`findIndexSpending` / `findIndexMinted` / etc).

---

## Script Errors

### Phase-2 script failure during balancing (`ScriptExecutionErr`)

**Symptom:** `balanceTx` fails with a `BalancingError` containing `ScriptExecutionErr [...]`.

**Cause:** Plutus validation failed during the “evaluate ex-units / balance” step.

**Fix checklist:**

1. Verify you used the correct spend helper:
   - UTxO has inline datum → `spendPlutusInlineDatum` / `spendPlutusRefWithInlineDatum`
   - UTxO needs datum hash (no inline datum) → `spendPlutus` / `spendPlutusRef`
2. Verify redeemer/datum are correct and match the on-chain types.
3. Add required signers when the validator checks signatures:
   - `addRequiredSignature`
4. Ensure the validator script hash matches the address you’re spending from.
5. If using reference scripts, confirm the reference input UTxO actually contains the reference script.

Tip: `ScriptExecutionErr` includes execution logs when available; print them and match by witness index.

### Interpreting `ScriptWitnessIndex`

`ScriptExecutionErr` and ex-unit tracing use `C.ScriptWitnessIndex` to identify *which* script failed.
Common constructors you’ll see:

- `C.ScriptWitnessIndexTxIn ix`: a spending input script witness (index is after ledger ordering by `TxIn`).
- `C.ScriptWitnessIndexWithdrawal ix`: a withdrawal witness (ledger-ordered by stake address).
- `C.ScriptWitnessIndexCertificate ix`: a certificate witness.
- `C.ScriptWitnessIndexVoting ix`: a voting witness (Conway).
- `C.ScriptWitnessIndexProposing ix`: a proposal witness (Conway).

Note: indices are usually **not** the order you added things in; they follow ledger-defined ordering rules.
If you need to compute indices for redeemers, use `lookupIndexSpending` / `lookupIndexWithdrawal` / etc from
`Convex.BuildTx` (see [TRANSACTION.md](TRANSACTION.md#inspecting-indices)).

### Matching/triaging script failures programmatically

If you’re pattern-matching on balancing errors, `Convex.CoinSelection.exactScriptExecutionError` is a useful prism
to match a particular witness index (and optionally an expected last log line).

### Missing collateral / no ada-only UTxOs

**Symptom:** balancing fails with `NoAdaOnlyUTxOsForCollateral` (or a ledger error mentioning collateral).

**Cause:** Script transactions require collateral, and coin selection couldn’t find an ADA-only input.

**Fix:**

- Ensure the wallet UTxO set contains at least one ADA-only output.
- If all UTxOs carry tokens, create/consolidate an ADA-only UTxO first (a normal payment tx).

---

## Coin Selection Errors

These usually come from `Convex.CoinSelection.balanceTx` as `BalanceTxError`:

### `NoWalletUTxOs`

**Cause:** The wallet UTxO set you passed in is empty (or everything was filtered out).

**Fix:** Re-query wallet UTxOs and ensure you pass the correct set into balancing.

### `BodyError ...` (tx body rejected)

**Cause:** The constructed `TxBodyContent` is invalid for `cardano-api` (missing required fields, inconsistent witnesses, etc).

**Fix:** Pretty-print the underlying `cardano-api` error by attempting to build the tx body:

```haskell
case C.createTransactionBody C.shelleyBasedEra (BuildTx.buildTx txb) of
  Left err -> putStrLn (C.docToString (C.prettyError err))
  Right _  -> pure ()
```

### `UnsupportedBalance ...`

**Cause:** Coin selection/balancing hit an output/value form it doesn’t know how to balance (typically an internal invariant).

**Fix:** Reduce the transaction to a minimal reproducer and inspect the unbalanced body/output values; if it persists, it’s likely a library bug.

### `NotEnoughInputsFor { lovelaceRequired, lovelaceFound }`

**Cause:** Not enough ADA to pay outputs + fee + min-UTxO + collateral requirements.

**Fix:**

- Reduce outputs / number of outputs, or
- Fund the wallet, or
- Ensure change output address is correct and belongs to the wallet.

### `NotEnoughMixedOutputsFor { valuesNeeded, valueProvided, txBalance }`

**Cause:** The tx needs native assets that the wallet can’t supply (and you’re not minting them).

**Fix:**

- Include inputs that actually hold the required assets, or
- Add the missing minting step(s), or
- Reduce/split outputs that demand those assets.

### `CheckMinUtxoValueError txOut minAda`

**Cause:** At least one output doesn’t include enough lovelace to satisfy min-UTxO.

**Fix options:**

1. Add lovelace explicitly to that output.
2. Apply `setMinAdaDepositAll` using protocol parameters:

```haskell
params <- queryProtocolParameters
BuildTx.setMinAdaDepositAll params
```

### “Tx too big” / “value size too large”

These are often ledger errors surfaced during balancing or submission.

Common fixes:

- Split huge token bundles across multiple outputs.
- Prefer reference scripts over in-tx scripts when scripts are large.
- Prefer datum hashes over large inline datums when possible.
- Split one large tx into multiple smaller ones.

### Assets “missing” even though the wallet has them

**Symptom:** balancing traces `MissingAssets ...` or fails with `NotEnoughMixedOutputsFor ...`, but you believe the
wallet has the tokens.

**Common causes:**

- **Compatibility filtering**: coin selection can drop some UTxOs based on the transaction’s compatibility level.
  If you enable tracing, look for `CompatibilityLevel { droppedTxIns = ... }`.
- You’re balancing with the wrong wallet UTxO set (wrong credential/network).

---

## Submission Errors

`sendTx` returns `Either (ValidationError era) TxId`.

### Wrong network / mismatched Blockfrost project

**Cause:** You’re constructing addresses/inputs for one network but querying/submitting on another (common when the Blockfrost project token points at a different network).

**Fix:**

- Verify `queryNetworkId` matches your expected network.
- Ensure addresses have the correct prefix (`addr...` for mainnet vs `addr_test...` for testnets).
- Re-fetch UTxOs on the same network you submit to.

### `BadInputsUTxO` / missing inputs

**Cause:** One or more inputs don’t exist on-chain (already spent, wrong network, or not confirmed yet).

**Fix:** Re-query inputs (e.g. `utxoByTxIn`), wait for confirmation, and ensure you’re on the correct `NetworkId`.

### `ValueNotConservedUTxO` / fee errors

**Cause:** The final tx doesn’t satisfy value conservation, usually because:

- You didn’t run balancing, or
- You modified outputs/mints after balancing without re-balancing.

**Fix:** Re-run `balanceTx` (or re-balance after any manual body edits).

### `OutsideValidityIntervalUTxO`

**Cause:** The tx validity interval doesn’t include the current slot.

**Fix:** Set appropriate bounds (see [PATTERNS.md](PATTERNS.md#set-a-validity-interval)).

---

## Common Mistakes

### Using the wrong “reference script” helper

- `spendPlutusRef*` and `mintPlutusRef` expect the script to be provided via a reference input.
- If the reference script UTxO is also being *spent* in the same tx, use the `WithoutInRef` variants for spending.

### Forgetting required signers

Plutus validators often check signatures explicitly. In sc-tools, add them via:

- `addRequiredSignature`

### Misunderstanding `TxBuilder` composition order

`TxBuilder` composes in a way that matches `do`-notation ordering (`a >> b` applies `a` before `b`), which is the
reverse of `Data.Monoid.Endo`.

### Building without balancing

If you construct a `TxBodyContent` manually and skip `Convex.CoinSelection`, you are responsible for:

- fees
- collateral (when scripts run)
- execution units
- min-UTxO for outputs

It’s usually easier to build an unbalanced tx and call `balanceTx`.

---

## Debug Checklist

1. **Inspect the unbalanced body**

```haskell
let bodyContent = BuildTx.buildTx txb
case C.createTransactionBody C.shelleyBasedEra bodyContent of
  Left err -> putStrLn (C.docToString (C.prettyError err))
  Right _  -> putStrLn "BodyContent is structurally OK"
```

2. **Log balancing steps**

Pass a tracer to `balanceTx` (even a simple stdout tracer) to see missing assets, fee, ex-units, etc.

3. **Print script execution logs**

If you hit `ScriptExecutionErr`, print the `(witnessIndex, error, logs)` triples; logs are usually the fastest path
to the root cause.

4. **Verify UTxOs exist**

Use `utxoByTxIn` for any `TxIn` you expect to spend/reference.

5. **Check min-UTxO early**

If you’re producing multi-asset outputs or inline datums, consider calling `setMinAdaDepositAll` before balancing.
