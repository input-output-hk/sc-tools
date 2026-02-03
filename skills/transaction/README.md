# Transaction Skill (sc-tools)

AI assistant skill for building Cardano transactions in Haskell using **sc-tools**:

- `Convex.BuildTx` for constructing unbalanced transactions (`TxBuilder` / `TxBodyContent`)
- `Convex.CoinSelection` for coin selection, balancing, and signing

Part of this repository (`sc-tools`). See [`README.md`](../../../README.md) for a project overview.

## Coverage

- `Convex.BuildTx` helpers for spending, minting, staking, outputs, reference scripts
- Balancing/signing essentials (`Convex.CoinSelection`)
- Higher-level balancing helpers (`Convex.Query`) and backends (`Convex.Blockfrost.MonadBlockchain`)
- Transaction patterns and recipes with Haskell snippets
- Common errors, gotchas, and debugging checklists

## Files

| File | Purpose |
|------|---------|
| `SKILL.md` | Main entry — workflow + navigation |
| `TRANSACTION.md` | API index for `Convex.BuildTx` (+ related modules) |
| `PATTERNS.md` | Common transaction recipes with Haskell snippets |
| `TROUBLESHOOTING.md` | Errors, pitfalls, and debugging checklist |

## Example Prompts

- "Build a transaction that pays 10 ADA to this address using `Convex.BuildTx`"
- "Spend a script UTxO with an inline datum and produce a new script output"
- "Mint tokens with a reference script (`mintPlutusRef`) and balance the tx"
- "I'm getting `NoAdaOnlyUTxOsForCollateral` — how do I fix it?"
- "Why does balancing fail with `CheckMinUtxoValueError`?"

## License

Apache-2.0
