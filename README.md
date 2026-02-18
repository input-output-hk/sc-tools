# sc-tools

A collection of libraries that are helpful for building Cardano apps with Haskell. The main packages are:

* `convex-base`: Functions and types that are used by the other packages.
* `convex-node-client`: Some wrappers around the node client API from `cardano-api`
* `convex-wallet`: Minimal wallet implementation
* `convex-coin-selection`: Coin selection and transaction balancing
* `convex-mockchain`: Minimal mockchain for tests
* `convex-optics`: Some optics for plutus-ledger-api and cardano-api
* `convex-tx-mod`: Command-line tool for working with transactions

The API documentation (Haddocks) is published [here](https://j-mueller.github.io/sc-tools/)

### Dependencies

The `main` branch uses the following versions of its major dependencies:

|Name|Version|
|--|--|
|`cardano-node`|[10.6.1](https://github.com/IntersectMBO/cardano-node/releases/tag/10.6.1)|
|`cardano-api`|[10.23.0.0](https://chap.intersectmbo.org/package/cardano-api-10.23.0.0/)|
|`ghc`|9.6.6 or 9.10.3|
|`cabal`|3.10.3.0 or 3.16.1.0|

For a dev environment with all dependencies, run `nix develop` (defaults to GHC 9.10.3).
For GHC 9.6.6 specifically, run `nix develop .#ghc966`.

## Evaluating Transactions

The command-line tool `convex-tx-mod` can be used to download and analyse fully resolved transactions from blockfrost. Example:

```shell
export BLOCKFROST_TOKEN=<blockfrost_token>
convex-tx-mod download bba17fd7b99fd88e5cfffb3223cf3988367f5fa7371f6549474310b0453e4c0a -o tx.json
convex-tx-mod graph -f tx.json -o graph.dot
```

Note that the `BLOCKFROST_TOKEN` variable must be set to a token for the network that the transaction is from (ie mainnet, preprod).

This downloads the serialised transaction and all its inputs to `tx.json` and then generates a dot graph in `graph.dot`.

![](docs/img/example-transaction.svg)

## Building transactions

We use the `TxBodyContent BuildTx BabbageEra` type from `cardano-api` as the basic type for building transactions. The `MonadBuildTx` class from `Convex.BuildTx` is essentially a writer for `TxBodyContent` modifications. `Convex.BuildTx` defines a number of helper functions for common tasks such as spending and creating Plutus script outputs, minting native assets, setting collateral, etc.

`Convex.CardanoApi.Lenses` defines some lenses for adding content to transaction bodies. These can be used together with `MonadBuildTx.addBtx`.

```haskell
import qualified Cardano.API.Shelley as C
import Convex.BuildTx (execBuildTx', payToAddress)

payTenAda :: Address BabbageEra -> TxBodyContent BuildTx BabbageEra
payTenAda addr = execBuildTx' (payToAddress addr (C.lovelaceToValue 10_000_000))
```

## Coin selection and transaction balancing

The task of converting a `TxBodyContent BuildTx BabbageEra` to a valid cardano transaction that can be submitted to the network is split up into three parts: Coin selection, transaction balancing, and signing. These all take place in the context of a wallet.

Coin selection is the act of adding inputs and outputs to the unfinished transaction body so that the overall balance of the transaction is in Ada only (no native assets) and strictly positive, ideally a couple of Ada.

Transaction balancing means setting the transaction fee to the correct amount that covers both Plutus script execution costs and the regular transaction fees.

To sign the transaction we use the wallet's private key to create a signature of the serialised transaction body and attach this signature to the transaction's list of key witnesses.

Coin selection and balancing is implemented in `Convex.CoinSelection`. The function `balanceForWallet` takes care of all three tasks and returns a fully balanced and signed transaction, as well a list the changes affected by the transaction on each address it touches:

```haskell
balanceForWallet :: (MonadBlockchain m, MonadFail m) => NodeParams -> Wallet -> UtxoState -> TxBodyContent BuildTx ERA -> m (C.Tx ERA, BalanceChanges)
```

Note the `MonadBlockchain` effect which is explained in the next section.

## Blockchain effects

Clearly, to use `sc-tools` in practice we need to be able to talk to the Cardano network at some point. These interactions are covered by the `MonadBlockchain` class:

```haskell
{-| Send transactions and resolve tx inputs.
-}
class Monad m => MonadBlockchain m where
  sendTx :: Tx ConwayEra -> m TxId -- ^ Submit a transaction to the network
  utxoByTxIn :: Set C.TxIn -> m (C.UTxO C.ConwayEra) -- ^ Resolve tx inputs
```

Both operations can be performed efficiently by a Cardano node using `cardano-api`.
There `MonadUtxoQuery` class has support for queries that can only be answered efficiently by

## Testing

`sc-tools` includes a simple emulator that can be used to build and evaluate transactions. A simple unit test looks like this

```haskell
import qualified Cardano.Api                    as C
import qualified Convex.MockChain.CoinSelection as CoinSelection
import qualified Convex.MockChain.Defaults      as Defaults
import qualified Convex.Wallet                  as Wallet
import qualified Convex.Wallet.MockWallet       as Wallet

spendPublicKeyOutput :: Assertion
spendPublicKeyOutput = mockchainSucceeds $ do
  let tx = emptyTx & payToAddress (Wallet.addressInEra Defaults.networkId Wallet.w2) (C.lovelaceToValue 10_000_000)
  CoinSelection.balanceAndSubmit Wallet.w1 tx
```

See [src/coin-selection/test/Spec.hs](src/coin-selection/test/Spec.hs) for more details.

## Working with cardano node

The `node-client` package exposes a simple interface to some of node client functions from `cardano-api`. The main function is `foldClient`:

```haskell
foldClient ::
  forall s.
  s -> -- ^ Initial state
  Env -> -- ^ Node connection data
  (CatchingUp -> s -> BlockInMode CardanoMode -> IO (Maybe s)) -> -- ^ Fold
  PipelinedLedgerStateClient
```

With this you can write a node client that makes use of `cardano-node`'s parallel block fetching feature AND takes care of rollbacks for you, by rolling back to earlier states `s` if needed. You can use the `IO` effect to talk to the outside world and to update some shared state that is used by the rest of your app.

There is also a slightly more sophisticated version:

```haskell
{-| A variant of 'foldClient' with more detailed control over rollbacks.
-}
foldClient' ::
  forall s w.
  Monoid w =>
  s -> -- ^ Initial state
  Env -> -- ^ Node connection data
  (ChainPoint -> w -> s -> IO (w, s)) -> -- ^ Rollback
  (CatchingUp -> s -> BlockInMode CardanoMode -> IO (Maybe (w, s))) -> -- ^ Fold
  PipelinedLedgerStateClient
```

This lets you deal with rollbacks explicitly, by giving you a summary of type `w` of all the data that has been rolled back.

## Goals

The code in `sc-tools` has been written with the following goals in mind.

* Stick to `cardano-api` where possible, avoid defining additional types that duplicate those from `cardano-api` and `cardano-ledger`
* Provide an easy-to-use coin selection and balancing algorithm. It should be able to deal with Plutus scripts and native assets.
* Support off-chain code that fits in with the "fold of blocks"-style node client API from `cardano-api`.

## Contributing

Bug reports, pull requests and other contributions are welcome!
