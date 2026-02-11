-- | Node queries for use in testnet environments
module Convex.NodeQueries.Debug (
  -- The queries in this module are either inefficient (such as 'queryUTxOByAddress')
  -- or brittle (such as 'waitForTxIn')
  queryUTxOByAddress,
  queryUTxOWhole,
  waitForTxIn,
  waitForTx,
  waitForTxInSpend,
) where

import Cardano.Api (LocalNodeConnectInfo, Tx, TxIn)
import Cardano.Api qualified as C
import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Convex.NodeQueries (queryUTxOFilter)
import Convex.Utils (txnUtxos)
import Convex.Utxos (UtxoSet)
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set

-- | Get the set of UTxOs for a list of addresses
queryUTxOByAddress :: LocalNodeConnectInfo -> [C.AddressAny] -> IO (UtxoSet C.CtxUTxO ())
queryUTxOByAddress connectInfo addresses =
  queryUTxOFilter connectInfo $ C.QueryUTxOByAddress $ Set.fromList addresses

-- | Get the entire UTxO set
queryUTxOWhole :: LocalNodeConnectInfo -> IO (UtxoSet C.CtxUTxO ())
queryUTxOWhole connectInfo = queryUTxOFilter connectInfo C.QueryUTxOWhole

-- | Wait until the output appears on the chain
waitForTxIn :: LocalNodeConnectInfo -> TxIn -> IO ()
waitForTxIn connectInfo txIn = go
 where
  go = do
    utxo <- queryUTxOFilter connectInfo (C.QueryUTxOByTxIn (Set.singleton txIn))
    when (utxo == mempty) $ do
      threadDelay 2_000_000
      go

-- | Wait until the first output of the transaction appears on the chain
waitForTx :: forall era. LocalNodeConnectInfo -> Tx era -> IO ()
waitForTx connectInfo tx =
  waitForTxIn connectInfo $
    case listToMaybe $ txnUtxos tx of
      Nothing -> error "waitForTx: empty list of transaction outputs"
      Just (txIn, _) -> txIn

-- | Wait until the output disappears from the chain
waitForTxInSpend :: LocalNodeConnectInfo -> TxIn -> IO ()
waitForTxInSpend connectInfo txIn = go
 where
  go = do
    utxo <- queryUTxOFilter connectInfo (C.QueryUTxOByTxIn (Set.singleton txIn))
    unless (utxo == mempty) $ do
      threadDelay 2_000_000
      go
