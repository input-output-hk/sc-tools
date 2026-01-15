{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Custom class to encapsulate the general purpose
queries that we need for building transactions
-}
module Convex.Query (
  balanceTx,

  -- * Tx balancing for operator
  balanceAndSubmitOperator,
  balancePaymentCredential,
  balancePaymentCredentials,
  signTxOperator,
  signAndSubmitOperator,
  operatorUtxos,
  selectOperatorUTxO,
  BalanceAndSubmitError (..),
  AsBalanceAndSubmitError (..),
) where

import Cardano.Api (
  BalancedTxBody,
  PaymentCredential (..),
 )
import Cardano.Api qualified as C
import Control.Lens.TH (makeClassyPrisms)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (runExceptT)
import Control.Tracer (Tracer, natTracer)
import Convex.BuildTx (TxBuilder)
import Convex.Class (
  MonadBlockchain (..),
  MonadUtxoQuery (utxosByPaymentCredentials),
  SendTxError (..),
  utxosByPaymentCredential,
 )
import Convex.CoinSelection (
  BalanceTxError,
  ChangeOutputPosition,
  TxBalancingMessage,
 )
import Convex.CoinSelection qualified
import Convex.Utils (inBabbage, liftEither, mapError)
import Convex.Utxos (
  BalanceChanges,
  UtxoSet (_utxos),
 )
import Convex.Utxos qualified as Utxos
import Convex.Wallet.Operator (
  Operator (..),
  Signing,
  operatorPaymentCredential,
  returnOutputFor,
  signTxOperator,
 )
import Data.Functor (($>))
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import GHC.Generics (Generic)

{- | Balance the transaction body using the UTxOs locked by the payment credentials,
returning any unused funds to the given return output
|
-}
balanceTx
  :: (MonadBlockchain era m, MonadUtxoQuery m, C.IsBabbageBasedEra era)
  => Tracer m TxBalancingMessage
  -> [PaymentCredential]
  -> C.TxOut C.CtxTx era
  -> TxBuilder era
  -> ChangeOutputPosition
  -> m (Either (BalanceTxError era) (BalancedTxBody era, BalanceChanges))
balanceTx dbg inputCredentials changeOutput txBody changePosition = do
  o <- utxosByPaymentCredentials (Set.fromList inputCredentials)
  runExceptT (Convex.CoinSelection.balanceTx (natTracer lift dbg) changeOutput o txBody changePosition)

-- | Balance a transaction body using the funds locked by one of a list of payment credentials
balancePaymentCredentials
  :: forall era m
   . (MonadBlockchain era m, MonadUtxoQuery m, MonadError (BalanceAndSubmitError era) m, C.IsBabbageBasedEra era)
  => Tracer m TxBalancingMessage
  -> C.PaymentCredential
  -- ^ Primary payment credential, used for return output
  -> [C.PaymentCredential]
  -- ^ Other payment credentials, used for balancing
  -> Maybe (C.TxOut C.CtxTx era)
  -> TxBuilder era
  -> ChangeOutputPosition
  -> m (C.Tx era)
balancePaymentCredentials dbg primaryCred otherCreds returnOutput txBody changePosition = do
  output <- maybe (inBabbage @era returnOutputFor primaryCred) pure returnOutput
  (body, _) <- liftEither BalanceError (balanceTx dbg (primaryCred : otherCreds) output txBody changePosition)
  pure $ Convex.CoinSelection.signBalancedTxBody [] body

-- | Balance a transaction body using the funds locked by the payment credential
balancePaymentCredential
  :: (MonadBlockchain era m, MonadUtxoQuery m, MonadError (BalanceAndSubmitError era) m, C.IsBabbageBasedEra era)
  => Tracer m TxBalancingMessage
  -> C.PaymentCredential
  -> Maybe (C.TxOut C.CtxTx era)
  -> TxBuilder era
  -> ChangeOutputPosition
  -> m (C.Tx era)
balancePaymentCredential dbg cred = balancePaymentCredentials dbg cred []

-- | Balance a transaction body, sign it with the operator's key, and submit it to the network.
balanceAndSubmitOperator
  :: forall era m
   . (MonadBlockchain era m, MonadUtxoQuery m, MonadError (BalanceAndSubmitError era) m, C.IsBabbageBasedEra era)
  => Tracer m TxBalancingMessage
  -> Operator Signing
  -> Maybe (C.TxOut C.CtxTx era)
  -> TxBuilder era
  -> ChangeOutputPosition
  -> m (C.Tx era)
balanceAndSubmitOperator dbg op changeOut txBody changePosition =
  balancePaymentCredential dbg (operatorPaymentCredential op) changeOut txBody changePosition
    >>= inBabbage @era signAndSubmitOperator op

-- | Add the operator's signature to the transaction and send it to the blockchain
signAndSubmitOperator
  :: (MonadBlockchain era m, MonadError (BalanceAndSubmitError era) m, C.IsShelleyBasedEra era)
  => Operator Signing
  -> C.Tx era
  -> m (C.Tx era)
signAndSubmitOperator op tx = do
  let finalTx = signTxOperator op tx
  mapError SubmitError (sendTx finalTx) $> finalTx

{- | UTxOs that are locked by the operator's payment credential
|
-}
operatorUtxos :: forall era m k. (C.IsBabbageBasedEra era, MonadUtxoQuery m) => Operator k -> m [(C.TxIn, C.TxOut C.CtxUTxO era)]
operatorUtxos = fmap (Map.toList . fmap (Utxos.toTxOut @era . fst) . _utxos) . utxosByPaymentCredential . operatorPaymentCredential

-- | Select a single UTxO that is controlled by the operator. |
selectOperatorUTxO :: forall era m k. (C.IsBabbageBasedEra era, MonadUtxoQuery m) => Operator k -> m (Maybe (C.TxIn, C.TxOut C.CtxUTxO era))
selectOperatorUTxO operator = fmap listToMaybe (operatorUtxos operator)

-- | Failures during txn balancing and submission
data BalanceAndSubmitError era
  = BalanceError (BalanceTxError era)
  | SubmitError (SendTxError era)
  deriving stock (Show, Generic)

makeClassyPrisms ''BalanceAndSubmitError
