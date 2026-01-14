{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Convex.MockChain.Staking (registerPool) where

import Cardano.Api qualified as C
import Cardano.Api.Compatible.Certificate qualified as Cert
import Cardano.Api.Experimental.Certificate qualified as Ex
import Cardano.Api.Experimental.Era qualified as Ex
import Cardano.Api.Experimental.Tx qualified as Ex
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Convex.BuildTx qualified as BuildTx
import Convex.Class (
  MonadBlockchain (queryProtocolParameters),
  MonadMockchain,
 )
import Convex.CoinSelection (
  BalanceTxError,
  ChangeOutputPosition (TrailingChange),
 )
import Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import Convex.MockChain.Defaults qualified as Defaults
import Convex.Wallet (Wallet)
import Data.Ratio ((%))

-- | Run the 'Mockchain' action with registered pool
registerPool
  :: forall era m
   . ( MonadIO m
     , MonadMockchain era m
     , MonadError (BalanceTxError era) m
     , MonadFail m
     , C.IsConwayBasedEra era
     , Ledger.Delegatee ~ Cert.Delegatee era
     , Ex.IsEra era
     , Ex.LedgerEra era ~ C.ShelleyLedgerEra era
     )
  => Wallet -> m C.PoolId
registerPool wallet = C.conwayEraOnwardsConstraints @era C.conwayBasedEra $ do
  stakeKey <- C.generateSigningKey C.AsStakeKey
  vrfKey <- C.generateSigningKey C.AsVrfKey
  stakePoolKey <- C.generateSigningKey C.AsStakePoolKey

  let
    vrfHash =
      C.verificationKeyHash . C.getVerificationKey $ vrfKey

    stakeHash =
      C.verificationKeyHash . C.getVerificationKey $ stakeKey

    stakeCred = C.StakeCredentialByKey stakeHash

  pp <- fmap C.unLedgerProtocolParameters queryProtocolParameters
  let
    stakeCert = Ex.makeStakeAddressRegistrationCertificate stakeCred (pp ^. Ledger.ppKeyDepositL)
    stakeAddress = C.makeStakeAddress Defaults.networkId stakeCred

    stakePoolVerKey = C.getVerificationKey stakePoolKey
    poolId = C.verificationKeyHash stakePoolVerKey

    delegationCert =
      Ex.makeStakeAddressDelegationCertificate stakeCred (Ledger.DelegStake $ C.unStakePoolKeyHash poolId)

    stakePoolParams =
      C.StakePoolParameters
        poolId
        vrfHash
        340_000_000 -- cost
        (3 % 100) -- margin
        stakeAddress
        0 -- pledge
        [stakeHash] -- owners
        [] -- relays
        Nothing

    poolCert =
      Ex.makeStakePoolRegistrationCertificate (C.toShelleyPoolParams stakePoolParams)

    stakeCertTx = BuildTx.execBuildTx $ do
      BuildTx.addCertificate stakeCert Ex.AnyKeyWitnessPlaceholder

    poolCertTx = BuildTx.execBuildTx $ do
      BuildTx.addCertificate poolCert Ex.AnyKeyWitnessPlaceholder

    delegCertTx = BuildTx.execBuildTx $ do
      BuildTx.addCertificate delegationCert Ex.AnyKeyWitnessPlaceholder

  void $ tryBalanceAndSubmit mempty wallet stakeCertTx TrailingChange [C.WitnessStakeKey stakeKey]
  void $ tryBalanceAndSubmit mempty wallet poolCertTx TrailingChange [C.WitnessStakeKey stakeKey, C.WitnessStakePoolKey stakePoolKey]
  void $ tryBalanceAndSubmit mempty wallet delegCertTx TrailingChange [C.WitnessStakeKey stakeKey]

  pure poolId
