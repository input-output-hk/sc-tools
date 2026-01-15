{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- To pattern-match on the C.ShelleyBlock
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as L
import Cardano.Ledger.Api.PParams qualified as L
import Cardano.Ledger.Block qualified as Ledger
import Cardano.Ledger.Shelley.Genesis qualified as Ledger
import Control.Concurrent (threadDelay)
import Control.Lens (Contravariant (contramap), view)
import Control.Monad (forM_, unless, void)
import Control.Monad.Except (runExceptT)
import Control.Tracer (Tracer)
import Convex.BuildTx (execBuildTx, payToPublicKey)
import Convex.CoinSelection (ChangeOutputPosition (TrailingChange))
import Convex.Devnet.CardanoNode (
  NodeLog (..),
  getCardanoNodeVersion,
  withCardanoNodeDevnet,
  withCardanoNodeDevnetConfig,
  withCardanoStakePoolNodeDevnetConfig,
 )
import Convex.Devnet.CardanoNode.Types (
  PortsConfig (..),
  RunningNode (..),
  RunningStakePoolNode (..),
  StakePoolNodeParams (..),
  allowLargeTransactions,
  defaultConfigFastRewardDistribution,
  defaultPortsConfig,
  defaultStakePoolNodeParams,
 )
import Convex.Devnet.Logging (
  showLogsOnFailure,
  traceWith,
 )
import Convex.Devnet.Utils (
  failAfter,
  failure,
  withTempDir,
 )
import Convex.Devnet.Wallet (WalletLog)
import Convex.Devnet.Wallet qualified as W
import Convex.NodeClient.Fold (
  LedgerStateArgs (NoLedgerStateArgs),
  foldClient,
 )
import Convex.NodeClient.Types (runNodeClient)
import Convex.NodeQueries (
  loadConnectInfo,
  queryProtocolParameters,
  queryStakeAddresses,
  queryStakePools,
 )
import Convex.NodeQueries qualified as Queries
import Convex.Utxos qualified as Utxos
import Convex.Wallet qualified as W
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef (
  modifyIORef,
  newIORef,
  readIORef,
 )
import Data.List (isInfixOf)
import Data.ListMap qualified as LM
import Data.Map qualified as Map
import Data.Ratio ((%))
import Data.Set qualified as Set
import Devnet.Test.LatestEraTransitionSpec qualified as LatestEraTransitionSpec
import GHC.Generics (Generic)
import GHC.IO.Encoding (
  setLocaleEncoding,
  utf8,
 )
import Ouroboros.Consensus.Protocol.Praos.Header qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger.Block qualified as Consensus
import System.FilePath ((</>))
import Test.Tasty (
  defaultMain,
  testGroup,
 )
import Test.Tasty.HUnit (
  assertBool,
  assertEqual,
  testCase,
 )

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "test"
      [ testCase "cardano-node is available" checkCardanoNode
      , testCase "start local node" startLocalNode
      , testCase "check transition to conway era and protocol version 10" checkTransitionToConway
      , LatestEraTransitionSpec.tests
      , testCase "make a payment" makePayment
      , testCase "start local stake pool node" startLocalStakePoolNode
      , testCase "stake pool registration" registeredStakePoolNode
      , testCase "stake pool rewards" stakePoolRewards
      , testCase "change max tx size" changeMaxTxSize
      ]

checkCardanoNode :: IO ()
checkCardanoNode = do
  let expectedVersion = "10.6.1"
  version <- getCardanoNodeVersion
  let isExpected = expectedVersion `isInfixOf` version
  unless isExpected (putStrLn version)
  assertBool ("cardano-node version should be " <> expectedVersion) isExpected

startLocalNode :: IO ()
startLocalNode = do
  showLogsOnFailure $ \tr -> do
    failAfter 10 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet tr tmp $ \RunningNode{rnNodeSocket, rnNodeConfigFile} -> do
          runExceptT (loadConnectInfo rnNodeConfigFile rnNodeSocket) >>= \case
            Left err -> failure (show err)
            Right{} -> pure ()

checkTransitionToConway :: IO ()
checkTransitionToConway = do
  showLogsOnFailure $ \tr -> do
    failAfter 10 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet (contramap TLNode tr) tmp $ \runningNode@RunningNode{rnConnectInfo, rnNodeSocket, rnNodeConfigFile} -> do
          Queries.queryEra rnConnectInfo >>= assertEqual "Should be in conway era" (C.anyCardanoEra C.ConwayEra)
          let lovelacePerUtxo = 100_000_000
              numUtxos = 10
          void $ W.createSeededWallet C.BabbageEraOnwardsConway (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          majorProtVersionsRef <- newIORef []
          res <- C.liftIO $ runExceptT $ runNodeClient rnNodeConfigFile rnNodeSocket $ \_localNodeConnectInfo env -> do
            pure $ foldClient () NoLedgerStateArgs env $ \_catchingUp _ _ bim -> do
              case bim of
                ( C.BlockInMode
                    C.ConwayEra
                    ( C.ShelleyBlock
                        C.ShelleyBasedEraConway
                        ( Consensus.ShelleyBlock
                            (Ledger.Block (Consensus.Header hb _) _)
                            _
                          )
                      )
                  ) -> do
                    modifyIORef majorProtVersionsRef $ \majorProtVersions ->
                      L.pvMajor (Consensus.hbProtVer hb) : majorProtVersions
                    pure Nothing
                (C.BlockInMode _ _block) -> do
                  failure "Block should be a ShelleyBlock in Conway era"
          case res of
            Left err -> failure $ show err
            Right () -> do
              majorProtVersions <- readIORef majorProtVersionsRef
              expectedVersion <- L.mkVersion (10 :: Integer)
              assertBool "Should have correct conway era protocol version" $
                not (null majorProtVersions) && all (== expectedVersion) majorProtVersions

startLocalStakePoolNode :: IO ()
startLocalStakePoolNode = do
  showLogsOnFailure $ \tr -> do
    failAfter 30 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet (contramap TLNode tr) tmp $ \runningNode -> do
          let lovelacePerUtxo = 100_000_000
              numUtxos = 10
              nodeConfigFile = tmp </> "cardano-node.json"
          wllt <- W.createSeededWallet C.BabbageEraOnwardsConway (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          withTempDir "cardano-cluster-stakepool" $ \tmp' -> do
            withCardanoStakePoolNodeDevnetConfig (contramap TLNode tr) tmp' wllt defaultStakePoolNodeParams nodeConfigFile (PortsConfig 3002 [3001]) runningNode $ \RunningStakePoolNode{rspnNode} -> do
              runExceptT (loadConnectInfo (rnNodeConfigFile rspnNode) (rnNodeSocket rspnNode)) >>= \case
                Left err -> failure (show err)
                Right{} -> pure ()

registeredStakePoolNode :: IO ()
registeredStakePoolNode = do
  showLogsOnFailure $ \tr -> do
    failAfter 30 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet (contramap TLNode tr) tmp $ \runningNode@RunningNode{rnConnectInfo} -> do
          let lovelacePerUtxo = 100_000_000
              numUtxos = 10
              nodeConfigFile = tmp </> "cardano-node.json"
          wllt <- W.createSeededWallet C.BabbageEraOnwardsConway (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          initialStakePools <- queryStakePools rnConnectInfo
          withTempDir "cardano-cluster-stakepool" $ \tmp' -> do
            withCardanoStakePoolNodeDevnetConfig
              (contramap TLNode tr)
              tmp'
              wllt
              defaultStakePoolNodeParams
              nodeConfigFile
              (PortsConfig 3002 [3001])
              runningNode
              $ \RunningStakePoolNode{rspnStakeKey} -> do
                let stakeHash = C.verificationKeyHash . C.getVerificationKey $ rspnStakeKey
                    stakeCred = C.StakeCredentialByKey stakeHash
                (_, stakeKeyDelegation) <- queryStakeAddresses rnConnectInfo $ Set.singleton stakeCred
                currentStakePools <- queryStakePools rnConnectInfo
                let
                  initial = length initialStakePools
                  current = length currentStakePools
                assertEqual "Blockchain should have one new registered stake pool" 1 (current - initial)
                assertBool "The registered stake address should delegate it's stake to the new registered stake pool" $
                  snd (head $ Map.toList stakeKeyDelegation) `Set.member` currentStakePools

stakePoolRewards :: IO ()
stakePoolRewards = do
  showLogsOnFailure $ \tr -> do
    failAfter 600 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnetConfig (contramap TLNode tr) tmp defaultConfigFastRewardDistribution (PortsConfig 3001 [3002]) $ \runningNode@RunningNode{rnNodeConfigFile} -> do
          let lovelacePerUtxo = 100_000_000_000
              numUtxos = 100
          w1 <- W.createSeededWallet C.BabbageEraOnwardsConway (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          w2 <- W.createSeededWallet C.BabbageEraOnwardsConway (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo

          nodeConfig <- C.liftIO (C.runExceptT $ C.readNodeConfig $ C.File rnNodeConfigFile) >>= either (error . show) pure
          shelleyGenesisConfig <- C.liftIO (C.runExceptT $ C.readShelleyGenesisConfig nodeConfig) >>= either (error . show) pure
          let genesisPoolsStakeCreds = fmap (C.StakeCredentialByKey . C.StakeKeyHash) $ LM.keys $ Ledger.sgsStake $ Ledger.sgStaking $ C.scConfig shelleyGenesisConfig

          -- We verify that, at the beginning, the genesis stake addresses have no rewards.
          forM_ genesisPoolsStakeCreds $ \genesisPoolStakeCred -> do
            rewards <- getStakeRewards runningNode genesisPoolStakeCred
            assertBool "Expect initial staking rewards for genesis pool stake credential to be 0" $ rewards == 0

          -- We wait until the genesis stake addresses gain some new rewards
          forM_ genesisPoolsStakeCreds $ \genesisPoolStakeCred -> do
            rewards <- waitForStakeRewardsIncreaseFromBaseAmount tr runningNode genesisPoolStakeCred 0
            assertBool "Expect staking rewards change for genesis pool stake credential" $ rewards > 0

          let stakepoolParams =
                StakePoolNodeParams
                  { spnCost = 340_000_000
                  , spnMargin = 1 % 100
                  , -- 1M ADA pledge. This needs to be pretty high because the
                    -- proportion of this stake pool on the total stacked amount
                    -- must be on par with the stake value of genesis pool. Or
                    -- else, this pool will never mint blocks.
                    spnPledge = L.Coin 1_000_000_000_000
                  }
          withTempDir "cardano-cluster-stakepool-1" $ \tmp' -> do
            withCardanoStakePoolNodeDevnetConfig (contramap TLNode tr) tmp' w1 stakepoolParams rnNodeConfigFile (PortsConfig 3002 [3001]) runningNode $ \RunningStakePoolNode{rspnNode, rspnStakeKey} -> do
              let poolOwnerStakeCred = C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey rspnStakeKey

              -- Generate some transactions to ensure there are fees in the system for rewards.
              -- This is optionnal since the Cardano reserve at startup is positive, but still good to have.
              let tx = execBuildTx $ payToPublicKey (rnNetworkId rspnNode) (W.verificationKeyHash w2) $ C.lovelaceToValue 10_000_000
              forM_ (replicate 20 (0 :: Int)) $ \_ -> do
                void $ W.balanceAndSubmit @C.ConwayEra (contramap TLWallet tr) rspnNode w1 tx TrailingChange []
                threadDelay 100_000

              poolOwnerNewRewards <- waitForStakeRewardsIncrease tr runningNode poolOwnerStakeCred
              assertBool "Expect staking rewards" $ poolOwnerNewRewards > 0
 where
  getStakeRewards :: RunningNode -> C.StakeCredential -> IO C.Quantity
  getStakeRewards RunningNode{rnConnectInfo} cred =
    let
      creds = Set.singleton cred
     in
      sum . Map.elems . fst <$> queryStakeAddresses rnConnectInfo creds

  waitForStakeRewardsIncrease :: Tracer IO TestLog -> RunningNode -> C.StakeCredential -> IO C.Quantity
  waitForStakeRewardsIncrease tr node cred = do
    rw <- getStakeRewards node cred
    traceWith tr $ TLRewards rw
    waitForStakeRewardsIncreaseFromBaseAmount tr node cred rw

  waitForStakeRewardsIncreaseFromBaseAmount :: Tracer IO TestLog -> RunningNode -> C.StakeCredential -> C.Quantity -> IO C.Quantity
  waitForStakeRewardsIncreaseFromBaseAmount tr node cred amount = do
    rewards <- getStakeRewards node cred
    traceWith tr $ TLRewards rewards
    if rewards > amount
      then
        pure rewards
      else do
        threadDelay 1_000_000 >> waitForStakeRewardsIncreaseFromBaseAmount tr node cred amount

makePayment :: IO ()
makePayment = do
  showLogsOnFailure $ \tr -> do
    failAfter 20 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet (contramap TLNode tr) tmp $ \runningNode -> do
          let lovelacePerUtxo = 100_000_000
              numUtxos = 10
          wllt <- W.createSeededWallet C.BabbageEraOnwardsConway (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          bal <- Utxos.totalBalance <$> W.walletUtxos runningNode wllt
          assertEqual "Wallet should have the expected balance" (fromIntegral numUtxos * lovelacePerUtxo) (C.lovelaceToQuantity $ C.selectLovelace bal)

changeMaxTxSize :: IO ()
changeMaxTxSize =
  let getMaxTxSize = fmap (view L.ppMaxTxSizeL) . queryProtocolParameters . rnConnectInfo
   in showLogsOnFailure $ \tr -> do
        withTempDir "cardano-cluster" $ \tmp -> do
          standardTxSize <- withCardanoNodeDevnet (contramap TLNode tr) tmp getMaxTxSize
          largeTxSize <- withCardanoNodeDevnetConfig (contramap TLNode tr) tmp allowLargeTransactions defaultPortsConfig getMaxTxSize
          assertEqual "tx size should be large" (2 * standardTxSize) largeTxSize

data TestLog
  = TLWallet WalletLog
  | TLNode NodeLog
  | SubmitTx {txId :: C.TxId}
  | FoundTx {txId :: C.TxId}
  | TLRewards C.Quantity
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
