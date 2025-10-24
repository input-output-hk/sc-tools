{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Convex.Devnet.CardanoNode.Types (
  Port,
  PortsConfig (..),
  defaultPortsConfig,
  RunningNode (..),
  RunningStakePoolNode (..),
  StakePoolNodeParams (..),
  defaultStakePoolNodeParams,

  -- * Genesis config changes
  GenesisConfigChanges (..),
  allowLargeTransactions,
  defaultConfigFastRewardDistribution,
  setEpochLength,
) where

import Cardano.Api (
  Env,
  KesKey,
  Key (..),
  LocalNodeConnectInfo,
  NetworkId,
  OperationalCertificateIssueCounter,
  ShelleyGenesis,
  StakeKey,
  StakePoolKey,
  VrfKey,
 )
import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Ledger.BaseTypes (EpochSize)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Shelley.API (Coin)
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis (..))
import Control.Lens (over)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import GHC.Generics (Generic)
import Ouroboros.Consensus.Shelley.Eras (ShelleyEra)

type Port = Int

{- | Configuration of ports from the perspective of a peer in the context of a
fully sockected topology.
-}
data PortsConfig = PortsConfig
  { ours :: Port
  -- ^ Our node TCP port.
  , peers :: [Port]
  -- ^ Other peers TCP ports.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default value for 'PortsConfig'
defaultPortsConfig :: PortsConfig
defaultPortsConfig = PortsConfig 3001 []

-- | Describes a running pool node
data RunningNode = RunningNode
  { rnNodeSocket :: FilePath
  -- ^ Cardano node socket
  , rnNetworkId :: NetworkId
  -- ^ Network ID used by the cardano node
  , rnNodeConfigFile :: FilePath
  -- ^ Cardano node config file (JSON)
  , rnConnectInfo :: LocalNodeConnectInfo
  -- ^ Connection info for node queries
  , rnEnv :: Env
  -- ^ Node environment
  }

-- | Describes a running stake pool node
data RunningStakePoolNode = RunningStakePoolNode
  { rspnNode :: RunningNode
  -- ^ Running Cardano node
  , rspnStakeKey :: SigningKey StakeKey
  , rspnVrfKey :: SigningKey VrfKey
  , rspnKesKey :: SigningKey KesKey
  , rspnStakePoolKey :: SigningKey StakePoolKey
  , rspnCounter :: OperationalCertificateIssueCounter
  }

-- | Stake pool node params
data StakePoolNodeParams = StakePoolNodeParams
  { spnCost :: Coin
  , spnMargin :: Rational
  , spnPledge :: Coin
  }

defaultStakePoolNodeParams :: StakePoolNodeParams
defaultStakePoolNodeParams = StakePoolNodeParams 0 (3 % 100) 0

-- | Modifications to apply to the default genesis configurations
data GenesisConfigChanges
  = GenesisConfigChanges
  { cfShelley :: ShelleyGenesis -> ShelleyGenesis
  , -- this is spiritually a 'Cardano.Ledger.Alonzo.Genesis.AlonzoGenesis' value
    -- we can't JSON roundtrip it here because the cardano node that we use in
    -- CI uses a slightly different JSON encoding and will trip even if we
    -- just write 'toJSON . fromJSON' without modifying the value
    -- Note that the problem is with the ToJSON instance!
    cfAlonzo :: Aeson.Value -> Aeson.Value
  , -- this is spiritually a 'Cardano.Ledger.Conway.Genesis.ConwayGenesis' value
    -- we can't JSON roundtrip it here because the cardano node that we use in
    -- CI uses a slightly different JSON encoding and will trip even if we
    -- just write 'toJSON . fromJSON' without modifying the value
    -- Note that the problem is with the ToJSON instance!
    cfConway :: Aeson.Value -> Aeson.Value
  , cfNodeConfig :: Aeson.Value -> Aeson.Value
  -- ^ Changes to the node config file
  }

instance Semigroup GenesisConfigChanges where
  l <> r =
    GenesisConfigChanges
      { cfShelley = cfShelley r . cfShelley l
      , cfAlonzo = cfAlonzo r . cfAlonzo l
      , cfConway = cfConway r . cfConway l
      , cfNodeConfig = cfNodeConfig r . cfNodeConfig l
      }

instance Monoid GenesisConfigChanges where
  mempty = GenesisConfigChanges id id id id

-- | Change the alonzo genesis config to allow transactions with up to twice the normal size
allowLargeTransactions :: GenesisConfigChanges
allowLargeTransactions =
  let change :: ShelleyGenesis -> ShelleyGenesis
      change g = g{sgProtocolParams = double (sgProtocolParams g)}
      double :: Core.PParams ShelleyEra -> Core.PParams ShelleyEra
      double = over (Core.ppLens . Core.hkdMaxTxSizeL) (* 2)
   in mempty{cfShelley = change}

{- | Change the shelley genesis config in such as way that it enables reward
distribution for stake pools which mint blocks for testing purposes. It makes
the epoch length as short as possible for faster testing runtime.

In order to get rewards at each epoch, a few constraints must be satisfied.

TLDR: You *must* allow sufficient computation time for cardano-ledger to
compute stake pool rewards after each epoch. If epochs go by too fast,
rewards will always be 0.

Here are the constraints themselves. First, epoch length is typically equal
to `10K/f` where `K` is `securityParam` and `f` is `activeSlotsCoeff`.
Second, the node/ledger needs actual computation time to calculate rewards in
first 4k/f slots of an epoch (assuming each slot is 1s). If we reduce slot
length in order to speed up the devnet, we must unfortunatelly increase the
epoch length and `K` parameters. Third, by setting `f` to 1, we introduce
lots of collisions between nodes which triggers lots of forks. Not mandatory
for this test, but it makes it easier to reason about the adequate slot
length for getting rewards.

This default `GenesisConfigChanges` provides a good starting for enabling
rewards, but can be tweaked based on requirements on how the devnet should
behave.
-}
defaultConfigFastRewardDistribution :: GenesisConfigChanges
defaultConfigFastRewardDistribution =
  let change :: ShelleyGenesis -> ShelleyGenesis
      change g =
        g
          { C.sgEpochLength = Ledger.EpochSize 100
          , C.sgSlotLength = 0.1
          , C.sgSecurityParam = Ledger.unsafeNonZero 10
          , C.sgActiveSlotsCoeff = fromJust $ Ledger.boundRational 1.0
          }
   in mempty{cfShelley = change}

{- | Set the epoch length in the shelley genesis configuration. Note that the parameter is a multiple of
  the slot length. With the default slot length of 0.1s, an epoch length of 100 would result in
  10 second epochs.
-}
setEpochLength :: EpochSize -> GenesisConfigChanges
setEpochLength n =
  let change :: ShelleyGenesis -> ShelleyGenesis
      change g = g{sgEpochLength = n}
   in mempty{cfShelley = change}
