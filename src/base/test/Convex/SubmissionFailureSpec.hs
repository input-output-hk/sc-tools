{-# LANGUAGE DataKinds #-}

module Convex.SubmissionFailureSpec where

import Cardano.Api qualified as C
import Cardano.Chain.Common (AddrAttributes (..), Address (..), Attributes (..))
import Cardano.Crypto.DSIGN qualified as DSIGN
import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..), RewardAccount (..), Withdrawals (..))
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Plutus.TxInfo (AlonzoContextError (..))
import Cardano.Ledger.Alonzo.Rules (FailureDescription (..), TagMismatchDescription (..))
import Cardano.Ledger.Alonzo.Rules qualified as AlonzoRules (AlonzoUtxoPredFailure (..), AlonzoUtxowPredFailure (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..), AlonzoScript (..), AsItem (..), AsIx (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxOut (AlonzoTxOut (..))
import Cardano.Ledger.Babbage.Rules qualified as BabbageRules (BabbageUtxoPredFailure (..), BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..))
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (CertIx (..), Mismatch (..), Network (..), ProtVer (..), Relation (..), TxIx (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Conway.Rules (ConwayLedgerPredFailure (..))
import Cardano.Ledger.Conway.Rules qualified as ConwayRules (ConwayUtxoPredFailure (..), ConwayUtxosPredFailure (..), ConwayUtxowPredFailure (..))
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.TxInfo (ConwayContextError (..))
import Cardano.Ledger.Credential (Credential (..), Ptr (..), SlotNo32 (..), StakeReference (..))
import Cardano.Ledger.Hashes (KeyHash (..), SafeHash, ScriptHash (..), TxAuxDataHash (..))
import Cardano.Ledger.Keys (DSIGN)
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus.Data (BinaryData, Datum (..))
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.TxInfo (TxOutSource (..))
import Cardano.Ledger.Shelley.Rules (ShelleyPpupPredFailure (..), VotingPeriod (..))
import Cardano.Ledger.Shelley.Rules qualified as ShelleyRules (ShelleyUtxowPredFailure (..))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.State (UTxO (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import Cardano.Ledger.BaseTypes ()
import Convex.Utils.SubmissionFailure ()
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Text.Read (readMaybe)

type CurrentEra = C.ShelleyLedgerEra C.ConwayEra

type Failure = ConwayLedgerPredFailure CurrentEra

prop_readFailure :: Failure -> Bool
prop_readFailure (ConwayCertsFailure _) = True
prop_readFailure (ConwayGovFailure _) = True
prop_readFailure pf = readTest pf

containsUnsupportedPlutusPurpose :: String -> Bool
containsUnsupportedPlutusPurpose s =
  any (`isInfixOf` s) ["ConwayVoting (AsItem", "ConwayProposing (AsItem"]

containsPParamsUpdate :: String -> Bool
containsPParamsUpdate s = any (`isInfixOf` s) ["PParamsUpdate"]

readTest :: forall a. (Show a, Read a) => a -> Bool
readTest x | containsUnsupportedPlutusPurpose (show x) = True
readTest x | containsPParamsUpdate (show x) = True
readTest x = isJust (readMaybe (show x) :: Maybe a)

prop_readSafeHash :: SafeHash i -> Bool
prop_readSafeHash = readTest

prop_readScriptHash :: ScriptHash -> Bool
prop_readScriptHash = readTest

prop_readKeyHash :: KeyHash i -> Bool
prop_readKeyHash = readTest

prop_readTxIx :: TxIx -> Bool
prop_readTxIx = readTest

prop_readTxId :: TxId -> Bool
prop_readTxId = readTest

prop_readTxIn :: TxIn -> Bool
prop_readTxIn = readTest

prop_readCoin :: Coin -> Bool
prop_readCoin = readTest

prop_readAssetName :: AssetName -> Bool
prop_readAssetName = readTest

prop_readPolicyID :: PolicyID -> Bool
prop_readPolicyID = readTest

prop_readMultiAsset :: MultiAsset -> Bool
prop_readMultiAsset = readTest

prop_readMaryValue :: MaryValue -> Bool
prop_readMaryValue = readTest

prop_readNetwork :: Network -> Bool
prop_readNetwork = readTest

prop_readSlotNo :: SlotNo -> Bool
prop_readSlotNo = readTest

prop_readEpochNo :: EpochNo -> Bool
prop_readEpochNo = readTest

prop_readValidityInterval :: ValidityInterval -> Bool
prop_readValidityInterval = readTest

prop_readTxAuxDataHash :: TxAuxDataHash -> Bool
prop_readTxAuxDataHash = readTest

prop_readDeltaCoin :: DeltaCoin -> Bool
prop_readDeltaCoin = readTest

prop_readExUnits :: ExUnits -> Bool
prop_readExUnits = readTest

prop_readProtVer :: ProtVer -> Bool
prop_readProtVer = readTest

prop_readVotingPeriod :: VotingPeriod -> Bool
prop_readVotingPeriod = readTest

prop_readShelleyPpupPredFailure :: ShelleyPpupPredFailure CurrentEra -> Bool
prop_readShelleyPpupPredFailure = readTest

prop_readAlonzoScript :: AlonzoScript CurrentEra -> Bool
prop_readAlonzoScript = readTest

prop_readBinaryData :: BinaryData CurrentEra -> Bool
prop_readBinaryData = readTest

prop_readDatum :: Datum CurrentEra -> Bool
prop_readDatum = readTest

prop_readIsValid :: IsValid -> Bool
prop_readIsValid = readTest

prop_readFailureDescription :: FailureDescription -> Bool
prop_readFailureDescription = readTest

prop_readTagMismatchDescription :: TagMismatchDescription -> Bool
prop_readTagMismatchDescription = readTest

prop_readDsign :: DSIGN.VerKeyDSIGN DSIGN -> Bool
prop_readDsign = readTest

prop_readUtxo :: UTxO CurrentEra -> Bool
prop_readUtxo = readTest

prop_readMismatch :: Mismatch RelEQ Bool -> Bool
prop_readMismatch = readTest

prop_readCredential :: Credential kr -> Bool
prop_readCredential = readTest

-- Note: No Arbitrary instances for the following types

-- prop_readAddrType :: AddrType -> Bool
-- prop_readAddrType = readTest

-- prop_readNetworkMagic :: NetworkMagic -> Bool
-- prop_readNetworkMagic = readTest

-- prop_readHDAddressPayload :: HDAddressPayload -> Bool
-- prop_readHDAddressPayload = readTest

prop_readAddrAttributes :: AddrAttributes -> Bool
prop_readAddrAttributes = readTest

prop_readAttributesAddrAttributes :: Attributes AddrAttributes -> Bool
prop_readAttributesAddrAttributes = readTest

prop_readAddress :: Address -> Bool
prop_readAddress = readTest

prop_readBootstrapAddress :: BootstrapAddress -> Bool
prop_readBootstrapAddress = readTest

prop_readAddr :: Addr -> Bool
prop_readAddr = readTest

prop_readRewardAccount :: RewardAccount -> Bool
prop_readRewardAccount = readTest

prop_readWithdrawals :: Withdrawals -> Bool
prop_readWithdrawals = readTest

prop_readStakeReference :: StakeReference -> Bool
prop_readStakeReference = readTest

prop_readSlotNo32 :: SlotNo32 -> Bool
prop_readSlotNo32 = readTest

prop_readPtr :: Ptr -> Bool
prop_readPtr = readTest

prop_readCertIx :: CertIx -> Bool
prop_readCertIx = readTest

prop_readShelleyTxOut :: ShelleyTxOut CurrentEra -> Bool
prop_readShelleyTxOut = readTest

prop_readAlonzoTxOut :: AlonzoTxOut CurrentEra -> Bool
prop_readAlonzoTxOut = readTest

prop_readBabbageTxOut :: BabbageTxOut CurrentEra -> Bool
prop_readBabbageTxOut = readTest

prop_readTxOutSource :: TxOutSource -> Bool
prop_readTxOutSource = readTest

prop_readAlonzoPlutusPurposeAsIx :: AlonzoPlutusPurpose AsIx CurrentEra -> Bool
prop_readAlonzoPlutusPurposeAsIx = readTest

prop_readAlonzoPlutusPurposeAsItem :: AlonzoPlutusPurpose AsItem CurrentEra -> Bool
prop_readAlonzoPlutusPurposeAsItem = readTest

prop_readConwayPlutusPurposeAsIx :: ConwayPlutusPurpose AsIx CurrentEra -> Bool
prop_readConwayPlutusPurposeAsIx = readTest

prop_readConwayPlutusPurposeAsItem :: ConwayPlutusPurpose AsItem CurrentEra -> Bool
prop_readConwayPlutusPurposeAsItem = readTest

prop_readCollectError :: CollectError CurrentEra -> Bool
prop_readCollectError = readTest

prop_readAlonzoContextError :: AlonzoContextError CurrentEra -> Bool
prop_readAlonzoContextError = readTest

prop_readBabbageContextError :: BabbageContextError CurrentEra -> Bool
prop_readBabbageContextError = readTest

prop_readConwayContextError :: ConwayContextError CurrentEra -> Bool
prop_readConwayContextError = readTest

-- Note: missing Read instances
-- prop_readShelleyUtxoPredFailure :: ShelleyRules.ShelleyUtxoPredFailure CurrentEra -> Bool
-- prop_readShelleyUtxoPredFailure = readTest

prop_readShelleyUtxowPredFailure :: ShelleyRules.ShelleyUtxowPredFailure CurrentEra -> Bool
prop_readShelleyUtxowPredFailure = readTest

-- Note: missing Read instances
-- prop_readAllegraUtxoPredFailure :: AllegraRules.AllegraUtxoPredFailure CurrentEra -> Bool
-- prop_readAllegraUtxoPredFailure = readTest

prop_readAlonzoUtxoPredFailure :: AlonzoRules.AlonzoUtxoPredFailure CurrentEra -> Bool
prop_readAlonzoUtxoPredFailure = readTest

prop_readAlonzoUtxowPredFailure :: AlonzoRules.AlonzoUtxowPredFailure CurrentEra -> Bool
prop_readAlonzoUtxowPredFailure = readTest

-- Note: missing Read instances
-- prop_readAlonzoUtxosPredFailure :: AlonzoRules.AlonzoUtxosPredFailure CurrentEra -> Bool
-- prop_readAlonzoUtxosPredFailure = readTest

prop_readBabbageUtxoPredFailure :: BabbageRules.BabbageUtxoPredFailure CurrentEra -> Bool
prop_readBabbageUtxoPredFailure = readTest

prop_readBabbageUtxowPredFailure :: BabbageRules.BabbageUtxowPredFailure CurrentEra -> Bool
prop_readBabbageUtxowPredFailure = readTest

prop_readConwayUtxoPredFailure :: ConwayRules.ConwayUtxoPredFailure CurrentEra -> Bool
prop_readConwayUtxoPredFailure = readTest

prop_readConwayUtxowPredFailure :: ConwayRules.ConwayUtxowPredFailure CurrentEra -> Bool
prop_readConwayUtxowPredFailure = readTest

prop_readConwayUtxosPredFailure :: ConwayRules.ConwayUtxosPredFailure CurrentEra -> Bool
prop_readConwayUtxosPredFailure = readTest

-- Note: missing Read instances
-- prop_readShelleyLedgerPredFailure :: ShelleyLedgerPredFailure CurrentEra -> Bool
-- prop_readShelleyLedgerPredFailure = readTest
