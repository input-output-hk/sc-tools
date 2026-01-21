{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Transaction submission failure types and orphan Read instances for ledger error values.
module Convex.Utils.SubmissionFailure (
  -- * Top-level error types
  TxSubmitWebApiError (..),
  TxCmdError (..),
  TxValidationErrorInCardanoMode (..),
  ShelleyTxValidationError (..),
  ShelleyBasedEra (..),

  -- * Parsing utilities
  parseTxSubmitError,
  extractLedgerErrors,
) where

import Cardano.Chain.Common (AddrAttributes (..), AddrType (..), Address (..), Attributes (..), HDAddressPayload (..), NetworkMagic (..), UnparsedFields (..))
import Cardano.Crypto.DSIGN qualified as DSIGN
import Cardano.Crypto.Hash (hashFromTextAsHex)
import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..), RewardAccount (..), Withdrawals (..))
import Cardano.Ledger.Allegra.Rules qualified as AllegraRules (AllegraUtxoPredFailure (..))
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Plutus.Context (ContextError)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Plutus.TxInfo (AlonzoContextError (..))
import Cardano.Ledger.Alonzo.Rules (FailureDescription (..), TagMismatchDescription (..))
import Cardano.Ledger.Alonzo.Rules qualified as AlonzoRules (AlonzoUtxoPredFailure (..), AlonzoUtxosPredFailure (..), AlonzoUtxowPredFailure (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript (..), AlonzoPlutusPurpose (..), AlonzoScript (..), AsItem (..), AsIx (..), PlutusScript)
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxOut (AlonzoTxOut (..))
import Cardano.Ledger.Babbage.Rules qualified as BabbageRules (BabbageUtxoPredFailure (..), BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..))
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (CertIx (..), Mismatch (..), Network (..), ProtVer (..), StrictMaybe (SNothing), TxIx (..))
import Cardano.Ledger.Binary.Version (mkVersion)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Conway.Governance (ProposalProcedure, VotingProcedures (..))
import Cardano.Ledger.Conway.Rules (ConwayLedgerPredFailure (..), PredicateFailure)
import Cardano.Ledger.Conway.Rules qualified as ConwayRules (ConwayUtxoPredFailure (..), ConwayUtxosPredFailure (..), ConwayUtxowPredFailure (..))
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.TxCert (ConwayEraTxCert (..))
import Cardano.Ledger.Conway.TxInfo (ConwayContextError (..))
import Cardano.Ledger.Credential (Credential (..), Ptr (..), SlotNo32 (..), StakeReference (..))

-- Dijkstra era imports - commented out until cardano-ledger-dijkstra is available
-- import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerPredFailure (..))
-- import Cardano.Ledger.Dijkstra.Rules.SubLedgers (DijkstraSubLedgersPredFailure (..))
-- import Cardano.Ledger.Dijkstra.Rules.Utxo qualified as DijkstraUtxoRules (DijkstraUtxoPredFailure (..))
-- import Cardano.Ledger.Dijkstra.Rules.Utxow qualified as DijkstraUtxowRules (DijkstraUtxowPredFailure (..))
-- import Cardano.Ledger.Dijkstra.TxInfo qualified as DijkstraTxInfo (DijkstraContextError (..))

import Cardano.Api qualified as C
import Cardano.Ledger.Core (Era, EraRule, EraRuleFailure, Script, TxCert, TxOut)
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Hashes (ADDRHASH, HASH, KeyHash (..), SafeHash, ScriptHash (..), TxAuxDataHash (..), unsafeMakeSafeHash)
import Cardano.Ledger.Keys (DSIGN, KeyRole (..), VKey (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus.Data (BinaryData, Datum (..), makeBinaryData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..), ExUnits' (..))
import Cardano.Ledger.Plutus.Language (Language (..), Plutus (..), PlutusBinary (..), asSLanguage, withSLanguage)
import Cardano.Ledger.Plutus.TxInfo (TxOutSource (..))
import Cardano.Ledger.Shelley.API (ApplyTxError (ApplyTxError))
import Cardano.Ledger.Shelley.Rules (ShelleyLedgerPredFailure (..), ShelleyPpupPredFailure (..), VotingPeriod (..))
import Cardano.Ledger.Shelley.Rules qualified as ShelleyRules (ShelleyUtxoPredFailure (..), ShelleyUtxowPredFailure (..))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.State (UTxO (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.Val (Val)
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Control.Applicative (optional, (<|>))
import Convex.Class (SendTxError (..))
import Data.Aeson (
  FromJSON (..),
  Value (..),
  eitherDecode,
  withObject,
  (.:),
  (.:?),
 )
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.OSet.Strict qualified as OSet
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Text.ParserCombinators.ReadP (ReadP, between, char, get, look, pfail, readP_to_S, readS_to_P, skipSpaces, string)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Era types
--------------------------------------------------------------------------------

-- | Shelley-based eras
data ShelleyBasedEra
  = ShelleyBasedEraShelley
  | ShelleyBasedEraAllegra
  | ShelleyBasedEraMary
  | ShelleyBasedEraAlonzo
  | ShelleyBasedEraBabbage
  | ShelleyBasedEraConway
  deriving stock (Show, Eq, Generic)

instance FromJSON ShelleyBasedEra where
  parseJSON = \case
    String "ShelleyBasedEraShelley" -> pure ShelleyBasedEraShelley
    String "ShelleyBasedEraAllegra" -> pure ShelleyBasedEraAllegra
    String "ShelleyBasedEraMary" -> pure ShelleyBasedEraMary
    String "ShelleyBasedEraAlonzo" -> pure ShelleyBasedEraAlonzo
    String "ShelleyBasedEraBabbage" -> pure ShelleyBasedEraBabbage
    String "ShelleyBasedEraConway" -> pure ShelleyBasedEraConway
    other -> fail $ "Unknown ShelleyBasedEra: " <> show other

--------------------------------------------------------------------------------
-- Shelley validation error (innermost layer)
--------------------------------------------------------------------------------

-- | Shelley-era transaction validation error containing ledger predicate failures
data ShelleyTxValidationError = ShelleyTxValidationError
  { stveEra :: ShelleyBasedEra
  , stveErrors :: [Text]
  -- ^ Raw error strings (can be parsed with Read instances)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ShelleyTxValidationError where
  parseJSON = withObject "ShelleyTxValidationError" $ \o -> do
    kind <- o .: "kind"
    case kind of
      String "ShelleyTxValidationError" -> do
        era <- o .: "era"
        errors <- o .: "error"
        pure $ ShelleyTxValidationError era errors
      _ -> fail $ "Expected kind 'ShelleyTxValidationError', got: " <> show kind

--------------------------------------------------------------------------------
-- Validation error in Cardano mode
--------------------------------------------------------------------------------

-- | Validation error in Cardano mode (era-polymorphic wrapper)
data TxValidationErrorInCardanoMode
  = TxValidationErrorInCardanoMode ShelleyTxValidationError
  | TxValidationEraMismatch Text
  deriving stock (Show, Eq, Generic)

instance FromJSON TxValidationErrorInCardanoMode where
  parseJSON = withObject "TxValidationErrorInCardanoMode" $ \o -> do
    tag <- o .: "tag"
    case tag of
      String "TxValidationErrorInCardanoMode" -> do
        contents <- o .: "contents"
        TxValidationErrorInCardanoMode <$> parseJSON contents
      String "TxValidationEraMismatch" -> do
        contents <- o .: "contents"
        pure $ TxValidationEraMismatch contents
      _ -> fail $ "Unknown TxValidationErrorInCardanoMode tag: " <> show tag

--------------------------------------------------------------------------------
-- Command-level transaction error
--------------------------------------------------------------------------------

-- | Command-level transaction error
data TxCmdError
  = TxCmdSocketEnvError Text
  | TxCmdTxReadError Text
  | TxCmdTxSubmitValidationError TxValidationErrorInCardanoMode
  deriving stock (Show, Eq, Generic)

instance FromJSON TxCmdError where
  parseJSON = withObject "TxCmdError" $ \o -> do
    tag <- o .: "tag"
    case tag of
      String "TxCmdSocketEnvError" -> do
        contents <- o .: "contents"
        pure $ TxCmdSocketEnvError contents
      String "TxCmdTxReadError" -> do
        contents <- o .: "contents"
        pure $ TxCmdTxReadError contents
      String "TxCmdTxSubmitValidationError" -> do
        contents <- o .: "contents"
        TxCmdTxSubmitValidationError <$> parseJSON contents
      _ -> fail $ "Unknown TxCmdError tag: " <> show tag

--------------------------------------------------------------------------------
-- Top-level web API error
--------------------------------------------------------------------------------

-- | Top-level web API error from cardano-submit-api
data TxSubmitWebApiError
  = TxSubmitDecodeHex
  | TxSubmitEmpty
  | TxSubmitDecodeFail Text
  | TxSubmitBadTx Text
  | TxSubmitFail TxCmdError
  deriving stock (Show, Eq, Generic)

instance FromJSON TxSubmitWebApiError where
  parseJSON = withObject "TxSubmitWebApiError" $ \o -> do
    tag <- o .: "tag"
    mContents <- o .:? "contents"
    case (tag, mContents) of
      (String "TxSubmitDecodeHex", _) -> pure TxSubmitDecodeHex
      (String "TxSubmitEmpty", _) -> pure TxSubmitEmpty
      (String "TxSubmitDecodeFail", Just contents) ->
        TxSubmitDecodeFail <$> parseJSON contents
      (String "TxSubmitBadTx", Just contents) ->
        TxSubmitBadTx <$> parseJSON contents
      (String "TxSubmitFail", Just contents) ->
        TxSubmitFail <$> parseJSON contents
      _ -> fail $ "Unknown TxSubmitWebApiError tag: " <> show tag

--------------------------------------------------------------------------------
-- Parsing utilities
--------------------------------------------------------------------------------

-- | Parse a raw JSON ByteString into a TxSubmitWebApiError
parseTxSubmitError :: LBS.ByteString -> Either String TxSubmitWebApiError
parseTxSubmitError = eitherDecode

{- | Extract parsed ConwayLedgerPredFailure from a TxSubmitWebApiError

Returns a list of successfully parsed errors. Errors that fail to parse
are silently dropped. Use 'stveErrors' to access raw error strings.
-}
extractLedgerErrors :: C.ShelleyBasedEra era -> TxSubmitWebApiError -> Maybe (SendTxError era)
extractLedgerErrors era = \case
  TxSubmitFail (TxCmdTxSubmitValidationError (TxValidationErrorInCardanoMode shelleyErr)) ->
    case (stveEra shelleyErr, era) of
      (ShelleyBasedEraConway, C.ShelleyBasedEraConway) ->
        fmap ApplyTxFailure $ wrap $ mapMaybe (readMaybe . T.unpack) (stveErrors shelleyErr)
      (ShelleyBasedEraBabbage, C.ShelleyBasedEraBabbage) ->
        fmap ApplyTxFailure $ wrap $ mapMaybe (readMaybe . T.unpack) (stveErrors shelleyErr)
      _ -> Just EraMismatchError
  _ -> Nothing
 where
  wrap :: [PredicateFailure (EraRule "LEDGER" (C.ShelleyLedgerEra era))] -> Maybe (ApplyTxError (C.ShelleyLedgerEra era))
  wrap [] = Nothing
  wrap (e : es) = Just $ ApplyTxError (e :| es)

--------------------------------------------------------------------------------
-- Read instance parsing utilities
--------------------------------------------------------------------------------

type Parser a = ReadP a

readP :: (Read a) => Parser a
readP = readS_to_P reads

ident :: String -> Parser ()
ident name = string name *> skipSpaces

pChar :: Char -> Parser ()
pChar c = char c *> skipSpaces

pParens :: Parser a -> Parser a
pParens p = skipSpaces *> between (pChar '(') (pChar ')') p

-- Parse constructor with a single field in record syntax: Con {field = value}
record1 :: String -> String -> Parser a -> Parser a
record1 con field p = do
  ident con
  pChar '{'
  ident field
  pChar '='
  v <- p
  pChar '}'
  pure v

record2 :: String -> (String, Parser a) -> (String, Parser b) -> Parser (a, b)
record2 con (field1, p1) (field2, p2) = do
  ident con
  pChar '{'
  ident field1
  pChar '='
  v1 <- p1
  _ <- optional (pChar ',')
  ident field2
  pChar '='
  v2 <- p2
  pChar '}'
  pure (v1, v2)

record3 :: String -> (String, Parser a) -> (String, Parser b) -> (String, Parser c) -> Parser (a, b, c)
record3 con (field1, p1) (field2, p2) (field3, p3) = do
  ident con
  pChar '{'
  ident field1
  pChar '='
  v1 <- p1
  _ <- optional (pChar ',')
  ident field2
  pChar '='
  v2 <- p2
  _ <- optional (pChar ',')
  ident field3
  pChar '='
  v3 <- p3
  pChar '}'
  pure (v1, v2, v3)

-- Parse constructor with positional arguments: Con arg1 arg2 ...
con0 :: a -> String -> Parser a
con0 val con = ident con *> pure val

con1 :: (Read a) => (a -> b) -> String -> Parser b
con1 f con = ident con *> (f <$> readP)

con2 :: (Read a, Read b) => (a -> b -> c) -> String -> Parser c
con2 f con = ident con *> (f <$> readP <*> readP)

con3 :: (Read a, Read b, Read c) => (a -> b -> c -> d) -> String -> Parser d
con3 f con = ident con *> (f <$> readP <*> readP <*> readP)

tryParens :: Parser a -> Parser a
tryParens p = pParens p <|> p

skipValue :: Parser ()
skipValue = skipSpaces *> go 0
 where
  go :: Int -> Parser ()
  go depth = do
    rest <- look
    case rest of
      [] -> pure ()
      c : _
        | depth == 0 && c `elem` [',', ')', ']', '}'] -> pure ()
        | c == '"' -> skipQuoted '"' *> go depth
        | c == '\'' -> skipQuoted '\'' *> go depth
        | c == '(' -> get *> go (depth + 1)
        | c == '[' -> get *> go (depth + 1)
        | c == '{' -> get *> go (depth + 1)
        | c == ')' && depth > 0 -> get *> go (depth - 1)
        | c == ']' && depth > 0 -> get *> go (depth - 1)
        | c == '}' && depth > 0 -> get *> go (depth - 1)
        | otherwise -> get *> go depth

skipQuoted :: Char -> Parser ()
skipQuoted quote = do
  _ <- get
  go
 where
  go = do
    c <- get
    case c of
      '\\' -> get >> go
      _
        | c == quote -> pure ()
        | otherwise -> go

--------------------------------------------------------------------------------
-- Stub values for unparseable types
--------------------------------------------------------------------------------

stubKeyHash :: KeyHash r
stubKeyHash =
  case hashFromTextAsHex @ADDRHASH (T.pack (replicate 56 '0')) of
    Nothing -> error "stubKeyHash: invalid hash"
    Just h -> KeyHash h

stubStakingCred :: Credential Staking
stubStakingCred = KeyHashObj stubKeyHash

-- stubRewardAccount :: RewardAccount
-- stubRewardAccount = RewardAccount Testnet stubStakingCred

stubTxCert :: (ConwayEraTxCert era) => TxCert era
stubTxCert = mkRegDepositTxCert stubStakingCred (Coin 0)

-- stubConwayDelegPredFailure :: forall era. ConwayDelegPredFailure era
-- stubConwayDelegPredFailure = IncorrectDepositDELEG (Coin 0)

-- stubConwayCertPredFailure :: forall era. ConwayCertPredFailure era
-- stubConwayCertPredFailure = DelegFailure (stubConwayDelegPredFailure @era)

-- stubConwayCertsPredFailure :: ConwayCertsPredFailure era
-- stubConwayCertsPredFailure = WithdrawalsNotInRewardsCERTS Map.empty

-- stubConwayGovPredFailure :: ConwayGovPredFailure era
-- stubConwayGovPredFailure = ProposalProcedureNetworkIdMismatch stubRewardAccount Testnet

stubProposalProcedures :: OSet.OSet (ProposalProcedure era)
stubProposalProcedures = OSet.empty

-- stubShelleyDelegsPredFailure :: ShelleyDelegsPredFailure era
-- stubShelleyDelegsPredFailure = DelegateeNotRegisteredDELEG stubKeyHash

-- Dijkstra era stub - commented out until cardano-ledger-dijkstra is available
-- stubSubLedgersFailure :: DijkstraSubLedgersPredFailure era
-- stubSubLedgersFailure = SubLedgerFailure (error "Sub-ledger failure stub")

stubPlutusBinary :: PlutusBinary
stubPlutusBinary = PlutusBinary SBS.empty

stubAlonzoScript :: forall era. (AlonzoEraScript era) => Language -> AlonzoScript era
stubAlonzoScript lang =
  case mkPlutusScriptMaybe lang of
    Just ps -> PlutusScript ps
    Nothing ->
      case mkPlutusScriptMaybe (eraMaxLanguage @era) of
        Just ps -> PlutusScript ps
        Nothing -> error "stubAlonzoScript: no supported Plutus language"
 where
  mkPlutusScriptMaybe :: Language -> Maybe (PlutusScript era)
  mkPlutusScriptMaybe l =
    withSLanguage l $ \slang ->
      (mkPlutusScript (asSLanguage slang (Plutus stubPlutusBinary)) :: Maybe (PlutusScript era))

--------------------------------------------------------------------------------
-- Orphan Read instances for ledger error types
--------------------------------------------------------------------------------

instance Read (SafeHash i) where
  readsPrec _ = readP_to_S $ tryParens $ do
    ident "SafeHash"
    hex <- readP
    case hashFromTextAsHex @HASH (T.pack hex) of
      Nothing -> pfail
      Just h -> pure (unsafeMakeSafeHash h)

instance Read ScriptHash where
  readsPrec _ = readP_to_S $ tryParens $ do
    ident "ScriptHash"
    hex <- readP
    case hashFromTextAsHex @ADDRHASH (T.pack hex) of
      Nothing -> pfail
      Just h -> pure (ScriptHash h)

instance Read (KeyHash r) where
  readsPrec _ = readP_to_S $ tryParens $ record1 "KeyHash" "unKeyHash" $ do
    hex <- readP
    case hashFromTextAsHex @ADDRHASH (T.pack hex) of
      Nothing -> pfail
      Just h -> pure (KeyHash h)

instance Read TxIx where
  readsPrec _ =
    readP_to_S $
      tryParens $
        (TxIx <$> record1 "TxIx" "unTxIx" readP) <|> con1 TxIx "TxIx"

instance Read TxId where
  readsPrec _ = readP_to_S $ tryParens $ do
    sh <- record1 "TxId" "unTxId" readP
    pure (TxId sh)

instance Read TxIn where
  readsPrec _ = readP_to_S $ tryParens $ con2 TxIn "TxIn"

instance Read Coin where
  readsPrec _ = readP_to_S $ tryParens $ con1 Coin "Coin"

instance Read AssetName where
  readsPrec _ = readP_to_S $ tryParens $ do
    hex <- readP
    case B16.decode (BSC.pack hex) of
      Left _ -> pfail
      Right bs -> pure (AssetName (SBS.toShort bs))

instance Read PolicyID where
  readsPrec _ = readP_to_S $ tryParens $ do
    sh <- record1 "PolicyID" "policyID" readP
    pure (PolicyID sh)

instance Read MultiAsset where
  readsPrec _ = readP_to_S $ tryParens $ con1 MultiAsset "MultiAsset"

instance Read MaryValue where
  readsPrec _ = readP_to_S $ tryParens $ con2 MaryValue "MaryValue"

-- NOTE: StrictMaybe Read instance already defined in Data.Maybe.Strict
-- instance Read a => Read (StrictMaybe a) where
--   readsPrec _ = readP_to_S $ tryParens $
--     (ident "SNothing" *> pure SNothing)
--       <|> (SJust <$> con1 "SJust" readP)

instance Read Network where
  readsPrec _ = readP_to_S $ tryParens $ con0 Testnet "Testnet" <|> con0 Mainnet "Mainnet"

instance Read SlotNo where
  readsPrec _ =
    readP_to_S $
      tryParens $
        (SlotNo <$> record1 "SlotNo" "unSlotNo" readP) <|> con1 SlotNo "SlotNo"

instance Read EpochNo where
  readsPrec _ =
    readP_to_S $
      tryParens $
        (EpochNo <$> record1 "EpochNo" "unEpochNo" readP) <|> con1 EpochNo "EpochNo"

instance Read ValidityInterval where
  readsPrec _ = readP_to_S $ tryParens $ do
    (before, hereafter) <-
      record2
        "ValidityInterval"
        ("invalidBefore", readP)
        ("invalidHereafter", readP)
    pure (ValidityInterval before hereafter)

instance Read TxAuxDataHash where
  readsPrec _ =
    readP_to_S $
      tryParens $
        (TxAuxDataHash <$> record1 "TxAuxDataHash" "unTxAuxDataHash" readP) <|> con1 TxAuxDataHash "TxAuxDataHash"

instance Read DeltaCoin where
  readsPrec _ = readP_to_S $ tryParens $ con1 DeltaCoin "DeltaCoin"

instance (Read a) => Read (ExUnits' a) where
  readsPrec _ = readP_to_S $ tryParens $ do
    (memUnits, stepUnits) <-
      record2
        "ExUnits'"
        ("exUnitsMem'", readP)
        ("exUnitsSteps'", readP)
    pure (ExUnits' memUnits stepUnits)

instance Read ExUnits where
  readsPrec _ =
    readP_to_S $
      tryParens $
        (WrapExUnits <$> record1 "WrapExUnits" "unWrapExUnits" readP) <|> con1 WrapExUnits "WrapExUnits"

-- NOTE: Language Read instance already defined in Cardano.Ledger.Plutus.Language
-- instance Read Language where
--   readsPrec _ = readP_to_S $ tryParens $
--     (ident "PlutusV1" *> pure PlutusV1)
--       <|> (ident "PlutusV2" *> pure PlutusV2)
--       <|> (ident "PlutusV3" *> pure PlutusV3)

-- ProtVer contains Version which doesn't have a public constructor, skip and parse manually
instance Read ProtVer where
  readsPrec _ = readP_to_S $ tryParens $ do
    ident "ProtVer"
    skipValue
    -- Return a stub ProtVer since we can't parse Version fields
    -- mkVersion 9 is Conway era protocol version
    case mkVersion (9 :: Natural) of
      Just v -> pure (ProtVer v 0)
      Nothing -> pfail

instance Read VotingPeriod where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con0 VoteForThisEpoch "VoteForThisEpoch"
          <|> con0 VoteForNextEpoch "VoteForNextEpoch"

instance Read (ShelleyPpupPredFailure era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 NonGenesisUpdatePPUP "NonGenesisUpdatePPUP"
          <|> con3 PPUpdateWrongEpoch "PPUpdateWrongEpoch"
          <|> con1 PVCannotFollowPPUP "PVCannotFollowPPUP"

-- NOTE: Serialisation of AlonzoScript is very broken
instance (AlonzoEraScript era) => Read (AlonzoScript era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        (ident "NativeScript" *> skipValue *> pure (stubAlonzoScript (eraMaxLanguage @era)))
          <|> (ident "PlutusScript" *> (stubAlonzoScript <$> readP <* skipValue))

-- NOTE: Can't have Read instance for type family 'Script era'
-- instance (AlonzoEraScript era, Script era ~ AlonzoScript era) => Read (Script era) where
--   readsPrec _ = readP_to_S (readP @(AlonzoScript era))

instance (Era era) => Read (BinaryData era) where
  readsPrec _ = readP_to_S $ tryParens $ do
    sbs <- readP
    case makeBinaryData sbs of
      Left _ -> pfail
      Right bd -> pure bd

instance (Era era) => Read (Datum era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con0 NoDatum "NoDatum"
          <|> con1 DatumHash "DatumHash"
          <|> con1 Datum "Datum"

instance Read IsValid where
  readsPrec _ = readP_to_S $ tryParens $ con1 IsValid "IsValid"

instance Read FailureDescription where
  readsPrec _ = readP_to_S $ tryParens $ con2 PlutusFailure "PlutusFailure"

instance Read TagMismatchDescription where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con0 PassedUnexpectedly "PassedUnexpectedly"
          <|> con1 FailedUnexpectedly "FailedUnexpectedly"

instance (Read it) => Read (AsItem ix it) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        (AsItem <$> record1 "AsItem" "unAsItem" readP) <|> con1 AsItem "AsItem"

instance (Read ix) => Read (AsIx ix it) where
  readsPrec _ = readP_to_S $ tryParens $ (AsIx <$> record1 "AsIx" "unAsIx" readP) <|> con1 AsIx "AsIx"

instance Read (DSIGN.VerKeyDSIGN DSIGN) where
  readsPrec _ = readP_to_S $ tryParens $ do
    ident "VerKeyEd25519DSIGN"
    hex <- readP
    case B16.decode (BSC.pack hex) of
      Left _ -> pfail
      Right bs -> case DSIGN.rawDeserialiseVerKeyDSIGN bs of
        Nothing -> pfail
        Just vk -> pure vk

instance Read (VKey kd) where
  readsPrec _ = readP_to_S $ tryParens $ con1 VKey "VKey"

-- UTxO is complex, so we skip parsing its contents
instance Read (UTxO era) where
  readsPrec _ = readP_to_S $ tryParens $ do
    ident "UTxO"
    skipValue
    pure (UTxO Map.empty)

instance (Read a) => Read (Mismatch r a) where
  readsPrec _ = readP_to_S $ tryParens $ do
    ident "Mismatch"
    _ <- optional (pParens (ident "RelEQ" <|> ident "RelLTEQ" <|> ident "RelGTEQ" <|> ident "RelLT" <|> ident "RelGT" <|> ident "RelSubset"))
    (recordStyle <|> positionalStyle)
   where
    recordStyle = do
      pChar '{'
      supplied <-
        (ident "mismatchSupplied" *> pChar '=' *> readP)
          <|> (ident "supplied:" *> readP)
      _ <- optional (pChar ',')
      expected <-
        (ident "mismatchExpected" *> pChar '=' *> readP)
          <|> (ident "expected:" *> readP)
      pChar '}'
      pure (Mismatch supplied expected)
    positionalStyle = Mismatch <$> readP <*> readP

-- NonEmptySet and NonEmptyMap instances require cardano-data >= 1.3
-- which adds Data.Set.NonEmpty and Data.Map.NonEmpty modules.
-- Currently commented out until cardano-data is upgraded.
--
-- instance (Ord a, Read a) => Read (NonEmptySet a) where
--   readsPrec _ = readP_to_S $ tryParens $ do
--     _ <- optional (ident "NonEmptySet")
--     let setParser = setParser (con1 "fromList" (pBrackets (sepBy readP (pChar ','))))
--     set <- setParser
--     case NonEmptySet.fromFoldable set of
--       Nothing -> pfail
--       Just nes -> pure nes

-- instance (Ord k, Read k, Read v) => Read (NonEmptyMap k v) where
--   readsPrec _ = readP_to_S $ tryParens $ do
--     ident "NonEmptyMap"
--     m <- tryParens readP <|> readP
--     case NonEmptyMap.fromMap m of
--       Nothing -> pfail
--       Just nem -> pure nem

instance Read (Credential r) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 KeyHashObj "KeyHashObj"
          <|> con1 ScriptHashObj "ScriptHashObj"

instance Read AddrType where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con0 ATVerKey "ATVerKey"
          <|> con0 ATRedeem "ATRedeem"

instance Read NetworkMagic where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con0 NetworkMainOrStage "NetworkMainOrStage"
          <|> con1 NetworkTestnet "NetworkTestnet"

instance Read HDAddressPayload where
  readsPrec _ =
    readP_to_S $
      tryParens $
        (HDAddressPayload <$> record1 "HDAddressPayload" "getHDAddressPayload" readP)
          <|> con1 HDAddressPayload "HDAddressPayload"

instance Read AddrAttributes where
  readsPrec _ = readP_to_S $ tryParens $ do
    (path, magic) <-
      record2
        "AddrAttributes"
        ("aaVKDerivationPath", readP)
        ("aaNetworkMagic", readP)
    pure (AddrAttributes path magic)

instance Read (Attributes AddrAttributes) where
  readsPrec _ = readP_to_S $ tryParens $ do
    ident "Attributes"
    pChar '{'
    ident "data_"
    pChar '='
    attrs <- readP
    _ <-
      optional $ do
        pChar ','
        ident "remain:"
        pChar '<'
        _ <- readP @Int
        _ <- optional (pChar ' ')
        ident "bytes>"
    pChar '}'
    pure (Attributes attrs (UnparsedFields Map.empty))

instance Read Address where
  readsPrec _ = readP_to_S $ tryParens $ do
    (root, attrs, addrTy) <-
      record3
        "Address"
        ("addrRoot", read <$> sequenceA (replicate 56 get))
        ("addrAttributes", readP)
        ("addrType", readP)
    pure (Address root attrs addrTy)

instance Read BootstrapAddress where
  readsPrec _ = readP_to_S $ tryParens $ con1 BootstrapAddress "BootstrapAddress"

instance Read Addr where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con3 Addr "Addr"
          <|> con1 AddrBootstrap "AddrBootstrap"

instance Read RewardAccount where
  readsPrec _ = readP_to_S $ tryParens $ do
    (network, cred) <-
      record2
        "RewardAccount"
        ("raNetwork", readP)
        ("raCredential", readP)
    pure (RewardAccount network cred)

instance Read Withdrawals where
  readsPrec _ =
    readP_to_S $
      tryParens $
        (Withdrawals <$> record1 "Withdrawals" "unWithdrawals" readP) <|> con1 Withdrawals "Withdrawals"

instance Read StakeReference where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 StakeRefBase "StakeRefBase"
          <|> con1 StakeRefPtr "StakeRefPtr"
          <|> con0 StakeRefNull "StakeRefNull"

instance Read SlotNo32 where
  readsPrec _ =
    readP_to_S $
      tryParens $
        (SlotNo32 <$> record1 "SlotNo32" "unSlotNo32" readP) <|> con1 SlotNo32 "SlotNo32"

-- Ptr contains CertIx which has no Read instance, skip parsing
instance Read Ptr where
  readsPrec _ = readP_to_S $ tryParens $ con3 Ptr "Ptr"

instance Read CertIx where
  readsPrec _ = readP_to_S $ tryParens $ CertIx <$> record1 "CertIx" "unCertIx" readP

instance (Era era, Val (Ledger.Value era), Read (Ledger.Value era)) => Read (ShelleyTxOut era) where
  readsPrec _ = readP_to_S $ tryParens $ do
    (addr, value) <- readP
    pure (ShelleyTxOut addr value)

instance (Era era, Val (Ledger.Value era), Read (Ledger.Value era)) => Read (AlonzoTxOut era) where
  readsPrec _ = readP_to_S $ tryParens $ do
    (addr, value, mDataHash) <- readP
    pure (AlonzoTxOut addr value mDataHash)

instance
  ( Era era
  , Val (Ledger.Value era)
  , Read (Ledger.Value era)
  , AlonzoEraScript era
  , Script era ~ AlonzoScript era
  )
  => Read (BabbageTxOut era)
  where
  readsPrec _ = readP_to_S $ do
    pChar '('
    addr <- readP
    pChar ','
    value <- readP
    pChar ','
    datum <- readP
    pChar ','
    skipValue
    pChar ')'
    pure (BabbageTxOut addr value datum SNothing)

instance Read TxOutSource where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 TxOutFromInput "TxOutFromInput"
          <|> con1 TxOutFromOutput "TxOutFromOutput"

instance Read (AlonzoPlutusPurpose AsIx era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 AlonzoSpending "AlonzoSpending"
          <|> con1 AlonzoMinting "AlonzoMinting"
          <|> con1 AlonzoCertifying "AlonzoCertifying"
          <|> con1 AlonzoRewarding "AlonzoRewarding"

instance (ConwayEraTxCert era) => Read (AlonzoPlutusPurpose AsItem era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 AlonzoSpending "AlonzoSpending"
          <|> con1 AlonzoMinting "AlonzoMinting"
          -- AlonzoCertifying contains TxCert which has no Read instance, cannot parse
          <|> (ident "AlonzoCertifying" *> skipValue *> pure (AlonzoCertifying (AsItem stubTxCert)))
          <|> con1 AlonzoRewarding "AlonzoRewarding"

instance Read (ConwayPlutusPurpose AsIx era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 ConwaySpending "ConwaySpending"
          <|> con1 ConwayMinting "ConwayMinting"
          <|> con1 ConwayCertifying "ConwayCertifying"
          <|> con1 ConwayRewarding "ConwayRewarding"
          <|> con1 ConwayVoting "ConwayVoting"
          <|> con1 ConwayProposing "ConwayProposing"

instance (ConwayEraTxCert era) => Read (ConwayPlutusPurpose AsItem era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 ConwaySpending "ConwaySpending"
          <|> con1 ConwayMinting "ConwayMinting"
          -- ConwayCertifying contains TxCert which has no Read instance, cannot parse
          <|> (ident "ConwayCertifying" *> skipValue *> pure (ConwayCertifying (AsItem stubTxCert)))
          <|> con1 ConwayRewarding "ConwayRewarding"
          -- ConwayVoting contains Voter which has no Read instance, cannot parse
          <|> (ident "ConwayVoting" *> skipValue *> pfail)
          -- ConwayProposing contains ProposalProcedure which has no Read instance, cannot parse
          <|> (ident "ConwayProposing" *> skipValue *> pfail)

instance (Read (PlutusPurpose AsItem era), Read (ContextError era)) => Read (CollectError era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 NoRedeemer "NoRedeemer"
          <|> con1 BadTranslation "BadTranslation"
          <|> con1 NoWitness "NoWitness"
          <|> con1 NoCostModel "NoCostModel"

instance Read (AlonzoContextError era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 TranslationLogicMissingInput "TranslationLogicMissingInput"
          <|> con1 TimeTranslationPastHorizon "TimeTranslationPastHorizon"

instance (Read (PlutusPurpose AsIx era)) => Read (BabbageContextError era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 AlonzoContextError "AlonzoContextError"
          <|> con1 ByronTxOutInContext "ByronTxOutInContext"
          <|> con1 ReferenceInputsNotSupported "ReferenceInputsNotSupported"
          <|> con1 InlineDatumsNotSupported "InlineDatumsNotSupported"
          <|> con1 ReferenceScriptsNotSupported "ReferenceScriptsNotSupported"
          <|> con1 RedeemerPointerPointsToNothing "RedeemerPointerPointsToNothing"

instance (Read (PlutusPurpose AsIx era), Read (PlutusPurpose AsItem era), ConwayEraTxCert era) => Read (ConwayContextError era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 BabbageContextError "BabbageContextError"
          <|> (ident "CertificateNotSupported" *> skipValue *> pure (CertificateNotSupported stubTxCert))
          <|> con1 PlutusPurposeNotSupported "PlutusPurposeNotSupported"
          <|> con1 CurrentTreasuryFieldNotSupported "CurrentTreasuryFieldNotSupported"
          <|> (ident "VotingProceduresFieldNotSupported" *> skipValue *> pure (VotingProceduresFieldNotSupported (VotingProcedures Map.empty)))
          <|> (ident "ProposalProceduresFieldNotSupported" *> skipValue *> pure (ProposalProceduresFieldNotSupported stubProposalProcedures))
          <|> con1 TreasuryDonationFieldNotSupported "TreasuryDonationFieldNotSupported"
          <|> con1 ReferenceInputsNotDisjointFromInputs "ReferenceInputsNotDisjointFromInputs"

-- Dijkstra era instance - commented out until cardano-ledger-dijkstra is available
-- instance ConwayEraTxCert era => Read (DijkstraTxInfo.DijkstraContextError era) where
--   readsPrec _ = readP_to_S $ tryParens $
--     DijkstraTxInfo.ConwayContextError <$> con1 "ConwayContextError" readP

instance (Read (TxOut era), Read (Ledger.Value era), Read (EraRuleFailure "PPUP" era)) => Read (ShelleyRules.ShelleyUtxoPredFailure era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 ShelleyRules.BadInputsUTxO "BadInputsUTxO"
          <|> con1 ShelleyRules.ExpiredUTxO "ExpiredUTxO"
          <|> con1 ShelleyRules.MaxTxSizeUTxO "MaxTxSizeUTxO"
          <|> con0 ShelleyRules.InputSetEmptyUTxO "InputSetEmptyUTxO"
          <|> con1 ShelleyRules.FeeTooSmallUTxO "FeeTooSmallUTxO"
          <|> con1 ShelleyRules.ValueNotConservedUTxO "ValueNotConservedUTxO"
          <|> con2 ShelleyRules.WrongNetwork "WrongNetwork"
          <|> con2 ShelleyRules.WrongNetworkWithdrawal "WrongNetworkWithdrawal"
          <|> con1 ShelleyRules.OutputTooSmallUTxO "OutputTooSmallUTxO"
          <|> con1 ShelleyRules.UpdateFailure "UpdateFailure"
          <|> con1 ShelleyRules.OutputBootAddrAttrsTooBig "OutputBootAddrAttrsTooBig"

instance (Read (PredicateFailure (EraRule "UTXO" era))) => Read (ShelleyRules.ShelleyUtxowPredFailure era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        -- UtxoFailure contains era-specific failure type
        con1 ShelleyRules.UtxoFailure "UtxoFailure"
          <|> con1 ShelleyRules.InvalidWitnessesUTXOW "InvalidWitnessesUTXOW"
          <|> con1 ShelleyRules.MissingVKeyWitnessesUTXOW "MissingVKeyWitnessesUTXOW"
          <|> con1 ShelleyRules.MissingScriptWitnessesUTXOW "MissingScriptWitnessesUTXOW"
          <|> con1 ShelleyRules.ScriptWitnessNotValidatingUTXOW "ScriptWitnessNotValidatingUTXOW"
          <|> con1 ShelleyRules.MIRInsufficientGenesisSigsUTXOW "MIRInsufficientGenesisSigsUTXOW"
          <|> con1 ShelleyRules.MissingTxBodyMetadataHash "MissingTxBodyMetadataHash"
          <|> con1 ShelleyRules.MissingTxMetadata "MissingTxMetadata"
          <|> con1 ShelleyRules.ConflictingMetadataHash "ConflictingMetadataHash"
          <|> con0 ShelleyRules.InvalidMetadata "InvalidMetadata"
          <|> con1 ShelleyRules.ExtraneousScriptWitnessesUTXOW "ExtraneousScriptWitnessesUTXOW"

instance (Read (Ledger.Value era), Read (TxOut era), Read (EraRuleFailure "PPUP" era)) => Read (AllegraRules.AllegraUtxoPredFailure era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 AllegraRules.BadInputsUTxO "BadInputsUTxO"
          <|> con2 AllegraRules.OutsideValidityIntervalUTxO "OutsideValidityIntervalUTxO"
          <|> con1 AllegraRules.MaxTxSizeUTxO "MaxTxSizeUTxO"
          <|> con0 AllegraRules.InputSetEmptyUTxO "InputSetEmptyUTxO"
          <|> con1 AllegraRules.FeeTooSmallUTxO "FeeTooSmallUTxO"
          <|> con1 AllegraRules.ValueNotConservedUTxO "ValueNotConservedUTxO"
          <|> con2 AllegraRules.WrongNetwork "WrongNetwork"
          <|> con2 AllegraRules.WrongNetworkWithdrawal "WrongNetworkWithdrawal"
          <|> con1 AllegraRules.OutputTooSmallUTxO "OutputTooSmallUTxO"
          <|> con1 AllegraRules.UpdateFailure "UpdateFailure"
          <|> con1 AllegraRules.OutputBootAddrAttrsTooBig "OutputBootAddrAttrsTooBig"
          <|> con1 AllegraRules.OutputTooBigUTxO "OutputTooBigUTxO"

instance (Read (PredicateFailure (EraRule "UTXOS" era)), Read (Ledger.Value era), Read (TxOut era)) => Read (AlonzoRules.AlonzoUtxoPredFailure era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 AlonzoRules.BadInputsUTxO "BadInputsUTxO"
          <|> con2 AlonzoRules.OutsideValidityIntervalUTxO "OutsideValidityIntervalUTxO"
          <|> con1 AlonzoRules.MaxTxSizeUTxO "MaxTxSizeUTxO"
          <|> con0 AlonzoRules.InputSetEmptyUTxO "InputSetEmptyUTxO"
          <|> con1 AlonzoRules.FeeTooSmallUTxO "FeeTooSmallUTxO"
          <|> con1 AlonzoRules.ValueNotConservedUTxO "ValueNotConservedUTxO"
          <|> con2 AlonzoRules.WrongNetwork "WrongNetwork"
          <|> con2 AlonzoRules.WrongNetworkWithdrawal "WrongNetworkWithdrawal"
          <|> con1 AlonzoRules.OutputTooSmallUTxO "OutputTooSmallUTxO"
          <|> con1 AlonzoRules.UtxosFailure "UtxosFailure"
          <|> con1 AlonzoRules.OutputBootAddrAttrsTooBig "OutputBootAddrAttrsTooBig"
          <|> con1 AlonzoRules.OutputTooBigUTxO "OutputTooBigUTxO"
          <|> con2 AlonzoRules.InsufficientCollateral "InsufficientCollateral"
          <|> con1 AlonzoRules.ScriptsNotPaidUTxO "ScriptsNotPaidUTxO"
          <|> con1 AlonzoRules.ExUnitsTooBigUTxO "ExUnitsTooBigUTxO"
          <|> con1 AlonzoRules.CollateralContainsNonADA "CollateralContainsNonADA"
          <|> con1 AlonzoRules.WrongNetworkInTxBody "WrongNetworkInTxBody"
          <|> con1 AlonzoRules.OutsideForecast "OutsideForecast"
          <|> con1 AlonzoRules.TooManyCollateralInputs "TooManyCollateralInputs"
          <|> con0 AlonzoRules.NoCollateralInputs "NoCollateralInputs"

instance (Read (PlutusPurpose AsItem era), Read (PlutusPurpose AsIx era), Read (PredicateFailure (EraRule "UTXO" era))) => Read (AlonzoRules.AlonzoUtxowPredFailure era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 AlonzoRules.ShelleyInAlonzoUtxowPredFailure "ShelleyInAlonzoUtxowPredFailure"
          <|> con1 AlonzoRules.MissingRedeemers "MissingRedeemers"
          <|> con2 AlonzoRules.MissingRequiredDatums "MissingRequiredDatums"
          <|> con2 AlonzoRules.NotAllowedSupplementalDatums "NotAllowedSupplementalDatums"
          <|> con1 AlonzoRules.PPViewHashesDontMatch "PPViewHashesDontMatch"
          <|> con1 AlonzoRules.UnspendableUTxONoDatumHash "UnspendableUTxONoDatumHash"
          <|> con2 AlonzoRules.ScriptIntegrityHashMismatch "ScriptIntegrityHashMismatch"
          <|> con1 AlonzoRules.ExtraRedeemers "ExtraRedeemers"

instance (Read (EraRuleFailure "PPUP" era), Read (PlutusPurpose AsItem era), Read (ContextError era)) => Read (AlonzoRules.AlonzoUtxosPredFailure era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con2 AlonzoRules.ValidationTagMismatch "ValidationTagMismatch"
          <|> con1 AlonzoRules.CollectErrors "CollectErrors"
          <|> con1 AlonzoRules.UpdateFailure "UpdateFailure"

instance (Read (PredicateFailure (EraRule "UTXOS" era)), Read (Ledger.Value era), Read (TxOut era)) => Read (BabbageRules.BabbageUtxoPredFailure era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 BabbageRules.AlonzoInBabbageUtxoPredFailure "AlonzoInBabbageUtxoPredFailure"
          <|> con2 BabbageRules.IncorrectTotalCollateralField "IncorrectTotalCollateralField"
          <|> con1 BabbageRules.BabbageOutputTooSmallUTxO "BabbageOutputTooSmallUTxO"
          <|> con1 BabbageRules.BabbageNonDisjointRefInputs "BabbageNonDisjointRefInputs"

instance (Read (PredicateFailure (EraRule "UTXO" era)), Read (PlutusPurpose AsItem era), Read (PlutusPurpose AsIx era)) => Read (BabbageRules.BabbageUtxowPredFailure era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 BabbageRules.AlonzoInBabbageUtxowPredFailure "AlonzoInBabbageUtxowPredFailure"
          <|> con1 BabbageRules.UtxoFailure "UtxoFailure"
          <|> con1 BabbageRules.MalformedScriptWitnesses "MalformedScriptWitnesses"
          <|> con1 BabbageRules.MalformedReferenceScripts "MalformedReferenceScripts"
          <|> con2 BabbageRules.ScriptIntegrityHashMismatch "ScriptIntegrityHashMismatch"

instance (Read (PredicateFailure (EraRule "UTXOS" era)), Read (Ledger.Value era), Read (TxOut era)) => Read (ConwayRules.ConwayUtxoPredFailure era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 ConwayRules.UtxosFailure "UtxosFailure"
          <|> con1 ConwayRules.BadInputsUTxO "BadInputsUTxO"
          <|> con2 ConwayRules.OutsideValidityIntervalUTxO "OutsideValidityIntervalUTxO"
          <|> con1 ConwayRules.MaxTxSizeUTxO "MaxTxSizeUTxO"
          <|> con0 ConwayRules.InputSetEmptyUTxO "InputSetEmptyUTxO"
          <|> con1 ConwayRules.FeeTooSmallUTxO "FeeTooSmallUTxO"
          <|> con1 ConwayRules.ValueNotConservedUTxO "ValueNotConservedUTxO"
          <|> con2 ConwayRules.WrongNetwork "WrongNetwork"
          <|> con2 ConwayRules.WrongNetworkWithdrawal "WrongNetworkWithdrawal"
          <|> con1 ConwayRules.OutputTooSmallUTxO "OutputTooSmallUTxO"
          <|> con1 ConwayRules.OutputBootAddrAttrsTooBig "OutputBootAddrAttrsTooBig"
          <|> con1 ConwayRules.OutputTooBigUTxO "OutputTooBigUTxO"
          <|> con2 ConwayRules.InsufficientCollateral "InsufficientCollateral"
          <|> con1 ConwayRules.ScriptsNotPaidUTxO "ScriptsNotPaidUTxO"
          <|> con1 ConwayRules.ExUnitsTooBigUTxO "ExUnitsTooBigUTxO"
          <|> con1 ConwayRules.CollateralContainsNonADA "CollateralContainsNonADA"
          <|> con1 ConwayRules.WrongNetworkInTxBody "WrongNetworkInTxBody"
          <|> con1 ConwayRules.OutsideForecast "OutsideForecast"
          <|> con1 ConwayRules.TooManyCollateralInputs "TooManyCollateralInputs"
          <|> con0 ConwayRules.NoCollateralInputs "NoCollateralInputs"
          <|> con2 ConwayRules.IncorrectTotalCollateralField "IncorrectTotalCollateralField"
          <|> con1 ConwayRules.BabbageOutputTooSmallUTxO "BabbageOutputTooSmallUTxO"
          <|> con1 ConwayRules.BabbageNonDisjointRefInputs "BabbageNonDisjointRefInputs"

instance (Read (PredicateFailure (EraRule "UTXO" era)), Read (PlutusPurpose AsItem era), Read (PlutusPurpose AsIx era)) => Read (ConwayRules.ConwayUtxowPredFailure era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        -- UtxoFailure contains era-specific type
        con1 ConwayRules.UtxoFailure "UtxoFailure"
          <|> con1 ConwayRules.InvalidWitnessesUTXOW "InvalidWitnessesUTXOW"
          <|> con1 ConwayRules.MissingVKeyWitnessesUTXOW "MissingVKeyWitnessesUTXOW"
          <|> con1 ConwayRules.MissingScriptWitnessesUTXOW "MissingScriptWitnessesUTXOW"
          <|> con1 ConwayRules.ScriptWitnessNotValidatingUTXOW "ScriptWitnessNotValidatingUTXOW"
          <|> con1 ConwayRules.MissingTxBodyMetadataHash "MissingTxBodyMetadataHash"
          <|> con1 ConwayRules.MissingTxMetadata "MissingTxMetadata"
          <|> con1 ConwayRules.ConflictingMetadataHash "ConflictingMetadataHash"
          <|> con0 ConwayRules.InvalidMetadata "InvalidMetadata"
          <|> con1 ConwayRules.ExtraneousScriptWitnessesUTXOW "ExtraneousScriptWitnessesUTXOW"
          <|> con1 ConwayRules.MissingRedeemers "MissingRedeemers"
          <|> con2 ConwayRules.MissingRequiredDatums "MissingRequiredDatums"
          <|> con2 ConwayRules.NotAllowedSupplementalDatums "NotAllowedSupplementalDatums"
          <|> con1 ConwayRules.PPViewHashesDontMatch "PPViewHashesDontMatch"
          <|> con1 ConwayRules.UnspendableUTxONoDatumHash "UnspendableUTxONoDatumHash"
          <|> con1 ConwayRules.ExtraRedeemers "ExtraRedeemers"
          <|> con1 ConwayRules.MalformedScriptWitnesses "MalformedScriptWitnesses"
          <|> con1 ConwayRules.MalformedReferenceScripts "MalformedReferenceScripts"
          <|> con2 ConwayRules.ScriptIntegrityHashMismatch "ScriptIntegrityHashMismatch"

instance (Read (PlutusPurpose AsItem era), Read (ContextError era)) => Read (ConwayRules.ConwayUtxosPredFailure era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con2 ConwayRules.ValidationTagMismatch "ValidationTagMismatch"
          <|> con1 ConwayRules.CollectErrors "CollectErrors"

-- Dijkstra era instances - commented out until cardano-ledger-dijkstra is available
-- instance Read (DijkstraUtxoRules.DijkstraUtxoPredFailure era) where
--   readsPrec _ = readP_to_S $ tryParens $
--     (DijkstraUtxoRules.UtxosFailure <$> con1 "UtxosFailure" readP)
--       <|> (DijkstraUtxoRules.BadInputsUTxO <$> con1 "BadInputsUTxO" readP)
--       <|> (DijkstraUtxoRules.OutsideValidityIntervalUTxO <$> con2 "OutsideValidityIntervalUTxO" readP readP)
--       <|> (DijkstraUtxoRules.MaxTxSizeUTxO <$> con1 "MaxTxSizeUTxO" readP)
--       <|> (ident "InputSetEmptyUTxO" *> pure DijkstraUtxoRules.InputSetEmptyUTxO)
--       <|> (DijkstraUtxoRules.FeeTooSmallUTxO <$> con1 "FeeTooSmallUTxO" readP)
--       <|> (DijkstraUtxoRules.ValueNotConservedUTxO <$> con1 "ValueNotConservedUTxO" readP)
--       <|> (DijkstraUtxoRules.WrongNetwork <$> con2 "WrongNetwork" readP readP)
--       <|> (DijkstraUtxoRules.WrongNetworkWithdrawal <$> con2 "WrongNetworkWithdrawal" readP readP)
--       <|> (DijkstraUtxoRules.OutputTooSmallUTxO <$> con1 "OutputTooSmallUTxO" readP)
--       <|> (DijkstraUtxoRules.OutputBootAddrAttrsTooBig <$> con1 "OutputBootAddrAttrsTooBig" readP)
--       <|> (DijkstraUtxoRules.OutputTooBigUTxO <$> con1 "OutputTooBigUTxO" readP)
--       <|> (DijkstraUtxoRules.InsufficientCollateral <$> con2 "InsufficientCollateral" readP readP)
--       <|> (DijkstraUtxoRules.ScriptsNotPaidUTxO <$> con1 "ScriptsNotPaidUTxO" readP)
--       <|> (DijkstraUtxoRules.ExUnitsTooBigUTxO <$> con1 "ExUnitsTooBigUTxO" readP)
--       <|> (DijkstraUtxoRules.CollateralContainsNonADA <$> con1 "CollateralContainsNonADA" readP)
--       <|> (DijkstraUtxoRules.WrongNetworkInTxBody <$> con1 "WrongNetworkInTxBody" readP)
--       <|> (DijkstraUtxoRules.OutsideForecast <$> con1 "OutsideForecast" readP)
--       <|> (DijkstraUtxoRules.TooManyCollateralInputs <$> con1 "TooManyCollateralInputs" readP)
--       <|> (ident "NoCollateralInputs" *> pure DijkstraUtxoRules.NoCollateralInputs)
--       <|> (DijkstraUtxoRules.IncorrectTotalCollateralField <$> con2 "IncorrectTotalCollateralField" readP readP)
--       <|> (DijkstraUtxoRules.BabbageOutputTooSmallUTxO <$> con1 "BabbageOutputTooSmallUTxO" readP)
--       <|> (DijkstraUtxoRules.BabbageNonDisjointRefInputs <$> con1 "BabbageNonDisjointRefInputs" readP)
--       <|> (DijkstraUtxoRules.PtrPresentInCollateralReturn <$> con1 "PtrPresentInCollateralReturn" readP)

-- instance Read (DijkstraUtxowRules.DijkstraUtxowPredFailure era) where
--   readsPrec _ = readP_to_S $ tryParens $
--     (DijkstraUtxowRules.UtxoFailure <$> con1 "UtxoFailure" readP)
--       <|> (DijkstraUtxowRules.InvalidWitnessesUTXOW <$> con1 "InvalidWitnessesUTXOW" readP)
--       <|> (DijkstraUtxowRules.MissingVKeyWitnessesUTXOW <$> con1 "MissingVKeyWitnessesUTXOW" readP)
--       <|> (DijkstraUtxowRules.MissingScriptWitnessesUTXOW <$> con1 "MissingScriptWitnessesUTXOW" readP)
--       <|> (DijkstraUtxowRules.ScriptWitnessNotValidatingUTXOW <$> con1 "ScriptWitnessNotValidatingUTXOW" readP)
--       <|> (DijkstraUtxowRules.MissingTxBodyMetadataHash <$> con1 "MissingTxBodyMetadataHash" readP)
--       <|> (DijkstraUtxowRules.MissingTxMetadata <$> con1 "MissingTxMetadata" readP)
--       <|> (DijkstraUtxowRules.ConflictingMetadataHash <$> con1 "ConflictingMetadataHash" readP)
--       <|> (ident "InvalidMetadata" *> pure DijkstraUtxowRules.InvalidMetadata)
--       <|> (DijkstraUtxowRules.ExtraneousScriptWitnessesUTXOW <$> con1 "ExtraneousScriptWitnessesUTXOW" readP)
--       <|> (DijkstraUtxowRules.MissingRedeemers <$> con1 "MissingRedeemers" readP)
--       <|> (DijkstraUtxowRules.MissingRequiredDatums <$> con2 "MissingRequiredDatums" readP readP)
--       <|> (DijkstraUtxowRules.NotAllowedSupplementalDatums <$> con2 "NotAllowedSupplementalDatums" readP readP)
--       <|> (DijkstraUtxowRules.PPViewHashesDontMatch <$> con1 "PPViewHashesDontMatch" readP)
--       <|> (DijkstraUtxowRules.UnspendableUTxONoDatumHash <$> con1 "UnspendableUTxONoDatumHash" readP)
--       <|> (DijkstraUtxowRules.ExtraRedeemers <$> con1 "ExtraRedeemers" readP)
--       <|> (DijkstraUtxowRules.MalformedScriptWitnesses <$> con1 "MalformedScriptWitnesses" readP)
--       <|> (DijkstraUtxowRules.MalformedReferenceScripts <$> con1 "MalformedReferenceScripts" readP)
--       <|> (DijkstraUtxowRules.ScriptIntegrityHashMismatch <$> con2 "ScriptIntegrityHashMismatch" readP readP)

instance Read (ShelleyLedgerPredFailure era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        (ident "UtxowFailure" *> skipValue *> pfail) -- No Read instance for PredicateFailure
        -- DelegsFailure requires era-specific type, skip and return stub UtxowFailure
          <|> (ident "DelegsFailure" *> skipValue *> pfail)

instance (Read (PredicateFailure (EraRule "UTXOW" era))) => Read (ConwayLedgerPredFailure era) where
  readsPrec _ =
    readP_to_S $
      tryParens $
        con1 ConwayUtxowFailure "ConwayUtxowFailure"
          -- ConwayCertsFailure and ConwayGovFailure require era-specific types, skip
          <|> (ident "ConwayCertsFailure" *> skipValue *> pfail)
          <|> (ident "ConwayGovFailure" *> skipValue *> pfail)
          <|> con1 ConwayWdrlNotDelegatedToDRep "ConwayWdrlNotDelegatedToDRep"
          <|> con1 ConwayTreasuryValueMismatch "ConwayTreasuryValueMismatch"
          <|> con1 ConwayTxRefScriptsSizeTooBig "ConwayTxRefScriptsSizeTooBig"
          <|> con1 ConwayMempoolFailure "ConwayMempoolFailure"

-- Dijkstra era instance - commented out until cardano-ledger-dijkstra is available
-- instance Read (DijkstraLedgerPredFailure era) where
--   readsPrec _ = readP_to_S $ tryParens $
--     (DijkstraUtxowFailure <$> con1 "DijkstraUtxowFailure" readP)
--       <|> (ident "DijkstraCertsFailure" *> skipValue *> pure (DijkstraCertsFailure stubConwayCertsPredFailure))
--       <|> (ident "DijkstraGovFailure" *> skipValue *> pure (DijkstraGovFailure stubConwayGovPredFailure))
--       <|> (DijkstraWdrlNotDelegatedToDRep <$> con1 "DijkstraWdrlNotDelegatedToDRep" readP)
--       <|> (DijkstraTreasuryValueMismatch <$> con1 "DijkstraTreasuryValueMismatch" readP)
--       <|> (DijkstraTxRefScriptsSizeTooBig <$> con1 "DijkstraTxRefScriptsSizeTooBig" readP)
--       <|> (DijkstraWithdrawalsMissingAccounts <$> con1 "DijkstraWithdrawalsMissingAccounts" readP)
--       <|> (DijkstraIncompleteWithdrawals <$> con1 "DijkstraIncompleteWithdrawals" readP)
--       <|> (ident "DijkstraSubLedgersFailure" *> skipValue *> pure (DijkstraSubLedgersFailure stubSubLedgersFailure))
