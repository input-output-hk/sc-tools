{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Reading plutus scripts from blueprint JSON files
module Convex.Aiken.Blueprint (
  BlueprintScriptVersion (..),
  Blueprint (..),
  BlueprintValidator (..),
  Preamble (..),
  loadFromFile,
  loadFromFile_,
  deserialise,
  getPlutusV3,

  -- * Etc.
  unsafeDeserialiseScript,
  fromCardanoApiScriptToProgram,
  fromProgramToCardanoApiScript,
)
where

import Cardano.Api (AnyPlutusScriptVersion, ScriptInAnyLang)
import Cardano.Api.Plutus qualified as C
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Convex.Aiken.BlueprintKey (BlueprintKey)
import Convex.Aiken.Error (AsBlueprintError (..))
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BSS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import PlutusCore qualified as PLC
import PlutusLedgerApi.Common (
  MajorProtocolVersion (..),
  PlutusLedgerLanguage (..),
  ScriptForEvaluation,
  ScriptNamedDeBruijn (ScriptNamedDeBruijn),
  SerialisedScript,
  deserialisedScript,
  serialiseUPLC,
 )
import PlutusLedgerApi.Common qualified as PlutusLedgerApi
import PlutusPrelude (over)
import UntypedPlutusCore qualified as UPLC

-- | Plutus script version with a blueprint-specific JSON encoding
newtype BlueprintScriptVersion = BlueprintScriptVersion AnyPlutusScriptVersion
  deriving stock (Eq, Show)

instance ToJSON BlueprintScriptVersion where
  toJSON (BlueprintScriptVersion (C.AnyPlutusScriptVersion k)) = case k of
    C.PlutusScriptV1 -> toJSON @String "v1"
    C.PlutusScriptV2 -> toJSON @String "v2"
    C.PlutusScriptV3 -> toJSON @String "v3"
    C.PlutusScriptV4 -> toJSON @String "v4"

instance FromJSON BlueprintScriptVersion where
  parseJSON = fmap (fmap BlueprintScriptVersion) $ Aeson.withText "BlueprintScriptVersion" $ \x -> case T.unpack x of
    "v1" -> pure (C.AnyPlutusScriptVersion C.PlutusScriptV1)
    "v2" -> pure (C.AnyPlutusScriptVersion C.PlutusScriptV2)
    "v3" -> pure (C.AnyPlutusScriptVersion C.PlutusScriptV3)
    "v4" -> pure (C.AnyPlutusScriptVersion C.PlutusScriptV4)
    v -> fail $ "Unexpected plutus script version: " <> v

data Blueprint = Blueprint
  { preamble :: Preamble
  , validators :: Map BlueprintKey ScriptInAnyLang
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Blueprint where
  parseJSON = withObject "Blueprint" $ \obj ->
    let mkb p v = Blueprint p <$> deserialise p v
     in ( mkb
            <$> obj .: "preamble"
            <*> obj .: "validators"
        )
          >>= either fail pure

data BlueprintValidator = BlueprintValidator
  { title :: BlueprintKey
  , compiledCode :: Text
  , hash :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Preamble = Preamble
  { description :: Text
  , plutusVersion :: BlueprintScriptVersion
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Preamble where
  parseJSON = withObject "Preamble" $ \obj ->
    Preamble
      <$> obj .: "description"
      <*> obj .: "plutusVersion"

loadFromFile :: FilePath -> IO (Either String Blueprint)
loadFromFile fp = Aeson.eitherDecode . BSL.fromStrict <$> BS.readFile fp

loadFromFile_ :: (MonadIO m, MonadError err m, AsBlueprintError err) => FilePath -> m Blueprint
loadFromFile_ fp =
  liftIO (loadFromFile fp) >>= either (throwing _BlueprintJsonError) pure

deserialise :: Preamble -> [BlueprintValidator] -> Either String (Map BlueprintKey ScriptInAnyLang)
deserialise Preamble{plutusVersion = BlueprintScriptVersion v} validators =
  Map.fromList <$> traverse (deserialiseScript v) validators

deserialiseScript :: AnyPlutusScriptVersion -> BlueprintValidator -> Either String (BlueprintKey, ScriptInAnyLang)
deserialiseScript (C.AnyPlutusScriptVersion v) BlueprintValidator{title, compiledCode} =
  let lng = C.PlutusScriptLanguage v
   in fmap ((title,) . C.ScriptInAnyLang lng) (deserialisePlutus v compiledCode)

deserialisePlutus :: forall lang. (C.IsPlutusScriptLanguage lang) => C.PlutusScriptVersion lang -> Text -> Either String (C.Script lang)
deserialisePlutus scriptVersion text =
  -- first show $ C.deserialiseFromCBOR (C.proxyToAsType $ Proxy @(C.Script lang))
  let bs = Base16.decodeLenient $ TE.encodeUtf8 text
   in -- lang = case scriptVersion of
      --   C.PlutusScriptV1 -> PlutusV1
      --   C.PlutusScriptV2 -> PlutusV2
      --   C.PlutusScriptV3 -> PlutusV3
      -- in fmap _ (unsafeDeserialiseScript lang (BSS.toShort bs))
      Right $ C.PlutusScript scriptVersion (C.PlutusScriptSerialised $ BSS.toShort bs)

-- | Extract the Plutus V3 script, fail if the script is not V3
getPlutusV3 :: (MonadError err m, AsBlueprintError err) => ScriptInAnyLang -> m (C.Script C.PlutusScriptV3)
getPlutusV3 (C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3) script) = pure script
getPlutusV3 (C.ScriptInAnyLang otherLang _) =
  throwing _UnexpectedPlutusVersionError (C.AnyPlutusScriptVersion C.PlutusScriptV3, C.AnyScriptLanguage otherLang)

unsafeDeserialiseScript
  :: PlutusLedgerLanguage
  -- ^ the Plutus ledger language of the script.
  -> SerialisedScript
  -- ^ the script to deserialise.
  -> Either String ScriptForEvaluation
unsafeDeserialiseScript ll sScript = do
  let majorProtocolVersion = if ll == PlutusV3 then MajorProtocolVersion 10 else MajorProtocolVersion 9
  case PlutusLedgerApi.deserialiseScript ll majorProtocolVersion sScript of
    Left err -> Left (show err)
    Right s -> Right s

type Script = UPLC.Program UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()

fromCardanoApiScriptToProgram :: C.PlutusScript C.PlutusScriptV3 -> Either String Script
fromCardanoApiScriptToProgram (C.PlutusScriptSerialised script) =
  case unsafeDeserialiseScript PlutusV3 script of
    Right (deserialisedScript -> ScriptNamedDeBruijn program) -> Right (toNameless program)
    Left err -> Left err

fromProgramToCardanoApiScript :: Script -> C.PlutusScript C.PlutusScriptV3
fromProgramToCardanoApiScript script =
  C.PlutusScriptSerialised $ serialiseUPLC script

toNameless
  :: UPLC.Program UPLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ()
  -> UPLC.Program UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()
toNameless = over UPLC.progTerm $ UPLC.termMapNames UPLC.unNameDeBruijn
