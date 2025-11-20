{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Off-chain code for aiken blueprints
module Convex.Aiken.Offchain (
  Cip143Blueprint (..),
  blueprintKeys,
  lookupScripts,
  lookupScripts_,
  extractV3Scripts_,

  -- * Error types
  AsLookupScriptFailure (..),
  LookupScriptFailure (..),
)
where

import Cardano.Api (ScriptInAnyLang)
import Cardano.Api qualified as C
import Control.Lens (review)
import Control.Monad.Except (MonadError (..))
import Convex.Aiken.Blueprint (Blueprint (..))
import Convex.Aiken.Blueprint qualified as Blueprint
import Convex.Aiken.BlueprintKey (BlueprintKey)
import Convex.Aiken.Error (AsBlueprintError (..), AsLookupScriptFailure (..), LookupScriptFailure (..))
import Data.Map qualified as Map
import PlutusLedgerApi.V3 (CurrencySymbol)

data Cip143Blueprint v
  = Cip143Blueprint
  { cbTransfer :: v
  , cbIssuance :: v
  , cbGlobalStateCS :: Maybe CurrencySymbol
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

blueprintKeys :: Cip143Blueprint BlueprintKey
blueprintKeys =
  Cip143Blueprint
    { cbTransfer = "transfer.transfer.withdraw"
    , cbIssuance = "transfer.issue.withdraw"
    , cbGlobalStateCS = Nothing
    }

-- | Lookup the scripts that are referenced in the CIP 143 blueprint
lookupScripts :: Blueprint -> Cip143Blueprint BlueprintKey -> Either LookupScriptFailure (Cip143Blueprint ScriptInAnyLang)
lookupScripts Blueprint{validators} b@Cip143Blueprint{cbTransfer, cbIssuance} = do
  tr <- maybe (Left $ FailedToFindTransferScript cbTransfer) Right (Map.lookup cbTransfer validators)
  i <- maybe (Left $ FailedToFindIssuanceScript cbIssuance) Right (Map.lookup cbIssuance validators)
  pure $ b{cbIssuance = i, cbTransfer = tr}

-- | Lookup the scripts that are referenced in the CIP 143 blueprint
lookupScripts_ :: (MonadError err m, AsLookupScriptFailure err) => Blueprint -> Cip143Blueprint BlueprintKey -> m (Cip143Blueprint ScriptInAnyLang)
lookupScripts_ bp =
  either (throwError . review _LookupScriptFailure) pure
    . lookupScripts bp

getPlutus :: C.Script C.PlutusScriptV3 -> C.PlutusScript C.PlutusScriptV3
getPlutus = \case
  C.PlutusScript C.PlutusScriptV3 script -> script

extractV3Scripts_ :: (MonadError err m, AsBlueprintError err) => Cip143Blueprint ScriptInAnyLang -> m (Cip143Blueprint (C.PlutusScript C.PlutusScriptV3))
extractV3Scripts_ = traverse (fmap getPlutus . Blueprint.getPlutusV3)
