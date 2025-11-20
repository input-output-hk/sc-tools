{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Convex.Aiken.Error (
  AsBlueprintError (..),
  BlueprintError (..),
  LookupScriptFailure (..),
  AsLookupScriptFailure (..),
) where

import Cardano.Api (AnyPlutusScriptVersion, AnyScriptLanguage)
import Control.Lens (makeClassyPrisms)
import Convex.Aiken.BlueprintKey (BlueprintKey)

data BlueprintError
  = -- | Failed to convert 'ScriptInAnyLang' to the target script version
    UnexpectedPlutusVersionError
      { expectedVersion :: AnyPlutusScriptVersion
      , actualVersion :: AnyScriptLanguage
      }
  | BlueprintJsonError String
  deriving stock (Show)

makeClassyPrisms ''BlueprintError

data LookupScriptFailure
  = FailedToFindTransferScript BlueprintKey
  | FailedToFindIssuanceScript BlueprintKey
  deriving stock (Eq, Show)

makeClassyPrisms ''LookupScriptFailure
