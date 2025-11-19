{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Convex.Aiken.Test where

import Cardano.Api qualified as C
import Convex.Aiken.Blueprint (Blueprint (..))
import Convex.Aiken.Blueprint qualified as Blueprint
import Data.Bifunctor (first)
import Data.Functor (void)
import Data.Map qualified as Map
import Paths_convex_base qualified as Pkg
import Test.Tasty.HUnit (assertEqual)

unit_load_blueprint :: IO ()
unit_load_blueprint = do
  void loadExample

unit_deserialise_script :: IO ()
unit_deserialise_script = do
  Blueprint{validators} <- loadExample
  maybe (fail "Expected script named 'transfer.issue.withdraw'") pure (Map.lookup "transfer.issue.withdraw" validators)
    >>= \case
      (C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3) script) -> do
        let hsh = C.hashScript script
        assertEqual
          "Script hash"
          (first (const ()) $ C.deserialiseFromRawBytesHex "0f8107a024cfbc7e5e787d67acddcec748ceb280fcc4b14c305e6a2d")
          (Right hsh)
      _ -> fail "Unexpected script language"

loadExample :: IO Blueprint
loadExample = do
  Pkg.getDataFileName "data/aiken-scripts.json"
    >>= Blueprint.loadFromFile
    >>= either fail pure
