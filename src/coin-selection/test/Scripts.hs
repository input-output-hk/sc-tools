{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

#if __GLASGOW_HASKELL__ < 910
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fobject-code -fno-ignore-interface-pragmas -fno-omit-interface-pragmas -fplugin-opt PlutusTx.Plugin:target-version=1.1.0.0 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
#endif

-- | Scripts used for testing
module Scripts (
  v2SpendingScriptSerialised,
  v2SpendingScript,
  v2StakingScript,
  matchingIndexValidatorScript,
  matchingIndexMPScript,
  spendMatchingIndex,
  mintMatchingIndex,
)
where

import Cardano.Api qualified as C
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.Test.Examples (alwaysSucceedingNAryFunction)

#if __GLASGOW_HASKELL__ < 910
import Convex.BuildTx (MonadBuildTx)
import Convex.BuildTx qualified as BuildTx
import Convex.PlutusTx (compiledCodeToScript)
import Convex.Scripts (toHashableScriptData)
import Convex.Utils (inAlonzo)
import PlutusTx (BuiltinData, CompiledCode)
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinUnit)
import Scripts.MatchingIndex qualified as MatchingIndex
#else
#endif

v2SpendingScript :: C.PlutusScript C.PlutusScriptV2
v2SpendingScript = C.PlutusScriptSerialised $ alwaysSucceedingNAryFunction 3

v2SpendingScriptSerialised :: SerialisedScript
v2SpendingScriptSerialised = alwaysSucceedingNAryFunction 3

v2StakingScript :: C.PlutusScript C.PlutusScriptV2
v2StakingScript = C.PlutusScriptSerialised $ alwaysSucceedingNAryFunction 2

#if __GLASGOW_HASKELL__ < 910
matchingIndexValidatorCompiled :: CompiledCode (BuiltinData -> BuiltinUnit)
matchingIndexValidatorCompiled = $$(PlutusTx.compile [||MatchingIndex.matchingIndexValidatorScript||])

matchingIndexMPCompiled :: CompiledCode (BuiltinData -> BuiltinUnit)
matchingIndexMPCompiled = $$(PlutusTx.compile [||MatchingIndex.matchingIndexMPScript||])

{- | Script that passes if the input's index (in the list of transaction inputs)
  matches the number passed as the redeemer
-}
matchingIndexValidatorScript :: C.PlutusScript C.PlutusScriptV3
matchingIndexValidatorScript = compiledCodeToScript matchingIndexValidatorCompiled

matchingIndexMPScript :: C.PlutusScript C.PlutusScriptV3
matchingIndexMPScript = compiledCodeToScript matchingIndexMPCompiled

{- | Spend an output locked by 'matchingIndexValidatorScript', setting
the redeemer to the index of the input in the final transaction
-}
spendMatchingIndex :: forall era m. (C.IsAlonzoBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => C.TxIn -> m ()
spendMatchingIndex txi =
  let witness txBody =
        C.ScriptWitness C.ScriptWitnessForSpending $
          BuildTx.buildScriptWitness
            matchingIndexValidatorScript
            (C.ScriptDatumForTxIn $ Just $ toHashableScriptData ())
            (fromIntegral @Int @Integer $ BuildTx.findIndexSpending txi txBody)
   in BuildTx.setScriptsValid >> BuildTx.addInputWithTxBody txi witness

{- | Mint a token from the 'matchingIndexMPScript', setting
the redeemer to the index of its currency symbol in the final transaction mint
-}
mintMatchingIndex :: forall era m. (C.IsAlonzoBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era) => (MonadBuildTx era m) => C.PolicyId -> C.AssetName -> C.Quantity -> m ()
mintMatchingIndex policy assetName quantity =
  inAlonzo @era $
    let witness txBody =
          BuildTx.buildScriptWitness
            matchingIndexMPScript
            C.NoScriptDatumForMint
            (fromIntegral @Int @Integer $ BuildTx.findIndexMinted policy txBody)
     in BuildTx.setScriptsValid >> BuildTx.addMintWithTxBody policy assetName quantity witness
#else
-- Stubs for GHC 9.10+ where PlutusTx.Plugin is not available.
-- These should never be called as the tests using them are also CPP-guarded.
matchingIndexValidatorScript :: C.PlutusScript C.PlutusScriptV3
matchingIndexValidatorScript = error "matchingIndexValidatorScript: not available on GHC 9.10+"

matchingIndexMPScript :: C.PlutusScript C.PlutusScriptV3
matchingIndexMPScript = error "matchingIndexMPScript: not available on GHC 9.10+"

spendMatchingIndex :: forall m. C.TxIn -> m ()
spendMatchingIndex = error "spendMatchingIndex: not available on GHC 9.10+"

mintMatchingIndex :: forall m. C.PolicyId -> C.AssetName -> C.Quantity -> m ()
mintMatchingIndex = error "mintMatchingIndex: not available on GHC 9.10+"
#endif
