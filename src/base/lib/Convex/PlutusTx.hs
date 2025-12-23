{-# LANGUAGE TypeApplications #-}

-- | Functions for dealing with @plutus-tx@ scripts
module Convex.PlutusTx (
  compiledCodeToScript,

  -- * Applying parameters to scripts
  P.ScriptDecodeError (..),
  applyParams,
  unsafeApplyParamss,
) where

import Cardano.Api (PlutusScript)
import Cardano.Api qualified as C
import Control.Lens (over)
import PlutusCore qualified as PLC
import PlutusCore.MkPlc qualified as PLC
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.Common qualified as P
import PlutusTx.Code (CompiledCode)
import UntypedPlutusCore qualified as UPLC

-- | Get the 'PlutusScript' of a 'CompiledCode'
compiledCodeToScript :: CompiledCode a -> PlutusScript lang
compiledCodeToScript = C.PlutusScriptSerialised . serialiseCompiledCode

{- | Apply parameters to a serialised plutus script. The parameter type must be one of the
 Plutus builtin types, eg. @BuiltinInteger@, @BuiltinByteString@. You can use 'PlutusTx.toData'
 for types with a 'ToData' instance.
 May 'error' if the deserialisation of the script fails.
-}
unsafeApplyParams
  :: forall a
   . (PLC.Contains UPLC.DefaultUni a)
  => C.PlutusScript C.PlutusScriptV3
  -> [a]
  -> C.PlutusScript C.PlutusScriptV3
unsafeApplyParams script =
  either (error . show) id . applyParams script

{- | Apply parameters to a serialised plutus script. The parameter type must be one of the
 Plutus builtin types, eg. @BuiltinInteger@, @BuiltinByteString@. You can use 'PlutusTx.toData'
 for types with a 'ToData' instance.
-}
applyParams
  :: forall a
   . (PLC.Contains UPLC.DefaultUni a)
  => C.PlutusScript C.PlutusScriptV3
  -> [a]
  -> Either P.ScriptDecodeError (C.PlutusScript C.PlutusScriptV3)
applyParams (C.PlutusScriptSerialised script) params = do
  P.ScriptNamedDeBruijn program <- P.deserialisedScript <$> deserialiseScript P.PlutusV3 script
  pure (C.PlutusScriptSerialised $ P.serialiseUPLC $ toNameless $ program `applyArguments` params)

applyArguments
  :: (PLC.Contains uni a)
  => UPLC.Program name uni fun ()
  -> [a]
  -> UPLC.Program name uni fun ()
applyArguments p args =
  let termArgs = PLC.mkConstant () <$> args
      applied t = PLC.mkIterAppNoAnn t termArgs
   in over UPLC.progTerm applied p

{- | The deserialization from a serialised script into a `ScriptForEvaluation`,
ready to be evaluated on-chain.
Called inside phase-1 validation (i.e., deserialisation error is a phase-1 error).
-}
deserialiseScript
  :: P.PlutusLedgerLanguage
  -- ^ the Plutus ledger language of the script.
  -> P.SerialisedScript
  -- ^ the script to deserialise.
  -> Either P.ScriptDecodeError P.ScriptForEvaluation
deserialiseScript ll sScript =
  let majorProtocolVersion = P.ledgerLanguageIntroducedIn ll
   in P.deserialiseScript ll majorProtocolVersion sScript

toNameless
  :: UPLC.Program UPLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ()
  -> UPLC.Program UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()
toNameless = over UPLC.progTerm $ UPLC.termMapNames UPLC.unNameDeBruijn
