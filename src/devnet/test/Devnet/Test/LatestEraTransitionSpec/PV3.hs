{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | PV3 test scripts constructed directly with UPLC (no PlutusTx.compile needed)
module Devnet.Test.LatestEraTransitionSpec.PV3 (
  readBitTestMintingPolicyScriptPV3,
  writeBitTestMintingPolicyScriptPV3,
) where

import Cardano.Api qualified as C
import Data.ByteString (ByteString)
import PlutusCore qualified as PLC
import PlutusCore.MkPlc qualified as PLC
import PlutusCore.Version qualified as PLC
import PlutusLedgerApi.Common (serialiseUPLC)
import UntypedPlutusCore qualified as UPLC

{- | A V3 minting policy that reads bit 2 from the input bytestring.
Returns unit if the bit is set, errors otherwise.
V3 scripts take only 1 argument (the script context as BuiltinData)
Script structure: \d -> \c -> ifThenElse (readBit d 2) () error
-}
readBitTestMintingPolicyScriptPV3 :: ByteString -> C.PlutusScript C.PlutusScriptV3
readBitTestMintingPolicyScriptPV3 inputBs =
  C.PlutusScriptSerialised $ serialiseUPLC $ UPLC.Program () PLC.plcVersion110 body
 where
  body =
    UPLC.Apply
      ()
      script
      (PLC.mkConstant () inputBs)

  -- \d -> \c -> ... (V3 scripts take 1 arg: context)
  script =
    UPLC.LamAbs () (UPLC.DeBruijn 0) $ -- \d (the bytestring parameter)
      UPLC.LamAbs () (UPLC.DeBruijn 0) $ -- \c (context, ignored)
      -- ifThenElse (readBit d 2) () error
      -- d is at de Bruijn index 2 (we're 2 lambdas deep)
        UPLC.Force
          ()
          ( PLC.mkIterAppNoAnn
              (UPLC.Force () (UPLC.Builtin () PLC.IfThenElse)) -- Force to instantiate type variable
              [ -- condition: readBit d 2
                PLC.mkIterAppNoAnn
                  (UPLC.Builtin () PLC.ReadBit)
                  [ UPLC.Var () (UPLC.DeBruijn 2) -- d (the bytestring)
                  , PLC.mkConstant () (2 :: Integer) -- bit index
                  ]
              , -- then: unit
                UPLC.Delay () (PLC.mkConstant () ())
              , -- else: error
                UPLC.Delay () (UPLC.Error ())
              ]
          )

{- | A V3 minting policy that writes bit 0 to False in the input bytestring.
Always returns unit (the writeBits result is forced but discarded).
Script structure: \d -> \c -> seq (writeBits d [0] [False]) ()
-}
writeBitTestMintingPolicyScriptPV3 :: ByteString -> C.PlutusScript C.PlutusScriptV3
writeBitTestMintingPolicyScriptPV3 inputBs =
  C.PlutusScriptSerialised $ serialiseUPLC $ UPLC.Program () PLC.plcVersion110 body
 where
  body =
    UPLC.Apply
      ()
      script
      (PLC.mkConstant () inputBs)

  -- \d -> \c -> ...
  script =
    UPLC.LamAbs () (UPLC.DeBruijn 0) $ -- \d
      UPLC.LamAbs () (UPLC.DeBruijn 0) $ -- \c (ignored)
      -- Force evaluation of writeBits, then return unit
      -- Using: (\x -> ()) (writeBits d [0] [False])
        UPLC.Apply
          ()
          (UPLC.LamAbs () (UPLC.DeBruijn 0) (PLC.mkConstant () ()))
          ( PLC.mkIterAppNoAnn
              (UPLC.Builtin () PLC.WriteBits)
              [ UPLC.Var () (UPLC.DeBruijn 2) -- d (the bytestring)
              , PLC.mkConstant () [0 :: Integer] -- indices to write
              , PLC.mkConstant () False -- value to write at all indices
              ]
          )
