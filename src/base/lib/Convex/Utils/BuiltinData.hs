{- | Working with 'PlutusTx.BuiltinData'. This is useful for implementing 'PlutusTx.ToData'
and 'PlutusTx.FromData' instances.
-}
module Convex.Utils.BuiltinData (
  -- * Matching on data
  getConstr,
  getB,
  getI,
  getL,

  -- * Serialising @cardano-api@ types
  deserialiseHash,
  serialiseHash,
  deserialiseScriptHash,
  serialiseScriptHash,
  serialiseAssetId,
  deserialiseAssetId,
  serialiseTxIn,
  deserialiseTxIn,

  -- * Conveniences
  fromBuiltinDataHex,
  fromCborHex,
) where

import Cardano.Api qualified as C
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Enc
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx

getConstr :: PlutusTx.BuiltinData -> Maybe (Integer, [PlutusTx.BuiltinData])
getConstr dt =
  PlutusTx.matchData
    dt
    (\a b -> Just (a, b))
    (const Nothing)
    (const Nothing)
    (const Nothing)
    (const Nothing)

getB :: PlutusTx.BuiltinData -> Maybe PlutusTx.BuiltinByteString
getB dt =
  PlutusTx.matchData
    dt
    (\_ _ -> Nothing)
    (const Nothing)
    (const Nothing)
    (const Nothing)
    Just

getI :: PlutusTx.BuiltinData -> Maybe Integer
getI dt =
  PlutusTx.matchData
    dt
    (\_ _ -> Nothing)
    (const Nothing)
    (const Nothing)
    Just
    (const Nothing)

getL :: PlutusTx.BuiltinData -> Maybe [PlutusTx.BuiltinData]
getL dt =
  PlutusTx.matchData
    dt
    (\_ _ -> Nothing)
    (const Nothing)
    Just
    (const Nothing)
    (const Nothing)

deserialiseHash :: forall h. (C.SerialiseAsRawBytes (C.Hash h)) => PlutusTx.BuiltinByteString -> Maybe (C.Hash h)
deserialiseHash bs =
  case C.deserialiseFromRawBytes (C.proxyToAsType $ Proxy @(C.Hash h)) (PlutusTx.fromBuiltin bs) of
    Left{} -> Nothing
    Right k -> Just k

serialiseHash :: (C.SerialiseAsRawBytes (C.Hash h)) => C.Hash h -> PlutusTx.BuiltinData
serialiseHash = PlutusTx.mkB . PlutusTx.toBuiltin . C.serialiseToRawBytes

deserialiseScriptHash :: PlutusTx.BuiltinByteString -> Maybe C.ScriptHash
deserialiseScriptHash bs =
  case C.deserialiseFromRawBytes (C.proxyToAsType $ Proxy @C.ScriptHash) (PlutusTx.fromBuiltin bs) of
    Left{} -> Nothing
    Right k -> Just k

serialiseScriptHash :: C.ScriptHash -> PlutusTx.BuiltinData
serialiseScriptHash = PlutusTx.mkB . PlutusTx.toBuiltin . C.serialiseToRawBytes

-- | Encode an 'C.AssetId' as data, using a 2-element list of bytestrings
serialiseAssetId :: C.AssetId -> PlutusTx.BuiltinData
serialiseAssetId = \case
  C.AdaAssetId ->
    PlutusTx.mkList [PlutusTx.mkB mempty, PlutusTx.mkB mempty]
  C.AssetId policyId assetName ->
    PlutusTx.mkList
      [ PlutusTx.mkB (PlutusTx.toBuiltin $ C.serialiseToRawBytes policyId)
      , PlutusTx.mkB (PlutusTx.toBuiltin $ C.serialiseToRawBytes assetName)
      ]

-- | Decode an 'C.AssetId' from data, expecting a 2-element list of bytestrings
deserialiseAssetId :: PlutusTx.BuiltinData -> Maybe C.AssetId
deserialiseAssetId k =
  getL k >>= \case
    [getB -> Just p, getB -> Just a]
      | BS.null (PlutusTx.fromBuiltin p) && BS.null (PlutusTx.fromBuiltin a) -> Just C.AdaAssetId
      | otherwise ->
          C.AssetId
            <$> either (const Nothing) Just (C.deserialiseFromRawBytes C.AsPolicyId (PlutusTx.fromBuiltin p))
            <*> either (const Nothing) Just (C.deserialiseFromRawBytes C.AsAssetName (PlutusTx.fromBuiltin a))
    _ -> Nothing

serialiseTxIn :: C.TxIn -> PlutusTx.BuiltinData
serialiseTxIn (C.TxIn txId (C.TxIx idx)) =
  PlutusTx.mkConstr
    0
    [ PlutusTx.mkB $ PlutusTx.toBuiltin $ C.serialiseToRawBytes txId
    , PlutusTx.mkI $ PlutusTx.toBuiltin $ toInteger idx
    ]

deserialiseTxIn :: PlutusTx.BuiltinData -> Maybe C.TxIn
deserialiseTxIn k =
  getConstr k >>= \case
    (0, [getB -> Just txi, getI -> Just n]) ->
      C.TxIn
        <$> either (const Nothing) Just (C.deserialiseFromRawBytes C.asType (PlutusTx.fromBuiltin txi))
        <*> Just (C.TxIx $ fromIntegral n)
    _ -> Nothing

-- | Decode a hex-encoded serialised datum
fromBuiltinDataHex :: forall a. (PlutusTx.FromData a) => String -> Either String a
fromBuiltinDataHex hex = do
  datum <- fromCborHex hex
  let k = PlutusTx.fromBuiltinData @a $ PlutusTx.toBuiltinData $ PlutusTx.dataToBuiltinData $ C.toPlutusData datum
  maybe (Left "fromBuiltinDataHex: PlutusTx.fromBuiltinData failed") Right k

-- | Decode a hex-encoded serialised datum
fromCborHex :: forall a. (C.SerialiseAsCBOR a) => String -> Either String a
fromCborHex hex =
  either
    (Left . show)
    pure
    (C.deserialiseFromCBOR (C.proxyToAsType $ Proxy @a) (either error id $ Base16.decode $ Enc.encodeUtf8 $ Text.pack hex))
