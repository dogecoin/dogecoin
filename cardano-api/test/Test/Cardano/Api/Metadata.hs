{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api.Metadata
  ( tests
  , genTxMetadata
  ) where

import           Cardano.Prelude

import           Cardano.Api

import           Data.Aeson (ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Hedgehog (Gen, Property, discover, property, (===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog.Group (fromGroup)


-- ----------------------------------------------------------------------------
-- Golden / unit tests
--

prop_golden_1 :: Property
prop_golden_1 = matchMetadata
                  "{\"0\": 1}"
                  (TxMetadata (Map.fromList [(0, TxMetaNumber 1)]))

prop_golden_2 :: Property
prop_golden_2 = matchMetadata
                  "{\"0\": \"deadbeef\"}"
                  (txMetadataSingleton 0 (TxMetaText "deadbeef"))

prop_golden_3 :: Property
prop_golden_3 = matchMetadata
                  "{\"0\": \"0xDEADBEEF\"}"
                  (txMetadataSingleton 0 (TxMetaText "0xDEADBEEF"))

prop_golden_4 :: Property
prop_golden_4 = matchMetadata
                  "{\"0\": \"0xdeadbeef\"}"
                  (txMetadataSingleton 0 (TxMetaBytes "\xde\xad\xbe\xef"))

prop_golden_5 :: Property
prop_golden_5 = matchMetadata
                  "{\"0\": [] }"
                  (txMetadataSingleton 0 (TxMetaList []))

prop_golden_6 :: Property
prop_golden_6 = matchMetadata
                  "{\"0\": [1, \"a\", \"0x42\"] }"
                  (txMetadataSingleton 0
                    (TxMetaList [TxMetaNumber 1
                                ,TxMetaText "a"
                                ,TxMetaBytes "\x42"]))

prop_golden_7 :: Property
prop_golden_7 = matchMetadata
                  "{\"0\": {} }"
                  (txMetadataSingleton 0 (TxMetaMap []))

prop_golden_8 :: Property
prop_golden_8 = matchMetadata
                  "{\"0\": { \"0x41\": \"0x42\", \"1\": 2, \"a\" : \"b\" }}"
                  (txMetadataSingleton 0
                    (TxMetaMap [(TxMetaBytes "\x41", TxMetaBytes "\x42")
                               ,(TxMetaNumber 1,     TxMetaNumber 2)
                               ,(TxMetaText  "a",    TxMetaText "b")]))

txMetadataSingleton :: Word64 -> TxMetadataValue -> TxMetadata
txMetadataSingleton n v = TxMetadata (Map.fromList [(n, v)])

matchMetadata :: ByteString -> TxMetadata -> Property
matchMetadata jsonStr metadata =
  Hedgehog.withTests 1 $ Hedgehog.property $ Hedgehog.test $
    case Aeson.decodeStrict' jsonStr of
      Nothing -> Hedgehog.failure
      Just json -> do
        Hedgehog.annotateShow json
        metadataFromJson TxMetadataJsonNoSchema json === Right metadata


-- ----------------------------------------------------------------------------
-- Round trip properties
--

-- | Any JSON (within the supported subset) can be converted to tx metadata and
-- back, to give the same original JSON.
--
-- This uses the \"no schema\" mapping. Note that with this mapping it is /not/
-- the case that any tx metadata can be converted to JSON and back to give the
-- original value.
--
prop_noschema_json_roundtrip_via_metadata :: Property
prop_noschema_json_roundtrip_via_metadata = Hedgehog.property $ do
    json <- Hedgehog.forAll (genJsonForTxMetadata TxMetadataJsonNoSchema)
    Right json === (fmap (metadataToJson   TxMetadataJsonNoSchema)
                        . metadataFromJson TxMetadataJsonNoSchema) json

-- | Any JSON (fitting the detailed schema) can be converted to tx metadata and
-- back, to give the same original JSON.
--
prop_schema_json_roundtrip_via_metadata :: Property
prop_schema_json_roundtrip_via_metadata = Hedgehog.property $ do
    json <- Hedgehog.forAll (genJsonForTxMetadata TxMetadataJsonDetailedSchema)
    Right json === (fmap (metadataToJson   TxMetadataJsonDetailedSchema)
                        . metadataFromJson TxMetadataJsonDetailedSchema) json


-- | Any tx metadata can be converted to JSON (using the detailed schema) and
-- back, to give the same original tx metadata.
--
prop_metadata_roundtrip_via_schema_json :: Property
prop_metadata_roundtrip_via_schema_json = Hedgehog.property $ do
    md <- Hedgehog.forAll genTxMetadata
    Right md === (metadataFromJson TxMetadataJsonDetailedSchema
                . metadataToJson   TxMetadataJsonDetailedSchema) md


-- ----------------------------------------------------------------------------
-- Generators
--

genJsonForTxMetadata :: TxMetadataJsonSchema -> Gen Aeson.Value
genJsonForTxMetadata mapping =
    Gen.sized $ \sz ->
      Aeson.object <$>
      Gen.list (Range.linear 0 (fromIntegral sz))
               ((,) <$> (Text.pack . show <$> Gen.word64 Range.constantBounded)
                    <*> genJsonForTxMetadataValue mapping)

genJsonForTxMetadataValue :: TxMetadataJsonSchema -> Gen Aeson.Value
genJsonForTxMetadataValue TxMetadataJsonNoSchema = genJsonValue
  where
    genJsonValue :: Gen Aeson.Value
    genJsonValue =
      Gen.sized $ \sz ->
        Gen.frequency
          [ (1,         Aeson.toJSON <$> genJsonNumber)
          , (2,         Aeson.toJSON <$> genJsonText)
          , (fromIntegral (signum sz),
                        Aeson.toJSON <$> Gen.scale (`div` 2) genJsonList)
          , (fromIntegral (signum sz),
                        Aeson.object <$> Gen.scale (`div` 2) genJsonMap)
          ]

    genJsonNumber :: Gen Integer
    genJsonNumber = Gen.integral
                      (Range.linear
                        (-fromIntegral (maxBound :: Word64) :: Integer)
                        ( fromIntegral (maxBound :: Word64) :: Integer))

    genJsonText  :: Gen Text
    genJsonText = Gen.choice
                    [ Gen.ensure validText (genText 64)
                    , Gen.ensure validText ((bytesPrefix <>) <$> genText 62)
                    , genBytes
                    , Text.pack . show <$> genJsonNumber
                    ]
      where
        validText t = BS.length (Text.encodeUtf8 t) <= 64
        bytesPrefix = "0x"
        genText sz  = Text.pack <$> Gen.list (Range.linear 0 sz) Gen.alphaNum
        genBytes    = (bytesPrefix <>)
                    . Text.decodeUtf8
                    . Base16.encode
                    . BS.pack
                  <$> Gen.list (Range.linear 0 64)
                               (Gen.word8 Range.constantBounded)

    genJsonList :: Gen [Aeson.Value]
    genJsonList = Gen.sized $ \sz ->
                    Gen.list (Range.linear 0 (fromIntegral sz)) genJsonValue

    genJsonMap :: Gen [(Text, Aeson.Value)]
    genJsonMap = Gen.sized $ \sz ->
                   Gen.list (Range.linear 0 (fromIntegral sz)) $
                     (,) <$> genJsonText <*> genJsonValue


genJsonForTxMetadataValue TxMetadataJsonDetailedSchema = genJsonValue
  where
    genJsonValue :: Gen Aeson.Value
    genJsonValue =
      Gen.sized $ \sz ->
        Gen.frequency
          [ (1,         singleFieldObject "int"    <$> genJsonNumber)
          , (1,         singleFieldObject "bytes"  <$> genJsonBytes)
          , (1,         singleFieldObject "string" <$> genJsonText)
          , (fromIntegral (signum sz),
                        singleFieldObject "list" <$>
                          Gen.scale (`div` 2) genJsonList)
          , (fromIntegral (signum sz),
                        singleFieldObject "map" <$>
                          Gen.scale (`div` 2) genJsonMap)
          ]

    singleFieldObject name v = Aeson.object [(name, v)]

    genJsonNumber :: Gen Aeson.Value
    genJsonNumber = toJSON <$>
                    Gen.integral
                      (Range.linear
                        (-fromIntegral (maxBound :: Word64) :: Integer)
                        ( fromIntegral (maxBound :: Word64) :: Integer))

    genJsonBytes :: Gen Aeson.Value
    genJsonBytes = toJSON
                 . Text.decodeLatin1
                 . Base16.encode
                 . BS.pack
               <$> Gen.list (Range.linear 0 64)
                            (Gen.word8 Range.constantBounded)

    genJsonText  :: Gen Aeson.Value
    genJsonText = fmap toJSON $
                    Gen.ensure validText $
                      Text.pack <$> Gen.list (Range.linear 0 64) Gen.alphaNum
      where
        validText t = BS.length (Text.encodeUtf8 t) <= 64

    genJsonList :: Gen Aeson.Value
    genJsonList = fmap toJSON $
                    Gen.sized $ \sz ->
                      Gen.list (Range.linear 0 (fromIntegral sz)) genJsonValue

    genJsonMap :: Gen Aeson.Value
    genJsonMap = fmap toJSON $
                   Gen.sized $ \sz ->
                     Gen.list (Range.linear 0 (fromIntegral sz)) $
                       mkKVPair <$> genJsonValue <*> genJsonValue
      where
        mkKVPair :: Aeson.Value -> Aeson.Value -> Aeson.Value
        mkKVPair k v = Aeson.object [ ("k", k), ("v", v) ]


genTxMetadata :: Gen TxMetadata
genTxMetadata =
    Gen.sized $ \sz ->
      TxMetadata . Map.fromList <$>
      Gen.list (Range.linear 0 (fromIntegral sz))
               ((,) <$> Gen.word64 Range.constantBounded
                    <*> genTxMetadataValue)

genTxMetadataValue :: Gen TxMetadataValue
genTxMetadataValue =
    Gen.sized $ \sz ->
      Gen.frequency
        [ (1,         TxMetaNumber <$> genTxMetaNumber)
        , (1,         TxMetaBytes  <$> genTxMetaBytes)
        , (1,         TxMetaText   <$> genTxMetaText)
        , (fromIntegral (signum sz),
                      TxMetaList   <$> Gen.scale (`div` 2) genTxMetaList)
        , (fromIntegral (signum sz),
                      TxMetaMap    <$> Gen.scale (`div` 2) genTxMetaMap)
        ]
  where
    genTxMetaNumber :: Gen Integer
    genTxMetaNumber = Gen.integral
                        (Range.linear
                          (-fromIntegral (maxBound :: Word64) :: Integer)
                          ( fromIntegral (maxBound :: Word64) :: Integer))

    genTxMetaBytes :: Gen ByteString
    genTxMetaBytes = BS.pack <$> Gen.list (Range.linear 0 64)
                                          (Gen.word8 Range.constantBounded)

    genTxMetaText  :: Gen Text
    genTxMetaText = Text.pack <$> Gen.list (Range.linear 0 64) Gen.alphaNum

    genTxMetaList :: Gen [TxMetadataValue]
    genTxMetaList = Gen.sized $ \sz ->
                      Gen.list (Range.linear 0 (fromIntegral sz))
                               genTxMetadataValue

    genTxMetaMap  :: Gen [(TxMetadataValue, TxMetadataValue)]
    genTxMetaMap = Gen.sized $ \sz ->
                      Gen.list (Range.linear 0 (fromIntegral sz)) $
                        (,) <$> genTxMetadataValue <*> genTxMetadataValue


-- ----------------------------------------------------------------------------
-- Automagically collecting all the tests
--

tests :: TestTree
tests = fromGroup $$discover
