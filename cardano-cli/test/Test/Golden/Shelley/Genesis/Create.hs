{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Golden.Shelley.Genesis.Create
  ( golden_shelleyGenesisCreate
  ) where

import           Cardano.Prelude
import           Hedgehog (Property, forAll, (===))
import           Prelude (String)
import           Test.OptParse as OP

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Time.Clock as DT
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.Time as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R
import qualified System.Directory as IO

{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Reduce duplication" -}

parseMaxLovelaceSupply :: J.Value -> J.Parser Int
parseMaxLovelaceSupply = J.withObject "Object" $ \o -> o J..: "maxLovelaceSupply"

parseSystemStart :: J.Value -> J.Parser String
parseSystemStart = J.withObject "Object" $ \o -> o J..: "systemStart"

parseHashMap :: J.Value -> J.Parser (HM.HashMap String J.Value)
parseHashMap (J.Object hm) = pure $ HM.fromList $ fmap (first T.unpack) (HM.toList hm)
parseHashMap v = J.typeMismatch "Object" v

parseDelegateCount :: J.Value -> J.Parser Int
parseDelegateCount = J.withObject "Object" $ \o -> do
  delegates <- (o J..: "genDelegs") >>= parseHashMap
  pure $ HM.size delegates

parseDelegateKey :: J.Value -> J.Parser String
parseDelegateKey = J.withObject "Object" $ \o -> o J..: "delegate"

parseDelegateKeys :: J.Value -> J.Parser [String]
parseDelegateKeys = J.withObject "Object" $ \o -> do
  delegates <- (o J..: "genDelegs") >>= parseHashMap
  sequence $ fmap (parseDelegateKey . snd) (HM.toList delegates)

parseHashKeys :: J.Value -> J.Parser [String]
parseHashKeys = J.withObject "Object" $ \o -> do
  delegates <- (o J..: "genDelegs") >>= parseHashMap
  pure $ fmap fst (HM.toList delegates)

parseTotalSupply :: J.Value -> J.Parser Int
parseTotalSupply = J.withObject "Object" $ \ o -> do
  initialFunds <- (o J..: "initialFunds") >>= parseHashMap
  fmap sum (sequence (fmap (J.parseJSON @Int . snd) (HM.toList initialFunds)))

golden_shelleyGenesisCreate :: Property
golden_shelleyGenesisCreate = propertyOnce $ do
  H.moduleWorkspace "tmp" $ \tempDir -> do
    sourceGenesisSpecFile <- noteInputFile "test/data/golden/shelley/genesis/genesis.spec.json"
    genesisSpecFile <- noteTempFile tempDir "genesis.spec.json"

    liftIO $ IO.copyFile sourceGenesisSpecFile genesisSpecFile

    let genesisFile = tempDir <> "/genesis.json"

    fmtStartTime <- fmap H.formatIso8601 $ liftIO DT.getCurrentTime

    (supply, fmtSupply) <- fmap (OP.withSnd show) $ forAll $ G.int (R.linear 10000000 4000000000)
    (delegateCount, fmtDelegateCount) <- fmap (OP.withSnd show) $ forAll $ G.int (R.linear 4 19)
    (utxoCount, fmtUtxoCount) <- fmap (OP.withSnd show) $ forAll $ G.int (R.linear 4 19)

    -- Create the genesis json file and required keys
    void $ execCardanoCLI
      [ "genesis","create"
      , "--testnet-magic", "12"
      , "--start-time", fmtStartTime
      , "--supply", fmtSupply
      , "--gen-genesis-keys", fmtDelegateCount
      , "--gen-utxo-keys", fmtUtxoCount
      , "--genesis-dir", tempDir
      ]

    H.assertFilesExist [genesisFile]

    genesisContents <- liftIO $ LBS.readFile genesisFile

    actualJson <- H.evalEither $ J.eitherDecode genesisContents
    actualSupply <- H.evalEither $ J.parseEither parseMaxLovelaceSupply actualJson
    actualStartTime <- H.evalEither $ J.parseEither parseSystemStart actualJson
    actualDelegateCount <- H.evalEither $ J.parseEither parseDelegateCount actualJson
    actualTotalSupply <- H.evalEither $ J.parseEither parseTotalSupply actualJson
    actualHashKeys <- H.evalEither $ J.parseEither parseHashKeys actualJson
    actualDelegateKeys <- H.evalEither $ J.parseEither parseDelegateKeys actualJson

    actualSupply === supply
    actualStartTime === fmtStartTime
    actualDelegateCount === delegateCount
    actualDelegateCount === utxoCount
    actualTotalSupply === supply -- Check that the sum of the initial fund amounts matches the total supply

    -- Check uniqueness and count of hash keys
    S.size (S.fromList actualHashKeys) === length actualHashKeys -- This isn't strictly necessary because we use aeson which guarantees uniqueness of keys
    S.size (S.fromList actualHashKeys) === delegateCount

    -- Check uniqueness and count of hash keys
    S.size (S.fromList actualDelegateKeys) === length actualDelegateKeys
    S.size (S.fromList actualDelegateKeys) === delegateCount

    for_ [1 .. delegateCount] $ \i -> do
      -- Check Genesis keys
      H.assertFileOccurences 1 "GenesisSigningKey_ed25519" $ tempDir <> "/genesis-keys/genesis" <> show i <> ".skey"
      H.assertFileOccurences 1 "GenesisVerificationKey_ed25519" $ tempDir <> "/genesis-keys/genesis" <> show i <> ".vkey"

      H.assertEndsWithSingleNewline $ tempDir <> "/genesis-keys/genesis" <> show i <> ".skey"
      H.assertEndsWithSingleNewline $ tempDir <> "/genesis-keys/genesis" <> show i <> ".vkey"

      -- Check delegate keys
      H.assertFileOccurences 1 "GenesisDelegateSigningKey_ed25519" $ tempDir <> "/delegate-keys/delegate" <> show i <> ".skey"
      H.assertFileOccurences 1 "GenesisDelegateVerificationKey_ed25519" $ tempDir <> "/delegate-keys/delegate" <> show i <> ".vkey"
      H.assertFileOccurences 1 "NodeOperationalCertificateIssueCounter" $ tempDir <> "/delegate-keys/delegate" <> show i <> ".counter"

      H.assertEndsWithSingleNewline $ tempDir <> "/delegate-keys/delegate" <> show i <> ".skey"
      H.assertEndsWithSingleNewline $ tempDir <> "/delegate-keys/delegate" <> show i <> ".vkey"
      H.assertEndsWithSingleNewline $ tempDir <> "/delegate-keys/delegate" <> show i <> ".counter"

      -- Check utxo keys
      H.assertFileOccurences 1 "GenesisUTxOSigningKey_ed25519" $ tempDir <> "/utxo-keys/utxo" <> show i <> ".skey"
      H.assertFileOccurences 1 "GenesisUTxOVerificationKey_ed25519"  $ tempDir <> "/utxo-keys/utxo" <> show i <> ".vkey"

      H.assertEndsWithSingleNewline $ tempDir <> "/utxo-keys/utxo" <> show i <> ".skey"
      H.assertEndsWithSingleNewline $ tempDir <> "/utxo-keys/utxo" <> show i <> ".vkey"

  H.moduleWorkspace "tmp" $ \tempDir -> do
    let genesisFile = tempDir <> "/genesis.json"

    fmtStartTime <- fmap H.formatIso8601 $ liftIO DT.getCurrentTime

    (supply, fmtSupply) <- fmap (OP.withSnd show) $ forAll $ G.int (R.linear 10000000 4000000000)
    (delegateCount, fmtDelegateCount) <- fmap (OP.withSnd show) $ forAll $ G.int (R.linear 4 19)
    (utxoCount, fmtUtxoCount) <- fmap (OP.withSnd show) $ forAll $ G.int (R.linear 4 19)

    -- Create the genesis json file and required keys
    void $ execCardanoCLI
        [ "genesis","create"
        , "--testnet-magic", "12"
        , "--start-time", fmtStartTime
        , "--supply", fmtSupply
        , "--gen-genesis-keys", fmtDelegateCount
        , "--gen-utxo-keys", fmtUtxoCount
        , "--genesis-dir", tempDir
        ]

    H.assertFilesExist [genesisFile]

    genesisContents <- liftIO $ LBS.readFile genesisFile

    actualJson <- H.evalEither $ J.eitherDecode genesisContents
    actualSupply <- H.evalEither $ J.parseEither parseMaxLovelaceSupply actualJson
    actualStartTime <- H.evalEither $ J.parseEither parseSystemStart actualJson
    actualDelegateCount <- H.evalEither $ J.parseEither parseDelegateCount actualJson
    actualTotalSupply <- H.evalEither $ J.parseEither parseTotalSupply actualJson
    actualHashKeys <- H.evalEither $ J.parseEither parseHashKeys actualJson
    actualDelegateKeys <- H.evalEither $ J.parseEither parseDelegateKeys actualJson

    actualSupply === supply
    actualStartTime === fmtStartTime
    actualDelegateCount === delegateCount
    actualDelegateCount === utxoCount
    actualTotalSupply === supply -- Check that the sum of the initial fund amounts matches the total supply

    -- Check uniqueness and count of hash keys
    S.size (S.fromList actualHashKeys) === length actualHashKeys -- This isn't strictly necessary because we use aeson which guarantees uniqueness of keys
    S.size (S.fromList actualHashKeys) === delegateCount

    -- Check uniqueness and count of hash keys
    S.size (S.fromList actualDelegateKeys) === length actualDelegateKeys
    S.size (S.fromList actualDelegateKeys) === delegateCount

    for_ [1 .. delegateCount] $ \i -> do
      -- Check Genesis keys
      H.assertFileOccurences 1 "GenesisSigningKey_ed25519" $ tempDir <> "/genesis-keys/genesis" <> show i <> ".skey"
      H.assertFileOccurences 1 "GenesisVerificationKey_ed25519" $ tempDir <> "/genesis-keys/genesis" <> show i <> ".vkey"

      H.assertEndsWithSingleNewline $ tempDir <> "/genesis-keys/genesis" <> show i <> ".skey"
      H.assertEndsWithSingleNewline $ tempDir <> "/genesis-keys/genesis" <> show i <> ".vkey"

      -- Check delegate keys
      H.assertFileOccurences 1 "GenesisDelegateSigningKey_ed25519" $ tempDir <> "/delegate-keys/delegate" <> show i <> ".skey"
      H.assertFileOccurences 1 "GenesisDelegateVerificationKey_ed25519" $ tempDir <> "/delegate-keys/delegate" <> show i <> ".vkey"
      H.assertFileOccurences 1 "NodeOperationalCertificateIssueCounter" $ tempDir <> "/delegate-keys/delegate" <> show i <> ".counter"

      H.assertEndsWithSingleNewline $ tempDir <> "/delegate-keys/delegate" <> show i <> ".skey"
      H.assertEndsWithSingleNewline $ tempDir <> "/delegate-keys/delegate" <> show i <> ".vkey"
      H.assertEndsWithSingleNewline $ tempDir <> "/delegate-keys/delegate" <> show i <> ".counter"

      -- Check utxo keys
      H.assertFileOccurences 1 "GenesisUTxOSigningKey_ed25519" $ tempDir <> "/utxo-keys/utxo" <> show i <> ".skey"
      H.assertFileOccurences 1 "GenesisUTxOVerificationKey_ed25519"  $ tempDir <> "/utxo-keys/utxo" <> show i <> ".vkey"

      H.assertEndsWithSingleNewline $ tempDir <> "/utxo-keys/utxo" <> show i <> ".skey"
      H.assertEndsWithSingleNewline $ tempDir <> "/utxo-keys/utxo" <> show i <> ".vkey"
