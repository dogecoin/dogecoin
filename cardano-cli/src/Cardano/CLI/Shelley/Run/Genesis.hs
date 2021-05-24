{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Shelley.Run.Genesis
  ( ShelleyGenesisCmdError(..)
  , readShelleyGenesis
  , readAlonzoGenesis
  , renderShelleyGenesisCmdError
  , runGenesisCmd
  ) where

import           Cardano.Prelude
import           Prelude (id)

import           Data.Aeson
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Binary.Get as Bin
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Map.Strict as Map

import qualified Data.Sequence.Strict as Seq
import           Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)

import           Cardano.Binary (ToCBOR (..))

import           Cardano.Crypto.Hash (HashAlgorithm)
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Random as Crypto
import           Crypto.Random as Crypto

import           Cardano.Ledger.Crypto (ADDRHASH, Crypto)

import           System.Directory (createDirectoryIfMissing, listDirectory)
import           System.FilePath (takeExtension, takeExtensions, (</>))
import           System.IO.Error (isDoesNotExistError)

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left,
                   newExceptT)

import qualified Cardano.Crypto.Hash as Crypto

import           Cardano.Api
import           Cardano.Api.Shelley

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesisStaking (..))
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)

import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import           Cardano.Ledger.Alonzo.Translation (AlonzoGenesis (..))
import qualified Cardano.Ledger.Alonzo.Translation as Alonzo
import           Cardano.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.API as Ledger
import qualified Shelley.Spec.Ledger.BaseTypes as Ledger
import qualified Shelley.Spec.Ledger.Keys as Ledger
import qualified Shelley.Spec.Ledger.PParams as Shelley

-- TODO: Remove me, cli should not depend directly on plutus repo.
import qualified PlutusCore.Evaluation.Machine.ExBudgeting as Plutus
import qualified PlutusCore.Evaluation.Machine.ExBudgetingDefaults as Plutus

import           Cardano.Ledger.Era ()

import           Cardano.CLI.Helpers (textShow)
import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Key
import           Cardano.CLI.Shelley.Orphans ()
import           Cardano.CLI.Shelley.Parsers (renderTxIn)
import           Cardano.CLI.Shelley.Run.Address
import           Cardano.CLI.Shelley.Run.Node (ShelleyNodeCmdError (..), renderShelleyNodeCmdError,
                   runNodeIssueOpCert, runNodeKeyGenCold, runNodeKeyGenKES, runNodeKeyGenVRF)
import           Cardano.CLI.Shelley.Run.Pool (ShelleyPoolCmdError (..), renderShelleyPoolCmdError)
import           Cardano.CLI.Shelley.Run.StakeAddress (ShelleyStakeAddressCmdError (..),
                   renderShelleyStakeAddressCmdError, runStakeAddressKeyGen)
import           Cardano.CLI.Types

{- HLINT ignore "Reduce duplication" -}

data ShelleyGenesisCmdError
  = ShelleyGenesisCmdAesonDecodeError !FilePath !Text
  | ShelleyGenesisCmdGenesisFileError !(FileError ())
  | ShelleyGenesisCmdFileError !(FileError ())
  | ShelleyGenesisCmdMismatchedGenesisKeyFiles [Int] [Int] [Int]
  | ShelleyGenesisCmdFilesNoIndex [FilePath]
  | ShelleyGenesisCmdFilesDupIndex [FilePath]
  | ShelleyGenesisCmdTextEnvReadFileError !(FileError TextEnvelopeError)
  | ShelleyGenesisCmdUnexpectedAddressVerificationKey !VerificationKeyFile !Text !SomeAddressVerificationKey
  | ShelleyGenesisCmdTooFewPoolsForBulkCreds !Word !Word !Word
  | ShelleyGenesisCmdAddressCmdError !ShelleyAddressCmdError
  | ShelleyGenesisCmdNodeCmdError !ShelleyNodeCmdError
  | ShelleyGenesisCmdPoolCmdError !ShelleyPoolCmdError
  | ShelleyGenesisCmdStakeAddressCmdError !ShelleyStakeAddressCmdError
  | ShelleyGenesisCmdCostModelsError !FilePath
  deriving Show

renderShelleyGenesisCmdError :: ShelleyGenesisCmdError -> Text
renderShelleyGenesisCmdError err =
  case err of
    ShelleyGenesisCmdAesonDecodeError fp decErr ->
      "Error while decoding Shelley genesis at: " <> textShow fp <> " Error: " <> textShow decErr
    ShelleyGenesisCmdGenesisFileError fe -> Text.pack $ displayError fe
    ShelleyGenesisCmdFileError fe -> Text.pack $ displayError fe
    ShelleyGenesisCmdMismatchedGenesisKeyFiles gfiles dfiles vfiles ->
      "Mismatch between the files found:\n"
        <> "Genesis key file indexes:      " <> textShow gfiles <> "\n"
        <> "Delegate key file indexes:     " <> textShow dfiles <> "\n"
        <> "Delegate VRF key file indexes: " <> textShow vfiles
    ShelleyGenesisCmdFilesNoIndex files ->
      "The genesis keys files are expected to have a numeric index but these do not:\n"
        <> Text.unlines (map Text.pack files)
    ShelleyGenesisCmdFilesDupIndex files ->
      "The genesis keys files are expected to have a unique numeric index but these do not:\n"
        <> Text.unlines (map Text.pack files)
    ShelleyGenesisCmdTextEnvReadFileError fileErr -> Text.pack $ displayError fileErr
    ShelleyGenesisCmdUnexpectedAddressVerificationKey (VerificationKeyFile file) expect got -> mconcat
      [ "Unexpected address verification key type in file ", Text.pack file
      , ", expected: ", expect, ", got: ", textShow got
      ]
    ShelleyGenesisCmdTooFewPoolsForBulkCreds pools files perPool -> mconcat
      [ "Number of pools requested for generation (", textShow pools
      , ") is insufficient to fill ", textShow files
      , " bulk files, with ", textShow perPool, " pools per file."
      ]
    ShelleyGenesisCmdAddressCmdError e -> renderShelleyAddressCmdError e
    ShelleyGenesisCmdNodeCmdError e -> renderShelleyNodeCmdError e
    ShelleyGenesisCmdPoolCmdError e -> renderShelleyPoolCmdError e
    ShelleyGenesisCmdStakeAddressCmdError e -> renderShelleyStakeAddressCmdError e
    ShelleyGenesisCmdCostModelsError fp ->
      "Cost model is invalid: " <> Text.pack fp

runGenesisCmd :: GenesisCmd -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisCmd (GenesisKeyGenGenesis vk sk) = runGenesisKeyGenGenesis vk sk
runGenesisCmd (GenesisKeyGenDelegate vk sk ctr) = runGenesisKeyGenDelegate vk sk ctr
runGenesisCmd (GenesisKeyGenUTxO vk sk) = runGenesisKeyGenUTxO vk sk
runGenesisCmd (GenesisCmdKeyHash vk) = runGenesisKeyHash vk
runGenesisCmd (GenesisVerKey vk sk) = runGenesisVerKey vk sk
runGenesisCmd (GenesisTxIn vk nw mOutFile) = runGenesisTxIn vk nw mOutFile
runGenesisCmd (GenesisAddr vk nw mOutFile) = runGenesisAddr vk nw mOutFile
runGenesisCmd (GenesisCreate gd gn un ms am nw) = runGenesisCreate gd gn un ms am nw
runGenesisCmd (GenesisCreateStaked gd gn gp gl un ms am ds nw bf bp su) = runGenesisCreateStaked gd gn gp gl un ms am ds nw bf bp su
runGenesisCmd (GenesisHashFile gf) = runGenesisHashFile gf

--
-- Genesis command implementations
--

runGenesisKeyGenGenesis :: VerificationKeyFile -> SigningKeyFile
                        -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenGenesis (VerificationKeyFile vkeyPath)
                        (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsGenesisKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Genesis Signing Key"
    vkeyDesc = "Genesis Verification Key"


runGenesisKeyGenDelegate :: VerificationKeyFile
                         -> SigningKeyFile
                         -> OpCertCounterFile
                         -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenDelegate (VerificationKeyFile vkeyPath)
                         (SigningKeyFile skeyPath)
                         (OpCertCounterFile ocertCtrPath) = do
    skey <- liftIO $ generateSigningKey AsGenesisDelegateKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope ocertCtrPath (Just certCtrDesc)
      $ OperationalCertificateIssueCounter
          initialCounter
          (castVerificationKey vkey)  -- Cast to a 'StakePoolKey'
  where
    skeyDesc, vkeyDesc, certCtrDesc :: TextEnvelopeDescr
    skeyDesc = "Genesis delegate operator key"
    vkeyDesc = "Genesis delegate operator key"
    certCtrDesc = "Next certificate issue number: "
               <> fromString (show initialCounter)

    initialCounter :: Word64
    initialCounter = 0


runGenesisKeyGenDelegateVRF :: VerificationKeyFile -> SigningKeyFile
                            -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenDelegateVRF (VerificationKeyFile vkeyPath)
                            (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsVrfKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "VRF Signing Key"
    vkeyDesc = "VRF Verification Key"


runGenesisKeyGenUTxO :: VerificationKeyFile -> SigningKeyFile
                     -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenUTxO (VerificationKeyFile vkeyPath)
                     (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsGenesisUTxOKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Genesis Initial UTxO Signing Key"
    vkeyDesc = "Genesis Initial UTxO Verification Key"


runGenesisKeyHash :: VerificationKeyFile -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyHash (VerificationKeyFile vkeyPath) = do
    vkey <- firstExceptT ShelleyGenesisCmdTextEnvReadFileError . newExceptT $
            readFileTextEnvelopeAnyOf
              [ FromSomeType (AsVerificationKey AsGenesisKey)
                             AGenesisKey
              , FromSomeType (AsVerificationKey AsGenesisDelegateKey)
                             AGenesisDelegateKey
              , FromSomeType (AsVerificationKey AsGenesisUTxOKey)
                             AGenesisUTxOKey
              ]
              vkeyPath
    liftIO $ BS.putStrLn (renderKeyHash vkey)
  where
    renderKeyHash :: SomeGenesisKey VerificationKey -> ByteString
    renderKeyHash (AGenesisKey         vk) = renderVerificationKeyHash vk
    renderKeyHash (AGenesisDelegateKey vk) = renderVerificationKeyHash vk
    renderKeyHash (AGenesisUTxOKey     vk) = renderVerificationKeyHash vk

    renderVerificationKeyHash :: Key keyrole => VerificationKey keyrole -> ByteString
    renderVerificationKeyHash = serialiseToRawBytesHex
                              . verificationKeyHash


runGenesisVerKey :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisVerKey (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- firstExceptT ShelleyGenesisCmdTextEnvReadFileError . newExceptT $
            readFileTextEnvelopeAnyOf
              [ FromSomeType (AsSigningKey AsGenesisKey)
                             AGenesisKey
              , FromSomeType (AsSigningKey AsGenesisDelegateKey)
                             AGenesisDelegateKey
              , FromSomeType (AsSigningKey AsGenesisUTxOKey)
                             AGenesisUTxOKey
              ]
              skeyPath

    let vkey :: SomeGenesisKey VerificationKey
        vkey = case skey of
          AGenesisKey         sk -> AGenesisKey         (getVerificationKey sk)
          AGenesisDelegateKey sk -> AGenesisDelegateKey (getVerificationKey sk)
          AGenesisUTxOKey     sk -> AGenesisUTxOKey     (getVerificationKey sk)

    firstExceptT ShelleyGenesisCmdGenesisFileError . newExceptT . liftIO $
      case vkey of
        AGenesisKey         vk -> writeFileTextEnvelope vkeyPath Nothing vk
        AGenesisDelegateKey vk -> writeFileTextEnvelope vkeyPath Nothing vk
        AGenesisUTxOKey     vk -> writeFileTextEnvelope vkeyPath Nothing vk

data SomeGenesisKey f
     = AGenesisKey         (f GenesisKey)
     | AGenesisDelegateKey (f GenesisDelegateKey)
     | AGenesisUTxOKey     (f GenesisUTxOKey)


runGenesisTxIn :: VerificationKeyFile -> NetworkId -> Maybe OutputFile
               -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisTxIn (VerificationKeyFile vkeyPath) network mOutFile = do
    vkey <- firstExceptT ShelleyGenesisCmdTextEnvReadFileError . newExceptT $
            readFileTextEnvelope (AsVerificationKey AsGenesisUTxOKey) vkeyPath
    let txin = genesisUTxOPseudoTxIn network (verificationKeyHash vkey)
    liftIO $ writeOutput mOutFile (renderTxIn txin)


runGenesisAddr :: VerificationKeyFile -> NetworkId -> Maybe OutputFile
               -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisAddr (VerificationKeyFile vkeyPath) network mOutFile = do
    vkey <- firstExceptT ShelleyGenesisCmdTextEnvReadFileError . newExceptT $
            readFileTextEnvelope (AsVerificationKey AsGenesisUTxOKey) vkeyPath
    let vkh  = verificationKeyHash (castVerificationKey vkey)
        addr = makeShelleyAddress network (PaymentCredentialByKey vkh)
                                  NoStakeAddress
    liftIO $ writeOutput mOutFile (serialiseAddress addr)

writeOutput :: Maybe OutputFile -> Text -> IO ()
writeOutput (Just (OutputFile fpath)) = Text.writeFile fpath
writeOutput Nothing                   = Text.putStrLn


--
-- Create Genesis command implementation
--

runGenesisCreate :: GenesisDir
                 -> Word  -- ^ num genesis & delegate keys to make
                 -> Word  -- ^ num utxo keys to make
                 -> Maybe SystemStart
                 -> Maybe Lovelace
                 -> NetworkId
                 -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisCreate (GenesisDir rootdir)
                 genNumGenesisKeys genNumUTxOKeys
                 mStart mAmount network = do
  liftIO $ do
    createDirectoryIfMissing False rootdir
    createDirectoryIfMissing False gendir
    createDirectoryIfMissing False deldir
    createDirectoryIfMissing False utxodir

  template <- readShelleyGenesis (rootdir </> "genesis.spec.json") adjustTemplate

  forM_ [ 1 .. genNumGenesisKeys ] $ \index -> do
    createGenesisKeys  gendir  index
    createDelegateKeys deldir index

  forM_ [ 1 .. genNumUTxOKeys ] $ \index ->
    createUtxoKeys utxodir index

  genDlgs <- readGenDelegsMap gendir deldir
  utxoAddrs <- readInitialFundAddresses utxodir network
  start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mStart

  let finalGenesis = updateTemplate
                       -- Shelley genesis parameters
                       start genDlgs mAmount utxoAddrs mempty (Lovelace 0) [] [] template
                       -- Alono genesis parameters TODO: Parameterize
                       (Lovelace 10) (Lovelace 1, Lovelace 1) (1,1) (1,1) 1 1 1

  writeShelleyGenesis (rootdir </> "genesis.json") finalGenesis
  where
    adjustTemplate t = t { sgNetworkMagic = unNetworkMagic (toNetworkMagic network) }
    gendir  = rootdir </> "genesis-keys"
    deldir  = rootdir </> "delegate-keys"
    utxodir = rootdir </> "utxo-keys"

runGenesisCreateStaked
  :: GenesisDir
  -> Word           -- ^ num genesis & delegate keys to make
  -> Word           -- ^ num utxo keys to make
  -> Word           -- ^ num pools to make
  -> Word           -- ^ num delegators to make
  -> Maybe SystemStart
  -> Maybe Lovelace -- ^ supply going to non-delegators
  -> Lovelace       -- ^ supply going to delegators
  -> NetworkId
  -> Word           -- ^ bulk credential files to write
  -> Word           -- ^ pool credentials per bulk file
  -> Word           -- ^ num stuffed UTxO entries
  -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisCreateStaked (GenesisDir rootdir)
                 genNumGenesisKeys genNumUTxOKeys genNumPools genNumStDelegs
                 mStart mNonDlgAmount stDlgAmount network
                 bulkPoolCredFiles bulkPoolsPerFile numStuffedUtxo = do
  liftIO $ do
    createDirectoryIfMissing False rootdir
    createDirectoryIfMissing False gendir
    createDirectoryIfMissing False deldir
    createDirectoryIfMissing False pooldir
    createDirectoryIfMissing False stdeldir
    createDirectoryIfMissing False utxodir

  template <- readShelleyGenesis (rootdir </> "genesis.spec.json") adjustTemplate

  forM_ [ 1 .. genNumGenesisKeys ] $ \index -> do
    createGenesisKeys  gendir  index
    createDelegateKeys deldir index

  forM_ [ 1 .. genNumUTxOKeys ] $ \index ->
    createUtxoKeys utxodir index

  pools <- forM [ 1 .. genNumPools ] $ \index -> do
    createPoolCredentials pooldir index
    buildPool network pooldir index

  when (bulkPoolCredFiles * bulkPoolsPerFile > genNumPools) $
    left $ ShelleyGenesisCmdTooFewPoolsForBulkCreds  genNumPools bulkPoolCredFiles bulkPoolsPerFile
  -- We generate the bulk files for the last pool indices,
  -- so that all the non-bulk pools have stable indices at beginning:
  let bulkOffset  = fromIntegral $ genNumPools - bulkPoolCredFiles * bulkPoolsPerFile
      bulkIndices :: [Word]   = [ 1 + bulkOffset .. genNumPools ]
      bulkSlices  :: [[Word]] = List.chunksOf (fromIntegral bulkPoolsPerFile) bulkIndices
  forM_ (zip [ 1 .. bulkPoolCredFiles ] bulkSlices) $
    uncurry (writeBulkPoolCredentials pooldir)

  forM_ [ 1 .. genNumStDelegs ] $ \index ->
    createDelegatorCredentials stdeldir index

  delegations :: [Delegation] <-
    -- Distribute M delegates across N pools:
    forM [ (pool, delegIx)
         | (pool, poolIx) <- zip pools [1 ..]
         , delegIxLocal <- [ 1 .. delegsPerPool ] ++
                           -- Add all remaining delegates to the last pool:
                           if delegsRemaining /= 0 && poolIx == genNumPools
                           then [ delegsPerPool + 1 .. delegsPerPool + delegsRemaining ]
                           else []
         , let delegIx = delegIxLocal + delegsPerPool * (poolIx - 1)] $
      uncurry (computeDelegation network stdeldir)

  genDlgs <- readGenDelegsMap gendir deldir
  nonDelegAddrs <- readInitialFundAddresses utxodir network
  start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mStart

  stuffedUtxoAddrs <- liftIO $ replicateM (fromIntegral numStuffedUtxo)
                      genStuffedAddress

  let poolMap :: Map (Ledger.KeyHash Ledger.Staking StandardCrypto) (Ledger.PoolParams StandardCrypto)
      poolMap = Map.fromList $ mkDelegationMapEntry <$> delegations
      delegAddrs = dInitialUtxoAddr <$> delegations
      finalGenesis = updateTemplate
                       -- Shelley genesis parameters
                       start genDlgs mNonDlgAmount nonDelegAddrs poolMap stDlgAmount delegAddrs stuffedUtxoAddrs template
                       -- Alonzo genesis parameters TODO: Parameterize
                       (Lovelace 10) (Lovelace 1, Lovelace 1) (1,1) (1,1) 1 1 1

  writeShelleyGenesis (rootdir </> "genesis.json") finalGenesis
  liftIO $ Text.putStrLn $ mconcat $
    [ "generated genesis with: "
    , textShow genNumGenesisKeys, " genesis keys, "
    , textShow genNumUTxOKeys, " non-delegating UTxO keys, "
    , textShow genNumPools, " stake pools, "
    , textShow genNumStDelegs, " delegating UTxO keys, "
    , textShow (length delegations), " delegation relationships, "
    , textShow (Map.size poolMap), " delegation map entries, "
    , textShow (length delegAddrs), " delegating addresses"
    ] ++
    [ mconcat
      [ ", "
      , textShow bulkPoolCredFiles, " bulk pool credential files, "
      , textShow bulkPoolsPerFile, " pools per bulk credential file, indices starting from "
      , textShow bulkOffset, ", "
      , textShow $ length bulkIndices, " total pools in bulk nodes, each bulk node having this many entries: "
      , textShow $ length <$> bulkSlices
      ]
    | bulkPoolCredFiles * bulkPoolsPerFile > 0 ]

  where
    (,) delegsPerPool delegsRemaining = divMod genNumStDelegs genNumPools
    adjustTemplate t = t { sgNetworkMagic = unNetworkMagic (toNetworkMagic network) }
    mkDelegationMapEntry :: Delegation -> (Ledger.KeyHash Ledger.Staking StandardCrypto, Ledger.PoolParams StandardCrypto)
    mkDelegationMapEntry d = (dDelegStaking d, dPoolParams d)

    gendir   = rootdir </> "genesis-keys"
    deldir   = rootdir </> "delegate-keys"
    pooldir  = rootdir </> "pools"
    stdeldir = rootdir </> "stake-delegator-keys"
    utxodir  = rootdir </> "utxo-keys"

    genStuffedAddress :: IO (AddressInEra ShelleyEra)
    genStuffedAddress =
      shelleyAddressInEra <$>
      (ShelleyAddress
       <$> pure Ledger.Testnet
       <*> (Ledger.KeyHashObj . mkKeyHash . read64BitInt
             <$> Crypto.runSecureRandom (getRandomBytes 8))
       <*> pure Ledger.StakeRefNull)

    read64BitInt :: ByteString -> Int
    read64BitInt = (fromIntegral :: Word64 -> Int)
      . Bin.runGet Bin.getWord64le . LBS.fromStrict

    mkDummyHash :: forall h a. HashAlgorithm h => Proxy h -> Int -> Hash.Hash h a
    mkDummyHash _ = coerce . Ledger.hashWithSerialiser @h toCBOR

    mkKeyHash :: forall c discriminator. Crypto c => Int -> Ledger.KeyHash discriminator c
    mkKeyHash = Ledger.KeyHash . mkDummyHash (Proxy @(ADDRHASH c))

-- -------------------------------------------------------------------------------------------------

createDelegateKeys :: FilePath -> Word -> ExceptT ShelleyGenesisCmdError IO ()
createDelegateKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  runGenesisKeyGenDelegate
        (VerificationKeyFile $ dir </> "delegate" ++ strIndex ++ ".vkey")
        coldSK
        opCertCtr
  runGenesisKeyGenDelegateVRF
        (VerificationKeyFile $ dir </> "delegate" ++ strIndex ++ ".vrf.vkey")
        (SigningKeyFile $ dir </> "delegate" ++ strIndex ++ ".vrf.skey")
  firstExceptT ShelleyGenesisCmdNodeCmdError $ do
    runNodeKeyGenKES
        kesVK
        (SigningKeyFile $ dir </> "delegate" ++ strIndex ++ ".kes.skey")
    runNodeIssueOpCert
        (VerificationKeyFilePath kesVK)
        coldSK
        opCertCtr
        (KESPeriod 0)
        (OutputFile $ dir </> "opcert" ++ strIndex ++ ".cert")
 where
   strIndex = show index
   kesVK = VerificationKeyFile $ dir </> "delegate" ++ strIndex ++ ".kes.vkey"
   coldSK = SigningKeyFile $ dir </> "delegate" ++ strIndex ++ ".skey"
   opCertCtr = OpCertCounterFile $ dir </> "delegate" ++ strIndex ++ ".counter"

createGenesisKeys :: FilePath -> Word -> ExceptT ShelleyGenesisCmdError IO ()
createGenesisKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  let strIndex = show index
  runGenesisKeyGenGenesis
        (VerificationKeyFile $ dir </> "genesis" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "genesis" ++ strIndex ++ ".skey")


createUtxoKeys :: FilePath -> Word -> ExceptT ShelleyGenesisCmdError IO ()
createUtxoKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  let strIndex = show index
  runGenesisKeyGenUTxO
        (VerificationKeyFile $ dir </> "utxo" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "utxo" ++ strIndex ++ ".skey")

createPoolCredentials :: FilePath -> Word -> ExceptT ShelleyGenesisCmdError IO ()
createPoolCredentials dir index = do
  liftIO $ createDirectoryIfMissing False dir
  firstExceptT ShelleyGenesisCmdNodeCmdError $ do
    runNodeKeyGenKES
        kesVK
        (SigningKeyFile $ dir </> "kes" ++ strIndex ++ ".skey")
    runNodeKeyGenVRF
        (VerificationKeyFile $ dir </> "vrf" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "vrf" ++ strIndex ++ ".skey")
    runNodeKeyGenCold
        (VerificationKeyFile $ dir </> "cold" ++ strIndex ++ ".vkey")
        coldSK
        opCertCtr
    runNodeIssueOpCert
        (VerificationKeyFilePath kesVK)
        coldSK
        opCertCtr
        (KESPeriod 0)
        (OutputFile $ dir </> "opcert" ++ strIndex ++ ".cert")
  firstExceptT ShelleyGenesisCmdStakeAddressCmdError $
    runStakeAddressKeyGen
        (VerificationKeyFile $ dir </> "staking-reward" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "staking-reward" ++ strIndex ++ ".skey")
 where
   strIndex = show index
   kesVK = VerificationKeyFile $ dir </> "kes" ++ strIndex ++ ".vkey"
   coldSK = SigningKeyFile $ dir </> "cold" ++ strIndex ++ ".skey"
   opCertCtr = OpCertCounterFile $ dir </> "opcert" ++ strIndex ++ ".counter"

createDelegatorCredentials :: FilePath -> Word -> ExceptT ShelleyGenesisCmdError IO ()
createDelegatorCredentials dir index = do
  liftIO $ createDirectoryIfMissing False dir
  firstExceptT ShelleyGenesisCmdAddressCmdError $ do
    runAddressKeyGen
        AddressKeyShelley
        addrVK
        (SigningKeyFile $ dir </> "payment" ++ strIndex ++ ".skey")
  firstExceptT ShelleyGenesisCmdStakeAddressCmdError $
    runStakeAddressKeyGen
        (VerificationKeyFile $ dir </> "staking" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "staking" ++ strIndex ++ ".skey")
 where
   strIndex = show index
   addrVK = VerificationKeyFile $ dir </> "payment" ++ strIndex ++ ".vkey"

data Delegation
  = Delegation
    { dInitialUtxoAddr  :: AddressInEra ShelleyEra
    , dDelegStaking     :: Ledger.KeyHash Ledger.Staking StandardCrypto
    , dPoolParams       :: Ledger.PoolParams StandardCrypto
    }

buildPool :: NetworkId -> FilePath -> Word -> ExceptT ShelleyGenesisCmdError IO (Ledger.PoolParams StandardCrypto)
buildPool nw dir index = do
    StakePoolVerificationKey poolColdVK <- firstExceptT (ShelleyGenesisCmdPoolCmdError
                                                         . ShelleyPoolCmdReadFileError)
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakePoolKey) poolColdVKF
    VrfVerificationKey poolVrfVK <- firstExceptT (ShelleyGenesisCmdNodeCmdError
                                                  . ShelleyNodeCmdReadFileError)
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsVrfKey) poolVrfVKF
    rewardsSVK <- firstExceptT ShelleyGenesisCmdTextEnvReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) poolRewardVKF
    pure Ledger.PoolParams
      { Ledger._poolId     = Ledger.hashKey poolColdVK
      , Ledger._poolVrf    = Ledger.hashVerKeyVRF poolVrfVK
      , Ledger._poolPledge = Ledger.Coin 0
      , Ledger._poolCost   = Ledger.Coin 0
      , Ledger._poolMargin = Ledger.truncateUnitInterval 0
      , Ledger._poolRAcnt  =
          toShelleyStakeAddr $ makeStakeAddress nw $ StakeCredentialByKey (verificationKeyHash rewardsSVK)
      , Ledger._poolOwners = mempty
      , Ledger._poolRelays = Seq.empty
      , Ledger._poolMD     = Ledger.SNothing
      }
 where
   strIndex = show index
   poolColdVKF = dir </> "cold" ++ strIndex ++ ".vkey"
   poolVrfVKF = dir </> "vrf" ++ strIndex ++ ".vkey"
   poolRewardVKF = dir </> "staking-reward" ++ strIndex ++ ".vkey"

writeBulkPoolCredentials :: FilePath -> Word -> [Word] -> ExceptT ShelleyGenesisCmdError IO ()
writeBulkPoolCredentials dir bulkIx poolIxs = do
  creds <- mapM readPoolCreds poolIxs
  handleIOExceptT (ShelleyGenesisCmdFileError . FileIOError bulkFile) $
    LBS.writeFile bulkFile $ Aeson.encode creds
 where
   bulkFile = dir </> "bulk" ++ show bulkIx ++ ".creds"

   readPoolCreds :: Word -> ExceptT ShelleyGenesisCmdError IO
                                   (TextEnvelope, TextEnvelope, TextEnvelope)
   readPoolCreds ix = do
     (,,) <$> readEnvelope poolCert
          <*> readEnvelope poolVrfSKF
          <*> readEnvelope poolKesSKF
    where
      strIndex = show ix
      poolCert = dir </> "opcert" ++ strIndex ++ ".cert"
      poolVrfSKF = dir </> "vrf" ++ strIndex ++ ".skey"
      poolKesSKF = dir </> "kes" ++ strIndex ++ ".skey"
   readEnvelope :: FilePath -> ExceptT ShelleyGenesisCmdError IO TextEnvelope
   readEnvelope fp = do
     content <- handleIOExceptT (ShelleyGenesisCmdFileError . FileIOError fp) $
                  BS.readFile fp
     firstExceptT (ShelleyGenesisCmdAesonDecodeError fp . Text.pack) . hoistEither $
       Aeson.eitherDecodeStrict' content

computeDelegation :: NetworkId -> FilePath -> Ledger.PoolParams StandardCrypto -> Word -> ExceptT ShelleyGenesisCmdError IO Delegation
computeDelegation nw delegDir pool delegIx = do
    paySVK <- firstExceptT (ShelleyGenesisCmdAddressCmdError
                           . ShelleyAddressCmdVerificationKeyTextOrFileError) $
                 readAddressVerificationKeyTextOrFile
                   (VktofVerificationKeyFile payVKF)
    initialUtxoAddr <- case paySVK of
      APaymentVerificationKey payVK ->
        firstExceptT ShelleyGenesisCmdAddressCmdError
        $ buildShelleyAddress payVK (Just . StakeVerifierKey . VerificationKeyFilePath . VerificationKeyFile $ stakeVKF) nw
      _ -> left $ ShelleyGenesisCmdUnexpectedAddressVerificationKey payVKF "APaymentVerificationKey" paySVK

    StakeVerificationKey stakeVK <- firstExceptT ShelleyGenesisCmdTextEnvReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) stakeVKF

    pure Delegation
      { dInitialUtxoAddr = shelleyAddressInEra initialUtxoAddr
      , dDelegStaking = Ledger.hashKey stakeVK
      , dPoolParams = pool
      }
 where
   strIndexDeleg = show delegIx
   payVKF = VerificationKeyFile $ delegDir </> "payment" ++ strIndexDeleg ++ ".vkey"
   stakeVKF = delegDir </> "staking" ++ strIndexDeleg ++ ".vkey"

-- | Current UTCTime plus 30 seconds
getCurrentTimePlus30 :: ExceptT ShelleyGenesisCmdError IO UTCTime
getCurrentTimePlus30 =
    plus30sec <$> liftIO getCurrentTime
  where
    plus30sec :: UTCTime -> UTCTime
    plus30sec = addUTCTime (30 :: NominalDiffTime)


readShelleyGenesis
  :: FilePath
  -> (ShelleyGenesis StandardShelley -> ShelleyGenesis StandardShelley)
  -> ExceptT ShelleyGenesisCmdError IO (ShelleyGenesis StandardShelley)
readShelleyGenesis fpath adjustDefaults = do
    readAndDecode
      `catchError` \err ->
        case err of
          ShelleyGenesisCmdGenesisFileError (FileIOError _ ioe)
            | isDoesNotExistError ioe -> writeDefault
          _                           -> left err
  where
    readAndDecode = do
      lbs <- handleIOExceptT (ShelleyGenesisCmdGenesisFileError . FileIOError fpath) $ LBS.readFile fpath
      firstExceptT (ShelleyGenesisCmdAesonDecodeError fpath . Text.pack)
        . hoistEither $ Aeson.eitherDecode' lbs

    defaults :: ShelleyGenesis StandardShelley
    defaults = adjustDefaults shelleyGenesisDefaults

    writeDefault = do
      handleIOExceptT (ShelleyGenesisCmdGenesisFileError . FileIOError fpath) $
        LBS.writeFile fpath (encodePretty defaults)
      return defaults


updateTemplate
    :: SystemStart
    -- Genesis delegation (not stake-based):
    -> Map (Hash GenesisKey) (Hash GenesisDelegateKey, Hash VrfKey)
    -- Non-delegated initial UTxO spec:
    -> Maybe Lovelace
    -> [AddressInEra ShelleyEra]
    -- Genesis staking: pools/delegation map & delegated initial UTxO spec:
    -> Map (Ledger.KeyHash 'Ledger.Staking StandardCrypto) (Ledger.PoolParams StandardCrypto)
    -> Lovelace
    -> [AddressInEra ShelleyEra]
    -> [AddressInEra ShelleyEra]
    -> ShelleyGenesis StandardShelley
    -- Alonzo genesis parameters
    -> Lovelace
    -- ^ Ada per UTxO word
    -> (Lovelace, Lovelace)
    -- ^ Execution prices (memory, steps)
    -> (Word64, Word64)
    -- ^ Max Tx execution units
    -> (Word64, Word64)
    -- ^ Max block execution units
    -> Natural
    -- ^ Max value size
    -> Natural
    -- ^ Collateral percentage
    -> Natural
    -- ^ Max collateral inputs
    -> (ShelleyGenesis StandardShelley, Alonzo.AlonzoGenesis)
updateTemplate (SystemStart start)
               genDelegMap mAmountNonDeleg utxoAddrsNonDeleg
               poolSpecs (Lovelace amountDeleg) utxoAddrsDeleg stuffedUtxoAddrs
               template  adaPerUtxoWrd' (exMem,exStep) (maxTxMem, maxTxStep)
               (maxBlkMem, maxBlkStep) maxValSize' collPercent maxColInputs = do

    let shelleyGenesis = template
          { sgSystemStart = start
          , sgMaxLovelaceSupply = fromIntegral $ nonDelegCoin + delegCoin
          , sgGenDelegs = shelleyDelKeys
          , sgInitialFunds = Map.fromList
                              [ (toShelleyAddr addr, toShelleyLovelace v)
                              | (addr, v) <-
                                distribute nonDelegCoin utxoAddrsNonDeleg ++
                                distribute delegCoin    utxoAddrsDeleg ++
                                mkStuffedUtxo stuffedUtxoAddrs ]
          , sgStaking =
            ShelleyGenesisStaking
              { sgsPools = Map.fromList
                            [ (Ledger._poolId poolParams, poolParams)
                            | poolParams <- Map.elems poolSpecs ]
              , sgsStake = Ledger._poolId <$> poolSpecs
              }
          }
        cModel = case Plutus.extractModelParams Plutus.defaultCostModel of
                   Just m ->
                     if Alonzo.validateCostModelParams m
                     then Map.singleton Alonzo.PlutusV1 $ Alonzo.CostModel m
                     else panic "updateTemplate: Plutus.defaultCostModel is invalid"

                   Nothing -> panic "updateTemplate: Could not extract cost model params from Plutus.defaultCostModel"
        alonzoGenesis = AlonzoGenesis
                          { adaPerUTxOWord = toShelleyLovelace adaPerUtxoWrd'
                          , costmdls = cModel
                          , prices = Alonzo.Prices (toShelleyLovelace exMem) (toShelleyLovelace exStep)
                          , maxTxExUnits = Alonzo.ExUnits maxTxMem maxTxStep
                          , maxBlockExUnits = Alonzo.ExUnits maxBlkMem maxBlkStep
                          , maxValSize = maxValSize'
                          , collateralPercentage = collPercent
                          , maxCollateralInputs = maxColInputs
                          }
    (shelleyGenesis, alonzoGenesis)
  where
    nonDelegCoin, delegCoin :: Integer
    nonDelegCoin = fromIntegral $ fromMaybe (sgMaxLovelaceSupply template) (unLovelace <$> mAmountNonDeleg)
    delegCoin = fromIntegral amountDeleg

    distribute :: Integer -> [AddressInEra ShelleyEra] -> [(AddressInEra ShelleyEra, Lovelace)]
    distribute funds addrs =
      fst $ List.foldl' folder ([], fromIntegral funds) addrs
     where
       nAddrs, coinPerAddr, splitThreshold :: Integer
       nAddrs = fromIntegral $ length addrs
       coinPerAddr = funds `div` nAddrs
       splitThreshold = coinPerAddr + nAddrs

       folder :: ([(AddressInEra ShelleyEra, Lovelace)], Integer)
              -> AddressInEra ShelleyEra
              -> ([(AddressInEra ShelleyEra, Lovelace)], Integer)
       folder (acc, rest) addr
         | rest > splitThreshold =
             ((addr, Lovelace coinPerAddr) : acc, rest - coinPerAddr)
         | otherwise = ((addr, Lovelace rest) : acc, 0)

    mkStuffedUtxo :: [AddressInEra ShelleyEra] -> [(AddressInEra ShelleyEra, Lovelace)]
    mkStuffedUtxo xs = (, Lovelace minUtxoVal) <$> xs
      where (Coin minUtxoVal) = Shelley._minUTxOValue $ sgProtocolParams template

    shelleyDelKeys =
      Map.fromList
        [ (gh, Ledger.GenDelegPair gdh h)
        | (GenesisKeyHash gh,
           (GenesisDelegateKeyHash gdh, VrfKeyHash h)) <- Map.toList genDelegMap
        ]

    unLovelace :: Integral a => Lovelace -> a
    unLovelace (Lovelace coin) = fromIntegral coin

-- We need to include Alonzo genesis parameters
writeShelleyGenesis
  :: FilePath
  -> (ShelleyGenesis StandardShelley, AlonzoGenesis)
  -> ExceptT ShelleyGenesisCmdError IO ()
writeShelleyGenesis fpath (sg, ag) = do
  let sgValue = toJSON sg
      agValue = toJSON ag
  genesisCombined <- hoistEither $ combineAndEncode sgValue agValue
  handleIOExceptT
    (ShelleyGenesisCmdGenesisFileError . FileIOError fpath)
    $ LBS.writeFile fpath genesisCombined
 where
  combineAndEncode :: Aeson.Value -> Aeson.Value -> Either ShelleyGenesisCmdError LBS.ByteString
  combineAndEncode (Object sgO) (Object agO) = Right $ encodePretty $ sgO <> agO
  combineAndEncode _sgWrong _agWrong = panic "combineAndEncode: Implement ShelleyGenesisCmdError constuctor"
-- -------------------------------------------------------------------------------------------------

readGenDelegsMap :: FilePath -> FilePath
                 -> ExceptT ShelleyGenesisCmdError IO
                            (Map (Hash GenesisKey)
                                 (Hash GenesisDelegateKey, Hash VrfKey))
readGenDelegsMap gendir deldir = do
    gkm <- readGenesisKeys gendir
    dkm <- readDelegateKeys deldir
    vkm <- readDelegateVrfKeys deldir

    let combinedMap :: Map Int (VerificationKey GenesisKey,
                                (VerificationKey GenesisDelegateKey,
                                 VerificationKey VrfKey))
        combinedMap =
          Map.intersectionWith (,)
            gkm
            (Map.intersectionWith (,)
               dkm vkm)

    -- All the maps should have an identical set of keys. Complain if not.
    let gkmExtra = gkm Map.\\ combinedMap
        dkmExtra = dkm Map.\\ combinedMap
        vkmExtra = vkm Map.\\ combinedMap
    unless (Map.null gkmExtra && Map.null dkmExtra && Map.null vkmExtra) $
      throwError $ ShelleyGenesisCmdMismatchedGenesisKeyFiles
                     (Map.keys gkm) (Map.keys dkm) (Map.keys vkm)

    let delegsMap :: Map (Hash GenesisKey)
                         (Hash GenesisDelegateKey, Hash VrfKey)
        delegsMap =
          Map.fromList [ (gh, (dh, vh))
                       | (g,(d,v)) <- Map.elems combinedMap
                       , let gh = verificationKeyHash g
                             dh = verificationKeyHash d
                             vh = verificationKeyHash v
                       ]

    pure delegsMap


readGenesisKeys :: FilePath -> ExceptT ShelleyGenesisCmdError IO
                                       (Map Int (VerificationKey GenesisKey))
readGenesisKeys gendir = do
  files <- liftIO (listDirectory gendir)
  fileIxs <- extractFileNameIndexes [ gendir </> file
                                    | file <- files
                                    , takeExtension file == ".vkey" ]
  firstExceptT ShelleyGenesisCmdTextEnvReadFileError $
    Map.fromList <$>
      sequence
        [ (,) ix <$> readKey file
        | (file, ix) <- fileIxs ]
  where
    readKey = newExceptT
              . readFileTextEnvelope (AsVerificationKey AsGenesisKey)

readDelegateKeys :: FilePath
                 -> ExceptT ShelleyGenesisCmdError IO
                            (Map Int (VerificationKey GenesisDelegateKey))
readDelegateKeys deldir = do
  files <- liftIO (listDirectory deldir)
  fileIxs <- extractFileNameIndexes [ deldir </> file
                                    | file <- files
                                    , takeExtensions file == ".vkey" ]
  firstExceptT ShelleyGenesisCmdTextEnvReadFileError $
    Map.fromList <$>
      sequence
        [ (,) ix <$> readKey file
        | (file, ix) <- fileIxs ]
  where
    readKey = newExceptT
            . readFileTextEnvelope (AsVerificationKey AsGenesisDelegateKey)

readDelegateVrfKeys :: FilePath -> ExceptT ShelleyGenesisCmdError IO
                                           (Map Int (VerificationKey VrfKey))
readDelegateVrfKeys deldir = do
  files <- liftIO (listDirectory deldir)
  fileIxs <- extractFileNameIndexes [ deldir </> file
                                    | file <- files
                                    , takeExtensions file == ".vrf.vkey" ]
  firstExceptT ShelleyGenesisCmdTextEnvReadFileError $
    Map.fromList <$>
      sequence
        [ (,) ix <$> readKey file
        | (file, ix) <- fileIxs ]
  where
    readKey = newExceptT
            . readFileTextEnvelope (AsVerificationKey AsVrfKey)


-- | The file path is of the form @"delegate-keys/delegate3.vkey"@.
-- This function reads the file and extracts the index (in this case 3).
--
extractFileNameIndex :: FilePath -> Maybe Int
extractFileNameIndex fp =
  case filter isDigit fp of
    [] -> Nothing
    xs -> readMaybe xs

extractFileNameIndexes :: [FilePath]
                       -> ExceptT ShelleyGenesisCmdError IO [(FilePath, Int)]
extractFileNameIndexes files = do
    case [ file | (file, Nothing) <- filesIxs ] of
      []     -> return ()
      files' -> throwError (ShelleyGenesisCmdFilesNoIndex files')
    case filter (\g -> length g > 1)
       . groupBy ((==) `on` snd)
       . sortBy (compare `on` snd)
       $ [ (file, ix) | (file, Just ix) <- filesIxs ] of
      [] -> return ()
      (g:_) -> throwError (ShelleyGenesisCmdFilesDupIndex (map fst g))

    return [ (file, ix) | (file, Just ix) <- filesIxs ]
  where
    filesIxs = [ (file, extractFileNameIndex file) | file <- files ]

readInitialFundAddresses :: FilePath -> NetworkId
                         -> ExceptT ShelleyGenesisCmdError IO [AddressInEra ShelleyEra]
readInitialFundAddresses utxodir nw = do
    files <- liftIO (listDirectory utxodir)
    vkeys <- firstExceptT ShelleyGenesisCmdTextEnvReadFileError $
               sequence
                 [ newExceptT $
                     readFileTextEnvelope (AsVerificationKey AsGenesisUTxOKey)
                                          (utxodir </> file)
                 | file <- files
                 , takeExtension file == ".vkey" ]
    return [ addr | vkey <- vkeys
           , let vkh  = verificationKeyHash (castVerificationKey vkey)
                 addr = makeShelleyAddressInEra nw (PaymentCredentialByKey vkh)
                                                NoStakeAddress
           ]


-- | Hash a genesis file
runGenesisHashFile :: GenesisFile -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisHashFile (GenesisFile fpath) = do
   content <- handleIOExceptT (ShelleyGenesisCmdGenesisFileError . FileIOError fpath) $
              BS.readFile fpath
   let gh :: Crypto.Hash Crypto.Blake2b_256 ByteString
       gh = Crypto.hashWith id content
   liftIO $ Text.putStrLn (Crypto.hashToTextAsHex gh)

--
-- Alonzo genesis
--

-- | In order to avoid introducing a separate Alonzo genesis file, we
-- have added additional fields to the Shelley genesis that are required
-- when hardforking to Alonzo. Unfortunately the 'ShelleyGenesis' 'FromJSON'
-- instance exists in cardano-ledger-specs so we must duplicate code for now.

readAlonzoGenesis
  :: FilePath
  -> ExceptT ShelleyGenesisCmdError IO Alonzo.AlonzoGenesis
readAlonzoGenesis fpath = do
  readAndDecode
    `catchError` \err ->
      case err of
        ShelleyGenesisCmdGenesisFileError (FileIOError _ ioe)
          | isDoesNotExistError ioe -> panic "Shelley genesis file not found."
        _                           -> left err

 where
  readAndDecode :: ExceptT ShelleyGenesisCmdError IO AlonzoGenesis
  readAndDecode = do
      lbs <- handleIOExceptT (ShelleyGenesisCmdGenesisFileError . FileIOError fpath) $ LBS.readFile fpath
      firstExceptT (ShelleyGenesisCmdAesonDecodeError fpath . Text.pack)
        . hoistEither $ Aeson.eitherDecode' lbs

