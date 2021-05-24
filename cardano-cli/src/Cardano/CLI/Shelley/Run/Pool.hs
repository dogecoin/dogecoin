module Cardano.CLI.Shelley.Run.Pool
  ( ShelleyPoolCmdError(ShelleyPoolCmdReadFileError)
  , renderShelleyPoolCmdError
  , runPoolCmd
  ) where

import           Cardano.Prelude

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                     newExceptT)

import qualified Data.ByteString.Char8 as BS

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Shelley.Spec.Ledger.Slot as Shelley

import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Key (InputDecodeError, VerificationKeyOrFile,
                     readVerificationKeyOrFile)
import           Cardano.CLI.Types (OutputFormat (..))


data ShelleyPoolCmdError
  = ShelleyPoolCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyPoolCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyPoolCmdWriteFileError !(FileError ())
  | ShelleyPoolCmdMetadataValidationError !StakePoolMetadataValidationError
  deriving Show

renderShelleyPoolCmdError :: ShelleyPoolCmdError -> Text
renderShelleyPoolCmdError err =
  case err of
    ShelleyPoolCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyPoolCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyPoolCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyPoolCmdMetadataValidationError validationErr ->
      "Error validating stake pool metadata: " <> Text.pack (displayError validationErr)



runPoolCmd :: PoolCmd -> ExceptT ShelleyPoolCmdError IO ()
runPoolCmd (PoolRegistrationCert sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp) =
  runStakePoolRegistrationCert sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp
runPoolCmd (PoolRetirementCert sPvkeyFp retireEpoch outfp) =
  runStakePoolRetirementCert sPvkeyFp retireEpoch outfp
runPoolCmd (PoolGetId sPvkey outputFormat) = runPoolId sPvkey outputFormat
runPoolCmd (PoolMetadataHash poolMdFile mOutFile) = runPoolMetadataHash poolMdFile mOutFile


--
-- Stake pool command implementations
--

-- | Create a stake pool registration cert.
-- TODO: Metadata and more stake pool relay support to be
-- added in the future.
runStakePoolRegistrationCert
  :: VerificationKeyOrFile StakePoolKey
  -- ^ Stake pool verification key.
  -> VerificationKeyOrFile VrfKey
  -- ^ VRF Verification key.
  -> Lovelace
  -- ^ Pool pledge.
  -> Lovelace
  -- ^ Pool cost.
  -> Rational
  -- ^ Pool margin.
  -> VerificationKeyOrFile StakeKey
  -- ^ Stake verification key for reward account.
  -> [VerificationKeyOrFile StakeKey]
  -- ^ Pool owner stake verification key(s).
  -> [StakePoolRelay]
  -- ^ Stake pool relays.
  -> Maybe StakePoolMetadataReference
  -- ^ Stake pool metadata.
  -> NetworkId
  -> OutputFile
  -> ExceptT ShelleyPoolCmdError IO ()
runStakePoolRegistrationCert
  stakePoolVerKeyOrFile
  vrfVerKeyOrFile
  pldg
  pCost
  pMrgn
  rwdStakeVerKeyOrFile
  ownerStakeVerKeyOrFiles
  relays
  mbMetadata
  network
  (OutputFile outfp) = do
    -- Pool verification key
    stakePoolVerKey <- firstExceptT ShelleyPoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakePoolKey stakePoolVerKeyOrFile
    let stakePoolId' = verificationKeyHash stakePoolVerKey

    -- VRF verification key
    vrfVerKey <- firstExceptT ShelleyPoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsVrfKey vrfVerKeyOrFile
    let vrfKeyHash' = verificationKeyHash vrfVerKey

    -- Pool reward account
    rwdStakeVerKey <- firstExceptT ShelleyPoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakeKey rwdStakeVerKeyOrFile
    let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
        rewardAccountAddr = makeStakeAddress network stakeCred

    -- Pool owner(s)
    sPoolOwnerVkeys <-
      mapM
        (firstExceptT ShelleyPoolCmdReadKeyFileError
          . newExceptT
          . readVerificationKeyOrFile AsStakeKey
        )
        ownerStakeVerKeyOrFiles
    let stakePoolOwners' = map verificationKeyHash sPoolOwnerVkeys

    let stakePoolParams =
          StakePoolParameters
            { stakePoolId = stakePoolId'
            , stakePoolVRF = vrfKeyHash'
            , stakePoolCost = pCost
            , stakePoolMargin = pMrgn
            , stakePoolRewardAccount = rewardAccountAddr
            , stakePoolPledge = pldg
            , stakePoolOwners = stakePoolOwners'
            , stakePoolRelays = relays
            , stakePoolMetadata = mbMetadata
            }

    let registrationCert = makeStakePoolRegistrationCertificate stakePoolParams

    firstExceptT ShelleyPoolCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope outfp (Just registrationCertDesc) registrationCert
  where
    registrationCertDesc :: TextEnvelopeDescr
    registrationCertDesc = "Stake Pool Registration Certificate"

runStakePoolRetirementCert
  :: VerificationKeyOrFile StakePoolKey
  -> Shelley.EpochNo
  -> OutputFile
  -> ExceptT ShelleyPoolCmdError IO ()
runStakePoolRetirementCert stakePoolVerKeyOrFile retireEpoch (OutputFile outfp) = do
    -- Pool verification key
    stakePoolVerKey <- firstExceptT ShelleyPoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakePoolKey stakePoolVerKeyOrFile

    let stakePoolId' = verificationKeyHash stakePoolVerKey
        retireCert = makeStakePoolRetirementCertificate stakePoolId' retireEpoch

    firstExceptT ShelleyPoolCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope outfp (Just retireCertDesc) retireCert
  where
    retireCertDesc :: TextEnvelopeDescr
    retireCertDesc = "Stake Pool Retirement Certificate"

runPoolId
  :: VerificationKeyOrFile StakePoolKey
  -> OutputFormat
  -> ExceptT ShelleyPoolCmdError IO ()
runPoolId verKeyOrFile outputFormat = do
    stakePoolVerKey <- firstExceptT ShelleyPoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakePoolKey verKeyOrFile
    liftIO $
      case outputFormat of
        OutputFormatHex ->
          BS.putStrLn $ serialiseToRawBytesHex (verificationKeyHash stakePoolVerKey)
        OutputFormatBech32 ->
          Text.putStrLn $ serialiseToBech32 (verificationKeyHash stakePoolVerKey)

runPoolMetadataHash :: PoolMetadataFile -> Maybe OutputFile -> ExceptT ShelleyPoolCmdError IO ()
runPoolMetadataHash (PoolMetadataFile poolMDPath) mOutFile = do
  metadataBytes <- handleIOExceptT (ShelleyPoolCmdReadFileError . FileIOError poolMDPath) $
    BS.readFile poolMDPath
  (_metadata, metadataHash) <-
      firstExceptT ShelleyPoolCmdMetadataValidationError
    . hoistEither
    $ validateAndHashStakePoolMetadata metadataBytes
  case mOutFile of
    Nothing -> liftIO $ BS.putStrLn (serialiseToRawBytesHex metadataHash)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyPoolCmdWriteFileError . FileIOError fpath)
        $ BS.writeFile fpath (serialiseToRawBytesHex metadataHash)
