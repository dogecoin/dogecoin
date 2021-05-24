module Cardano.CLI.Shelley.Run.StakeAddress
  ( ShelleyStakeAddressCmdError(ShelleyStakeAddressCmdReadKeyFileError)
  , renderShelleyStakeAddressCmdError
  , runStakeAddressCmd
  , runStakeAddressKeyGen
  ) where

import           Cardano.Prelude

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key (InputDecodeError, VerificationKeyOrFile,
                     VerificationKeyOrHashOrFile, readVerificationKeyOrFile,
                     readVerificationKeyOrHashOrFile)
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Types

data ShelleyStakeAddressCmdError
  = ShelleyStakeAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyStakeAddressCmdWriteFileError !(FileError ())
  deriving Show

renderShelleyStakeAddressCmdError :: ShelleyStakeAddressCmdError -> Text
renderShelleyStakeAddressCmdError err =
  case err of
    ShelleyStakeAddressCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyStakeAddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)


runStakeAddressCmd :: StakeAddressCmd -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressCmd (StakeAddressKeyGen vk sk) = runStakeAddressKeyGen vk sk
runStakeAddressCmd (StakeAddressKeyHash vk mOutputFp) = runStakeAddressKeyHash vk mOutputFp
runStakeAddressCmd (StakeAddressBuild vk nw mOutputFp) = runStakeAddressBuild vk nw mOutputFp
runStakeAddressCmd (StakeKeyRegistrationCert stkKeyVerKeyOrFp outputFp) =
  runStakeKeyRegistrationCert stkKeyVerKeyOrFp outputFp
runStakeAddressCmd (StakeKeyDelegationCert stkKeyVerKeyOrFp stkPoolVerKeyHashOrFp outputFp) =
  runStakeKeyDelegationCert stkKeyVerKeyOrFp stkPoolVerKeyHashOrFp outputFp
runStakeAddressCmd (StakeKeyDeRegistrationCert stkKeyVerKeyOrFp outputFp) =
  runStakeKeyDeRegistrationCert stkKeyVerKeyOrFp outputFp


--
-- Stake address command implementations
--

runStakeAddressKeyGen :: VerificationKeyFile -> SigningKeyFile -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressKeyGen (VerificationKeyFile vkFp) (SigningKeyFile skFp) = do
    skey <- liftIO $ generateSigningKey AsStakeKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyStakeAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skFp (Just skeyDesc) skey
    firstExceptT ShelleyStakeAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkFp (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Stake Signing Key"
    vkeyDesc = "Stake Verification Key"

runStakeAddressKeyHash
  :: VerificationKeyOrFile StakeKey
  -> Maybe OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressKeyHash stakeVerKeyOrFile mOutputFp = do
  vkey <- firstExceptT ShelleyStakeAddressCmdReadKeyFileError
    . newExceptT
    $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash

runStakeAddressBuild :: VerificationKeyOrFile StakeKey -> NetworkId -> Maybe OutputFile
                     -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressBuild stakeVerKeyOrFile network mOutputFp = do
    stakeVerKey <- firstExceptT ShelleyStakeAddressCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile

    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVerKey)
        stakeAddr = makeStakeAddress network stakeCred
        stakeAddrText = serialiseAddress stakeAddr

    case mOutputFp of
      Just (OutputFile fpath) -> liftIO $ Text.writeFile fpath stakeAddrText
      Nothing -> liftIO $ Text.putStrLn stakeAddrText


runStakeKeyRegistrationCert
  :: VerificationKeyOrFile StakeKey
  -> OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeKeyRegistrationCert stakeVerKeyOrFile (OutputFile oFp) = do
    stakeVerKey <- firstExceptT ShelleyStakeAddressCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile
    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVerKey)
        regCert = makeStakeAddressRegistrationCertificate stakeCred
    firstExceptT ShelleyStakeAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope oFp (Just regCertDesc) regCert
  where
    regCertDesc :: TextEnvelopeDescr
    regCertDesc = "Stake Address Registration Certificate"


runStakeKeyDelegationCert
  :: VerificationKeyOrFile StakeKey
  -- ^ Delegator stake verification key or verification key file.
  -> VerificationKeyOrHashOrFile StakePoolKey
  -- ^ Delegatee stake pool verification key or verification key file or
  -- verification key hash.
  -> OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeKeyDelegationCert stakeVerKeyOrFile poolVKeyOrHashOrFile (OutputFile outFp) = do
    stakeVkey <- firstExceptT ShelleyStakeAddressCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile

    poolStakeVKeyHash <-
      firstExceptT
        ShelleyStakeAddressCmdReadKeyFileError
        (newExceptT $ readVerificationKeyOrHashOrFile AsStakePoolKey poolVKeyOrHashOrFile)

    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVkey)
        delegCert = makeStakeAddressDelegationCertificate
                      stakeCred
                      poolStakeVKeyHash
    firstExceptT ShelleyStakeAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope outFp (Just delegCertDesc) delegCert
  where
    delegCertDesc :: TextEnvelopeDescr
    delegCertDesc = "Stake Address Delegation Certificate"


runStakeKeyDeRegistrationCert
  :: VerificationKeyOrFile StakeKey
  -> OutputFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeKeyDeRegistrationCert stakeVerKeyOrFile (OutputFile oFp) = do
    stakeVkey <- firstExceptT ShelleyStakeAddressCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile
    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVkey)
        deRegCert = makeStakeAddressDeregistrationCertificate stakeCred
    firstExceptT ShelleyStakeAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope oFp (Just deregCertDesc) deRegCert
  where
    deregCertDesc :: TextEnvelopeDescr
    deregCertDesc = "Stake Address Deregistration Certificate"
