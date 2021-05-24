{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Shelley.Run.Address
  ( ShelleyAddressCmdError(..)
  , SomeAddressVerificationKey(..)
  , buildShelleyAddress
  , renderShelleyAddressCmdError
  , runAddressCmd
  , runAddressKeyGen
  , readAddressVerificationKeyTextOrFile
  ) where

import           Cardano.Prelude hiding (putStrLn)

import           System.Console.ANSI

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Console.ANSI as ANSI
import qualified System.IO as IO

import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key (InputDecodeError, PaymentVerifier (..),
                   StakeVerifier (..), VerificationKeyTextOrFile,
                   VerificationKeyTextOrFileError (..), readVerificationKeyOrFile,
                   readVerificationKeyTextOrFileAnyOf, renderVerificationKeyTextOrFileError)
import           Cardano.CLI.Shelley.Script
import           Cardano.CLI.Shelley.Parsers (AddressCmd (..), AddressKeyType (..), OutputFile (..))
import           Cardano.CLI.Shelley.Run.Address.Info (ShelleyAddressInfoError, runAddressInfo)
import           Cardano.CLI.Types

data ShelleyAddressCmdError
  = ShelleyAddressCmdAddressInfoError !ShelleyAddressInfoError
  | ShelleyAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyAddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | ShelleyAddressCmdVerificationKeyTextOrFileError !VerificationKeyTextOrFileError
  | ShelleyAddressCmdWriteFileError !(FileError ())
  deriving Show

renderShelleyAddressCmdError :: ShelleyAddressCmdError -> Text
renderShelleyAddressCmdError err =
  case err of
    ShelleyAddressCmdAddressInfoError addrInfoErr ->
      Text.pack (displayError addrInfoErr)
    ShelleyAddressCmdReadKeyFileError fileErr ->
      Text.pack (displayError fileErr)
    ShelleyAddressCmdVerificationKeyTextOrFileError vkTextOrFileErr ->
      renderVerificationKeyTextOrFileError vkTextOrFileErr
    ShelleyAddressCmdReadScriptFileError fileErr ->
      Text.pack (displayError fileErr)
    ShelleyAddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)

runAddressCmd :: AddressCmd -> ExceptT ShelleyAddressCmdError IO ()
runAddressCmd cmd =
  case cmd of
    AddressKeyGen kt vkf skf -> runAddressKeyGen kt vkf skf
    AddressKeyHash vkf mOFp -> runAddressKeyHash vkf mOFp
    AddressBuild paymentVerifier mbStakeVerifier nw mOutFp -> runAddressBuild paymentVerifier mbStakeVerifier nw mOutFp
    AddressBuildMultiSig sFp nId mOutFp -> runAddressBuildScript sFp nId mOutFp
    AddressInfo txt mOFp -> firstExceptT ShelleyAddressCmdAddressInfoError $ runAddressInfo txt mOFp

runAddressKeyGen :: AddressKeyType
                 -> VerificationKeyFile
                 -> SigningKeyFile
                 -> ExceptT ShelleyAddressCmdError IO ()
runAddressKeyGen kt (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    case kt of
      AddressKeyShelley         -> generateAndWriteKeyFiles AsPaymentKey
      AddressKeyShelleyExtended -> generateAndWriteKeyFiles AsPaymentExtendedKey
      AddressKeyByron           -> generateAndWriteKeyFiles AsByronKey
  where
    generateAndWriteKeyFiles asType = do
      skey <- liftIO $ generateSigningKey asType
      let vkey = getVerificationKey skey
      firstExceptT ShelleyAddressCmdWriteFileError
        . newExceptT
        $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
      firstExceptT ShelleyAddressCmdWriteFileError
        . newExceptT
        $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey

    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Payment Signing Key"
    vkeyDesc = "Payment Verification Key"


runAddressKeyHash :: VerificationKeyTextOrFile
                  -> Maybe OutputFile
                  -> ExceptT ShelleyAddressCmdError IO ()
runAddressKeyHash vkeyTextOrFile mOutputFp = do
  vkey <- firstExceptT ShelleyAddressCmdVerificationKeyTextOrFileError $
            readAddressVerificationKeyTextOrFile vkeyTextOrFile

  let hexKeyHash = foldSomeAddressVerificationKey
                     (serialiseToRawBytesHex . verificationKeyHash) vkey

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash


runAddressBuild :: PaymentVerifier
                -> Maybe StakeVerifier
                -> NetworkId
                -> Maybe OutputFile
                -> ExceptT ShelleyAddressCmdError IO ()
runAddressBuild paymentVerifier mbStakeVerifier nw mOutFp = do
  outText <- case paymentVerifier of
    PaymentVerifierKey payVkeyTextOrFile -> do
      payVKey <- firstExceptT ShelleyAddressCmdVerificationKeyTextOrFileError $
        readAddressVerificationKeyTextOrFile payVkeyTextOrFile

      addr <- case payVKey of
        AByronVerificationKey vk ->
          return (AddressByron (makeByronAddress nw vk))

        APaymentVerificationKey vk ->
          AddressShelley <$> buildShelleyAddress vk mbStakeVerifier nw

        APaymentExtendedVerificationKey vk ->
          AddressShelley <$> buildShelleyAddress (castVerificationKey vk) mbStakeVerifier nw

        AGenesisUTxOVerificationKey vk ->
          AddressShelley <$> buildShelleyAddress (castVerificationKey vk) mbStakeVerifier nw

      return $ serialiseAddress (addr :: AddressAny)

    PaymentVerifierScriptFile (ScriptFile fp) -> do
      ScriptInAnyLang _lang script <-
        firstExceptT ShelleyAddressCmdReadScriptFileError $
          readFileScriptInAnyLang fp

      let payCred = PaymentCredentialByScript (hashScript script)

      serialiseAddress . makeShelleyAddress nw payCred <$> makeStakeAddressRef mbStakeVerifier

  case mOutFp of
    Just (OutputFile fpath) -> liftIO $ Text.writeFile fpath outText
    Nothing                 -> liftIO $ Text.putStr          outText

makeStakeAddressRef
  :: Maybe StakeVerifier
  -> ExceptT ShelleyAddressCmdError IO StakeAddressReference
makeStakeAddressRef mbStakeVerifier = do
  case mbStakeVerifier of
    Nothing -> pure NoStakeAddress
    Just stakeVerifier -> case stakeVerifier of
      StakeVerifierKey stkVkeyOrFile -> do
        mstakeVKey <- firstExceptT ShelleyAddressCmdReadKeyFileError $
          fmap Just $ newExceptT $ readVerificationKeyOrFile AsStakeKey stkVkeyOrFile

        return $ maybe NoStakeAddress
          (StakeAddressByValue . StakeCredentialByKey . verificationKeyHash)
          mstakeVKey

      StakeVerifierScriptFile (ScriptFile fp) -> do
        ScriptInAnyLang _lang script <-
          firstExceptT ShelleyAddressCmdReadScriptFileError $
            readFileScriptInAnyLang fp

        let stakeCred = StakeCredentialByScript (hashScript script)
        return (StakeAddressByValue stakeCred)

buildShelleyAddress
  :: VerificationKey PaymentKey
  -> Maybe StakeVerifier
  -> NetworkId
  -> ExceptT ShelleyAddressCmdError IO (Address ShelleyAddr)
buildShelleyAddress vkey mbStakeVerifier nw =
  makeShelleyAddress nw (PaymentCredentialByKey (verificationKeyHash vkey)) <$> makeStakeAddressRef mbStakeVerifier


--
-- Handling the variety of address key types
--

-- TODO: if we could make unions like this an instance of the Key class then
-- it would simplify some of the code above
data SomeAddressVerificationKey
  = AByronVerificationKey           (VerificationKey ByronKey)
  | APaymentVerificationKey         (VerificationKey PaymentKey)
  | APaymentExtendedVerificationKey (VerificationKey PaymentExtendedKey)
  | AGenesisUTxOVerificationKey     (VerificationKey GenesisUTxOKey)
  deriving (Show)

foldSomeAddressVerificationKey :: (forall keyrole. Key keyrole =>
                                   VerificationKey keyrole -> a)
                               -> SomeAddressVerificationKey -> a
foldSomeAddressVerificationKey f (AByronVerificationKey           vk) = f vk
foldSomeAddressVerificationKey f (APaymentVerificationKey         vk) = f vk
foldSomeAddressVerificationKey f (APaymentExtendedVerificationKey vk) = f vk
foldSomeAddressVerificationKey f (AGenesisUTxOVerificationKey     vk) = f vk

readAddressVerificationKeyTextOrFile
  :: VerificationKeyTextOrFile
  -> ExceptT VerificationKeyTextOrFileError IO SomeAddressVerificationKey
readAddressVerificationKeyTextOrFile vkTextOrFile =
    newExceptT $
      readVerificationKeyTextOrFileAnyOf bech32Types textEnvTypes vkTextOrFile
  where
    bech32Types =
      [ FromSomeType (AsVerificationKey AsPaymentKey)
                     APaymentVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentExtendedKey)
                     APaymentExtendedVerificationKey
      ]

    textEnvTypes =
      [ FromSomeType (AsVerificationKey AsByronKey)
                     AByronVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentKey)
                     APaymentVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentExtendedKey)
                     APaymentExtendedVerificationKey
      , FromSomeType (AsVerificationKey AsGenesisUTxOKey)
                     AGenesisUTxOVerificationKey
      ]

--
-- Multisig addresses
--

runAddressBuildScript
  :: ScriptFile
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyAddressCmdError IO ()
runAddressBuildScript scriptFile networkId mOutputFile = do
  liftIO deprecationWarning
  runAddressBuild (PaymentVerifierScriptFile scriptFile) Nothing networkId mOutputFile

deprecationWarning :: IO ()
deprecationWarning = do
  ANSI.hSetSGR IO.stderr [SetColor Foreground Vivid Yellow]
  IO.hPutStrLn IO.stderr "WARNING: This CLI command is deprecated.  Please use 'address build' command instead."
  ANSI.hSetSGR IO.stderr [Reset]
