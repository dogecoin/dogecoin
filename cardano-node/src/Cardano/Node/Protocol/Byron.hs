{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.Protocol.Byron
  ( mkSomeConsensusProtocolByron
    -- * Errors
  , ByronProtocolInstantiationError(..)
  , renderByronProtocolInstantiationError

    -- * Reusable parts
  , readGenesis
  , readLeaderCredentials
  ) where


import           Cardano.Prelude
import           Control.Monad.Trans.Except.Extra (bimapExceptT, firstExceptT, hoistEither,
                   hoistMaybe, left)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text

import           Cardano.Api.Byron
import qualified Cardano.Api.Protocol.Types as Protocol

import qualified Cardano.Crypto.Hash as Crypto

import qualified Cardano.Crypto.Hashing as Byron.Crypto

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.UTxO as UTxO
import qualified Cardano.Chain.Update as Update
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)

import           Ouroboros.Consensus.Cardano
import qualified Ouroboros.Consensus.Cardano as Consensus

import           Cardano.Node.Types

import           Cardano.Node.Protocol.Types
import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.HardFork ()
import           Cardano.Tracing.OrphanInstances.Shelley ()


------------------------------------------------------------------------------
-- Byron protocol
--

-- | Make 'SomeConsensusProtocol' using the Byron instance.
--
-- This lets us handle multiple protocols in a generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
--
mkSomeConsensusProtocolByron
  :: NodeByronProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ByronProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolByron NodeByronProtocolConfiguration {
                           npcByronGenesisFile,
                           npcByronGenesisFileHash,
                           npcByronReqNetworkMagic,
                           npcByronPbftSignatureThresh,
                           npcByronApplicationName,
                           npcByronApplicationVersion,
                           npcByronSupportedProtocolVersionMajor,
                           npcByronSupportedProtocolVersionMinor,
                           npcByronSupportedProtocolVersionAlt
                         }
                         files = do
    genesisConfig <- readGenesis npcByronGenesisFile
                                 npcByronGenesisFileHash
                                 npcByronReqNetworkMagic

    optionalLeaderCredentials <- readLeaderCredentials genesisConfig files

    return $ SomeConsensusProtocol Protocol.ByronBlockType $ Protocol.ProtocolInfoArgsByron $ Consensus.ProtocolParamsByron {
        byronGenesis = genesisConfig,
        byronPbftSignatureThreshold =
          PBftSignatureThreshold <$> npcByronPbftSignatureThresh,
        byronProtocolVersion =
          Update.ProtocolVersion
            npcByronSupportedProtocolVersionMajor
            npcByronSupportedProtocolVersionMinor
            npcByronSupportedProtocolVersionAlt,
        byronSoftwareVersion =
          Update.SoftwareVersion
            npcByronApplicationName
            npcByronApplicationVersion,
        byronLeaderCredentials =
          optionalLeaderCredentials
        }

readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> RequiresNetworkMagic
            -> ExceptT ByronProtocolInstantiationError IO
                       Genesis.Config
readGenesis (GenesisFile file) mbExpectedGenesisHash ncReqNetworkMagic = do
    (genesisData, genesisHash) <- firstExceptT (GenesisReadError file) $
                                  Genesis.readGenesisData file
    checkExpectedGenesisHash genesisHash
    return Genesis.Config {
      Genesis.configGenesisData       = genesisData,
      Genesis.configGenesisHash       = genesisHash,
      Genesis.configReqNetMagic       = ncReqNetworkMagic,
      Genesis.configUTxOConfiguration = UTxO.defaultUTxOConfiguration
      --TODO: add config support for the UTxOConfiguration if needed
    }
  where
    checkExpectedGenesisHash :: Genesis.GenesisHash
                             -> ExceptT ByronProtocolInstantiationError IO ()
    checkExpectedGenesisHash actual' =
      case mbExpectedGenesisHash of
        Just expected | actual /= expected ->
            throwError (GenesisHashMismatch actual expected)
          where
            actual = fromByronGenesisHash actual'

        _ -> return ()

    fromByronGenesisHash :: Genesis.GenesisHash -> GenesisHash
    fromByronGenesisHash (Genesis.GenesisHash h) =
        GenesisHash
      . fromMaybe impossible
      . Crypto.hashFromBytes
      . Byron.Crypto.hashToBytes
      $ h
      where
        impossible =
          panic "fromByronGenesisHash: old and new crypto libs disagree on hash size"



readLeaderCredentials :: Genesis.Config
                      -> Maybe ProtocolFilepaths
                      -> ExceptT ByronProtocolInstantiationError IO
                                 (Maybe ByronLeaderCredentials)
readLeaderCredentials _ Nothing = return Nothing
readLeaderCredentials genesisConfig
                      (Just ProtocolFilepaths {
                        byronCertFile,
                        byronKeyFile
                      }) =
  case (byronCertFile, byronKeyFile) of
    (Nothing, Nothing) -> pure Nothing
    (Just _, Nothing) -> left SigningKeyFilepathNotSpecified
    (Nothing, Just _) -> left DelegationCertificateFilepathNotSpecified
    (Just delegCertFile, Just signingKeyFile) -> do

         signingKeyFileBytes <- liftIO $ LB.readFile signingKeyFile
         delegCertFileBytes <- liftIO $ LB.readFile delegCertFile
         ByronSigningKey signingKey <- hoistMaybe (SigningKeyDeserialiseFailure signingKeyFile)
                         $ deserialiseFromRawBytes (AsSigningKey AsByronKey) $ LB.toStrict signingKeyFileBytes
         delegCert  <- firstExceptT (CanonicalDecodeFailure delegCertFile)
                         . hoistEither
                         $ canonicalDecodePretty delegCertFileBytes

         bimapExceptT CredentialsError Just
           . hoistEither
           $ mkByronLeaderCredentials genesisConfig signingKey delegCert "Byron"



------------------------------------------------------------------------------
-- Byron Errors
--

data ByronProtocolInstantiationError =
    CanonicalDecodeFailure !FilePath !Text
  | GenesisHashMismatch !GenesisHash !GenesisHash -- actual, expected
  | DelegationCertificateFilepathNotSpecified
  | GenesisConfigurationError !FilePath !Genesis.ConfigurationError
  | GenesisReadError !FilePath !Genesis.GenesisDataError
  | CredentialsError !ByronLeaderCredentialsError
  | SigningKeyDeserialiseFailure !FilePath
  | SigningKeyFilepathNotSpecified
  deriving Show


renderByronProtocolInstantiationError :: ByronProtocolInstantiationError -> Text
renderByronProtocolInstantiationError pie =
  case pie of
    CanonicalDecodeFailure fp failure -> "Canonical decode failure in " <> toS fp
                                         <> " Canonical failure: " <> failure
    GenesisHashMismatch actual expected ->
        "Wrong Byron genesis file: the actual hash is " <> show actual
     <> ", but the expected Byron genesis hash given in the node configuration "
     <> "file is " <> show expected
    DelegationCertificateFilepathNotSpecified -> "Delegation certificate filepath not specified"
    --TODO: Implement configuration error render function in cardano-ledger
    GenesisConfigurationError fp genesisConfigError -> "Genesis configuration error in: " <> toS fp
                                                       <> " Error: " <> Text.pack (show genesisConfigError)
    GenesisReadError fp err ->  "There was an error parsing the genesis file: " <> toS fp
                                <> " Error: " <> Text.pack (show err)
    -- TODO: Implement ByronLeaderCredentialsError render function in ouroboros-network
    CredentialsError byronLeaderCredentialsError -> "Byron leader credentials error: " <> Text.pack (show byronLeaderCredentialsError)
    SigningKeyDeserialiseFailure fp -> "Signing key deserialisation error in: " <> toS fp
    SigningKeyFilepathNotSpecified -> "Signing key filepath not specified"
