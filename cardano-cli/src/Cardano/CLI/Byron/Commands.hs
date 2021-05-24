{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.CLI.Byron.Commands
  ( ByronCommand (..)
  , NodeCmd (..)
  , VerificationKeyFile (..)
  , NewVerificationKeyFile (..)
  , CertificateFile (..)
  , NewCertificateFile (..)
  ) where

import           Cardano.Prelude

import           Cardano.Chain.Update (InstallerHash (..), ProtocolVersion (..),
                     SoftwareVersion (..), SystemTag (..))

import           Cardano.Api (NetworkId, TxIn)
import           Cardano.Api.Byron (Address (..), ByronAddr, ByronEra,
                     ByronProtocolParametersUpdate (..), TxOut)

import           Cardano.CLI.Byron.Genesis
import           Cardano.CLI.Byron.Key
import           Cardano.CLI.Byron.Tx
import           Cardano.CLI.Types

import           Cardano.CLI.Shelley.Commands (ByronKeyFormat)

data ByronCommand =

  --- Node Related Commands ---
    NodeCmd NodeCmd

  --- Genesis Related Commands ---
  | Genesis
        NewDirectory
        GenesisParameters

  | PrintGenesisHash
        GenesisFile

  --- Key Related Commands ---
  | Keygen
        NewSigningKeyFile

  | ToVerification
        ByronKeyFormat
        SigningKeyFile
        NewVerificationKeyFile

  | PrettySigningKeyPublic
        ByronKeyFormat
        SigningKeyFile

  | MigrateDelegateKeyFrom
        SigningKeyFile
        -- ^ Old key
        NewSigningKeyFile
        -- ^ New Key

  | PrintSigningKeyAddress
        ByronKeyFormat
        NetworkId
        SigningKeyFile

  | GetLocalNodeTip
        NetworkId

    -----------------------------------

  | SubmitTx
        NetworkId
        TxFile
        -- ^ Filepath of transaction to submit.

  | SpendGenesisUTxO
        GenesisFile
        NetworkId
        ByronKeyFormat
        NewTxFile
        -- ^ Filepath of the newly created transaction.
        SigningKeyFile
        -- ^ Signing key of genesis UTxO owner.
        (Address ByronAddr)
        -- ^ Genesis UTxO address.
        [TxOut ByronEra]
        -- ^ Tx output.
  | SpendUTxO
        NetworkId
        ByronKeyFormat
        NewTxFile
        -- ^ Filepath of the newly created transaction.
        SigningKeyFile
        -- ^ Signing key of Tx underwriter.
        [TxIn]
        -- ^ Inputs available for spending to the Tx underwriter's key.
        [TxOut ByronEra]
        -- ^ Genesis UTxO output Address.

  | GetTxId TxFile

    --- Misc Commands ---

  | ValidateCBOR
        CBORObject
        -- ^ Type of the CBOR object
        FilePath

  | PrettyPrintCBOR
        FilePath
  deriving Show


data NodeCmd = CreateVote
               NetworkId
               SigningKeyFile
               FilePath -- filepath to update proposal
               Bool
               FilePath
             | UpdateProposal
               NetworkId
               SigningKeyFile
               ProtocolVersion
               SoftwareVersion
               SystemTag
               InstallerHash
               FilePath
               ByronProtocolParametersUpdate
             | SubmitUpdateProposal
               NetworkId
               FilePath
               -- ^ Update proposal filepath.
             | SubmitVote
               NetworkId
               FilePath
               -- ^ Vote filepath.
              deriving Show


newtype NewCertificateFile
  = NewCertificateFile { nFp :: FilePath }
  deriving (Eq, Show, IsString)
