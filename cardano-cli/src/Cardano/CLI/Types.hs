{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Types
  ( CBORObject (..)
  , CertificateFile (..)
  , GenesisFile (..)
  , OutputFormat (..)
  , QueryFilter (..)
  , SigningKeyFile (..)
  , SocketPath (..)
  , ScriptFile (..)
  , TransferDirection(..)
  , TxOutAnyEra (..)
  , UpdateProposalFile (..)
  , VerificationKeyFile (..)
  , Stakes (..)
  , Params (..)
  ) where

import           Cardano.Prelude

import           Data.Aeson (ToJSON (..), object, pairs, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import qualified Cardano.Chain.Slotting as Byron

import           Cardano.Api

import qualified Cardano.Ledger.Crypto as Crypto

import           Shelley.Spec.Ledger.TxBody (PoolParams (..))

-- | Specify what the CBOR file is
-- i.e a block, a tx, etc
data CBORObject = CBORBlockByron Byron.EpochSlots
                | CBORDelegationCertificateByron
                | CBORTxByron
                | CBORUpdateProposalByron
                | CBORVoteByron
                deriving Show

-- Encompasses stake certificates, stake pool certificates,
-- genesis delegate certificates and MIR certificates.
newtype CertificateFile = CertificateFile { unCertificateFile :: FilePath }
                          deriving newtype (Eq, Show)

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

instance FromJSON GenesisFile where
  parseJSON (Aeson.String genFp) = pure . GenesisFile $ Text.unpack genFp
  parseJSON invalid = panic $ "Parsing of GenesisFile failed due to type mismatch. "
                           <> "Encountered: " <> Text.pack (show invalid)

-- | The desired output format.
data OutputFormat
  = OutputFormatHex
  | OutputFormatBech32
  deriving (Eq, Show)

-- | UTxO query filtering options.
data QueryFilter
  = FilterByAddress !(Set AddressAny)
  | NoFilter
  deriving (Eq, Show)

-- | This data structure is used to allow nicely formatted output within the query stake-snapshot command.
--
-- "markPool", "setPool", "goPool" are the three ledger state stake snapshots (from most recent to least recent)
-- go is the snapshot that is used for the current epoch, set will be used in the next epoch,
-- mark for the epoch after that.  "markTotal", "setTotal", "goTotal" record the total active stake for each snapshot.
--
-- This information can be used by community tools to calculate upcoming leader schedules.
data Stakes =  Stakes
  { markPool :: Integer
  , setPool :: Integer
  , goPool :: Integer
  , markTotal :: Integer
  , setTotal :: Integer
  , goTotal :: Integer
  } deriving Show

-- | Pretty printing for stake information
instance ToJSON Stakes where
  toJSON (Stakes m s g mt st gt) = object
    [ "poolStakeMark" .= m
    , "poolStakeSet" .= s
    , "poolStakeGo" .= g
    , "activeStakeMark" .= mt
    , "activeStakeSet" .= st
    , "activeStakeGo" .= gt
    ]

  toEncoding  (Stakes m s g mt st gt) = pairs $ mconcat
    [ "poolStakeMark" .= m
    , "poolStakeSet" .= s
    , "poolStakeGo" .= g
    , "activeStakeMark" .= mt
    , "activeStakeSet" .= st
    , "activeStakeGo" .= gt
    ]

-- | This data structure is used to allow nicely formatted output in the query pool-params command.
-- params are the current pool parameter settings, futureparams are new parameters, retiringEpoch is the
-- epoch that has been set for pool retirement.  Any of these may be Nothing.
data Params crypto = Params
  { poolParameters :: Maybe (PoolParams crypto)
  , futurePoolParameters :: Maybe (PoolParams crypto)
  , retiringEpoch :: Maybe EpochNo
  } deriving Show

-- | Pretty printing for pool parameters
instance Crypto.Crypto crypto =>  ToJSON (Params crypto) where
  toJSON (Params p fp r) = object
    [ "poolParams" .= p
    , "futurePoolParams" .= fp
    , "retiring" .= r
    ]

  toEncoding (Params p fp r) = pairs $ mconcat
    [ "poolParams" .= p
    , "futurePoolParams" .= fp
    , "retiring" .= r
    ]

newtype SigningKeyFile = SigningKeyFile
  { unSigningKeyFile :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

newtype SocketPath = SocketPath { unSocketPath :: FilePath }

newtype UpdateProposalFile = UpdateProposalFile { unUpdateProposalFile :: FilePath }
                             deriving newtype (Eq, Show)

newtype VerificationKeyFile
  = VerificationKeyFile { unVerificationKeyFile :: FilePath }
  deriving (Eq, Show)

newtype ScriptFile = ScriptFile { unScriptFile :: FilePath }
                     deriving (Eq, Show)

-- | Determines the direction in which the MIR certificate will transfer ADA.
data TransferDirection = TransferToReserves | TransferToTreasury
                         deriving Show

-- | A TxOut value that is the superset of possibilities for any era: any
-- address type and allowing multi-asset values. This is used as the type for
-- values passed on the command line. It can be converted into the
-- era-dependent 'TxOutValue' type.
--
data TxOutAnyEra = TxOutAnyEra AddressAny Value
                   -- TODO alonzo: ^^ add support for tx out data
  deriving (Eq, Show)
