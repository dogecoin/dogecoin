{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Shelley CLI command types
module Cardano.CLI.Shelley.Commands
  ( -- * CLI command types
    ShelleyCommand (..)
  , AddressCmd (..)
  , StakeAddressCmd (..)
  , KeyCmd (..)
  , TransactionCmd (..)
  , NodeCmd (..)
  , PoolCmd (..)
  , QueryCmd (..)
  , GovernanceCmd (..)
  , GenesisCmd (..)
  , TextViewCmd (..)
  , renderShelleyCommand

    -- * CLI flag types
  , AddressKeyType (..)
  , ByronKeyType (..)
  , ByronKeyFormat (..)
  , CardanoAddressKeyType (..)
  , GenesisDir (..)
  , TxInCount (..)
  , TxOutCount (..)
  , TxShelleyWitnessCount (..)
  , TxByronWitnessCount (..)
  , SomeKeyFile (..)
  , OpCertCounterFile (..)
  , OutputFile (..)
  , ProtocolParamsFile (..)
  , ProtocolParamsSourceSpec (..)
  , WitnessFile (..)
  , TxBodyFile (..)
  , TxFile (..)
  , InputTxFile (..)
  , VerificationKeyBase64 (..)
  , GenesisKeyFile (..)
  , MetadataFile (..)
  , PoolId (..)
  , PoolMetadataFile (..)
  , PrivKeyFile (..)
  , BlockId (..)
  , WitnessSigningData (..)
  , ColdVerificationKeyOrFile (..)
  ) where

import           Data.Text (Text)
import           Prelude

import           Cardano.Api.Shelley hiding (PoolId)

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))

import           Cardano.CLI.Shelley.Key (PaymentVerifier, StakeVerifier, VerificationKeyOrFile,
                   VerificationKeyOrHashOrFile, VerificationKeyTextOrFile)
import           Cardano.CLI.Types

import           Shelley.Spec.Ledger.TxBody (MIRPot)
--
-- Shelley CLI command data types
--

-- | All the CLI subcommands under \"shelley\".
--
data ShelleyCommand
  = AddressCmd      AddressCmd
  | StakeAddressCmd StakeAddressCmd
  | KeyCmd          KeyCmd
  | TransactionCmd  TransactionCmd
  | NodeCmd         NodeCmd
  | PoolCmd         PoolCmd
  | QueryCmd        QueryCmd
  | GovernanceCmd   GovernanceCmd
  | GenesisCmd      GenesisCmd
  | TextViewCmd     TextViewCmd
  deriving Show

renderShelleyCommand :: ShelleyCommand -> Text
renderShelleyCommand sc =
  case sc of
    AddressCmd cmd -> renderAddressCmd cmd
    StakeAddressCmd cmd -> renderStakeAddressCmd cmd
    KeyCmd cmd -> renderKeyCmd cmd
    TransactionCmd cmd -> renderTransactionCmd cmd
    NodeCmd cmd -> renderNodeCmd cmd
    PoolCmd cmd -> renderPoolCmd cmd
    QueryCmd cmd -> renderQueryCmd cmd
    GovernanceCmd cmd -> renderGovernanceCmd cmd
    GenesisCmd cmd -> renderGenesisCmd cmd
    TextViewCmd cmd -> renderTextViewCmd cmd

data AddressCmd
  = AddressKeyGen AddressKeyType VerificationKeyFile SigningKeyFile
  | AddressKeyHash VerificationKeyTextOrFile (Maybe OutputFile)
  | AddressBuild
      PaymentVerifier
      (Maybe StakeVerifier)
      NetworkId
      (Maybe OutputFile)
  | AddressBuildMultiSig ScriptFile NetworkId (Maybe OutputFile)
  | AddressInfo Text (Maybe OutputFile)
  deriving Show


renderAddressCmd :: AddressCmd -> Text
renderAddressCmd cmd =
  case cmd of
    AddressKeyGen {} -> "address key-gen"
    AddressKeyHash {} -> "address key-hash"
    AddressBuild {} -> "address build"
    AddressBuildMultiSig {} -> "address build-script"
    AddressInfo {} -> "address info"

data StakeAddressCmd
  = StakeAddressKeyGen VerificationKeyFile SigningKeyFile
  | StakeAddressKeyHash (VerificationKeyOrFile StakeKey) (Maybe OutputFile)
  | StakeAddressBuild (VerificationKeyOrFile StakeKey) NetworkId (Maybe OutputFile)
  | StakeKeyRegistrationCert (VerificationKeyOrFile StakeKey) OutputFile
  | StakeKeyDelegationCert
      (VerificationKeyOrFile StakeKey)
      (VerificationKeyOrHashOrFile StakePoolKey)
      OutputFile
  | StakeKeyDeRegistrationCert (VerificationKeyOrFile StakeKey) OutputFile
  deriving Show

renderStakeAddressCmd :: StakeAddressCmd -> Text
renderStakeAddressCmd cmd =
  case cmd of
    StakeAddressKeyGen {} -> "stake-address key-gen"
    StakeAddressKeyHash {} -> "stake-address key-hash"
    StakeAddressBuild {} -> "stake-address build"
    StakeKeyRegistrationCert {} -> "stake-address registration-certificate"
    StakeKeyDelegationCert {} -> "stake-address delegation-certificate"
    StakeKeyDeRegistrationCert {} -> "stake-address deregistration-certificate"

data KeyCmd
  = KeyGetVerificationKey SigningKeyFile VerificationKeyFile
  | KeyNonExtendedKey  VerificationKeyFile VerificationKeyFile
  | KeyConvertByronKey (Maybe Text) ByronKeyType SomeKeyFile OutputFile
  | KeyConvertByronGenesisVKey VerificationKeyBase64 OutputFile
  | KeyConvertITNStakeKey SomeKeyFile OutputFile
  | KeyConvertITNExtendedToStakeKey SomeKeyFile OutputFile
  | KeyConvertITNBip32ToStakeKey SomeKeyFile OutputFile
  | KeyConvertCardanoAddressSigningKey CardanoAddressKeyType SigningKeyFile OutputFile
  deriving Show

renderKeyCmd :: KeyCmd -> Text
renderKeyCmd cmd =
  case cmd of
    KeyGetVerificationKey {} -> "key verification-key"
    KeyNonExtendedKey {} -> "key non-extended-key"
    KeyConvertByronKey {} -> "key convert-byron-key"
    KeyConvertByronGenesisVKey {} -> "key convert-byron-genesis-key"
    KeyConvertITNStakeKey {} -> "key convert-itn-key"
    KeyConvertITNExtendedToStakeKey {} -> "key convert-itn-extended-key"
    KeyConvertITNBip32ToStakeKey {} -> "key convert-itn-bip32-key"
    KeyConvertCardanoAddressSigningKey {} -> "key convert-cardano-address-signing-key"

data TransactionCmd
  = TxBuildRaw
      AnyCardanoEra
      [(TxIn, Maybe ScriptFile)]
      -- ^ Transaction inputs with optional spending scripts
      [TxOutAnyEra]
      (Maybe (Value, [ScriptFile]))
      -- ^ Multi-Asset value with script witness
      (Maybe SlotNo)
      -- ^ Transaction lower bound
      (Maybe SlotNo)
      -- ^ Transaction upper bound
      (Maybe Lovelace)
      -- ^ Tx fee
      [(CertificateFile, Maybe ScriptFile)]
      -- ^ Certificates with potential script witness
      [(StakeAddress, Lovelace, Maybe ScriptFile)]
      TxMetadataJsonSchema
      [ScriptFile]
      -- ^ Auxillary scripts
      [MetadataFile]
      (Maybe UpdateProposalFile)
      TxBodyFile
  | TxSign TxBodyFile [WitnessSigningData] (Maybe NetworkId) TxFile
  | TxCreateWitness TxBodyFile WitnessSigningData (Maybe NetworkId) OutputFile
  | TxAssembleTxBodyWitness TxBodyFile [WitnessFile] OutputFile
  | TxSubmit AnyConsensusModeParams NetworkId FilePath
  | TxMintedPolicyId ScriptFile
  | TxCalculateMinFee
      TxBodyFile
      (Maybe NetworkId)
      ProtocolParamsSourceSpec
      TxInCount
      TxOutCount
      TxShelleyWitnessCount
      TxByronWitnessCount
  | TxCalculateMinValue
      ProtocolParamsSourceSpec
      Value
  | TxGetTxId InputTxFile
  | TxView InputTxFile
  deriving Show

data InputTxFile = InputTxBodyFile TxBodyFile | InputTxFile TxFile
  deriving Show

data ProtocolParamsSourceSpec
  = ParamsFromGenesis !GenesisFile
    -- ^ We allow an appropriately forewarned user to obtain protocol params
    --   directly from the genesis file, which allows them to avoid running
    --   the node in case they would like to estimate the fee using the
    --   blockchain's initial protocol parameters.
  | ParamsFromFile !ProtocolParamsFile
    -- ^ Obtain protocol parameters from a file structured by the
    --   'cardano-api' 'ProtocolParameters' data type.
  deriving Show

renderTransactionCmd :: TransactionCmd -> Text
renderTransactionCmd cmd =
  case cmd of
    TxBuildRaw {} -> "transaction build-raw"
    TxSign {} -> "transaction sign"
    TxCreateWitness {} -> "transaction witness"
    TxAssembleTxBodyWitness {} -> "transaction sign-witness"
    TxSubmit {} -> "transaction submit"
    TxMintedPolicyId {} -> "transaction policyid"
    TxCalculateMinFee {} -> "transaction calculate-min-fee"
    TxCalculateMinValue {} -> "transaction calculate-min-value"
    TxGetTxId {} -> "transaction txid"
    TxView {} -> "transaction view"

data NodeCmd
  = NodeKeyGenCold VerificationKeyFile SigningKeyFile OpCertCounterFile
  | NodeKeyGenKES  VerificationKeyFile SigningKeyFile
  | NodeKeyGenVRF  VerificationKeyFile SigningKeyFile
  | NodeKeyHashVRF  (VerificationKeyOrFile VrfKey) (Maybe OutputFile)
  | NodeNewCounter ColdVerificationKeyOrFile Word OpCertCounterFile
  | NodeIssueOpCert (VerificationKeyOrFile KesKey) SigningKeyFile OpCertCounterFile
                    KESPeriod OutputFile
  deriving Show

renderNodeCmd :: NodeCmd -> Text
renderNodeCmd cmd = do
  case cmd of
    NodeKeyGenCold {} -> "node key-gen"
    NodeKeyGenKES {} -> "node key-gen-KES"
    NodeKeyGenVRF {} -> "node key-gen-VRF"
    NodeKeyHashVRF {} -> "node key-hash-VRF"
    NodeNewCounter {} -> "node new-counter"
    NodeIssueOpCert{} -> "node issue-op-cert"


data PoolCmd
  = PoolRegistrationCert
      (VerificationKeyOrFile StakePoolKey)
      -- ^ Stake pool verification key.
      (VerificationKeyOrFile VrfKey)
      -- ^ VRF Verification key.
      Lovelace
      -- ^ Pool pledge.
      Lovelace
      -- ^ Pool cost.
      Rational
      -- ^ Pool margin.
      (VerificationKeyOrFile StakeKey)
      -- ^ Reward account verification staking key.
      [VerificationKeyOrFile StakeKey]
      -- ^ Pool owner verification staking key(s).
      [StakePoolRelay]
      -- ^ Stake pool relays.
      (Maybe StakePoolMetadataReference)
      -- ^ Stake pool metadata.
      NetworkId
      OutputFile
  | PoolRetirementCert
      (VerificationKeyOrFile StakePoolKey)
      -- ^ Stake pool verification key.
      EpochNo
      -- ^ Epoch in which to retire the stake pool.
      OutputFile
  | PoolGetId (VerificationKeyOrFile StakePoolKey) OutputFormat
  | PoolMetadataHash PoolMetadataFile (Maybe OutputFile)
  deriving Show

renderPoolCmd :: PoolCmd -> Text
renderPoolCmd cmd =
  case cmd of
    PoolRegistrationCert {} -> "stake-pool registration-certificate"
    PoolRetirementCert {} -> "stake-pool deregistration-certificate"
    PoolGetId {} -> "stake-pool id"
    PoolMetadataHash {} -> "stake-pool metadata-hash"

data QueryCmd =
    QueryProtocolParameters' AnyConsensusModeParams NetworkId (Maybe OutputFile)
  | QueryTip AnyConsensusModeParams NetworkId (Maybe OutputFile)
  | QueryStakeDistribution' AnyConsensusModeParams NetworkId (Maybe OutputFile)
  | QueryStakeAddressInfo AnyConsensusModeParams StakeAddress NetworkId (Maybe OutputFile)
  | QueryUTxO' AnyConsensusModeParams QueryFilter NetworkId (Maybe OutputFile)
  | QueryDebugLedgerState' AnyConsensusModeParams NetworkId (Maybe OutputFile)
  | QueryProtocolState' AnyConsensusModeParams NetworkId (Maybe OutputFile)
  | QueryStakeSnapshot' AnyConsensusModeParams NetworkId (Hash StakePoolKey)
  | QueryPoolParams' AnyConsensusModeParams NetworkId (Hash StakePoolKey)
  deriving Show

renderQueryCmd :: QueryCmd -> Text
renderQueryCmd cmd =
  case cmd of
    QueryProtocolParameters' {} -> "query protocol-parameters "
    QueryTip {} -> "query tip"
    QueryStakeDistribution' {} -> "query stake-distribution"
    QueryStakeAddressInfo {} -> "query stake-address-info"
    QueryUTxO' {} -> "query utxo"
    QueryDebugLedgerState' {} -> "query ledger-state"
    QueryProtocolState' {} -> "query protocol-state"
    QueryStakeSnapshot' {} -> "query stake-snapshot"
    QueryPoolParams' {} -> "query pool-params"

data GovernanceCmd
  = GovernanceMIRPayStakeAddressesCertificate
      MIRPot
      [StakeAddress]
      [Lovelace]
      OutputFile
  | GovernanceMIRTransfer Lovelace OutputFile TransferDirection
  | GovernanceGenesisKeyDelegationCertificate
      (VerificationKeyOrHashOrFile GenesisKey)
      (VerificationKeyOrHashOrFile GenesisDelegateKey)
      (VerificationKeyOrHashOrFile VrfKey)
      OutputFile
  | GovernanceUpdateProposal OutputFile EpochNo
                             [VerificationKeyFile]
                             ProtocolParametersUpdate
  deriving Show

renderGovernanceCmd :: GovernanceCmd -> Text
renderGovernanceCmd cmd =
  case cmd of
    GovernanceGenesisKeyDelegationCertificate {} -> "governance create-genesis-key-delegation-certificate"
    GovernanceMIRPayStakeAddressesCertificate {} -> "governance create-mir-certificate stake-addresses"
    GovernanceMIRTransfer _ _ TransferToTreasury -> "governance create-mir-certificate transfer-to-treasury"
    GovernanceMIRTransfer _ _ TransferToReserves -> "governance create-mir-certificate transfer-to-reserves"
    GovernanceUpdateProposal {} -> "governance create-update-proposal"

data TextViewCmd
  = TextViewInfo !FilePath (Maybe OutputFile)
  deriving Show


renderTextViewCmd :: TextViewCmd -> Text
renderTextViewCmd (TextViewInfo _ _) = "text-view decode-cbor"

data GenesisCmd
  = GenesisCreate GenesisDir Word Word (Maybe SystemStart) (Maybe Lovelace) NetworkId
  | GenesisCreateStaked GenesisDir Word Word Word Word (Maybe SystemStart) (Maybe Lovelace) Lovelace NetworkId Word Word Word
  | GenesisKeyGenGenesis VerificationKeyFile SigningKeyFile
  | GenesisKeyGenDelegate VerificationKeyFile SigningKeyFile OpCertCounterFile
  | GenesisKeyGenUTxO VerificationKeyFile SigningKeyFile
  | GenesisCmdKeyHash VerificationKeyFile
  | GenesisVerKey VerificationKeyFile SigningKeyFile
  | GenesisTxIn VerificationKeyFile NetworkId (Maybe OutputFile)
  | GenesisAddr VerificationKeyFile NetworkId (Maybe OutputFile)
  | GenesisHashFile GenesisFile
  deriving Show

renderGenesisCmd :: GenesisCmd -> Text
renderGenesisCmd cmd =
  case cmd of
    GenesisCreate {} -> "genesis create"
    GenesisCreateStaked {} -> "genesis create-staked"
    GenesisKeyGenGenesis {} -> "genesis key-gen-genesis"
    GenesisKeyGenDelegate {} -> "genesis key-gen-delegate"
    GenesisKeyGenUTxO {} -> "genesis key-gen-utxo"
    GenesisCmdKeyHash {} -> "genesis key-hash"
    GenesisVerKey {} -> "genesis get-ver-key"
    GenesisTxIn {} -> "genesis initial-txin"
    GenesisAddr {} -> "genesis initial-addr"
    GenesisHashFile {} -> "genesis hash"

--
-- Shelley CLI flag/option data types
--

newtype ProtocolParamsFile
  = ProtocolParamsFile FilePath
  deriving (Show, Eq)

newtype TxInCount
  = TxInCount Int
  deriving Show

newtype TxOutCount
  = TxOutCount Int
  deriving Show

newtype TxShelleyWitnessCount
  = TxShelleyWitnessCount Int
  deriving Show

newtype TxByronWitnessCount
  = TxByronWitnessCount Int
  deriving Show

newtype BlockId
  = BlockId String -- Probably not a String
  deriving Show

newtype GenesisKeyFile
  = GenesisKeyFile FilePath
  deriving Show

data MetadataFile = MetadataFileJSON FilePath
                  | MetadataFileCBOR FilePath

  deriving Show

newtype OutputFile
  = OutputFile FilePath
  deriving Show

newtype PoolId
  = PoolId String -- Probably not a String
  deriving Show

newtype PoolMetadataFile = PoolMetadataFile
  { unPoolMetadataFile :: FilePath }
  deriving Show

newtype GenesisDir
  = GenesisDir FilePath
  deriving Show

-- | Either a verification or signing key, used for conversions and other
-- commands that make sense for both.
--
data SomeKeyFile
  = AVerificationKeyFile VerificationKeyFile
  | ASigningKeyFile SigningKeyFile
  deriving Show

data AddressKeyType
  = AddressKeyShelley
  | AddressKeyShelleyExtended
  | AddressKeyByron
  deriving Show

data ByronKeyType
  = ByronPaymentKey  ByronKeyFormat
  | ByronGenesisKey  ByronKeyFormat
  | ByronDelegateKey ByronKeyFormat
  deriving Show

data ByronKeyFormat = NonLegacyByronKeyFormat
                    | LegacyByronKeyFormat
  deriving Show

-- | The type of @cardano-address@ key.
data CardanoAddressKeyType
  = CardanoAddressShelleyPaymentKey
  | CardanoAddressShelleyStakeKey
  | CardanoAddressIcarusPaymentKey
  | CardanoAddressByronPaymentKey
  deriving Show

newtype OpCertCounterFile
  = OpCertCounterFile FilePath
  deriving Show

newtype PrivKeyFile
  = PrivKeyFile FilePath
  deriving Show

newtype WitnessFile
  = WitnessFile FilePath
  deriving Show

newtype TxBodyFile
  = TxBodyFile FilePath
  deriving Show

newtype TxFile
  = TxFile FilePath
  deriving Show

-- | A raw verification key given in Base64, and decoded into a ByteString.
newtype VerificationKeyBase64
  = VerificationKeyBase64 String
  deriving Show

-- | Data required to construct a witness.
data WitnessSigningData
  = KeyWitnessSigningData
      !SigningKeyFile
      -- ^ Path to a file that should contain a signing key.
      !(Maybe (Address ByronAddr))
      -- ^ An optionally specified Byron address.
      --
      -- If specified, both the network ID and derivation path are extracted
      -- from the address and used in the construction of the Byron witness.
  deriving Show

-- | Either a stake pool verification key, genesis delegate verification key,
-- or a path to a cold verification key file.
--
-- Note that a "cold verification key" refers to either a stake pool or
-- genesis delegate verification key.
--
-- TODO: A genesis delegate extended key should also be valid here.
data ColdVerificationKeyOrFile
  = ColdStakePoolVerificationKey !(VerificationKey StakePoolKey)
  | ColdGenesisDelegateVerificationKey !(VerificationKey GenesisDelegateKey)
  | ColdVerificationKeyFile !VerificationKeyFile
  deriving Show
