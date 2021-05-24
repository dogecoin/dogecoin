-- | This module provides a library interface that is intended to be
-- the complete API for Shelley covering everything, including
-- exposing constructors for the lower level types.
--

module Cardano.Api.Shelley
  ( module Cardano.Api,

    -- * Genesis
    ShelleyGenesis(..),
    shelleyGenesisDefaults,

    -- * Cryptographic key interface
    -- $keys
    VerificationKey(..),
    SigningKey(..),

    -- * Hashes
    Hash(..),

    -- * Payment addresses
    -- | Constructing and inspecting Shelley payment addresses
    Address(ShelleyAddress),
    toShelleyAddr,
    fromShelleyAddr,
    toShelleyStakeCredential,
    fromShelleyStakeCredential,
    NetworkId(Mainnet, Testnet),

    -- * Stake addresses
    PaymentCredential(..),
    StakeAddress(..),
    StakeAddressReference(..),
    StakeCredential(..),
    toShelleyStakeAddr,
    fromShelleyStakeAddr,
    fromShelleyStakeReference,
    fromShelleyPaymentCredential,

    -- * Building transactions
    -- | Constructing and inspecting transactions
    TxBody(ShelleyTxBody),
    TxId(TxId),
    toShelleyTxId,
    fromShelleyTxId,
    TxIn(TxIn),
    toShelleyTxIn,
    fromShelleyTxIn,
    TxOut(TxOut),
    toShelleyTxOut,
    fromShelleyTxOut,
    TxIx(TxIx),
    Lovelace(Lovelace),
    toShelleyLovelace,
    fromShelleyLovelace,
    toMaryValue,
    fromMaryValue,
    calcMinimumDeposit,

    -- * Signing transactions
    -- | Creating transaction witnesses one by one, or all in one go.
    Tx(ShelleyTx),

    -- ** Incremental signing and separate witnesses
    KeyWitness
      ( ShelleyBootstrapWitness
      , ShelleyKeyWitness
      ),
    ShelleyWitnessSigningKey
      ( WitnessPaymentKey
      , WitnessPaymentExtendedKey
      , WitnessStakeKey
      , WitnessStakeExtendedKey
      , WitnessStakePoolKey
      , WitnessGenesisKey
      , WitnessGenesisExtendedKey
      , WitnessGenesisDelegateKey
      , WitnessGenesisDelegateExtendedKey
      ),
    ShelleySigningKey,
    getShelleyKeyWitnessVerificationKey,
    makeShelleySignature,
    toShelleySigningKey,

    -- * Transaction metadata
    -- | Embedding additional structured data within transactions.
    toShelleyMetadata,
    fromShelleyMetadata,

    -- * Protocol parameters
    ProtocolParameters(..),

    -- * Scripts
    toShelleyScript,
    toShelleyMultiSig,
    fromShelleyMultiSig,
    toAllegraTimelock,
    fromAllegraTimelock,
    toShelleyScriptHash,
    fromShelleyScriptHash,
    PlutusScript(..),

    -- * Certificates
    Certificate (..),
    toShelleyCertificate,
    fromShelleyCertificate,

    -- ** Operational certificates
    OperationalCertificate(OperationalCertificate),
    OperationalCertificateIssueCounter(OperationalCertificateIssueCounter),
    OperationalCertIssueError(..),

    -- * Stake Pool
    StakePoolMetadata(StakePoolMetadata),
    stakePoolName,
    stakePoolDescription,
    stakePoolTicker,
    stakePoolHomepage,
    StakePoolMetadataReference(StakePoolMetadataReference),
    stakePoolMetadataURL,
    stakePoolMetadataHash,
    StakePoolParameters(StakePoolParameters),
    stakePoolId,
    stakePoolVRF,
    stakePoolCost,
    stakePoolMargin,
    stakePoolRewardAccount,
    stakePoolPledge,
    stakePoolOwners,
    stakePoolRelays,
    stakePoolMetadata,
    StakePoolRelay
      ( StakePoolRelayIp
      , StakePoolRelayDnsARecord
      , StakePoolRelayDnsSrvRecord
      ),
    EpochNo(..),

    -- ** Stake pool operator's keys
    StakePoolKey,
    PoolId,

    -- ** KES keys
    KesKey,
    KESPeriod(..),

    -- ** VRF keys
    VrfKey,

    -- ** Low level protocol interaction with a Cardano node
    LocalNodeConnectInfo(LocalNodeConnectInfo),
    ShelleyMode,
    ConsensusMode
      ( ShelleyMode
      ),
    LocalNodeClientProtocols(LocalNodeClientProtocols),

    -- ** Shelley based eras
    ShelleyLedgerEra,


    -- ** Local State Query
    QueryInShelleyBasedEra(..),
    DebugLedgerState(..),
    ProtocolState(..),
    SerialisedDebugLedgerState(..),
    UTxO(..),

    -- ** Conversions
    --TODO: arrange not to export these
    toShelleyNetwork,
    fromShelleyPParams,

  ) where

import           Cardano.Api
import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.IPC
import           Cardano.Api.KeysPraos
import           Cardano.Api.KeysShelley
import           Cardano.Api.NetworkId
import           Cardano.Api.OperationalCertificate
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.Script
import           Cardano.Api.Shelley.Genesis
import           Cardano.Api.StakePoolMetadata
import           Cardano.Api.Tx
import           Cardano.Api.TxBody
import           Cardano.Api.TxMetadata
import           Cardano.Api.Value
