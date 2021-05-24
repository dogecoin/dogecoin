{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- | Certificates embedded in transactions
--
module Cardano.Api.Certificate (
    Certificate(..),

    -- * Registering stake address and delegating
    makeStakeAddressRegistrationCertificate,
    makeStakeAddressDeregistrationCertificate,
    makeStakeAddressDelegationCertificate,
    PoolId,

    -- * Registering stake pools
    makeStakePoolRegistrationCertificate,
    makeStakePoolRetirementCertificate,
    StakePoolParameters(..),
    StakePoolRelay(..),
    StakePoolMetadataReference(..),

    -- * Special certificates
    makeMIRCertificate,
    makeGenesisKeyDelegationCertificate,
    MIRTarget (..),

    -- * Internal conversion functions
    toShelleyCertificate,
    fromShelleyCertificate,
    toShelleyPoolParams,

    -- * Data family instances
    AsType(..)
  ) where

import           Prelude

import           Data.ByteString (ByteString)
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           Data.IP (IPv4, IPv6)
import           Network.Socket (PortNumber)

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Slotting.Slot (EpochNo (..))

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)

import           Shelley.Spec.Ledger.BaseTypes (maybeToStrictMaybe, strictMaybeToMaybe)
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Cardano.Ledger.Coin as Shelley (toDeltaCoin)
import           Shelley.Spec.Ledger.TxBody (MIRPot (..))
import qualified Shelley.Spec.Ledger.TxBody as Shelley

import           Cardano.Api.Address
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Hash
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysPraos
import           Cardano.Api.KeysShelley
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.StakePoolMetadata
import           Cardano.Api.Value


-- ----------------------------------------------------------------------------
-- Certificates embedded in transactions
--

data Certificate =

     -- Stake address certificates
     StakeAddressRegistrationCertificate   StakeCredential
   | StakeAddressDeregistrationCertificate StakeCredential
   | StakeAddressDelegationCertificate     StakeCredential PoolId

     -- Stake pool certificates
   | StakePoolRegistrationCertificate StakePoolParameters
   | StakePoolRetirementCertificate   PoolId EpochNo

     -- Special certificates
   | GenesisKeyDelegationCertificate (Hash GenesisKey)
                                     (Hash GenesisDelegateKey)
                                     (Hash VrfKey)
   | MIRCertificate MIRPot MIRTarget

  deriving stock (Eq, Show)
  deriving anyclass SerialiseAsCBOR

instance HasTypeProxy Certificate where
    data AsType Certificate = AsCertificate
    proxyToAsType _ = AsCertificate

instance ToCBOR Certificate where
    toCBOR = toCBOR . toShelleyCertificate

instance FromCBOR Certificate where
    fromCBOR = fromShelleyCertificate <$> fromCBOR

instance HasTextEnvelope Certificate where
    textEnvelopeType _ = "CertificateShelley"
    textEnvelopeDefaultDescr cert = case cert of
      StakeAddressRegistrationCertificate{}   -> "Stake address registration"
      StakeAddressDeregistrationCertificate{} -> "Stake address de-registration"
      StakeAddressDelegationCertificate{}     -> "Stake address delegation"
      StakePoolRegistrationCertificate{}      -> "Pool registration"
      StakePoolRetirementCertificate{}        -> "Pool retirement"
      GenesisKeyDelegationCertificate{}       -> "Genesis key delegation"
      MIRCertificate{}                        -> "MIR"

-- | The 'MIRTarget' determines the target of a 'MIRCertificate'.
-- A 'MIRCertificate' moves lovelace from either the reserves or the treasury
-- to either a collection of stake credentials or to the other pot.
data MIRTarget =

     -- | Use 'StakeAddressesMIR' to make the target of a 'MIRCertificate'
     -- a mapping of stake credentials to lovelace.
     StakeAddressesMIR [(StakeCredential, Lovelace)]

     -- | Use 'SendToReservesMIR' to make the target of a 'MIRCertificate'
     -- the reserves pot.
   | SendToReservesMIR Lovelace

     -- | Use 'SendToTreasuryMIR' to make the target of a 'MIRCertificate'
     -- the treasury pot.
   | SendToTreasuryMIR Lovelace
  deriving stock (Eq, Show)

-- ----------------------------------------------------------------------------
-- Stake pool parameters
--

type PoolId = Hash StakePoolKey

data StakePoolParameters =
     StakePoolParameters {
       stakePoolId            :: PoolId,
       stakePoolVRF           :: Hash VrfKey,
       stakePoolCost          :: Lovelace,
       stakePoolMargin        :: Rational,
       stakePoolRewardAccount :: StakeAddress,
       stakePoolPledge        :: Lovelace,
       stakePoolOwners        :: [Hash StakeKey],
       stakePoolRelays        :: [StakePoolRelay],
       stakePoolMetadata      :: Maybe StakePoolMetadataReference
     }
  deriving (Eq, Show)

data StakePoolRelay =

       -- | One or both of IPv4 & IPv6
       StakePoolRelayIp
          (Maybe IPv4) (Maybe IPv6) (Maybe PortNumber)

       -- | An DNS name pointing to a @A@ or @AAAA@ record.
     | StakePoolRelayDnsARecord
          ByteString (Maybe PortNumber)

       -- | A DNS name pointing to a @SRV@ record.
     | StakePoolRelayDnsSrvRecord
          ByteString

  deriving (Eq, Show)

data StakePoolMetadataReference =
     StakePoolMetadataReference {
       stakePoolMetadataURL  :: Text,
       stakePoolMetadataHash :: Hash StakePoolMetadata
     }
  deriving (Eq, Show)


-- ----------------------------------------------------------------------------
-- Constructor functions
--

makeStakeAddressRegistrationCertificate :: StakeCredential -> Certificate
makeStakeAddressRegistrationCertificate = StakeAddressRegistrationCertificate

makeStakeAddressDeregistrationCertificate :: StakeCredential -> Certificate
makeStakeAddressDeregistrationCertificate = StakeAddressDeregistrationCertificate

makeStakeAddressDelegationCertificate :: StakeCredential -> PoolId -> Certificate
makeStakeAddressDelegationCertificate = StakeAddressDelegationCertificate

makeStakePoolRegistrationCertificate :: StakePoolParameters -> Certificate
makeStakePoolRegistrationCertificate = StakePoolRegistrationCertificate

makeStakePoolRetirementCertificate :: PoolId -> EpochNo -> Certificate
makeStakePoolRetirementCertificate = StakePoolRetirementCertificate

makeGenesisKeyDelegationCertificate :: Hash GenesisKey
                                    -> Hash GenesisDelegateKey
                                    -> Hash VrfKey
                                    -> Certificate
makeGenesisKeyDelegationCertificate = GenesisKeyDelegationCertificate

makeMIRCertificate :: MIRPot -> MIRTarget -> Certificate
makeMIRCertificate = MIRCertificate


-- ----------------------------------------------------------------------------
-- Internal conversion functions
--

toShelleyCertificate :: Certificate -> Shelley.DCert StandardCrypto
toShelleyCertificate (StakeAddressRegistrationCertificate stakecred) =
    Shelley.DCertDeleg $
      Shelley.RegKey
        (toShelleyStakeCredential stakecred)

toShelleyCertificate (StakeAddressDeregistrationCertificate stakecred) =
    Shelley.DCertDeleg $
      Shelley.DeRegKey
        (toShelleyStakeCredential stakecred)

toShelleyCertificate (StakeAddressDelegationCertificate
                        stakecred (StakePoolKeyHash poolid)) =
    Shelley.DCertDeleg $
    Shelley.Delegate $
      Shelley.Delegation
        (toShelleyStakeCredential stakecred)
        poolid

toShelleyCertificate (StakePoolRegistrationCertificate poolparams) =
    Shelley.DCertPool $
      Shelley.RegPool
        (toShelleyPoolParams poolparams)

toShelleyCertificate (StakePoolRetirementCertificate
                       (StakePoolKeyHash poolid) epochno) =
    Shelley.DCertPool $
      Shelley.RetirePool
        poolid
        epochno

toShelleyCertificate (GenesisKeyDelegationCertificate
                       (GenesisKeyHash         genesiskh)
                       (GenesisDelegateKeyHash delegatekh)
                       (VrfKeyHash             vrfkh)) =
    Shelley.DCertGenesis $
      Shelley.GenesisDelegCert
        genesiskh
        delegatekh
        vrfkh

toShelleyCertificate (MIRCertificate mirpot (StakeAddressesMIR amounts)) =
    Shelley.DCertMir $
      Shelley.MIRCert
        mirpot
        (Shelley.StakeAddressesMIR $ Map.fromListWith (<>)
           [ (toShelleyStakeCredential sc, Shelley.toDeltaCoin . toShelleyLovelace $ v)
           | (sc, v) <- amounts ])

toShelleyCertificate (MIRCertificate mirPot (SendToReservesMIR amount)) =
    case mirPot of
      TreasuryMIR ->
        Shelley.DCertMir $
          Shelley.MIRCert
            TreasuryMIR
            (Shelley.SendToOppositePotMIR $ toShelleyLovelace amount)
      ReservesMIR ->
        error "toShelleyCertificate: Incorrect MIRPot specified. Expected TreasuryMIR but got ReservesMIR"

toShelleyCertificate (MIRCertificate mirPot (SendToTreasuryMIR amount)) =
    case mirPot of
      ReservesMIR ->
        Shelley.DCertMir $
          Shelley.MIRCert
            ReservesMIR
            (Shelley.SendToOppositePotMIR $ toShelleyLovelace amount)
      TreasuryMIR ->
        error "toShelleyCertificate: Incorrect MIRPot specified. Expected ReservesMIR but got TreasuryMIR"


fromShelleyCertificate :: Shelley.DCert StandardCrypto -> Certificate
fromShelleyCertificate (Shelley.DCertDeleg (Shelley.RegKey stakecred)) =
    StakeAddressRegistrationCertificate
      (fromShelleyStakeCredential stakecred)

fromShelleyCertificate (Shelley.DCertDeleg (Shelley.DeRegKey stakecred)) =
    StakeAddressDeregistrationCertificate
      (fromShelleyStakeCredential stakecred)

fromShelleyCertificate (Shelley.DCertDeleg
                         (Shelley.Delegate (Shelley.Delegation stakecred poolid))) =
    StakeAddressDelegationCertificate
      (fromShelleyStakeCredential stakecred)
      (StakePoolKeyHash poolid)

fromShelleyCertificate (Shelley.DCertPool (Shelley.RegPool poolparams)) =
    StakePoolRegistrationCertificate
      (fromShelleyPoolParams poolparams)

fromShelleyCertificate (Shelley.DCertPool (Shelley.RetirePool poolid epochno)) =
    StakePoolRetirementCertificate
      (StakePoolKeyHash poolid)
      epochno

fromShelleyCertificate (Shelley.DCertGenesis
                         (Shelley.GenesisDelegCert genesiskh delegatekh vrfkh)) =
    GenesisKeyDelegationCertificate
      (GenesisKeyHash         genesiskh)
      (GenesisDelegateKeyHash delegatekh)
      (VrfKeyHash             vrfkh)

fromShelleyCertificate (Shelley.DCertMir
                         (Shelley.MIRCert mirpot (Shelley.StakeAddressesMIR amounts))) =
    MIRCertificate
      mirpot
      (StakeAddressesMIR
        [ (fromShelleyStakeCredential sc, fromShelleyDeltaLovelace v)
        | (sc, v) <- Map.toList amounts ]
      )
fromShelleyCertificate (Shelley.DCertMir
                         (Shelley.MIRCert ReservesMIR (Shelley.SendToOppositePotMIR amount))) =
    MIRCertificate ReservesMIR
      (SendToTreasuryMIR $ fromShelleyLovelace amount)

fromShelleyCertificate (Shelley.DCertMir
                         (Shelley.MIRCert TreasuryMIR (Shelley.SendToOppositePotMIR amount))) =
    MIRCertificate TreasuryMIR
      (SendToReservesMIR $ fromShelleyLovelace amount)

toShelleyPoolParams :: StakePoolParameters -> Shelley.PoolParams StandardCrypto
toShelleyPoolParams StakePoolParameters {
                      stakePoolId            = StakePoolKeyHash poolkh
                    , stakePoolVRF           = VrfKeyHash vrfkh
                    , stakePoolCost
                    , stakePoolMargin
                    , stakePoolRewardAccount
                    , stakePoolPledge
                    , stakePoolOwners
                    , stakePoolRelays
                    , stakePoolMetadata
                    } =
    --TODO: validate pool parameters
    Shelley.PoolParams {
      Shelley._poolId     = poolkh
    , Shelley._poolVrf    = vrfkh
    , Shelley._poolPledge = toShelleyLovelace stakePoolPledge
    , Shelley._poolCost   = toShelleyLovelace stakePoolCost
    , Shelley._poolMargin = Shelley.unitIntervalFromRational stakePoolMargin
    , Shelley._poolRAcnt  = toShelleyStakeAddr stakePoolRewardAccount
    , Shelley._poolOwners = Set.fromList
                              [ kh | StakeKeyHash kh <- stakePoolOwners ]
    , Shelley._poolRelays = Seq.fromList
                              (map toShelleyStakePoolRelay stakePoolRelays)
    , Shelley._poolMD     = toShelleyPoolMetadata <$>
                              maybeToStrictMaybe stakePoolMetadata
    }
  where
    toShelleyStakePoolRelay :: StakePoolRelay -> Shelley.StakePoolRelay
    toShelleyStakePoolRelay (StakePoolRelayIp mipv4 mipv6 mport) =
      Shelley.SingleHostAddr
        (fromIntegral <$> maybeToStrictMaybe mport)
        (maybeToStrictMaybe mipv4)
        (maybeToStrictMaybe mipv6)

    toShelleyStakePoolRelay (StakePoolRelayDnsARecord dnsname mport) =
      Shelley.SingleHostName
        (fromIntegral <$> maybeToStrictMaybe mport)
        (toShelleyDnsName dnsname)

    toShelleyStakePoolRelay (StakePoolRelayDnsSrvRecord dnsname) =
      Shelley.MultiHostName
        (toShelleyDnsName dnsname)

    toShelleyPoolMetadata :: StakePoolMetadataReference -> Shelley.PoolMetadata
    toShelleyPoolMetadata StakePoolMetadataReference {
                            stakePoolMetadataURL
                          , stakePoolMetadataHash = StakePoolMetadataHash mdh
                          } =
      Shelley.PoolMetadata {
        Shelley._poolMDUrl  = toShelleyUrl stakePoolMetadataURL
      , Shelley._poolMDHash = Crypto.hashToBytes mdh
      }

    toShelleyDnsName :: ByteString -> Shelley.DnsName
    toShelleyDnsName = fromMaybe (error "toShelleyDnsName: invalid dns name. TODO: proper validation")
                     . Shelley.textToDns
                     . Text.decodeLatin1

    toShelleyUrl :: Text -> Shelley.Url
    toShelleyUrl = fromMaybe (error "toShelleyUrl: invalid url. TODO: proper validation")
                 . Shelley.textToUrl


fromShelleyPoolParams :: Shelley.PoolParams StandardCrypto
                      -> StakePoolParameters
fromShelleyPoolParams
    Shelley.PoolParams {
      Shelley._poolId
    , Shelley._poolVrf
    , Shelley._poolPledge
    , Shelley._poolCost
    , Shelley._poolMargin
    , Shelley._poolRAcnt
    , Shelley._poolOwners
    , Shelley._poolRelays
    , Shelley._poolMD
    } =
    StakePoolParameters {
      stakePoolId            = StakePoolKeyHash _poolId
    , stakePoolVRF           = VrfKeyHash _poolVrf
    , stakePoolCost          = fromShelleyLovelace _poolCost
    , stakePoolMargin        = Shelley.unitIntervalToRational _poolMargin
    , stakePoolRewardAccount = fromShelleyStakeAddr _poolRAcnt
    , stakePoolPledge        = fromShelleyLovelace _poolPledge
    , stakePoolOwners        = map StakeKeyHash (Set.toList _poolOwners)
    , stakePoolRelays        = map fromShelleyStakePoolRelay
                                   (Foldable.toList _poolRelays)
    , stakePoolMetadata      = fromShelleyPoolMetadata <$>
                                 strictMaybeToMaybe _poolMD
    }
  where
    fromShelleyStakePoolRelay :: Shelley.StakePoolRelay -> StakePoolRelay
    fromShelleyStakePoolRelay (Shelley.SingleHostAddr mport mipv4 mipv6) =
      StakePoolRelayIp
        (strictMaybeToMaybe mipv4)
        (strictMaybeToMaybe mipv6)
        (fromIntegral . Shelley.portToWord16 <$> strictMaybeToMaybe mport)

    fromShelleyStakePoolRelay (Shelley.SingleHostName mport dnsname) =
      StakePoolRelayDnsARecord
        (fromShelleyDnsName dnsname)
        (fromIntegral . Shelley.portToWord16 <$> strictMaybeToMaybe mport)

    fromShelleyStakePoolRelay (Shelley.MultiHostName dnsname) =
      StakePoolRelayDnsSrvRecord
        (fromShelleyDnsName dnsname)

    fromShelleyPoolMetadata :: Shelley.PoolMetadata -> StakePoolMetadataReference
    fromShelleyPoolMetadata Shelley.PoolMetadata {
                              Shelley._poolMDUrl
                            , Shelley._poolMDHash
                            } =
      StakePoolMetadataReference {
        stakePoolMetadataURL  = Shelley.urlToText _poolMDUrl
      , stakePoolMetadataHash = StakePoolMetadataHash
                              . fromMaybe (error "fromShelleyPoolMetadata: invalid hash. TODO: proper validation")
                              . Crypto.hashFromBytes
                              $ _poolMDHash
      }

    --TODO: change the ledger rep of the DNS name to use ShortByteString
    fromShelleyDnsName :: Shelley.DnsName -> ByteString
    fromShelleyDnsName = Text.encodeUtf8
                       . Shelley.dnsToText
