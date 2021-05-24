{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | User-friendly pretty-printing for textual user interfaces (TUI)
module Cardano.CLI.Run.Friendly (friendlyTxBodyBS) where

import           Cardano.Prelude

import           Data.Aeson (Object, Value (..), object, toJSON, (.=))
import qualified Data.HashMap.Strict as HashMap
import           Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare)

import           Cardano.Api as Api (AddressInEra (..),
                   AddressTypeInEra (ByronAddressInAnyEra, ShelleyAddressInEra), CardanoEra,
                   ShelleyBasedEra (ShelleyBasedEraAllegra, ShelleyBasedEraAlonzo, ShelleyBasedEraMary, ShelleyBasedEraShelley),
                   ShelleyEra, TxBody, serialiseAddress)
import           Cardano.Api.Byron (TxBody (ByronTxBody))
import           Cardano.Api.Shelley (TxBody (ShelleyTxBody), fromShelleyAddr)
import           Cardano.Binary (Annotated)
import qualified Cardano.Chain.UTxO as Byron
import           Cardano.Ledger.Shelley as Ledger (ShelleyEra)
import           Cardano.Ledger.ShelleyMA (MaryOrAllegra (Allegra, Mary), ShelleyMAEra)
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMA
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import           Shelley.Spec.Ledger.API (Addr (..), TxOut (TxOut))
import qualified Shelley.Spec.Ledger.API as Shelley

import           Cardano.CLI.Helpers (textShow)

friendlyTxBodyBS :: CardanoEra era -> Api.TxBody era -> ByteString
friendlyTxBodyBS era =
  encodePretty (setConfCompare compare defConfig) . friendlyTxBody era

friendlyTxBody :: CardanoEra era -> Api.TxBody era -> Value
friendlyTxBody era txbody =
  Object $
    HashMap.fromList ["era" .= toJSON era]
    <>
    case txbody of
      ByronTxBody body -> friendlyTxBodyByron body
      ShelleyTxBody ShelleyBasedEraShelley body _scripts aux ->
        addAuxData aux $ friendlyTxBodyShelley body
      ShelleyTxBody ShelleyBasedEraAllegra body _scripts aux ->
        addAuxData aux $ friendlyTxBodyAllegra body
      ShelleyTxBody ShelleyBasedEraMary body _scripts aux ->
        addAuxData aux $ friendlyTxBodyMary body
      ShelleyTxBody ShelleyBasedEraAlonzo _ _ _ ->
        panic "friendlyTxBody: Alonzo not implemented yet"

addAuxData :: Show a => Maybe a -> Object -> Object
addAuxData = HashMap.insert "auxiliary data" . maybe Null (toJSON . textShow)

friendlyTxBodyByron :: Annotated Byron.Tx ByteString -> Object
friendlyTxBodyByron = assertObject . toJSON

friendlyTxBodyShelley
  :: Shelley.TxBody (Ledger.ShelleyEra StandardCrypto) -> Object
friendlyTxBodyShelley body =
  HashMap.fromList
    [ "inputs" .= Shelley._inputs body
    , "outputs" .= fmap friendlyTxOutShelley (Shelley._outputs body)
    , "certificates" .= fmap textShow (Shelley._certs body)
    , "withdrawals" .= Shelley.unWdrl (Shelley._wdrls body)
    , "fee" .= Shelley._txfee body
    , "time to live" .= Shelley._ttl body
    , "update" .= fmap textShow (Shelley._txUpdate body)
    , "metadata hash" .= fmap textShow (Shelley._mdHash body)
    ]

friendlyTxBodyAllegra
  :: ShelleyMA.TxBody (ShelleyMAEra 'Allegra StandardCrypto) -> Object
friendlyTxBodyAllegra
  (ShelleyMA.TxBody
    inputs
    outputs
    certificates
    (Shelley.Wdrl withdrawals)
    txfee
    validity
    update
    adHash
    _mint -- mint is not used in Allegra, only in Mary+
    ) =
  HashMap.fromList
    [ "inputs" .= inputs
    , "outputs" .= fmap friendlyTxOutAllegra outputs
    , "certificates" .= fmap textShow certificates
    , "withdrawals" .= withdrawals
    , "fee" .= txfee
    , "validity interval" .= friendlyValidityInterval validity
    , "update" .= fmap textShow update
    , "auxiliary data hash" .= fmap textShow adHash
    ]

friendlyTxBodyMary
  :: ShelleyMA.TxBody (ShelleyMAEra 'Mary StandardCrypto) -> Object
friendlyTxBodyMary
  (ShelleyMA.TxBody
    inputs
    outputs
    certificates
    (Shelley.Wdrl withdrawals)
    txfee
    validity
    update
    adHash
    mint) =
  HashMap.fromList
    [ "inputs" .= inputs
    , "outputs" .= fmap friendlyTxOutMary outputs
    , "certificates" .= fmap textShow certificates
    , "withdrawals" .= withdrawals
    , "fee" .= txfee
    , "validity interval" .= friendlyValidityInterval validity
    , "update" .= fmap textShow update
    , "auxiliary data hash" .= fmap textShow adHash
    , "mint" .= mint
    ]

friendlyValidityInterval :: ShelleyMA.ValidityInterval -> Value
friendlyValidityInterval
  ShelleyMA.ValidityInterval{invalidBefore, invalidHereafter} =
    object
      [ "invalid before" .= invalidBefore
      , "invalid hereafter" .= invalidHereafter
      ]

friendlyTxOutShelley :: TxOut (Ledger.ShelleyEra StandardCrypto) -> Value
friendlyTxOutShelley (TxOut addr amount) =
  Object $ HashMap.insert "amount" (toJSON amount) $ friendlyAddress addr

friendlyTxOutAllegra :: TxOut (ShelleyMAEra 'Allegra StandardCrypto) -> Value
friendlyTxOutAllegra (TxOut addr amount) =
  Object $ HashMap.insert "amount" (toJSON amount) $ friendlyAddress addr

friendlyTxOutMary :: TxOut (ShelleyMAEra 'Mary StandardCrypto) -> Value
friendlyTxOutMary (TxOut addr amount) =
  Object $ HashMap.insert "amount" (toJSON amount) $ friendlyAddress addr

friendlyAddress :: Addr StandardCrypto -> Object
friendlyAddress addr =
  HashMap.fromList $
    case addr of
      Addr net cred ref ->
        [ "address" .=
            object
              [ "network" .= net
              , "credential" .= cred
              , "stake reference" .= textShow ref
              , "Bech32" .= addressBech32
              ]
        ]
      AddrBootstrap _ ->
        ["bootstrap address" .= object ["Bech32" .= String addressBech32]]
  where
    addressBech32 =
      case fromShelleyAddr @Api.ShelleyEra addr of
        AddressInEra (ShelleyAddressInEra _) a -> serialiseAddress a
        AddressInEra ByronAddressInAnyEra a -> serialiseAddress a

assertObject :: HasCallStack => Value -> Object
assertObject = \case
  Object obj -> obj
  val -> panic $ "expected JSON Object, but got " <> typ
    where
      typ =
        case val of
          Array{}  -> "an Array"
          Bool{}   -> "a Boolean"
          Null     -> "Null"
          Number{} -> "a Number"
          Object{} -> "an Object"
          String{} -> "a String"
