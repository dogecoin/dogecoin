{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Orphans () where

import           Cardano.Prelude
import           Prelude (fail)

import           Cardano.Api.Orphans ()

import           Data.Aeson (eitherDecode)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import           Data.MemoBytes (MemoBytes)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.BM.Data.Tracer (TracingVerbosity (..))
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Ledger.Alonzo as Alonzo
import           Cardano.Ledger.Alonzo.Language
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import           Cardano.Ledger.Alonzo.Translation as Alonzo
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import qualified Shelley.Spec.Ledger.CompactAddr as Shelley

-- TODO: Remove me, cli should not depend directly on plutus repo.
import qualified PlutusCore.Evaluation.Machine.ExBudgeting as Plutus
import qualified PlutusCore.Evaluation.Machine.ExBudgetingDefaults as Plutus

instance FromJSON TracingVerbosity where
  parseJSON (String str) = case str of
    "MinimalVerbosity" -> pure MinimalVerbosity
    "MaximalVerbosity" -> pure MaximalVerbosity
    "NormalVerbosity" -> pure NormalVerbosity
    err -> fail $ "Parsing of TracingVerbosity failed, "
                <> Text.unpack err <> " is not a valid TracingVerbosity"
  parseJSON invalid  = fail $ "Parsing of TracingVerbosity failed due to type mismatch. "
                           <> "Encountered: " <> show invalid

deriving instance Show TracingVerbosity

deriving instance ToJSON (Alonzo.PParamsUpdate (Alonzo.AlonzoEra StandardCrypto))
deriving instance ToJSON Alonzo.ExUnits
deriving instance ToJSON Alonzo.Prices
deriving instance ToJSON Alonzo.Language
deriving instance ToJSONKey Alonzo.Language

instance ToJSON Alonzo.CostModel where
  toJSON (Alonzo.CostModel m) = toJSON m

deriving instance FromJSON Alonzo.Prices
deriving instance FromJSON Alonzo.ExUnits

instance ToJSON (Shelley.CompactAddr StandardCrypto) where
  toJSON = toJSON . Shelley.decompactAddr

--Not currently needed, but if we do need it, this is the general instance.
--instance (ToJSON a, Ledger.Compactible a) => ToJSON (Ledger.CompactForm a) where
--  toJSON = toJSON  . Ledger.fromCompact

instance FromJSON Update.ApplicationName where
  parseJSON (String x) = pure $ Update.ApplicationName x
  parseJSON invalid  =
    fail $ "Parsing of application name failed due to type mismatch. "
    <> "Encountered: " <> show invalid

-- We defer parsing of the cost model so that we can
-- read it as a filepath. This is to reduce further pollution
-- of the genesis file.
instance FromJSON Alonzo.AlonzoGenesis where
  parseJSON =
    withObject "Alonzo Genesis" $ \o -> do
      adaPerUTxOWord       <- o .:  "adaPerUTxOWord"
      cModels              <- o .:? "costModels"
      prices               <- o .:  "executionPrices"
      maxTxExUnits         <- o .:  "maxTxExUnits"
      maxBlockExUnits      <- o .:  "maxBlockExUnits"
      maxValSize           <- o .:  "maxValueSize"
      collateralPercentage <- o .:  "collateralPercentage"
      maxCollateralInputs  <- o .:  "maxCollateralInputs"
      case cModels of
        Nothing ->
          case Plutus.extractModelParams Plutus.defaultCostModel of
            Just m ->
              return Alonzo.AlonzoGenesis {
                adaPerUTxOWord,
                costmdls = Map.singleton Alonzo.PlutusV1 (Alonzo.CostModel m),
                prices,
                maxTxExUnits,
                maxBlockExUnits,
                maxValSize,
                collateralPercentage,
                maxCollateralInputs
              }
            Nothing -> fail "Failed to extract the cost model params from Plutus.defaultCostModel"
        Just costmdls ->
          return Alonzo.AlonzoGenesis {
            adaPerUTxOWord,
            costmdls,
            prices,
            maxTxExUnits,
            maxBlockExUnits,
            maxValSize,
            collateralPercentage,
            maxCollateralInputs
          }

instance FromJSON Language  where
  parseJSON v =
    case v of
      Aeson.String "PlutusV1" -> return Alonzo.PlutusV1
      wrong -> fail $ "Error decoding Language. \
                      \Expected a JSON string but got: " <> show wrong

instance FromJSONKey Language where
  fromJSONKey = FromJSONKeyText parseLang
   where
     parseLang :: Text -> Language
     parseLang lang =
       case eitherDecode $ LBS.fromStrict $ Text.encodeUtf8 lang of
         Left err -> panic $ Text.pack err
         Right lang' -> lang'

instance FromJSON Alonzo.CostModel
instance FromJSON (Data.MemoBytes.MemoBytes (Map Text Integer))

instance FromJSON SBS.ShortByteString where
  parseJSON v = case v of
                  Aeson.String b16 ->
                    case Base16.decode $ Text.encodeUtf8 b16 of
                      Right decoded -> return $ SBS.toShort decoded
                      Left err -> fail err
                  wrong -> fail $ "Error decoding ShortByteString. \
                                  \Expected a JSON string but got: " <> show wrong
