{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Byron.Delegation
  ( ByronDelegationError(..)
  , checkByronGenesisDelegation
  , issueByronGenesisDelegation
  , renderByronDelegationError
  , serialiseDelegationCert
  , serialiseByronWitness
  )
where

import           Cardano.Prelude hiding (option, show, trace)

import           Control.Monad.Trans.Except.Extra (left)
import qualified Data.ByteString.Lazy as LB
import           Formatting (Format, sformat)

import           Cardano.Api.Byron

import           Cardano.Binary (Annotated (..), serialize')
import qualified Cardano.Chain.Delegation as Dlg
import           Cardano.Chain.Slotting (EpochNumber)
import           Cardano.Crypto (ProtocolMagicId)
import qualified Cardano.Crypto as Crypto

import           Cardano.CLI.Byron.Key (ByronKeyFailure, renderByronKeyFailure)
import           Cardano.CLI.Helpers (textShow)
import           Cardano.CLI.Types (CertificateFile (..))

data ByronDelegationError
  = CertificateValidationErrors !FilePath ![Text]
  | DlgCertificateDeserialisationFailed !FilePath !Text
  | ByronDelegationKeyError !ByronKeyFailure
  deriving Show

renderByronDelegationError :: ByronDelegationError -> Text
renderByronDelegationError err =
  case err of
    CertificateValidationErrors certFp errs ->
      "Certificate validation error(s) at: " <> textShow certFp <> " Errors: " <> textShow errs
    DlgCertificateDeserialisationFailed certFp deSererr ->
      "Certificate deserialisation error at: " <> textShow certFp <> " Error: " <> textShow deSererr
    ByronDelegationKeyError kerr -> renderByronKeyFailure kerr

-- TODO:  we need to support password-protected secrets.
-- | Issue a certificate for genesis delegation to a delegate key, signed by the
--   issuer key, for a given protocol magic and coming into effect at given epoch.
issueByronGenesisDelegation
  :: ProtocolMagicId
  -> EpochNumber
  -> Crypto.SigningKey
  -> Crypto.VerificationKey
  -> Dlg.Certificate
issueByronGenesisDelegation magic epoch issuerSK delegateVK =
  Dlg.signCertificate magic delegateVK epoch $
  Crypto.noPassSafeSigner issuerSK

-- | Verify that a certificate signifies genesis delegation by assumed genesis key
--   to a delegate key, for a given protocol magic.
--   If certificate fails validation, throw an error.
checkByronGenesisDelegation
  :: CertificateFile
  -> ProtocolMagicId
  -> Crypto.VerificationKey
  -> Crypto.VerificationKey
  -> ExceptT ByronDelegationError IO ()
checkByronGenesisDelegation (CertificateFile certF) magic issuer delegate = do
  ecert <- liftIO $ canonicalDecodePretty <$> LB.readFile certF
  case ecert of
    Left e -> left $ DlgCertificateDeserialisationFailed certF e
    Right (cert :: Dlg.Certificate) -> do
      let issues = checkDlgCert cert magic issuer delegate
      unless (null issues) $
        left $ CertificateValidationErrors certF issues

checkDlgCert
  :: Dlg.ACertificate a
  -> ProtocolMagicId
  -> Crypto.VerificationKey
  -> Crypto.VerificationKey -> [Text]
checkDlgCert cert magic issuerVK' delegateVK' =
  mconcat
  [ [ sformat "Certificate does not have a valid signature."
      | not (Dlg.isValid magic' cert')
    ]
  , [ sformat ("Certificate issuer ".vkF." doesn't match expected: ".vkF)
      ( Dlg.issuerVK cert) issuerVK'
      | Dlg.issuerVK cert /= issuerVK'
    ]
  , [ sformat ("Certificate delegate ".vkF." doesn't match expected: ".vkF)
      ( Dlg.delegateVK cert) delegateVK'
      | Dlg.delegateVK cert /= delegateVK'
    ]
  ]
  where
    magic' :: Annotated ProtocolMagicId ByteString
    magic' = Annotated magic (serialize' magic)

    epoch :: EpochNumber
    epoch = unAnnotated $ Dlg.aEpoch cert

    cert' :: Dlg.ACertificate ByteString
    cert' =
      let unannotated = cert { Dlg.aEpoch = Annotated epoch ()
                             , Dlg.annotation = () }
      in unannotated { Dlg.annotation = serialize' unannotated
                     , Dlg.aEpoch = Annotated epoch (serialize' epoch) }

    vkF :: forall r. Format r (Crypto.VerificationKey -> r)
    vkF = Crypto.fullVerificationKeyF


serialiseDelegationCert :: Dlg.Certificate -> ByteString
serialiseDelegationCert = LB.toStrict . canonicalEncodePretty

serialiseByronWitness :: SomeByronSigningKey -> ByteString
serialiseByronWitness sk =
  case sk of
    AByronSigningKeyLegacy bSkey -> serialiseToRawBytes bSkey
    AByronSigningKey legBKey -> serialiseToRawBytes legBKey

