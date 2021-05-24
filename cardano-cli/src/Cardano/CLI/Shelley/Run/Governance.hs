module Cardano.CLI.Shelley.Run.Governance
  ( ShelleyGovernanceCmdError
  , renderShelleyGovernanceError
  , runGovernanceCmd
  ) where

import           Cardano.Prelude

import qualified Data.Text as Text

import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT)

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key (InputDecodeError, VerificationKeyOrHashOrFile,
                   readVerificationKeyOrHashOrFile, readVerificationKeyOrHashOrTextEnvFile)
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Types

import qualified Shelley.Spec.Ledger.TxBody as Shelley


data ShelleyGovernanceCmdError
  = ShelleyGovernanceCmdTextEnvReadError !(FileError TextEnvelopeError)
  | ShelleyGovernanceCmdKeyReadError !(FileError InputDecodeError)
  | ShelleyGovernanceCmdTextEnvWriteError !(FileError ())
  | ShelleyGovernanceCmdEmptyUpdateProposalError
  | ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach
      !FilePath
      !Int
      -- ^ Number of stake verification keys
      !Int
      -- ^ Number of reward amounts
  deriving Show

renderShelleyGovernanceError :: ShelleyGovernanceCmdError -> Text
renderShelleyGovernanceError err =
  case err of
    ShelleyGovernanceCmdTextEnvReadError fileErr -> Text.pack (displayError fileErr)
    ShelleyGovernanceCmdKeyReadError fileErr -> Text.pack (displayError fileErr)
    ShelleyGovernanceCmdTextEnvWriteError fileErr -> Text.pack (displayError fileErr)
    -- TODO: The equality check is still not working for empty update proposals.
    ShelleyGovernanceCmdEmptyUpdateProposalError ->
      "Empty update proposals are not allowed"
    ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach fp numVKeys numRwdAmts ->
       "Error creating the MIR certificate at: " <> textShow fp
       <> " The number of staking keys: " <> textShow numVKeys
       <> " and the number of reward amounts: " <> textShow numRwdAmts
       <> " are not equivalent."
  where
    textShow x = Text.pack (show x)


runGovernanceCmd :: GovernanceCmd -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceCmd (GovernanceMIRPayStakeAddressesCertificate mirpot vKeys rewards out) =
  runGovernanceMIRCertificatePayStakeAddrs mirpot vKeys rewards out
runGovernanceCmd (GovernanceMIRTransfer amt out direction) =
  runGovernanceMIRCertificateTransfer amt out direction
runGovernanceCmd (GovernanceGenesisKeyDelegationCertificate genVk genDelegVk vrfVk out) =
  runGovernanceGenesisKeyDelegationCertificate genVk genDelegVk vrfVk out
runGovernanceCmd (GovernanceUpdateProposal out eNo genVKeys ppUp) =
  runGovernanceUpdateProposal out eNo genVKeys ppUp

runGovernanceMIRCertificatePayStakeAddrs
  :: Shelley.MIRPot
  -> [StakeAddress] -- ^ Stake addresses
  -> [Lovelace]     -- ^ Corresponding reward amounts (same length)
  -> OutputFile
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceMIRCertificatePayStakeAddrs mirPot sAddrs rwdAmts (OutputFile oFp) = do

    unless (length sAddrs == length rwdAmts) $
      left $ ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach
               oFp (length sAddrs) (length rwdAmts)

    let sCreds  = map stakeAddrToStakeCredential sAddrs
        mirCert = makeMIRCertificate mirPot (StakeAddressesMIR $ zip sCreds rwdAmts)

    firstExceptT ShelleyGovernanceCmdTextEnvWriteError
      . newExceptT
      $ writeFileTextEnvelope oFp (Just mirCertDesc) mirCert
  where
    mirCertDesc :: TextEnvelopeDescr
    mirCertDesc = "Move Instantaneous Rewards Certificate"

    --TODO: expose a pattern for StakeAddress that give us the StakeCredential
    stakeAddrToStakeCredential :: StakeAddress -> StakeCredential
    stakeAddrToStakeCredential (StakeAddress _ scred) =
      fromShelleyStakeCredential scred

runGovernanceMIRCertificateTransfer
  :: Lovelace
  -> OutputFile
  -> TransferDirection
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceMIRCertificateTransfer ll (OutputFile oFp) direction = do
  mirCert <- case direction of
                 TransferToReserves ->
                   return . makeMIRCertificate Shelley.TreasuryMIR $ SendToReservesMIR ll
                 TransferToTreasury ->
                   return . makeMIRCertificate Shelley.ReservesMIR $ SendToTreasuryMIR ll

  firstExceptT ShelleyGovernanceCmdTextEnvWriteError
    . newExceptT
    $ writeFileTextEnvelope oFp (Just $ mirCertDesc direction) mirCert
 where
  mirCertDesc :: TransferDirection -> TextEnvelopeDescr
  mirCertDesc TransferToTreasury = "MIR Certificate Send To Treasury"
  mirCertDesc TransferToReserves = "MIR Certificate Send To Reserves"


runGovernanceGenesisKeyDelegationCertificate
  :: VerificationKeyOrHashOrFile GenesisKey
  -> VerificationKeyOrHashOrFile GenesisDelegateKey
  -> VerificationKeyOrHashOrFile VrfKey
  -> OutputFile
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceGenesisKeyDelegationCertificate genVkOrHashOrFp
                                             genDelVkOrHashOrFp
                                             vrfVkOrHashOrFp
                                             (OutputFile oFp) = do
    genesisVkHash <- firstExceptT ShelleyGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrTextEnvFile AsGenesisKey genVkOrHashOrFp
    genesisDelVkHash <-firstExceptT ShelleyGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrTextEnvFile AsGenesisDelegateKey genDelVkOrHashOrFp
    vrfVkHash <- firstExceptT ShelleyGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrFile AsVrfKey vrfVkOrHashOrFp
    firstExceptT ShelleyGovernanceCmdTextEnvWriteError
      . newExceptT
      $ writeFileTextEnvelope oFp (Just genKeyDelegCertDesc)
      $ makeGenesisKeyDelegationCertificate genesisVkHash genesisDelVkHash vrfVkHash
  where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Genesis Key Delegation Certificate"

runGovernanceUpdateProposal
  :: OutputFile
  -> EpochNo
  -> [VerificationKeyFile]
  -- ^ Genesis verification keys
  -> ProtocolParametersUpdate
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceUpdateProposal (OutputFile upFile) eNo genVerKeyFiles upPprams = do
    when (upPprams == mempty) $ left ShelleyGovernanceCmdEmptyUpdateProposalError
    genVKeys <- sequence
                  [ firstExceptT ShelleyGovernanceCmdTextEnvReadError . newExceptT $
                      readFileTextEnvelope
                        (AsVerificationKey AsGenesisKey)
                        vkeyFile
                  | VerificationKeyFile vkeyFile <- genVerKeyFiles ]
    let genKeyHashes = map verificationKeyHash genVKeys
        upProp = makeShelleyUpdateProposal upPprams genKeyHashes eNo
    firstExceptT ShelleyGovernanceCmdTextEnvWriteError . newExceptT $
      writeFileTextEnvelope upFile Nothing upProp
