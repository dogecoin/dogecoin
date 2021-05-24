{-# LANGUAGE GADTs #-}

module Cardano.CLI.Byron.Run
  ( ByronClientCmdError
  , renderByronClientCmdError
  , runByronClientCommand
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, left)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.IO as TL
import qualified Formatting as F

import qualified Cardano.Chain.Genesis as Genesis

import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Signing as Crypto

import           Cardano.Api hiding (UpdateProposal)
import           Cardano.Api.Byron (SomeByronSigningKey (..), Tx (..), VerificationKey (..))

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)

import           Cardano.CLI.Byron.Commands
import           Cardano.CLI.Byron.Delegation
import           Cardano.CLI.Byron.Genesis
import           Cardano.CLI.Byron.Key
import           Cardano.CLI.Byron.Query
import           Cardano.CLI.Byron.Tx
import           Cardano.CLI.Byron.UpdateProposal
import           Cardano.CLI.Byron.Vote
import           Cardano.CLI.Helpers
import           Cardano.CLI.Shelley.Commands (ByronKeyFormat (..))
import           Cardano.CLI.Types

-- | Data type that encompasses all the possible errors of the
-- Byron client.
data ByronClientCmdError
  = ByronCmdDelegationError !ByronDelegationError
  | ByronCmdGenesisError !ByronGenesisError
  | ByronCmdHelpersError !HelpersError
  | ByronCmdKeyFailure !ByronKeyFailure
  | ByronCmdQueryError !ByronQueryError
  | ByronCmdTxError !ByronTxError
  | ByronCmdTxSubmitError !(ApplyTxErr ByronBlock)
  | ByronCmdUpdateProposalError !ByronUpdateProposalError
  | ByronCmdVoteError !ByronVoteError
  deriving Show

renderByronClientCmdError :: ByronClientCmdError -> Text
renderByronClientCmdError err =
  case err of
    ByronCmdDelegationError e -> renderByronDelegationError e
    ByronCmdGenesisError e -> renderByronGenesisError e
    ByronCmdHelpersError e -> renderHelpersError e
    ByronCmdKeyFailure e -> renderByronKeyFailure e
    ByronCmdQueryError e -> renderByronQueryError e
    ByronCmdTxError e -> renderByronTxError e
    ByronCmdTxSubmitError e ->
      "Error while submitting Byron tx: " <> Text.pack (show e)
    ByronCmdUpdateProposalError e -> renderByronUpdateProposalError e
    ByronCmdVoteError e -> renderByronVoteError e

runByronClientCommand :: ByronCommand -> ExceptT ByronClientCmdError IO ()
runByronClientCommand c =
  case c of
    NodeCmd bc -> runNodeCmd bc
    Genesis outDir params -> runGenesisCommand outDir params
    GetLocalNodeTip network -> firstExceptT ByronCmdQueryError $ runGetLocalNodeTip network
    ValidateCBOR cborObject fp -> runValidateCBOR cborObject fp
    PrettyPrintCBOR fp -> runPrettyPrintCBOR fp
    PrettySigningKeyPublic bKeyFormat skF -> runPrettySigningKeyPublic bKeyFormat skF
    MigrateDelegateKeyFrom oldKey nskf ->
       runMigrateDelegateKeyFrom oldKey nskf
    PrintGenesisHash genFp -> runPrintGenesisHash genFp
    PrintSigningKeyAddress bKeyFormat networkid skF -> runPrintSigningKeyAddress bKeyFormat networkid skF
    Keygen nskf -> runKeygen nskf
    ToVerification bKeyFormat skFp nvkFp -> runToVerification bKeyFormat skFp nvkFp
    SubmitTx network fp -> runSubmitTx network fp
    GetTxId fp -> runGetTxId fp
    SpendGenesisUTxO genFp nw era nftx ctKey genRichAddr outs ->
      runSpendGenesisUTxO genFp nw era nftx ctKey genRichAddr outs
    SpendUTxO nw era nftx ctKey ins outs ->
      runSpendUTxO nw era nftx ctKey ins outs


runNodeCmd :: NodeCmd -> ExceptT ByronClientCmdError IO ()
runNodeCmd (CreateVote nw sKey upPropFp voteBool outputFp) =
  firstExceptT ByronCmdVoteError $ runVoteCreation nw sKey upPropFp voteBool outputFp

runNodeCmd (SubmitUpdateProposal network proposalFp) =
    firstExceptT ByronCmdUpdateProposalError
      $ submitByronUpdateProposal network proposalFp

runNodeCmd (SubmitVote network voteFp) =
    firstExceptT ByronCmdVoteError $ submitByronVote network voteFp

runNodeCmd (UpdateProposal nw sKey pVer sVer sysTag insHash outputFp params) =
  firstExceptT ByronCmdUpdateProposalError
    $ runProposalCreation nw sKey pVer sVer sysTag insHash outputFp params

runGenesisCommand :: NewDirectory -> GenesisParameters -> ExceptT ByronClientCmdError IO ()
runGenesisCommand outDir params = do
  (genData, genSecrets) <- firstExceptT ByronCmdGenesisError $ mkGenesis params
  firstExceptT ByronCmdGenesisError $ dumpGenesis outDir genData genSecrets

runValidateCBOR :: CBORObject -> FilePath -> ExceptT ByronClientCmdError IO ()
runValidateCBOR cborObject fp = do
  bs <- firstExceptT ByronCmdHelpersError $ readCBOR fp
  res <- hoistEither . first ByronCmdHelpersError $ validateCBOR cborObject bs
  liftIO $ putTextLn res

runPrettyPrintCBOR :: FilePath -> ExceptT ByronClientCmdError IO ()
runPrettyPrintCBOR fp = do
  bs <- firstExceptT ByronCmdHelpersError $ readCBOR fp
  firstExceptT ByronCmdHelpersError $ pPrintCBOR bs

runPrettySigningKeyPublic :: ByronKeyFormat -> SigningKeyFile -> ExceptT ByronClientCmdError IO ()
runPrettySigningKeyPublic bKeyFormat skF = do
  sK <- firstExceptT ByronCmdKeyFailure $ readByronSigningKey bKeyFormat skF
  liftIO . putTextLn . prettyPublicKey $ byronWitnessToVerKey sK

runMigrateDelegateKeyFrom
  :: SigningKeyFile
  -- ^ Legacy Byron signing key
  -> NewSigningKeyFile
  -> ExceptT ByronClientCmdError IO ()
runMigrateDelegateKeyFrom oldKey@(SigningKeyFile fp) (NewSigningKeyFile newKey) = do
  sk <- firstExceptT ByronCmdKeyFailure $ readByronSigningKey LegacyByronKeyFormat oldKey
  migratedWitness <- case sk of
                       AByronSigningKeyLegacy (ByronSigningKeyLegacy sKey) ->
                         return . AByronSigningKey $ ByronSigningKey sKey
                       AByronSigningKey _ ->
                         left . ByronCmdKeyFailure $ CannotMigrateFromNonLegacySigningKey fp
  firstExceptT ByronCmdHelpersError . ensureNewFileLBS newKey $ serialiseByronWitness migratedWitness

runPrintGenesisHash :: GenesisFile -> ExceptT ByronClientCmdError IO ()
runPrintGenesisHash genFp = do
    genesis <- firstExceptT ByronCmdGenesisError $
                 readGenesis genFp dummyNetwork
    liftIO . putTextLn $ formatter genesis
  where
    -- For this purpose of getting the hash, it does not matter what network
    -- value we use here.
    dummyNetwork :: NetworkId
    dummyNetwork = Mainnet

    formatter :: Genesis.Config -> Text
    formatter = F.sformat Crypto.hashHexF
              . Genesis.unGenesisHash
              . Genesis.configGenesisHash

runPrintSigningKeyAddress
  :: ByronKeyFormat
  -> NetworkId
  -> SigningKeyFile
  -> ExceptT ByronClientCmdError IO ()
runPrintSigningKeyAddress bKeyFormat networkid skF = do
  sK <- firstExceptT ByronCmdKeyFailure $ readByronSigningKey bKeyFormat skF
  let sKeyAddr = prettyAddress . makeByronAddress networkid $ byronWitnessToVerKey sK
  liftIO $ putTextLn sKeyAddr

runKeygen :: NewSigningKeyFile -> ExceptT ByronClientCmdError IO ()
runKeygen (NewSigningKeyFile skF)  = do
  sK <- liftIO $ generateSigningKey AsByronKey
  firstExceptT ByronCmdHelpersError . ensureNewFileLBS skF $ serialiseToRawBytes sK

runToVerification :: ByronKeyFormat -> SigningKeyFile -> NewVerificationKeyFile -> ExceptT ByronClientCmdError IO ()
runToVerification bKeyFormat skFp (NewVerificationKeyFile vkFp) = do
  sk <- firstExceptT ByronCmdKeyFailure $ readByronSigningKey bKeyFormat skFp
  let ByronVerificationKey vK = byronWitnessToVerKey sk
  let vKey = Builder.toLazyText $ Crypto.formatFullVerificationKey vK
  firstExceptT ByronCmdHelpersError $ ensureNewFile TL.writeFile vkFp vKey

runSubmitTx :: NetworkId -> TxFile -> ExceptT ByronClientCmdError IO ()
runSubmitTx network fp = do
    tx <- firstExceptT ByronCmdTxError $ readByronTx fp
    firstExceptT ByronCmdTxError $
      nodeSubmitTx network (normalByronTxToGenTx tx)

runGetTxId :: TxFile -> ExceptT ByronClientCmdError IO ()
runGetTxId fp = firstExceptT ByronCmdTxError $ do
    tx <- readByronTx fp
    let txbody = getTxBody (ByronTx tx)
        txid   = getTxId txbody
    liftIO $ BS.putStrLn $ serialiseToRawBytesHex txid

runSpendGenesisUTxO
  :: GenesisFile
  -> NetworkId
  -> ByronKeyFormat
  -> NewTxFile
  -> SigningKeyFile
  -> Address ByronAddr
  -> [TxOut ByronEra]
  -> ExceptT ByronClientCmdError IO ()
runSpendGenesisUTxO genesisFile nw bKeyFormat (NewTxFile ctTx) ctKey genRichAddr outs = do
    genesis <- firstExceptT ByronCmdGenesisError $ readGenesis genesisFile nw
    sk <- firstExceptT ByronCmdKeyFailure $ readByronSigningKey bKeyFormat ctKey

    let tx = txSpendGenesisUTxOByronPBFT genesis nw sk genRichAddr outs
    firstExceptT ByronCmdHelpersError . ensureNewFileLBS ctTx $ serialiseToCBOR tx

runSpendUTxO
  :: NetworkId
  -> ByronKeyFormat
  -> NewTxFile
  -> SigningKeyFile
  -> [TxIn]
  -> [TxOut ByronEra]
  -> ExceptT ByronClientCmdError IO ()
runSpendUTxO nw bKeyFormat (NewTxFile ctTx) ctKey ins outs = do
    sk <- firstExceptT ByronCmdKeyFailure $ readByronSigningKey bKeyFormat ctKey

    let gTx = txSpendUTxOByronPBFT nw sk ins outs
    firstExceptT ByronCmdHelpersError . ensureNewFileLBS ctTx $ serialiseToCBOR gTx
