{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.Byron.Tx
  ( ByronTxError(..)
  , TxFile(..)
  , NewTxFile(..)
  , prettyAddress
  , readByronTx
  , normalByronTxToGenTx
  , txSpendGenesisUTxOByronPBFT
  , txSpendUTxOByronPBFT
  , nodeSubmitTx
  , renderByronTxError

    --TODO: remove when they are exported from the ledger
  , fromCborTxAux
  , toCborTxAux
  )
where

import           Cardano.Prelude hiding (option, trace, (%))
import           Prelude (error)

import           Control.Monad.Trans.Except.Extra (firstExceptT, left)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text as Text
import           Formatting (sformat, (%))

import           Cardano.Api

import qualified Cardano.Binary as Binary

import qualified Cardano.Chain.Common as Common
import           Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.UTxO as UTxO
import qualified Cardano.Crypto.Signing as Crypto

import           Cardano.Api.Byron
import           Cardano.CLI.Byron.Key (byronWitnessToVerKey)
import           Cardano.CLI.Environment
import           Cardano.CLI.Helpers (textShow)
import           Cardano.CLI.Types (SocketPath (..))
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock, GenTx (..))
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx

data ByronTxError
  = TxDeserialisationFailed !FilePath !Binary.DecoderError
  | ByronTxSubmitError !Text
  | ByronTxSubmitErrorEraMismatch !EraMismatch
  | EnvSocketError !EnvSocketError
  deriving Show

renderByronTxError :: ByronTxError -> Text
renderByronTxError err =
  case err of
    ByronTxSubmitError res -> "Error while submitting tx: " <> res
    ByronTxSubmitErrorEraMismatch EraMismatch{ledgerEraName, otherEraName} ->
      "The era of the node and the tx do not match. " <>
      "The node is running in the " <> ledgerEraName <>
      " era, but the transaction is for the " <> otherEraName <> " era."
    TxDeserialisationFailed txFp decErr ->
      "Transaction deserialisation failed at " <> textShow txFp <> " Error: " <> textShow decErr
    EnvSocketError envSockErr -> renderEnvSocketError envSockErr


newtype TxFile =
  TxFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype NewTxFile =
  NewTxFile FilePath
  deriving (Eq, Ord, Show, IsString)


-- | Pretty-print an address in its Base58 form, and also
--   its full structure.
prettyAddress :: Address ByronAddr -> Text
prettyAddress (ByronAddress addr) = sformat
  (Common.addressF %"\n"%Common.addressDetailedF)
  addr addr

readByronTx :: TxFile -> ExceptT ByronTxError IO (UTxO.ATxAux ByteString)
readByronTx (TxFile fp) = do
  txBS <- liftIO $ LB.readFile fp
  case fromCborTxAux txBS of
    Left e -> left $ TxDeserialisationFailed fp e
    Right tx -> pure tx

-- | The 'GenTx' is all the kinds of transactions that can be submitted
-- and \"normal\" Byron transactions are just one of the kinds.
normalByronTxToGenTx :: UTxO.ATxAux ByteString -> GenTx ByronBlock
normalByronTxToGenTx tx' = Byron.ByronTx (Byron.byronIdTx tx') tx'

-- | Given a genesis, and a pair of a genesis public key and address,
--   reconstruct a TxIn corresponding to the genesis UTxO entry.
genesisUTxOTxIn :: Genesis.Config -> Crypto.VerificationKey -> Common.Address -> UTxO.TxIn
genesisUTxOTxIn gc vk genAddr =
  handleMissingAddr $ fst <$> Map.lookup genAddr initialUtxo
  where
    initialUtxo :: Map Common.Address (UTxO.TxIn, UTxO.TxOut)
    initialUtxo =
          Map.fromList
        . mapMaybe (\(inp, out) -> mkEntry inp genAddr <$> keyMatchesUTxO vk out)
        . fromCompactTxInTxOutList
        . Map.toList
        . UTxO.unUTxO
        . UTxO.genesisUtxo
        $ gc
      where
        mkEntry :: UTxO.TxIn
                -> Common.Address
                -> UTxO.TxOut
                -> (Common.Address, (UTxO.TxIn, UTxO.TxOut))
        mkEntry inp addr out = (addr, (inp, out))

    fromCompactTxInTxOutList :: [(UTxO.CompactTxIn, UTxO.CompactTxOut)]
                             -> [(UTxO.TxIn, UTxO.TxOut)]
    fromCompactTxInTxOutList =
        map (bimap UTxO.fromCompactTxIn UTxO.fromCompactTxOut)

    keyMatchesUTxO :: Crypto.VerificationKey -> UTxO.TxOut -> Maybe UTxO.TxOut
    keyMatchesUTxO key out =
      if Common.checkVerKeyAddress key (UTxO.txOutAddress out)
      then Just out else Nothing

    handleMissingAddr :: Maybe UTxO.TxIn -> UTxO.TxIn
    handleMissingAddr  = fromMaybe . error
      $  "\nGenesis UTxO has no address\n"
      <> T.unpack (prettyAddress (ByronAddress genAddr))
      <> "\n\nIt has the following, though:\n\n"
      <> Cardano.Prelude.concat (T.unpack . prettyAddress <$> map ByronAddress (Map.keys initialUtxo))

-- | Generate a transaction spending genesis UTxO at a given address,
--   to given outputs, signed by the given key.
txSpendGenesisUTxOByronPBFT
  :: Genesis.Config
  -> NetworkId
  -> SomeByronSigningKey
  -> Address ByronAddr
  -> [TxOut ByronEra]
  -> Tx ByronEra
txSpendGenesisUTxOByronPBFT gc nId sk (ByronAddress bAddr) outs = do
    let txBodyCont =
          TxBodyContent
            [ (fromByronTxIn txIn
              , BuildTxWith (KeyWitness KeyWitnessForSpending))
            ]
            outs
            (TxFeeImplicit TxFeesImplicitInByronEra)
            ( TxValidityNoLowerBound
            , TxValidityNoUpperBound ValidityNoUpperBoundInByronEra
            )
            TxMetadataNone
            TxAuxScriptsNone
            TxWithdrawalsNone
            TxCertificatesNone
            TxUpdateProposalNone
            TxMintNone
    case makeTransactionBody txBodyCont of
      Left err -> error $ "Error occured while creating a Byron genesis based UTxO transaction: " <> show err
      Right txBody -> let bWit = fromByronWitness sk nId txBody
                      in makeSignedTransaction [bWit] txBody
  where
    ByronVerificationKey vKey = byronWitnessToVerKey sk

    txIn :: UTxO.TxIn
    txIn  = genesisUTxOTxIn gc vKey bAddr

-- | Generate a transaction from given Tx inputs to outputs,
--   signed by the given key.
txSpendUTxOByronPBFT
  :: NetworkId
  -> SomeByronSigningKey
  -> [TxIn]
  -> [TxOut ByronEra]
  -> Tx ByronEra
txSpendUTxOByronPBFT nId sk txIns outs = do
  let txBodyCont = TxBodyContent
                     [ ( txIn
                       , BuildTxWith (KeyWitness KeyWitnessForSpending)
                       ) | txIn <- txIns
                     ]
                     outs
                     (TxFeeImplicit TxFeesImplicitInByronEra)
                     ( TxValidityNoLowerBound
                     , TxValidityNoUpperBound ValidityNoUpperBoundInByronEra
                     )
                     TxMetadataNone
                     TxAuxScriptsNone
                     TxWithdrawalsNone
                     TxCertificatesNone
                     TxUpdateProposalNone
                     TxMintNone
  case makeTransactionBody txBodyCont of
    Left err -> error $ "Error occured while creating a Byron genesis based UTxO transaction: " <> show err
    Right txBody -> let bWit = fromByronWitness sk nId txBody
                    in makeSignedTransaction [bWit] txBody

fromByronWitness :: SomeByronSigningKey -> NetworkId -> TxBody ByronEra -> KeyWitness ByronEra
fromByronWitness bw nId txBody =
  case bw of
    AByronSigningKeyLegacy sk -> makeByronKeyWitness nId txBody sk
    AByronSigningKey sk' -> makeByronKeyWitness nId txBody sk'

-- | Submit a transaction to a node specified by topology info.
nodeSubmitTx
  :: NetworkId
  -> GenTx ByronBlock
  -> ExceptT ByronTxError IO ()
nodeSubmitTx network gentx = do
    SocketPath socketPath <- firstExceptT EnvSocketError readEnvSocketPath
    let connctInfo =
          LocalNodeConnectInfo {
            localNodeSocketPath = socketPath,
            localNodeNetworkId = network,
            localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
          }
    res <- liftIO $ submitTxToNodeLocal connctInfo (TxInByronSpecial gentx ByronEraInCardanoMode)
    case res of
      Net.Tx.SubmitSuccess -> liftIO $ putTextLn "Transaction successfully submitted."
      Net.Tx.SubmitFail reason ->
        case reason of
          TxValidationErrorInMode err _eraInMode -> left . ByronTxSubmitError . Text.pack $ show err
          TxValidationEraMismatch mismatchErr -> left $ ByronTxSubmitErrorEraMismatch mismatchErr

    return ()


--TODO: remove these local definitions when the updated ledger lib is available
fromCborTxAux :: LB.ByteString ->  Either Binary.DecoderError (UTxO.ATxAux B.ByteString)
fromCborTxAux lbs =
    fmap (annotationBytes lbs)
      $ Binary.decodeFullDecoder "Cardano.Chain.UTxO.TxAux.fromCborTxAux"
                                 Binary.fromCBOR lbs
  where
    annotationBytes :: Functor f => LB.ByteString -> f Binary.ByteSpan -> f B.ByteString
    annotationBytes bytes = fmap (LB.toStrict . Binary.slice bytes)

toCborTxAux :: UTxO.ATxAux ByteString -> LB.ByteString
toCborTxAux = LB.fromStrict . UTxO.aTaAnnotation -- The ByteString anotation is the CBOR encoded version.
