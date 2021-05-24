{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Shelley.Run.Query
  ( ShelleyQueryCmdError
  , renderShelleyQueryCmdError
  , runQueryCmd
  ) where

import           Cardano.Prelude hiding (atomically)
import           Prelude (String)

import           Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HMS
import           Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import           Numeric (showEFloat)

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistMaybe, left)

import           Cardano.Api
import           Cardano.Api.Byron
import           Cardano.Api.Shelley

import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Cardano.CLI.Helpers (HelpersError (..), pPrintCBOR, renderHelpersError)
import           Cardano.CLI.Mary.RenderValue (defaultRenderValueOptions, renderValue)
import           Cardano.CLI.Shelley.Orphans ()
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), QueryCmd (..))
import           Cardano.CLI.Types

import           Cardano.Binary (decodeFull)
import           Cardano.Crypto.Hash (hashToBytesAsHex)

import           Cardano.Ledger.Coin
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Era as Era
import qualified Cardano.Ledger.Shelley.Constraints as Ledger
import           Ouroboros.Consensus.Cardano.Block as Consensus (EraMismatch (..))
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)
import           Ouroboros.Network.Block (Serialised (..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
                   (AcquireFailure (..))
import qualified Shelley.Spec.Ledger.API.Protocol as Ledger
import           Shelley.Spec.Ledger.EpochBoundary
import           Shelley.Spec.Ledger.Keys (KeyHash (..), KeyRole (..))
import           Shelley.Spec.Ledger.LedgerState hiding (_delegations)
import           Shelley.Spec.Ledger.Scripts ()

import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry
import qualified Data.Text.IO as T
import qualified System.IO as IO

{- HLINT ignore "Reduce duplication" -}


data ShelleyQueryCmdError
  = ShelleyQueryCmdEnvVarSocketErr !EnvSocketError
  | ShelleyQueryCmdLocalStateQueryError !ShelleyQueryCmdLocalStateQueryError
  | ShelleyQueryCmdWriteFileError !(FileError ())
  | ShelleyQueryCmdHelpersError !HelpersError
  | ShelleyQueryCmdAcquireFailure !AcquireFailure
  | ShelleyQueryCmdEraConsensusModeMismatch !AnyConsensusMode !AnyCardanoEra
  | ShelleyQueryCmdByronEra
  | ShelleyQueryCmdPoolIdError (Hash StakePoolKey)
  | ShelleyQueryCmdEraMismatch !EraMismatch
  | ShelleyQueryCmdUnsupportedMode !AnyConsensusMode
  | ShelleyQueryCmdPastHorizon !Qry.PastHorizonException
  deriving Show

renderShelleyQueryCmdError :: ShelleyQueryCmdError -> Text
renderShelleyQueryCmdError err =
  case err of
    ShelleyQueryCmdEnvVarSocketErr envSockErr -> renderEnvSocketError envSockErr
    ShelleyQueryCmdLocalStateQueryError lsqErr -> renderLocalStateQueryError lsqErr
    ShelleyQueryCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyQueryCmdHelpersError helpersErr -> renderHelpersError helpersErr
    ShelleyQueryCmdAcquireFailure aqFail -> Text.pack $ show aqFail
    ShelleyQueryCmdByronEra -> "This query cannot be used for the Byron era"
    ShelleyQueryCmdPoolIdError poolId -> "The pool id does not exist: " <> show poolId
    ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) (AnyCardanoEra era) ->
      "Consensus mode and era mismatch. Consensus mode: " <> show cMode <>
      " Era: " <> show era
    ShelleyQueryCmdEraMismatch (EraMismatch ledgerEra queryEra) ->
      "\nAn error mismatch occured." <> "\nSpecified query era: " <> queryEra <>
      "\nCurrent ledger era: " <> ledgerEra
    ShelleyQueryCmdUnsupportedMode mode -> "Unsupported mode: " <> renderMode mode
    ShelleyQueryCmdPastHorizon e -> "Past horizon: " <> show e

runQueryCmd :: QueryCmd -> ExceptT ShelleyQueryCmdError IO ()
runQueryCmd cmd =
  case cmd of
    QueryProtocolParameters' consensusModeParams network mOutFile ->
      runQueryProtocolParameters consensusModeParams network mOutFile
    QueryTip consensusModeParams network mOutFile ->
      runQueryTip consensusModeParams network mOutFile
    QueryStakeDistribution' consensusModeParams network mOutFile ->
      runQueryStakeDistribution consensusModeParams network mOutFile
    QueryStakeAddressInfo consensusModeParams addr network mOutFile ->
      runQueryStakeAddressInfo consensusModeParams addr network mOutFile
    QueryDebugLedgerState' consensusModeParams network mOutFile ->
      runQueryLedgerState consensusModeParams network mOutFile
    QueryStakeSnapshot' consensusModeParams network poolid ->
      runQueryStakeSnapshot consensusModeParams network poolid
    QueryPoolParams' consensusModeParams network poolid ->
      runQueryPoolParams consensusModeParams network poolid
    QueryProtocolState' consensusModeParams network mOutFile ->
      runQueryProtocolState consensusModeParams network mOutFile
    QueryUTxO' consensusModeParams qFilter networkId mOutFile ->
      runQueryUTxO consensusModeParams qFilter networkId mOutFile

runQueryProtocolParameters
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolParameters (AnyConsensusModeParams cModeParams) network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr
                           readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let query = QueryInEra eInMode
                    $ QueryInShelleyBasedEra sbe QueryProtocolParameters
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  query
      writeProtocolParameters mOutFile result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE
 where
  writeProtocolParameters
    :: Maybe OutputFile
    -> ProtocolParameters
    -> ExceptT ShelleyQueryCmdError IO ()
  writeProtocolParameters mOutFile' pparams =
    case mOutFile' of
      Nothing -> liftIO $ LBS.putStrLn (encodePretty pparams)
      Just (OutputFile fpath) ->
        handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath) $
          LBS.writeFile fpath (encodePretty pparams)

logExceptContinue :: MonadIO m => (e -> Text) -> ExceptT e m a -> ExceptT e m (Maybe a)
logExceptContinue renderError f = do
  r <- lift $ runExceptT f
  case r of
    Left e -> do
      liftIO $ T.hPutStrLn IO.stderr (renderError e)
      return Nothing
    Right a -> return (Just a)

runQueryTip
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTip (AnyConsensusModeParams cModeParams) network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath
      consensusMode = consensusModeOnly cModeParams

  anyEra <- determineEra cModeParams localNodeConnInfo
  tip <- liftIO $ getLocalChainTip localNodeConnInfo

  let tipSlotNo = case tip of
        ChainTipAtGenesis -> 0
        ChainTip slotNo _ _ -> slotNo

  mEpoch <- mSlotToEpoch consensusMode localNodeConnInfo tipSlotNo
    & fmap tuple3Fst
    & logExceptContinue renderShelleyQueryCmdError

  let output = encodePretty
        . toObject "era" (Just (toJSON anyEra))
        . toObject "epoch" (Just mEpoch)
        $ toJSON tip
  case mOutFile of
    Just (OutputFile fpath) -> liftIO $ LBS.writeFile fpath output
    Nothing                 -> liftIO $ LBS.putStrLn        output
    
  where
    tuple3Fst :: (a, b, c) -> a
    tuple3Fst (a, _, _) = a

    mSlotToEpoch
      :: ConsensusMode mode
      -> LocalNodeConnectInfo mode
      -> SlotNo
      -> ExceptT ShelleyQueryCmdError IO (EpochNo, SlotsInEpoch, SlotsToEpochEnd)
    mSlotToEpoch cMode lNodeConnInfo slotNo = case cMode of
      CardanoMode -> do
        let epochQuery = QueryEraHistory CardanoModeIsMultiEra
        eResult <- liftIO $ queryNodeLocalState lNodeConnInfo Nothing epochQuery
        case eResult of
          Left acqFail -> left (ShelleyQueryCmdAcquireFailure acqFail)
          Right eraHistory -> case slotToEpoch slotNo eraHistory of
            Left e -> throwE (ShelleyQueryCmdPastHorizon e)
            Right a -> return a

      mode -> left (ShelleyQueryCmdUnsupportedMode (AnyConsensusMode mode))

    toObject :: ToJSON a => Text -> Maybe a -> Aeson.Value -> Aeson.Value
    toObject name (Just a) (Aeson.Object obj) =
      Aeson.Object $ obj <> HMS.fromList [name .= toJSON a]
    toObject name Nothing (Aeson.Object obj) =
      Aeson.Object $ obj <> HMS.fromList [name .= Aeson.Null]
    toObject _ _ _ = Aeson.Null

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
--

runQueryUTxO
  :: AnyConsensusModeParams
  -> QueryFilter
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryUTxO (AnyConsensusModeParams cModeParams)
             qfilter network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      qInMode <- createQuery sbe eInMode
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  qInMode
      writeFilteredUTxOs sbe mOutFile result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE
 where
  createQuery
    :: ShelleyBasedEra era
    -> EraInMode era mode
    -> ExceptT ShelleyQueryCmdError IO (QueryInMode mode (Either EraMismatch (UTxO era)))
  createQuery sbe e = do
    let mFilter = maybeFiltered qfilter
        query = QueryInShelleyBasedEra sbe $ QueryUTxO mFilter
    return $ QueryInEra e query

  maybeFiltered :: QueryFilter -> Maybe (Set AddressAny)
  maybeFiltered (FilterByAddress as) = Just as
  maybeFiltered NoFilter = Nothing


-- | Query the current and future parameters for a stake pool, including the retirement date.
-- Any of these may be empty (in which case a null will be displayed).
--

runQueryPoolParams
  :: AnyConsensusModeParams
  -> NetworkId
  -> Hash StakePoolKey
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryPoolParams (AnyConsensusModeParams cModeParams) network poolid = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  eInMode <- toEraInMode era cMode
    & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

  let qInMode = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryDebugLedgerState
  result <- executeQuery era cModeParams localNodeConnInfo qInMode
  obtainLedgerEraClassConstraints sbe (writePoolParams poolid) result


-- | Obtain stake snapshot information for a pool, plus information about the total active stake.
-- This information can be used for leader slot calculation, for example, and has been requested by SPOs.
-- Obtaining the information directly is significantly more time and memory efficient than using a full ledger state dump.
runQueryStakeSnapshot
  :: AnyConsensusModeParams
  -> NetworkId
  -> Hash StakePoolKey
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeSnapshot (AnyConsensusModeParams cModeParams) network poolid = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  eInMode <- toEraInMode era cMode
    & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

  let qInMode = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryDebugLedgerState
  result <- executeQuery era cModeParams localNodeConnInfo qInMode
  obtainLedgerEraClassConstraints sbe (writeStakeSnapshot poolid) result


runQueryLedgerState
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLedgerState (AnyConsensusModeParams cModeParams)
                    network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let qInMode = QueryInEra eInMode
                      . QueryInShelleyBasedEra sbe
                      $ QueryDebugLedgerState
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  qInMode
      obtainLedgerEraClassConstraints sbe (writeLedgerState mOutFile) result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE


runQueryProtocolState
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolState (AnyConsensusModeParams cModeParams)
                      network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let qInMode = QueryInEra eInMode
                      . QueryInShelleyBasedEra sbe
                      $ QueryProtocolState
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  qInMode
      writeProtocolState mOutFile result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE

-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.

runQueryStakeAddressInfo
  :: AnyConsensusModeParams
  -> StakeAddress
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeAddressInfo (AnyConsensusModeParams cModeParams)
                         (StakeAddress _ addr) network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let stakeAddr = Set.singleton $ fromShelleyStakeCredential addr
          query = QueryInEra eInMode
                    . QueryInShelleyBasedEra sbe
                    $ QueryStakeAddresses stakeAddr network

      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  query
      writeStakeAddressInfo mOutFile $ DelegationsAndRewards result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE

-- -------------------------------------------------------------------------------------------------

-- | An error that can occur while querying a node's local state.
data ShelleyQueryCmdLocalStateQueryError
  = AcquireFailureError !LocalStateQuery.AcquireFailure
  | EraMismatchError !EraMismatch
  -- ^ A query from a certain era was applied to a ledger from a different
  -- era.
  | ByronProtocolNotSupportedError
  -- ^ The query does not support the Byron protocol.
  | ShelleyProtocolEraMismatch
  -- ^ The Shelley protocol only supports the Shelley era.
  deriving (Eq, Show)

renderLocalStateQueryError :: ShelleyQueryCmdLocalStateQueryError -> Text
renderLocalStateQueryError lsqErr =
  case lsqErr of
    AcquireFailureError err -> "Local state query acquire failure: " <> show err
    EraMismatchError err ->
      "A query from a certain era was applied to a ledger from a different era: " <> show err
    ByronProtocolNotSupportedError ->
      "The attempted local state query does not support the Byron protocol."
    ShelleyProtocolEraMismatch ->
        "The Shelley protocol mode can only be used with the Shelley era, "
     <> "i.e. with --shelley-mode use --shelley-era flag"

writeStakeAddressInfo
  :: Maybe OutputFile
  -> DelegationsAndRewards
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeAddressInfo mOutFile delegsAndRewards =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty delegsAndRewards)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath (encodePretty delegsAndRewards)

writeLedgerState :: forall era ledgerera.
                    ShelleyLedgerEra era ~ ledgerera
                 => ToJSON (DebugLedgerState era)
                 => FromCBOR (DebugLedgerState era)
                 => Maybe OutputFile
                 -> SerialisedDebugLedgerState era
                 -> ExceptT ShelleyQueryCmdError IO ()
writeLedgerState mOutFile qState@(SerialisedDebugLedgerState serLedgerState) =
  case mOutFile of
    Nothing -> case decodeLedgerState qState of
                 Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs
                 Right ledgerState -> liftIO . LBS.putStrLn $ encodePretty ledgerState
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath $ unSerialised serLedgerState

writeStakeSnapshot :: forall era ledgerera. ()
  => ShelleyLedgerEra era ~ ledgerera
  => Era.Crypto ledgerera ~ StandardCrypto
  => FromCBOR (DebugLedgerState era)
  => PoolId
  -> SerialisedDebugLedgerState era
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeSnapshot (StakePoolKeyHash hk) qState =
  case decodeLedgerState qState of
    -- In the event of decode failure print the CBOR instead
    Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs

    Right ledgerState -> do
      -- Ledger State
      let (DebugLedgerState snapshot) = ledgerState

      -- The three stake snapshots, obtained from the ledger state
      let (SnapShots markS setS goS _) = esSnapshots $ nesEs snapshot

      -- Calculate the three pool and active stake values for the given pool
      liftIO . LBS.putStrLn $ encodePretty $ Stakes
        { markPool = getPoolStake hk markS
        , setPool = getPoolStake hk setS
        , goPool = getPoolStake hk goS
        , markTotal = getAllStake markS
        , setTotal = getAllStake setS
        , goTotal = getAllStake goS
        }

-- | Sum all the stake that is held by the pool
getPoolStake :: KeyHash Shelley.Spec.Ledger.Keys.StakePool crypto -> SnapShot crypto -> Integer
getPoolStake hash ss = pStake
  where
    Coin pStake = fold s
    (Stake s) = poolStake hash (_delegations ss) (_stake ss)

-- | Sum the active stake from a snapshot
getAllStake :: SnapShot crypto -> Integer
getAllStake (SnapShot stake _ _) = activeStake
  where
    Coin activeStake = fold . unStake $ stake

-- | This function obtains the pool parameters, equivalent to the following jq query on the output of query ledger-state
--   .nesEs.esLState._delegationState._pstate._pParams.<pool_id>
writePoolParams :: forall era ledgerera. ()
  => ShelleyLedgerEra era ~ ledgerera
  => FromCBOR (DebugLedgerState era)
  => Crypto.Crypto (Era.Crypto ledgerera)
  => Era.Crypto ledgerera ~ StandardCrypto
  => PoolId
  -> SerialisedDebugLedgerState era
  -> ExceptT ShelleyQueryCmdError IO ()
writePoolParams (StakePoolKeyHash hk) qState =
  case decodeLedgerState qState of
    -- In the event of decode failure print the CBOR instead
    Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs

    Right ledgerState -> do
      let DebugLedgerState snapshot = ledgerState
      let poolState = _pstate $ _delegationState $ esLState $ nesEs snapshot

      -- Pool parameters
      let poolParams = Map.lookup hk $ _pParams poolState
      let fPoolParams = Map.lookup hk $ _fPParams poolState
      let retiring = Map.lookup hk $ _retiring poolState

      liftIO . LBS.putStrLn $ encodePretty $ Params poolParams fPoolParams retiring

decodeLedgerState :: forall era. ()
  => FromCBOR (DebugLedgerState era)
  => SerialisedDebugLedgerState era
  -> Either LBS.ByteString (DebugLedgerState era)
decodeLedgerState (SerialisedDebugLedgerState (Serialised ls)) = first (const ls) (decodeFull ls)

writeProtocolState :: Crypto.Crypto StandardCrypto
                   => Maybe OutputFile
                   -> ProtocolState era
                   -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolState mOutFile ps@(ProtocolState pstate) =
  case mOutFile of
    Nothing -> case decodeProtocolState ps of
                 Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs
                 Right chainDepstate -> liftIO . LBS.putStrLn $ encodePretty chainDepstate
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        . LBS.writeFile fpath $ unSerialised pstate
 where
  decodeProtocolState
    :: ProtocolState era
    -> Either LBS.ByteString (Ledger.ChainDepState StandardCrypto)
  decodeProtocolState (ProtocolState (Serialised pbs)) =
    first (const pbs) (decodeFull pbs)

writeFilteredUTxOs :: ShelleyBasedEra era
                   -> Maybe OutputFile
                   -> UTxO era
                   -> ExceptT ShelleyQueryCmdError IO ()
writeFilteredUTxOs shelleyBasedEra' mOutFile utxo =
    case mOutFile of
      Nothing -> liftIO $ printFilteredUTxOs shelleyBasedEra' utxo
      Just (OutputFile fpath) ->
        case shelleyBasedEra' of
          ShelleyBasedEraShelley -> writeUTxo fpath utxo
          ShelleyBasedEraAllegra -> writeUTxo fpath utxo
          ShelleyBasedEraMary -> writeUTxo fpath utxo
          ShelleyBasedEraAlonzo -> writeUTxo fpath utxo
 where
   writeUTxo fpath utxo' =
     handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
       $ LBS.writeFile fpath (encodePretty utxo')

printFilteredUTxOs :: ShelleyBasedEra era -> UTxO era -> IO ()
printFilteredUTxOs shelleyBasedEra' (UTxO utxo) = do
  Text.putStrLn title
  putStrLn $ replicate (Text.length title + 2) '-'
  case shelleyBasedEra' of
    ShelleyBasedEraShelley ->
      mapM_ (printUtxo shelleyBasedEra') $ Map.toList utxo
    ShelleyBasedEraAllegra ->
      mapM_ (printUtxo shelleyBasedEra') $ Map.toList utxo
    ShelleyBasedEraMary    ->
      mapM_ (printUtxo shelleyBasedEra') $ Map.toList utxo
    ShelleyBasedEraAlonzo -> panic "printFilteredUTxOs: Alonzo era not implemented yet"
 where
   title :: Text
   title =
     "                           TxHash                                 TxIx        Amount"

printUtxo
  :: ShelleyBasedEra era
  -> (TxIn, TxOut era)
  -> IO ()
printUtxo shelleyBasedEra' txInOutTuple =
  case shelleyBasedEra' of
    ShelleyBasedEraShelley ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]

    ShelleyBasedEraAllegra ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]
    ShelleyBasedEraMary ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]
    ShelleyBasedEraAlonzo -> panic "printUtxo: Alonzo era not implemented yet"
 where
  textShowN :: Show a => Int -> a -> Text
  textShowN len x =
    let str = show x
        slen = length str
    in Text.pack $ replicate (max 1 (len - slen)) ' ' ++ str

  printableValue :: TxOutValue era -> Text
  printableValue (TxOutValue _ val) = renderValue defaultRenderValueOptions val
  printableValue (TxOutAdaOnly _ (Lovelace i)) = Text.pack $ show i


runQueryStakeDistribution
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeDistribution (AnyConsensusModeParams cModeParams)
                          network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let query = QueryInEra eInMode
                    . QueryInShelleyBasedEra sbe
                    $ QueryStakeDistribution
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  query
      writeStakeDistribution mOutFile result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE


writeStakeDistribution
  :: Maybe OutputFile
  -> Map PoolId Rational
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeDistribution (Just (OutputFile outFile)) stakeDistrib =
  handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError outFile) $
    LBS.writeFile outFile (encodePretty stakeDistrib)

writeStakeDistribution Nothing stakeDistrib =
  liftIO $ printStakeDistribution stakeDistrib


printStakeDistribution :: Map PoolId Rational -> IO ()
printStakeDistribution stakeDistrib = do
  Text.putStrLn title
  putStrLn $ replicate (Text.length title + 2) '-'
  sequence_
    [ putStrLn $ showStakeDistr poolId stakeFraction
    | (poolId, stakeFraction) <- Map.toList stakeDistrib ]
 where
   title :: Text
   title =
     "                           PoolId                                 Stake frac"

   showStakeDistr :: PoolId
                  -> Rational
                  -- ^ Stake fraction
                  -> String
   showStakeDistr poolId stakeFraction =
     concat
       [ Text.unpack (serialiseToBech32 poolId)
       , "   "
       , showEFloat (Just 3) (fromRational stakeFraction :: Double) ""
       ]

-- | A mapping of Shelley reward accounts to both the stake pool that they
-- delegate to and their reward account balance.
newtype DelegationsAndRewards
  = DelegationsAndRewards (Map StakeAddress Lovelace, Map StakeAddress PoolId)


mergeDelegsAndRewards :: DelegationsAndRewards -> [(StakeAddress, Maybe Lovelace, Maybe PoolId)]
mergeDelegsAndRewards (DelegationsAndRewards (rewardsMap, delegMap)) =
 [ (stakeAddr, Map.lookup stakeAddr rewardsMap, Map.lookup stakeAddr delegMap)
 | stakeAddr <- nub $ Map.keys rewardsMap ++ Map.keys delegMap
 ]


instance ToJSON DelegationsAndRewards where
  toJSON delegsAndRwds =
      Aeson.Array . Vector.fromList
        . map delegAndRwdToJson $ mergeDelegsAndRewards delegsAndRwds
    where
      delegAndRwdToJson :: (StakeAddress, Maybe Lovelace, Maybe PoolId) -> Aeson.Value
      delegAndRwdToJson (addr, mRewards, mPoolId) =
        Aeson.object
          [ "address" .= serialiseAddress addr
          , "delegation" .= mPoolId
          , "rewardAccountBalance" .= mRewards
          ]

-- Helpers

calcEraInMode
  :: CardanoEra era
  -> ConsensusMode mode
  -> ExceptT ShelleyQueryCmdError IO (EraInMode era mode)
calcEraInMode era mode=
  hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode mode) (anyCardanoEra era))
                   $ toEraInMode era mode

determineEra
  :: ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> ExceptT ShelleyQueryCmdError IO AnyCardanoEra
determineEra cModeParams localNodeConnInfo =
  case consensusModeOnly cModeParams of
    ByronMode -> return $ AnyCardanoEra ByronEra
    ShelleyMode -> return $ AnyCardanoEra ShelleyEra
    CardanoMode -> do
      eraQ <- liftIO . queryNodeLocalState localNodeConnInfo Nothing
                     $ QueryCurrentEra CardanoModeIsMultiEra
      case eraQ of
        Left acqFail -> left $ ShelleyQueryCmdAcquireFailure acqFail
        Right anyCarEra -> return anyCarEra

executeQuery
  :: forall result era mode. CardanoEra era
  -> ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> QueryInMode mode (Either EraMismatch result)
  -> ExceptT ShelleyQueryCmdError IO result
executeQuery era cModeP localNodeConnInfo q = do
  eraInMode <- calcEraInMode era $ consensusModeOnly cModeP
  case eraInMode of
    ByronEraInByronMode -> left ShelleyQueryCmdByronEra
    _ -> liftIO execQuery >>= queryResult
 where
   execQuery :: IO (Either AcquireFailure (Either EraMismatch result))
   execQuery = queryNodeLocalState localNodeConnInfo Nothing q

getSbe :: CardanoEraStyle era -> ExceptT ShelleyQueryCmdError IO (ShelleyBasedEra era)
getSbe LegacyByronEra = left ShelleyQueryCmdByronEra
getSbe (ShelleyBasedEra sbe) = return sbe

queryResult
  :: Either AcquireFailure (Either EraMismatch a)
  -> ExceptT ShelleyQueryCmdError IO a
queryResult eAcq =
  case eAcq of
    Left acqFailure -> left $ ShelleyQueryCmdAcquireFailure acqFailure
    Right eResult ->
      case eResult of
        Left err -> left . ShelleyQueryCmdLocalStateQueryError $ EraMismatchError err
        Right result -> return result

obtainLedgerEraClassConstraints
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> ((Ledger.ShelleyBased ledgerera
      , ToJSON (DebugLedgerState era)
      , FromCBOR (DebugLedgerState era)
      , Era.Crypto ledgerera ~ StandardCrypto
      ) => a) -> a
obtainLedgerEraClassConstraints ShelleyBasedEraShelley f = f
obtainLedgerEraClassConstraints ShelleyBasedEraAllegra f = f
obtainLedgerEraClassConstraints ShelleyBasedEraMary    f = f
obtainLedgerEraClassConstraints ShelleyBasedEraAlonzo  _ =
  panic "obtainLedgerEraClassConstraints: Alonzo era not implemented yet"

