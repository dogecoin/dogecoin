{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.LedgerState
  ( -- * Initialization / Accumulation
    Env(..)
  , envSecurityParam
  , LedgerState
      ( ..
      , LedgerStateByron
      , LedgerStateShelley
      , LedgerStateAllegra
      , LedgerStateMary
      )
  , initialLedgerState
  , applyBlock

    -- * Traversing the block chain
  , foldBlocks

   -- * Errors
  , FoldBlocksError(..)
  , GenesisConfigError(..)
  , InitialLedgerStateError(..)
  , renderFoldBlocksError
  , renderGenesisConfigError
  , renderInitialLedgerStateError
  )
  where

import           Prelude

import           Control.Exception
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Aeson as Aeson
import qualified Data.Aeson.Types as Data.Aeson.Types.Internal
import           Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray
import           Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Short as BSS
import           Data.Foldable
import           Data.IORef
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word
import qualified Data.Yaml as Yaml
import           System.FilePath

import           Cardano.Api.Block
import           Cardano.Api.Eras
import           Cardano.Api.IPC (ConsensusModeParams (CardanoModeParams), EpochSlots (..),
                   LocalChainSyncClient (LocalChainSyncClientPipelined),
                   LocalNodeClientProtocols (..), LocalNodeClientProtocolsInMode,
                   LocalNodeConnectInfo (..), connectToLocalNode)
import           Cardano.Api.Modes (CardanoMode)
import           Cardano.Api.NetworkId (NetworkId)
import qualified Cardano.Chain.Genesis
import qualified Cardano.Chain.Update
import qualified Cardano.Crypto
import qualified Cardano.Crypto.Hash.Blake2b
import qualified Cardano.Crypto.Hash.Class
import qualified Cardano.Crypto.Hashing
import qualified Cardano.Crypto.ProtocolMagic
import           Cardano.Slotting.Slot (WithOrigin (At, Origin))
import qualified Cardano.Slotting.Slot as Slot
import           Network.TypedProtocol.Pipelined (Nat (..))
import qualified Ouroboros.Consensus.Block.Abstract as Consensus
import qualified Ouroboros.Consensus.Byron.Ledger.Block as Byron
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.Cardano.CanHardFork as Consensus
import qualified Ouroboros.Consensus.Cardano.Node as Consensus
import qualified Ouroboros.Consensus.Config as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as HFC
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics as HFC
import qualified Ouroboros.Consensus.Ledger.Abstract as Ledger
import qualified Ouroboros.Consensus.Ledger.Extended as Ledger
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import qualified Ouroboros.Consensus.Shelley.Eras as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Shelley
import qualified Ouroboros.Consensus.Shelley.Protocol as Shelley
import qualified Ouroboros.Network.Block
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
                   (ChainSyncClientPipelined (ChainSyncClientPipelined),
                   ClientPipelinedStIdle (CollectResponse, SendMsgDone, SendMsgRequestNextPipelined),
                   ClientStNext (..))
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley.Spec
import qualified Shelley.Spec.Ledger.Credential as Shelley.Spec
import qualified Shelley.Spec.Ledger.Genesis as Shelley.Spec
import qualified Shelley.Spec.Ledger.Keys as Shelley.Spec
import qualified Shelley.Spec.Ledger.PParams as Shelley.Spec

data InitialLedgerStateError
  = ILSEConfigFile Text
  -- ^ Failed to read or parse the network config file.
  | ILSEGenesisFile GenesisConfigError
  -- ^ Failed to read or parse a genesis file linked from the network config file.
  | ILSELedgerConsensusConfig GenesisConfigError
  -- ^ Failed to derive the Ledger or Consensus config.

renderInitialLedgerStateError :: InitialLedgerStateError -> Text
renderInitialLedgerStateError ilse = case ilse of
  ILSEConfigFile err ->
    "Failed to read or parse the network config file: " <> err
  ILSEGenesisFile err ->
    "Failed to read or parse a genesis file linked from the network config file: "
    <> renderGenesisConfigError err
  ILSELedgerConsensusConfig err ->
    "Failed to derive the Ledger or Consensus config: "
    <> renderGenesisConfigError err

-- | Get the environment and initial ledger state.
initialLedgerState
  :: FilePath
  -- ^ Path to the cardano-node config file (e.g. <path to cardano-node project>/configuration/cardano/mainnet-config.json)
  ->  ExceptT InitialLedgerStateError IO (Env, LedgerState)
  -- ^ The environment and initial ledger state
initialLedgerState networkConfigFile = do
  -- TODO Once support for querying the ledger config is added to the node, we
  -- can remove the networkConfigFile argument and much of the code in this
  -- module.
  config <- withExceptT ILSEConfigFile
                  (readNetworkConfig (NetworkConfigFile networkConfigFile))
  genesisConfig <- withExceptT ILSEGenesisFile (readCardanoGenesisConfig config)
  env <- withExceptT ILSELedgerConsensusConfig (except (genesisConfigToEnv genesisConfig))
  let ledgerState = initLedgerStateVar genesisConfig
  return (env, ledgerState)

-- | Apply a single block to the current ledger state.
applyBlock
  :: Env
  -- ^ The environment returned by @initialLedgerState@
  -> LedgerState
  -- ^ The current ledger state
  -> Bool
  -- ^ True to perform validation. If True, `tickThenApply` will be used instead
  -- of `tickThenReapply`.
  -> Block era
  -- ^ Some block to apply
  -> Either Text LedgerState
  -- ^ The new ledger state (or an error).
applyBlock env oldState enableValidation block
  = applyBlock' env oldState enableValidation $ case block of
      ByronBlock byronBlock -> Consensus.BlockByron byronBlock
      ShelleyBlock blockEra shelleyBlock -> case blockEra of
        ShelleyBasedEraShelley -> Consensus.BlockShelley shelleyBlock
        ShelleyBasedEraAllegra -> Consensus.BlockAllegra shelleyBlock
        ShelleyBasedEraMary    -> Consensus.BlockMary shelleyBlock
        ShelleyBasedEraAlonzo  -> Consensus.BlockAlonzo shelleyBlock

pattern LedgerStateByron
  :: Ledger.LedgerState Byron.ByronBlock
  -> LedgerState
pattern LedgerStateByron st <- LedgerState (Consensus.LedgerStateByron st)

pattern LedgerStateShelley
  :: Ledger.LedgerState (Shelley.ShelleyBlock (Shelley.ShelleyEra Shelley.StandardCrypto))
  -> LedgerState
pattern LedgerStateShelley st <- LedgerState  (Consensus.LedgerStateShelley st)

pattern LedgerStateAllegra
  :: Ledger.LedgerState (Shelley.ShelleyBlock (Shelley.AllegraEra Shelley.StandardCrypto))
  -> LedgerState
pattern LedgerStateAllegra st <- LedgerState  (Consensus.LedgerStateAllegra st)

pattern LedgerStateMary
  :: Ledger.LedgerState (Shelley.ShelleyBlock (Shelley.MaryEra Shelley.StandardCrypto))
  -> LedgerState
pattern LedgerStateMary st <- LedgerState  (Consensus.LedgerStateMary st)

{-# COMPLETE LedgerStateByron
           , LedgerStateShelley
           , LedgerStateAllegra
           , LedgerStateMary #-}

data FoldBlocksError
  = FoldBlocksInitialLedgerStateError InitialLedgerStateError
  | FoldBlocksApplyBlockError Text

renderFoldBlocksError :: FoldBlocksError -> Text
renderFoldBlocksError fbe = case fbe of
  FoldBlocksInitialLedgerStateError err -> renderInitialLedgerStateError err
  FoldBlocksApplyBlockError err -> "Failed when applying a block: " <> err

-- | Monadic fold over all blocks and ledger states. Stopping @k@ blocks before
-- the node's tip where @k@ is the security parameter.
foldBlocks
  :: forall a.
  FilePath
  -- ^ Path to the cardano-node config file (e.g. <path to cardano-node project>/configuration/cardano/mainnet-config.json)
  -> FilePath
  -- ^ Path to local cardano-node socket. This is the path specified by the @--socket-path@ command line option when running the node.
  -> NetworkId
  -- ^ The network ID.
  -> Bool
  -- ^ True to enable validation. Under the hood this will use @applyBlock@
  -- instead of @reapplyBlock@ from the @ApplyBlock@ type class.
  -> a
  -- ^ The initial accumulator state.
  -> (Env -> LedgerState -> BlockInMode CardanoMode -> a -> IO a)
  -- ^ Accumulator function Takes:
  --  * Environment (this is a constant over the whole fold)
  --  * The current Ledger state (with the current block applied)
  --  * The current Block
  --  * The previous state
  --
  -- And this should return the new state.
  --
  -- Note: This function can safely assume no rollback will occur even though
  -- internally this is implemented with a client protocol that may require
  -- rollback. This is achieved by only calling the accumulator on states/blocks
  -- that are older than the security parameter, k. This has the side effect of
  -- truncating the last k blocks before the node's tip.
  -> ExceptT FoldBlocksError IO a
  -- ^ The final state
foldBlocks nodeConfigFilePath socketPath networkId enableValidation state0 accumulate = do
  -- NOTE this was originally implemented with a non-pipelined client then
  -- changed to a pipelined client for a modest speedup:
  --  * Non-pipelined: 1h  0m  19s
  --  * Pipelined:        46m  23s

  (env, ledgerState) <- withExceptT FoldBlocksInitialLedgerStateError
                            (initialLedgerState nodeConfigFilePath)

  -- Place to store the accumulated state
  -- This is a bit ugly, but easy.
  errorIORef <- lift $ newIORef Nothing
  stateIORef <- lift $ newIORef state0

  -- Connect to the node.
  lift $ connectToLocalNode
    connectInfo
    (protocols stateIORef errorIORef env ledgerState)

  lift (readIORef errorIORef) >>= \case
    Just err -> throwE (FoldBlocksApplyBlockError err)
    Nothing -> lift $ readIORef stateIORef
  where
    connectInfo :: LocalNodeConnectInfo CardanoMode
    connectInfo =
        LocalNodeConnectInfo {
          localConsensusModeParams = CardanoModeParams (EpochSlots 21600),
          localNodeNetworkId       = networkId,
          localNodeSocketPath      = socketPath
        }

    protocols :: IORef a -> IORef (Maybe Text) -> Env -> LedgerState -> LocalNodeClientProtocolsInMode CardanoMode
    protocols stateIORef errorIORef env ledgerState =
        LocalNodeClientProtocols {
          localChainSyncClient    = LocalChainSyncClientPipelined (chainSyncClient 50 stateIORef errorIORef env ledgerState),
          localTxSubmissionClient = Nothing,
          localStateQueryClient   = Nothing
        }

    -- | Add a new ledger state to the history
    pushLedgerState
      :: Env                -- ^ Environement used to get the security param, k.
      -> LedgerStateHistory -- ^ History of k ledger states.
      -> SlotNo             -- ^ Slot number of the new ledger state.
      -> LedgerState        -- ^ New ledger state to add to the history
      -> BlockInMode CardanoMode
                            -- ^ The block that (when applied to the previous
                            -- ledger state) resulted in the new ledger state.
      -> (LedgerStateHistory, LedgerStateHistory)
      -- ^ ( The new history with the new state appended
      --   , Any exisiting ledger states that are now past the security parameter
      --      and hence can no longer be rolled back.
      --   )
    pushLedgerState env hist ix st block
      = Seq.splitAt
          (fromIntegral $ envSecurityParam env + 1)
          ((ix, st, At block) Seq.:<| hist)

    rollBackLedgerStateHist :: LedgerStateHistory -> SlotNo -> LedgerStateHistory
    rollBackLedgerStateHist hist maxInc = Seq.dropWhileL ((> maxInc) . (\(x,_,_) -> x)) hist

    -- | Defines the client side of the chain sync protocol.
    chainSyncClient :: Word32
                    -- ^ The maximum number of concurrent requests.
                    -> IORef a
                    -> IORef (Maybe Text)
                    -- ^ Resulting error if any. Written to once on protocol
                    -- completion.
                    -> Env
                    -> LedgerState
                    -> ChainSyncClientPipelined
                        (BlockInMode CardanoMode)
                        ChainPoint
                        ChainTip
                        IO ()
                    -- ^ Client returns maybe an error.
    chainSyncClient pipelineSize stateIORef errorIORef env ledgerState0
      = ChainSyncClientPipelined $ pure $ clientIdle_RequestMoreN Origin Origin Zero initialLedgerStateHistory
      where
          initialLedgerStateHistory = Seq.singleton (0, ledgerState0, Origin)

          pushLedgerState' = pushLedgerState env

          clientIdle_RequestMoreN
            :: WithOrigin BlockNo
            -> WithOrigin BlockNo
            -> Nat n -- Number of requests inflight.
            -> LedgerStateHistory
            -> ClientPipelinedStIdle n (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
          clientIdle_RequestMoreN clientTip serverTip n knownLedgerStates
            = case pipelineDecisionMax pipelineSize n clientTip serverTip  of
                Collect -> case n of
                  Succ predN -> CollectResponse Nothing (clientNextN predN knownLedgerStates)
                _ -> SendMsgRequestNextPipelined (clientIdle_RequestMoreN clientTip serverTip (Succ n) knownLedgerStates)

          clientNextN
            :: Nat n -- Number of requests inflight.
            -> LedgerStateHistory
            -> ClientStNext n (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
          clientNextN n knownLedgerStates =
            ClientStNext {
                recvMsgRollForward = \blockInMode@(BlockInMode block@(Block (BlockHeader slotNo _ currBlockNo) _) _era) serverChainTip -> do
                  let newLedgerStateE = applyBlock
                        env
                        (maybe
                          (error "Impossible! Missing Ledger state")
                          (\(_,x,_) -> x)
                          (Seq.lookup 0 knownLedgerStates)
                        )
                        enableValidation
                        block
                  case newLedgerStateE of
                    Left err -> clientIdle_DoneN n (Just err)
                    Right newLedgerState -> do
                      let (knownLedgerStates', committedStates) = pushLedgerState' knownLedgerStates slotNo newLedgerState blockInMode
                          newClientTip = At currBlockNo
                          newServerTip = fromChainTip serverChainTip
                      forM_ committedStates $ \(_, currLedgerState, currBlockMay) -> case currBlockMay of
                          Origin -> return ()
                          At currBlock -> do
                            newState <- accumulate env currLedgerState currBlock =<< readIORef stateIORef
                            writeIORef stateIORef newState
                      if newClientTip == newServerTip
                        then  clientIdle_DoneN n Nothing
                        else return (clientIdle_RequestMoreN newClientTip newServerTip n knownLedgerStates')
              , recvMsgRollBackward = \chainPoint serverChainTip -> do
                  let newClientTip = Origin -- We don't actually keep track of blocks so we temporarily "forget" the tip.
                      newServerTip = fromChainTip serverChainTip
                      truncatedKnownLedgerStates = case chainPoint of
                          ChainPointAtGenesis -> initialLedgerStateHistory
                          ChainPoint slotNo _ -> rollBackLedgerStateHist knownLedgerStates slotNo
                  return (clientIdle_RequestMoreN newClientTip newServerTip n truncatedKnownLedgerStates)
              }

          clientIdle_DoneN
            :: Nat n -- Number of requests inflight.
            -> Maybe Text -- Return value (maybe an error)
            -> IO (ClientPipelinedStIdle n (BlockInMode CardanoMode) ChainPoint ChainTip IO ())
          clientIdle_DoneN n errorMay = case n of
            Succ predN -> return (CollectResponse Nothing (clientNext_DoneN predN errorMay)) -- Ignore remaining message responses
            Zero -> do
              writeIORef errorIORef errorMay
              return (SendMsgDone ())

          clientNext_DoneN
            :: Nat n -- Number of requests inflight.
            -> Maybe Text -- Return value (maybe an error)
            -> ClientStNext n (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
          clientNext_DoneN n errorMay =
            ClientStNext {
                recvMsgRollForward = \_ _ -> clientIdle_DoneN n errorMay
              , recvMsgRollBackward = \_ _ -> clientIdle_DoneN n errorMay
              }

          fromChainTip :: ChainTip -> WithOrigin BlockNo
          fromChainTip ct = case ct of
            ChainTipAtGenesis -> Origin
            ChainTip _ _ bno -> At bno

-- | A history of k (security parameter) recent ledger states. The head is the
-- most recent item. Elements are:
--
-- * Slot number that a new block occurred
-- * The ledger state after applying the new block
-- * The new block
--
type LedgerStateHistory = Seq (SlotNo, LedgerState, WithOrigin (BlockInMode CardanoMode))

--------------------------------------------------------------------------------
-- Everything below was copied/adapted from db-sync                           --
--------------------------------------------------------------------------------

genesisConfigToEnv
  :: GenesisConfig
  -> Either GenesisConfigError Env
genesisConfigToEnv
  -- enp
  genCfg =
    case genCfg of
      GenesisCardano _ bCfg sCfg
        | Cardano.Crypto.ProtocolMagic.unProtocolMagicId (Cardano.Chain.Genesis.configProtocolMagicId bCfg) /= Shelley.Spec.sgNetworkMagic (scConfig sCfg) ->
            Left . NECardanoConfig $
              mconcat
                [ "ProtocolMagicId ", textShow (Cardano.Crypto.ProtocolMagic.unProtocolMagicId $ Cardano.Chain.Genesis.configProtocolMagicId bCfg)
                , " /= ", textShow (Shelley.Spec.sgNetworkMagic $ scConfig sCfg)
                ]
        | Cardano.Chain.Genesis.gdStartTime (Cardano.Chain.Genesis.configGenesisData bCfg) /= Shelley.Spec.sgSystemStart (scConfig sCfg) ->
            Left . NECardanoConfig $
              mconcat
                [ "SystemStart ", textShow (Cardano.Chain.Genesis.gdStartTime $ Cardano.Chain.Genesis.configGenesisData bCfg)
                , " /= ", textShow (Shelley.Spec.sgSystemStart $ scConfig sCfg)
                ]
        | otherwise ->
            let
              topLevelConfig = Consensus.pInfoConfig (mkProtocolInfoCardano genCfg)
            in
            Right $ Env
                  { envLedgerConfig = Consensus.topLevelConfigLedger topLevelConfig
                  , envProtocolConfig = Consensus.topLevelConfigProtocol topLevelConfig
                  }

readNetworkConfig :: NetworkConfigFile -> ExceptT Text IO NodeConfig
readNetworkConfig (NetworkConfigFile ncf) = do
    ncfg <- (except . parseNodeConfig) =<< readByteString ncf "node"
    return ncfg
      { ncByronGenesisFile = adjustGenesisFilePath (mkAdjustPath ncf) (ncByronGenesisFile ncfg)
      , ncShelleyGenesisFile = adjustGenesisFilePath (mkAdjustPath ncf) (ncShelleyGenesisFile ncfg)
      }

data NodeConfig = NodeConfig
  { ncPBftSignatureThreshold :: !(Maybe Double)
  , ncByronGenesisFile :: !GenesisFile
  , ncByronGenesisHash :: !GenesisHashByron
  , ncShelleyGenesisFile :: !GenesisFile
  , ncShelleyGenesisHash :: !GenesisHashShelley
  , ncRequiresNetworkMagic :: !Cardano.Crypto.RequiresNetworkMagic
  , ncByronSoftwareVersion :: !Cardano.Chain.Update.SoftwareVersion
  , ncByronProtocolVersion :: !Cardano.Chain.Update.ProtocolVersion

  -- Per-era parameters for the hardfok transitions:
  , ncByronToShelley   :: !(Consensus.ProtocolTransitionParamsShelleyBased
                              Shelley.StandardShelley)
  , ncShelleyToAllegra :: !(Consensus.ProtocolTransitionParamsShelleyBased
                              Shelley.StandardAllegra)
  , ncAllegraToMary    :: !(Consensus.ProtocolTransitionParamsShelleyBased
                              Shelley.StandardMary)
--TODO:
--, ncMaryToAlonzo     :: !(Consensus.ProtocolTransitionParamsShelleyBased
--                            Shelley.StandardAlonzo)
  }

instance FromJSON NodeConfig where
  parseJSON v =
      Aeson.withObject "NodeConfig" parse v
    where
      parse :: Object -> Data.Aeson.Types.Internal.Parser NodeConfig
      parse o =
        NodeConfig
          <$> o .:? "PBftSignatureThreshold"
          <*> fmap GenesisFile (o .: "ByronGenesisFile")
          <*> fmap GenesisHashByron (o .: "ByronGenesisHash")
          <*> fmap GenesisFile (o .: "ShelleyGenesisFile")
          <*> fmap GenesisHashShelley (o .: "ShelleyGenesisHash")
          <*> o .: "RequiresNetworkMagic"
          <*> parseByronSoftwareVersion o
          <*> parseByronProtocolVersion o
          <*> (Consensus.ProtocolTransitionParamsShelleyBased ()
                 <$> parseShelleyHardForkEpoch o)
          <*> (Consensus.ProtocolTransitionParamsShelleyBased ()
                 <$> parseAllegraHardForkEpoch o)
          <*> (Consensus.ProtocolTransitionParamsShelleyBased ()
                 <$> parseMaryHardForkEpoch o)
--        <*> (Consensus.ProtocolTransitionParamsShelleyBased ({-alonzo genesis-})
--               <$> parseAlonzoHardForkEpoch o)

      parseByronProtocolVersion :: Object -> Data.Aeson.Types.Internal.Parser Cardano.Chain.Update.ProtocolVersion
      parseByronProtocolVersion o =
        Cardano.Chain.Update.ProtocolVersion
          <$> o .: "LastKnownBlockVersion-Major"
          <*> o .: "LastKnownBlockVersion-Minor"
          <*> o .: "LastKnownBlockVersion-Alt"

      parseByronSoftwareVersion :: Object -> Data.Aeson.Types.Internal.Parser Cardano.Chain.Update.SoftwareVersion
      parseByronSoftwareVersion o =
        Cardano.Chain.Update.SoftwareVersion
          <$> fmap Cardano.Chain.Update.ApplicationName (o .: "ApplicationName")
          <*> o .: "ApplicationVersion"

      parseShelleyHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Consensus.TriggerHardFork
      parseShelleyHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestShelleyHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 2 -- Mainnet default
          ]

      parseAllegraHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Consensus.TriggerHardFork
      parseAllegraHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestAllegraHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 3 -- Mainnet default
          ]

      parseMaryHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Consensus.TriggerHardFork
      parseMaryHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestMaryHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 4 -- Mainnet default
          ]

parseNodeConfig :: ByteString -> Either Text NodeConfig
parseNodeConfig bs =
  case Yaml.decodeEither' bs of
    Left err -> Left $ "Error parsing node config: " <> textShow err
    Right nc -> Right nc

adjustGenesisFilePath :: (FilePath -> FilePath) -> GenesisFile -> GenesisFile
adjustGenesisFilePath f (GenesisFile p) = GenesisFile (f p)

mkAdjustPath :: FilePath -> (FilePath -> FilePath)
mkAdjustPath nodeConfigFilePath fp = takeDirectory nodeConfigFilePath </> fp

readByteString :: FilePath -> Text -> ExceptT Text IO ByteString
readByteString fp cfgType = ExceptT $
  catch (Right <$> BS.readFile fp) $ \(_ :: IOException) ->
    return $ Left $ mconcat
      [ "Cannot read the ", cfgType, " configuration file at : ", Text.pack fp ]

initLedgerStateVar :: GenesisConfig -> LedgerState
initLedgerStateVar genesisConfig = LedgerState
  { clsState = Ledger.ledgerState $ Consensus.pInfoInitLedger protocolInfo
  }
  where
    protocolInfo = mkProtocolInfoCardano genesisConfig

newtype LedgerState = LedgerState
  { clsState :: Ledger.LedgerState
                  (HFC.HardForkBlock
                    (Consensus.CardanoEras Consensus.StandardCrypto))
  }

-- Usually only one constructor, but may have two when we are preparing for a HFC event.
data GenesisConfig
  = GenesisCardano !NodeConfig !Cardano.Chain.Genesis.Config !ShelleyConfig

data ShelleyConfig = ShelleyConfig
  { scConfig :: !(Shelley.Spec.ShelleyGenesis Shelley.StandardShelley)
  , scGenesisHash :: !GenesisHashShelley
  }


newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  } deriving Show

newtype GenesisHashByron = GenesisHashByron
  { unGenesisHashByron :: Text
  } deriving newtype (Eq, Show)

newtype GenesisHashShelley = GenesisHashShelley
  { unGenesisHashShelley :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype LedgerStateDir = LedgerStateDir
  {  unLedgerStateDir :: FilePath
  } deriving Show

newtype NetworkName = NetworkName
  { unNetworkName :: Text
  } deriving Show

newtype NetworkConfigFile = NetworkConfigFile
  { unNetworkConfigFile :: FilePath
  } deriving Show

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  } deriving Show

mkProtocolInfoCardano ::
  GenesisConfig ->
  Consensus.ProtocolInfo
    IO
    (HFC.HardForkBlock
            (Consensus.CardanoEras Consensus.StandardCrypto))
mkProtocolInfoCardano (GenesisCardano dnc byronGenesis shelleyGenesis)
  = Consensus.protocolInfoCardano
          Consensus.ProtocolParamsByron
            { Consensus.byronGenesis = byronGenesis
            , Consensus.byronPbftSignatureThreshold = Consensus.PBftSignatureThreshold <$> ncPBftSignatureThreshold dnc
            , Consensus.byronProtocolVersion = ncByronProtocolVersion dnc
            , Consensus.byronSoftwareVersion = ncByronSoftwareVersion dnc
            , Consensus.byronLeaderCredentials = Nothing
            }
          Consensus.ProtocolParamsShelleyBased
            { Consensus.shelleyBasedGenesis = scConfig shelleyGenesis
            , Consensus.shelleyBasedInitialNonce = shelleyPraosNonce shelleyGenesis
            , Consensus.shelleyBasedLeaderCredentials = []
            }
          Consensus.ProtocolParamsShelley
            { Consensus.shelleyProtVer = shelleyProtVer dnc
            }
          Consensus.ProtocolParamsAllegra
            { Consensus.allegraProtVer = shelleyProtVer dnc
            }
          Consensus.ProtocolParamsMary
            { Consensus.maryProtVer = shelleyProtVer dnc
            }
          Consensus.ProtocolParamsAlonzo
            { Consensus.alonzoProtVer = error "mkProtocolInfoCardano: alonzoProtVer"
            }
          (ncByronToShelley dnc)
          (ncShelleyToAllegra dnc)
          (ncAllegraToMary dnc)
          (error "mkProtocolInfoCardano: ProtocolTransitionParamsShelleyBased StandardAlonzo")

shelleyPraosNonce :: ShelleyConfig -> Shelley.Spec.Nonce
shelleyPraosNonce sCfg = Shelley.Spec.Nonce (Cardano.Crypto.Hash.Class.castHash . unGenesisHashShelley $ scGenesisHash sCfg)

shelleyProtVer :: NodeConfig -> Shelley.Spec.ProtVer
shelleyProtVer dnc =
  let bver = ncByronProtocolVersion dnc in
  Shelley.Spec.ProtVer
    (fromIntegral $ Cardano.Chain.Update.pvMajor bver)
    (fromIntegral $ Cardano.Chain.Update.pvMinor bver)

readCardanoGenesisConfig
        :: NodeConfig
        -> ExceptT GenesisConfigError IO GenesisConfig
readCardanoGenesisConfig enc =
  GenesisCardano enc <$> readByronGenesisConfig enc <*> readShelleyGenesisConfig enc

data GenesisConfigError
  = NEError !Text
  | NEByronConfig !FilePath !Cardano.Chain.Genesis.ConfigurationError
  | NEShelleyConfig !FilePath !Text
  | NECardanoConfig !Text

renderGenesisConfigError :: GenesisConfigError -> Text
renderGenesisConfigError ne =
  case ne of
    NEError t -> "Error: " <> t
    NEByronConfig fp ce ->
      mconcat
        [ "Failed reading Byron genesis file ", textShow fp, ": ", textShow ce
        ]
    NEShelleyConfig fp txt ->
      mconcat
        [ "Failed reading Shelley genesis file ", textShow fp, ": ", txt
        ]
    NECardanoConfig err ->
      mconcat
        [ "With Cardano protocol, Byron/Shelley config mismatch:\n"
        , "   ", err
        ]

data LookupFail
  = DbLookupBlockHash !ByteString
  | DbLookupBlockId !Word64
  | DbLookupMessage !Text
  | DbLookupTxHash !ByteString
  | DbLookupTxOutPair !ByteString !Word16
  | DbLookupEpochNo !Word64
  | DbLookupSlotNo !Word64
  | DbMetaEmpty
  | DbMetaMultipleRows
  deriving (Eq, Show)

readByronGenesisConfig
        :: NodeConfig
        -> ExceptT GenesisConfigError IO Cardano.Chain.Genesis.Config
readByronGenesisConfig enc = do
  let file = unGenesisFile $ ncByronGenesisFile enc
  genHash <- firstExceptT NEError
                . hoistEither
                $ Cardano.Crypto.Hashing.decodeAbstractHash (unGenesisHashByron $ ncByronGenesisHash enc)
  firstExceptT (NEByronConfig file)
                $ Cardano.Chain.Genesis.mkConfigFromFile (ncRequiresNetworkMagic enc) file genHash

readShelleyGenesisConfig
    :: NodeConfig
    -> ExceptT GenesisConfigError IO ShelleyConfig
readShelleyGenesisConfig enc = do
  let file = unGenesisFile $ ncShelleyGenesisFile enc
  firstExceptT (NEShelleyConfig file . renderShelleyGenesisError)
    $ readGenesis (GenesisFile file) (ncShelleyGenesisHash enc)

textShow :: Show a => a -> Text
textShow = Text.pack . show

readGenesis
    :: GenesisFile -> GenesisHashShelley
    -> ExceptT ShelleyGenesisError IO ShelleyConfig
readGenesis (GenesisFile file) expectedGenesisHash = do
    content <- handleIOExceptT (GenesisReadError file . textShow) $ BS.readFile file
    let genesisHash = GenesisHashShelley (Cardano.Crypto.Hash.Class.hashWith id content)
    checkExpectedGenesisHash genesisHash
    genesis <- firstExceptT (GenesisDecodeError file . Text.pack)
                  . hoistEither
                  $ Aeson.eitherDecodeStrict' content
    pure $ ShelleyConfig genesis genesisHash
  where
    checkExpectedGenesisHash :: GenesisHashShelley -> ExceptT ShelleyGenesisError IO ()
    checkExpectedGenesisHash actual =
      if actual /= expectedGenesisHash
        then left (GenesisHashMismatch actual expectedGenesisHash)
        else pure ()

data ShelleyGenesisError
     = GenesisReadError !FilePath !Text
     | GenesisHashMismatch !GenesisHashShelley !GenesisHashShelley -- actual, expected
     | GenesisDecodeError !FilePath !Text
     deriving Show

renderShelleyGenesisError :: ShelleyGenesisError -> Text
renderShelleyGenesisError sge =
    case sge of
      GenesisReadError fp err ->
        mconcat
          [ "There was an error reading the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

      GenesisHashMismatch actual expected ->
        mconcat
          [ "Wrong Shelley genesis file: the actual hash is ", renderHash actual
          , ", but the expected Shelley genesis hash given in the node "
          , "configuration file is ", renderHash expected, "."
          ]

      GenesisDecodeError fp err ->
        mconcat
          [ "There was an error parsing the genesis file: ", Text.pack fp
          , " Error: ", err
          ]
  where
    renderHash :: GenesisHashShelley -> Text
    renderHash (GenesisHashShelley h) = Text.decodeUtf8 $ Base16.encode (Cardano.Crypto.Hash.Class.hashToBytes h)

newtype StakeCred
  = StakeCred { _unStakeCred :: Shelley.Spec.Credential 'Shelley.Spec.Staking Consensus.StandardCrypto }
  deriving (Eq, Ord)

data Env = Env
  { envLedgerConfig :: HFC.HardForkLedgerConfig (Consensus.CardanoEras Shelley.StandardCrypto)
  , envProtocolConfig :: Shelley.ConsensusConfig (HFC.HardForkProtocol (Consensus.CardanoEras Shelley.StandardCrypto))
  }

envSecurityParam :: Env -> Word64
envSecurityParam env = k
  where
    Consensus.SecurityParam k
      = HFC.hardForkConsensusConfigK
      $ envProtocolConfig env

-- The function 'tickThenReapply' does zero validation, so add minimal
-- validation ('blockPrevHash' matches the tip hash of the 'LedgerState'). This
-- was originally for debugging but the check is cheap enough to keep.
applyBlock'
  :: Env
  -> LedgerState
  -> Bool
  -- ^ True to validate
  ->  HFC.HardForkBlock
            (Consensus.CardanoEras Consensus.StandardCrypto)
  -> Either Text LedgerState
applyBlock' env oldState enableValidation block = do
  let config = envLedgerConfig env
      stateOld = clsState oldState
  stateNew <- if enableValidation
    then tickThenApply config block stateOld
    else tickThenReapplyCheckHash config block stateOld
  return oldState { clsState = stateNew }

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from
-- the block matches the head hash of the ledger state.
tickThenReapplyCheckHash
    :: HFC.HardForkLedgerConfig
        (Consensus.CardanoEras Shelley.StandardCrypto)
    -> Consensus.CardanoBlock Consensus.StandardCrypto
    -> Shelley.LedgerState
        (HFC.HardForkBlock
            (Consensus.CardanoEras Shelley.StandardCrypto))
    -> Either Text (Shelley.LedgerState
        (HFC.HardForkBlock
            (Consensus.CardanoEras Shelley.StandardCrypto)))
tickThenReapplyCheckHash cfg block lsb =
  if Consensus.blockPrevHash block == Ledger.ledgerTipHash lsb
    then Right $ Ledger.tickThenReapply cfg block lsb
    else Left $ mconcat
                  [ "Ledger state hash mismatch. Ledger head is slot "
                  , textShow
                      $ Slot.unSlotNo
                      $ Slot.fromWithOrigin
                          (Slot.SlotNo 0)
                          (Ledger.ledgerTipSlot lsb)
                  , " hash "
                  , renderByteArray
                      $ unChainHash
                      $ Ledger.ledgerTipHash lsb
                  , " but block previous hash is "
                  , renderByteArray (unChainHash $ Consensus.blockPrevHash block)
                  , " and block current hash is "
                  , renderByteArray
                      $ BSS.fromShort
                      $ HFC.getOneEraHash
                      $ Ouroboros.Network.Block.blockHash block
                  , "."
                  ]

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from
-- the block matches the head hash of the ledger state.
tickThenApply
    :: HFC.HardForkLedgerConfig
        (Consensus.CardanoEras Shelley.StandardCrypto)
    -> Consensus.CardanoBlock Consensus.StandardCrypto
    -> Shelley.LedgerState
        (HFC.HardForkBlock
            (Consensus.CardanoEras Shelley.StandardCrypto))
    -> Either Text (Shelley.LedgerState
        (HFC.HardForkBlock
            (Consensus.CardanoEras Shelley.StandardCrypto)))
tickThenApply cfg block lsb
  = either (Left . Text.pack . show) Right
  $ runExcept
  $ Ledger.tickThenApply cfg block lsb

renderByteArray :: ByteArrayAccess bin => bin -> Text
renderByteArray =
  Text.decodeUtf8 . Base16.encode . Data.ByteArray.convert

unChainHash :: Ouroboros.Network.Block.ChainHash (Consensus.CardanoBlock era) -> ByteString
unChainHash ch =
  case ch of
    Ouroboros.Network.Block.GenesisHash -> "genesis"
    Ouroboros.Network.Block.BlockHash bh -> BSS.fromShort (HFC.getOneEraHash bh)

