
import           Cardano.Api
import           Cardano.Api.ChainSync.Client
import           Cardano.Api.Shelley

import           Control.Monad (when)
import           Data.Time
import           System.Environment (getArgs)
import           System.FilePath ((</>))

-- | Connects to a local cardano node, requests the blocks and prints out the
-- number of transactions. To run this, you must first start a local node e.g.:
--
--     $ cabal run cardano-node:cardano-node -- run \
--        --config        configuration/cardano/mainnet-config.json \
--        --topology      configuration/cardano/mainnet-topology.json \
--        --database-path db \
--        --socket-path   db/node.sock \
--        --host-addr     127.0.0.1 \
--        --port          3001 \
--
-- Then run this with the path to the directory containing node.sock:
--
--     $ cabal run cardano-client-demo:scan-blocks -- db
--
main :: IO ()
main = do
  -- Get socket path from CLI argument.
  socketDir:_ <- getArgs
  let socketPath = socketDir </> "node.sock"

  -- Connect to the node.
  putStrLn $ "Connecting to socket: " <> socketPath
  connectToLocalNode
    (connectInfo socketPath)
    protocols
  where
  connectInfo :: FilePath -> LocalNodeConnectInfo CardanoMode
  connectInfo socketPath =
      LocalNodeConnectInfo {
        localConsensusModeParams = CardanoModeParams (EpochSlots 21600),
        localNodeNetworkId       = Mainnet,
        localNodeSocketPath      = socketPath
      }

  protocols :: LocalNodeClientProtocolsInMode CardanoMode
  protocols =
      LocalNodeClientProtocols {
        localChainSyncClient    = LocalChainSyncClient chainSyncClient,
        localTxSubmissionClient = Nothing,
        localStateQueryClient   = Nothing
      }

-- | Defines the client side of the chain sync protocol.
chainSyncClient :: ChainSyncClient
                     (BlockInMode CardanoMode)
                     ChainPoint
                     ChainTip
                     IO ()
chainSyncClient = ChainSyncClient $ do
  startTime <- getCurrentTime
  let
    clientStIdle :: IO (ClientStIdle (BlockInMode CardanoMode)
                                 ChainPoint ChainTip IO ())
    clientStIdle = do
      -- putStrLn "Chain Sync: requesting next"
      return $ SendMsgRequestNext
        -- There's more to get immediately
        clientStNext

        -- The node is asking us to wait. This is because we reached the
        -- tip. We can certainly carry on here, but for this demo we are
        -- going to stop when we hit the current chain tip.
        clientDone

    clientStNext :: ClientStNext (BlockInMode CardanoMode)
                                 ChainPoint ChainTip IO ()
    clientStNext =
      ClientStNext {
          recvMsgRollForward = \(BlockInMode block@(Block (BlockHeader _ _ (BlockNo blockNo)) _) _) _tip ->
            ChainSyncClient $ do
              when (blockNo `mod` 1000 == 0) $ do
                printBlock block
                now <- getCurrentTime
                let elapsedTime = realToFrac (now `diffUTCTime` startTime) :: Float
                    rate = fromIntegral blockNo / elapsedTime
                putStrLn $ "Rate = " ++ show rate ++ " blocks/second"
              clientStIdle

        , recvMsgRollBackward = \_ _ -> ChainSyncClient clientStIdle
        }

    -- We're still in the "Next" state here, but we've decided to stop
    -- as soon as we get the reply, no matter which reply.
    clientDone :: IO (ClientStNext (BlockInMode CardanoMode)
                                 ChainPoint ChainTip IO ())
    clientDone = do
      putStrLn "Chain Sync: done!"
      return $ ClientStNext {
        recvMsgRollForward  = \_ _ -> ChainSyncClient (pure (SendMsgDone ())),
        recvMsgRollBackward = \_ _ -> ChainSyncClient (pure (SendMsgDone ()))
      }

    printBlock :: Block era -> IO ()
    printBlock (Block (BlockHeader _ _ currBlockNo) transactions)
      = putStrLn $ show currBlockNo ++ " transactions: " ++ show (length transactions)

  clientStIdle
