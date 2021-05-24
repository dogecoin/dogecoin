{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

import           Cardano.Api
import           Cardano.Slotting.Slot (WithOrigin (At, Origin))
import           Control.Monad (when)
import           Control.Monad.Trans.Except
import           Data.Foldable
import           Data.IORef
import           Data.Maybe (fromMaybe)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Word
import           Network.TypedProtocol.Pipelined (Nat (..))
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
                   (ChainSyncClientPipelined (ChainSyncClientPipelined),
                   ClientPipelinedStIdle (CollectResponse, SendMsgDone, SendMsgRequestNextPipelined),
                   ClientStNext (..))
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
import           System.Environment (getArgs)


main :: IO ()
main = do
  -- Get socket path from CLI argument.
  configFilePath : socketPath : _ <- getArgs
  blockCount <- fmap (either (error . T.unpack . renderFoldBlocksError) id) $ runExceptT $ foldBlocks
    configFilePath
    socketPath
    Mainnet
    True -- enable validation?
    (0 :: Int) -- We just use a count of the blocks as the current state
    (\_env
      !ledgerState
      (BlockInMode (Block (BlockHeader _slotNo _blockHeaderHash (BlockNo blockNoI)) _transactions) _era)
      blockCount -> do
        case ledgerState of
            LedgerStateShelley (Shelley.ShelleyLedgerState shelleyTipWO _ _) -> case shelleyTipWO of
              Origin -> putStrLn "."
              At (Shelley.ShelleyTip _ _ hash) -> print hash
            _ -> when (blockNoI `mod` 100 == 0) (print blockNoI)
        return (blockCount + 1)
    )

  putStrLn $ "Processed " ++ show blockCount ++ " blocks"
  return ()
