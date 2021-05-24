module Cardano.CLI.Shelley.Run
  ( ShelleyClientCmdError
  , renderShelleyClientCmdError
  , runShelleyClientCommand
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Cardano.CLI.Shelley.Parsers

import           Cardano.CLI.Shelley.Run.Address
import           Cardano.CLI.Shelley.Run.Governance
import           Cardano.CLI.Shelley.Run.Key
import           Cardano.CLI.Shelley.Run.Node
import           Cardano.CLI.Shelley.Run.Pool
import           Cardano.CLI.Shelley.Run.Query
import           Cardano.CLI.Shelley.Run.StakeAddress
import           Cardano.CLI.Shelley.Run.Transaction
                                         -- Block, System, DevOps
import           Cardano.CLI.Shelley.Run.Genesis
import           Cardano.CLI.Shelley.Run.TextView

data ShelleyClientCmdError
  = ShelleyCmdAddressError !ShelleyAddressCmdError
  | ShelleyCmdGenesisError !ShelleyGenesisCmdError
  | ShelleyCmdGovernanceError !ShelleyGovernanceCmdError
  | ShelleyCmdNodeError !ShelleyNodeCmdError
  | ShelleyCmdPoolError !ShelleyPoolCmdError
  | ShelleyCmdStakeAddressError !ShelleyStakeAddressCmdError
  | ShelleyCmdTextViewError !ShelleyTextViewFileError
  | ShelleyCmdTransactionError !ShelleyTxCmdError
  | ShelleyCmdQueryError !ShelleyQueryCmdError
  | ShelleyCmdKeyError !ShelleyKeyCmdError
  deriving Show

renderShelleyClientCmdError :: ShelleyCommand -> ShelleyClientCmdError -> Text
renderShelleyClientCmdError cmd err =
  case err of
    ShelleyCmdAddressError addrCmdErr ->
       renderError cmd renderShelleyAddressCmdError addrCmdErr
    ShelleyCmdGenesisError genesisCmdErr ->
       renderError cmd renderShelleyGenesisCmdError genesisCmdErr
    ShelleyCmdGovernanceError govCmdErr ->
       renderError cmd renderShelleyGovernanceError govCmdErr
    ShelleyCmdNodeError nodeCmdErr ->
       renderError cmd renderShelleyNodeCmdError nodeCmdErr
    ShelleyCmdPoolError poolCmdErr ->
       renderError cmd renderShelleyPoolCmdError poolCmdErr
    ShelleyCmdStakeAddressError stakeAddrCmdErr ->
       renderError cmd renderShelleyStakeAddressCmdError stakeAddrCmdErr
    ShelleyCmdTextViewError txtViewErr ->
       renderError cmd renderShelleyTextViewFileError txtViewErr
    ShelleyCmdTransactionError txErr ->
       renderError cmd renderShelleyTxCmdError txErr
    ShelleyCmdQueryError queryErr ->
       renderError cmd renderShelleyQueryCmdError queryErr
    ShelleyCmdKeyError keyErr ->
       renderError cmd renderShelleyKeyCmdError keyErr
 where
   renderError :: ShelleyCommand -> (a -> Text) -> a -> Text
   renderError shelleyCmd renderer shelCliCmdErr =
      mconcat [ "Command failed: "
              , renderShelleyCommand shelleyCmd
              , "  Error: "
              , renderer shelCliCmdErr
              ]


--
-- CLI shelley command dispatch
--

runShelleyClientCommand :: ShelleyCommand -> ExceptT ShelleyClientCmdError IO ()
runShelleyClientCommand (AddressCmd      cmd) = firstExceptT ShelleyCmdAddressError $ runAddressCmd cmd
runShelleyClientCommand (StakeAddressCmd cmd) = firstExceptT ShelleyCmdStakeAddressError $ runStakeAddressCmd cmd
runShelleyClientCommand (KeyCmd          cmd) = firstExceptT ShelleyCmdKeyError $ runKeyCmd cmd
runShelleyClientCommand (TransactionCmd  cmd) = firstExceptT ShelleyCmdTransactionError $ runTransactionCmd  cmd
runShelleyClientCommand (NodeCmd         cmd) = firstExceptT ShelleyCmdNodeError $ runNodeCmd cmd
runShelleyClientCommand (PoolCmd         cmd) = firstExceptT ShelleyCmdPoolError $ runPoolCmd cmd
runShelleyClientCommand (QueryCmd        cmd) = firstExceptT ShelleyCmdQueryError $ runQueryCmd cmd
runShelleyClientCommand (GovernanceCmd   cmd) = firstExceptT ShelleyCmdGovernanceError $ runGovernanceCmd cmd
runShelleyClientCommand (GenesisCmd      cmd) = firstExceptT ShelleyCmdGenesisError $ runGenesisCmd cmd
runShelleyClientCommand (TextViewCmd     cmd) = firstExceptT ShelleyCmdTextViewError $ runTextViewCmd cmd
