{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-local-binds -Wno-unused-matches #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Testnet.ByronShelley
  ( ForkPoint(..)
  , TestnetOptions(..)
  , defaultTestnetOptions

  , testnet
  ) where

#ifdef UNIX
import           Prelude (map)
#endif

import           Control.Monad
import           Data.Aeson ((.=))
import           Data.Eq
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.List ((\\))
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.String
import           GHC.Float
import           GHC.Num
import           GHC.Real
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import           Hedgehog.Extras.Stock.Time
import           System.FilePath.Posix ((</>))
import           Text.Read
import           Text.Show

import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.Aeson as J
import qualified Hedgehog.Extras.Stock.IO.File as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.String as S
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Concurrent as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Network as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified System.IO as IO
import qualified System.Info as OS
#ifdef UNIX
import           System.Posix.Files
#endif
import qualified System.Process as IO
import qualified Test.Process as H
import qualified Testnet.Conf as H

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}

data ForkPoint
  = AtVersion Int
  | AtEpoch Int
  deriving (Show, Eq, Read)

data TestnetOptions = TestnetOptions
  { numBftNodes :: Int
  , numPoolNodes :: Int
  , activeSlotsCoeff :: Double
  , epochLength :: Int
  , forkPoint :: ForkPoint
  } deriving (Eq, Show)

defaultTestnetOptions :: TestnetOptions
defaultTestnetOptions = TestnetOptions
  { numBftNodes = 2
  , numPoolNodes = 1
  , activeSlotsCoeff = 0.1
  , epochLength = 1500
  , forkPoint = AtVersion 1
  }

ifaceAddress :: String
ifaceAddress = "127.0.0.1"

testnet :: TestnetOptions -> H.Conf -> H.Integration [String]
testnet testnetOptions H.Conf {..} = do
  -- This script sets up a cluster that starts out in Byron, and can transition to Shelley.
  --
  -- The script generates all the files needed for the setup, and prints commands
  -- to be run manually (to start the nodes, post transactions, etc.).
  --
  -- There are three ways of triggering the transition to Shelley:
  -- 1. Trigger transition at protocol version 2.0.0 (as on mainnet)
  --    The system starts at 0.0.0, and we can only increase it by 1 in the major
  --    version, so this does require to
  --    a) post an update proposal and votes to transition to 1.0.0
  --    b) wait for the protocol to change (end of the epoch, or end of the last
  --      epoch if it's posted near the end of the epoch)
  --    c) change configuration.yaml to have 'LastKnownBlockVersion-Major: 2',
  --      and restart the nodes
  --    d) post an update proposal and votes to transition to 2.0.0
  --    This is what will happen on the mainnet, so it's vital to test this, but
  --    it does contain some manual steps.
  -- 2. Trigger transition at protocol version 2.0.0
  --    For testing purposes, we can also modify the system to do the transition to
  --    Shelley at protocol version 1.0.0, by uncommenting the line containing
  --    'TestShelleyHardForkAtVersion' below. Then, we just need to execute step a)
  --    above in order to trigger the transition.
  --    This is still close to the procedure on the mainnet, and requires less
  --    manual steps.
  -- 3. Schedule transition in the configuration
  --    To do this, uncomment the line containing 'TestShelleyHardForkAtEpoch'
  --    below. It's good for a quick test, and does not rely on posting update
  --    proposals to the chain.
  --    This is quite convenient, but it does not test that we can do the
  --    transition by posting update proposals to the network.
  --
  -- TODO: The script allows transitioning to Shelley, but not yet to register a
  -- pool and delegate, so all blocks will still be produced by the BFT nodes.
  -- We will need CLI support for Byron witnesses in Shelley transactions to do
  -- that.

  void $ H.note OS.os
  env <- H.evalIO IO.getEnvironment
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime 15 currentTime -- 15 seconds into the future

  let bftNodesN = [1 .. numBftNodes testnetOptions]
  let poolNodesN = [1 .. numPoolNodes testnetOptions]
  let bftNodes = ("node-bft" <>) . show @Int <$> bftNodesN
  let poolNodes = ("node-pool" <> ) . show @Int <$> poolNodesN
  let allNodes = bftNodes <> poolNodes
  let initSupply = 1000000000
  let maxSupply = 1000000000
  let fundsPerGenesisAddress = initSupply `div` numBftNodes testnetOptions
  let fundsPerByronAddress = fundsPerGenesisAddress * 9 `div` 10
  let userPoolN = poolNodesN

  allPorts <- H.noteShowIO $ IO.allocateRandomPorts (L.length allNodes)
  nodeToPort <- H.noteShow (M.fromList (L.zip allNodes allPorts))

  let securityParam = 10

  H.createDirectoryIfMissing logDir

  -- Choose one of the following fork methods:
  forkMethod <- H.noteShow $ case forkPoint testnetOptions of
    AtVersion n -> ["TestShelleyHardForkAtVersion: " <> show @Int n]
    AtEpoch n -> ["TestShelleyHardForkAtEpoch: " <> show @Int n]

  H.readFile (base </> "configuration/chairman/byron-shelley/configuration.yaml")
    <&> L.unlines . (<> forkMethod) . L.lines
    >>= H.writeFile (tempAbsPath </> "configuration.yaml")

  forM_ allNodes $ \node -> do
    H.createDirectoryIfMissing $ tempAbsPath </> node
    H.createDirectoryIfMissing $ tempAbsPath </> node </> "byron"
    H.createDirectoryIfMissing $ tempAbsPath </> node </> "shelley"

  -- Make topology files
  forM_ allNodes $ \node -> do
    let port = fromJust $ M.lookup node nodeToPort
    H.lbsWriteFile (tempAbsPath </> node </> "topology.json") $ J.encode $
      J.object
      [ "Producers" .= J.toJSON
        [ J.object
          [ "addr" .= J.toJSON @String ifaceAddress
          , "port" .= J.toJSON @Int peerPort
          , "valency" .= J.toJSON @Int 1
          ]
        | peerPort <- allPorts \\ [port]
        ]
      ]
    H.writeFile (tempAbsPath </> node </> "port") (show port)

  H.lbsWriteFile (tempAbsPath </> "byron.genesis.spec.json") . J.encode $ J.object
    [ "heavyDelThd" .= J.toJSON @String "300000000000"
    , "maxBlockSize" .= J.toJSON @String "2000000"
    , "maxTxSize" .= J.toJSON @String "4096"
    , "maxHeaderSize" .= J.toJSON @String "2000000"
    , "maxProposalSize" .= J.toJSON @String "700"
    , "mpcThd" .= J.toJSON @String "20000000000000"
    , "scriptVersion" .= J.toJSON @Int 0
    , "slotDuration" .= J.toJSON @String "2000"
    , "softforkRule" .= J.object
      [ "initThd" .= J.toJSON @String "900000000000000"
      , "minThd" .= J.toJSON @String "600000000000000"
      , "thdDecrement" .= J.toJSON @String "50000000000000"
      ]
    , "txFeePolicy" .= J.object
      [ "multiplier" .= J.toJSON @String "43946000000"
      , "summand" .= J.toJSON @String "155381000000000"
      ]
    , "unlockStakeEpoch" .= J.toJSON @String "18446744073709551615"
    , "updateImplicit" .= J.toJSON @String "10000"
    , "updateProposalThd" .= J.toJSON @String "100000000000000"
    , "updateVoteThd" .= J.toJSON @String "1000000000000"
    ]

  -- stuff
  void . H.execCli $
    [ "byron"
    , "genesis"
    , "genesis"
    , "--protocol-magic", show @Int testnetMagic
    , "--start-time", showUTCTimeSeconds startTime
    , "--k", show @Int securityParam
    , "--n-poor-addresses", "0"
    , "--n-delegate-addresses", show @Int (numBftNodes testnetOptions)
    , "--total-balance", show @Int initSupply
    , "--delegate-share", "1"
    , "--avvm-entry-count", "0"
    , "--avvm-entry-balance", "0"
    , "--protocol-parameters-file", tempAbsPath </> "byron.genesis.spec.json"
    , "--genesis-output-dir", tempAbsPath </> "byron"
    ]

  H.renameFile
    (tempAbsPath </> "byron.genesis.spec.json")
    (tempAbsPath </> "byron/genesis.spec.json")

  -- Symlink the BFT operator keys from the genesis delegates, for uniformity
  forM_ bftNodesN $ \n -> do
    H.createFileLink (tempAbsPath </> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key") (tempAbsPath </> "node-bft" <> show @Int n </> "byron/delegate.key")
    H.createFileLink (tempAbsPath </> "byron/delegation-cert.00" <> show @Int (n - 1) <> ".json") (tempAbsPath </> "node-bft" <> show @Int n </> "byron/delegate.cert")

  -- Create keys and addresses to withdraw the initial UTxO into
  forM_ bftNodesN $ \n -> do
    void $ H.execCli
      [ "keygen"
      , "--secret", tempAbsPath </> "byron/payment-keys.00" <> show @Int (n - 1) <> ".key"
      ]

    H.execCli
      [ "signing-key-address"
      , "--testnet-magic", "42"
      , "--secret", tempAbsPath </> "byron/payment-keys.00" <> show @Int (n - 1) <> ".key"
      ] >>= H.writeFile (tempAbsPath </> "byron/address-00" <> show @Int (n - 1))

    -- Write Genesis addresses to files
    H.execCli
      [ "signing-key-address"
      , "--testnet-magic", "42"
      , "--secret", tempAbsPath </> "byron/genesis-keys.00" <> show @Int (n - 1) <> ".key"
      ] >>= H.writeFile (tempAbsPath </> "byron/genesis-address-00" <> show @Int (n - 1))

  do
    richAddrFrom <- S.firstLine <$> H.readFile (tempAbsPath </> "byron/genesis-address-000")
    txAddr <- S.firstLine <$> H.readFile (tempAbsPath </> "byron/address-000")

    -- Create Byron address that moves funds out of the genesis UTxO into a regular
    -- address.
    void $ H.execCli
      [ "issue-genesis-utxo-expenditure"
      , "--genesis-json", tempAbsPath </> "byron/genesis.json"
      , "--testnet-magic", "42"
      , "--tx", tempAbsPath </> "tx0.tx"
      , "--wallet-key", tempAbsPath </> "byron/delegate-keys.000.key"
      , "--rich-addr-from", richAddrFrom
      , "--txout", show @(String, Int) (txAddr, fundsPerGenesisAddress)
      ]

  -- Update Proposal and votes
  void $ H.execCli
    [ "byron", "governance", "create-update-proposal"
    , "--filepath", tempAbsPath </> "update-proposal"
    , "--testnet-magic", "42"
    , "--signing-key", tempAbsPath </> "byron/delegate-keys.000.key"
    , "--protocol-version-major", "1"
    , "--protocol-version-minor", "0"
    , "--protocol-version-alt", "0"
    , "--application-name", "cardano-sl"
    , "--software-version-num", "1"
    , "--system-tag", "linux"
    , "--installer-hash", "0"
    ]

  forM_ bftNodesN $ \n -> do
    void $ H.execCli
      [ "byron", "governance", "create-proposal-vote"
      , "--proposal-filepath", tempAbsPath </> "update-proposal"
      , "--testnet-magic", "42"
      , "--signing-key", tempAbsPath </> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key"
      , "--vote-yes"
      , "--output-filepath", tempAbsPath </> "update-vote.00" <> show @Int (n - 1)
      ]

  void $ H.execCli
    [ "byron", "governance", "create-update-proposal"
    , "--filepath", tempAbsPath </> "update-proposal-1"
    , "--testnet-magic", "42"
    , "--signing-key", tempAbsPath </> "byron/delegate-keys.000.key"
    , "--protocol-version-major", "2"
    , "--protocol-version-minor", "0"
    , "--protocol-version-alt", "0"
    , "--application-name", "cardano-sl"
    , "--software-version-num", "1"
    , "--system-tag", "linux"
    , "--installer-hash", "0"
    ]

  forM_ bftNodesN $ \n ->
    void $ H.execCli
      [ "byron", "governance", "create-proposal-vote"
      , "--proposal-filepath", tempAbsPath </> "update-proposal-1"
      , "--testnet-magic", "42"
      , "--signing-key", tempAbsPath </> "byron/delegate-keys.00" <> show @Int (n - 1) <> ".key"
      , "--vote-yes"
      , "--output-filepath", tempAbsPath </> "update-vote-1.00" <> show @Int (n - 1)
      ]

  -- Generated genesis keys and genesis files
  H.noteEachM_ . H.listDirectory $ tempAbsPath </> "byron"

  -- Set up our template
  H.createDirectoryIfMissing $ tempAbsPath </> "shelley"

  void $ H.execCli
    [ "genesis", "create"
    , "--testnet-magic", "42"
    , "--genesis-dir", tempAbsPath </> "shelley"
    , "--start-time", formatIso8601 startTime
    ]

  -- Then edit the genesis.spec.json ...

  -- We're going to use really quick epochs (300 seconds), by using short slots 0.2s
  -- and K=10, but we'll keep long KES periods so we don't have to bother
  -- cycling KES keys
  H.rewriteJsonFile (tempAbsPath </> "shelley/genesis.spec.json") . J.rewriteObject
    $ HM.insert "slotLength" (J.toJSON @Double 0.2)
    . HM.insert "activeSlotsCoeff" (J.toJSON @Double (activeSlotsCoeff testnetOptions))
    . HM.insert "securityParam" (J.toJSON @Int 10)
    . HM.insert "epochLength" (J.toJSON @Int (epochLength testnetOptions))
    . HM.insert "slotLength" (J.toJSON @Double 0.2)
    . HM.insert "maxLovelaceSupply" (J.toJSON @Int maxSupply)
    . flip HM.adjust "protocolParams"
      ( J.rewriteObject (HM.insert "decentralisationParam" (J.toJSON @Double 0.7))
      )

  -- Now generate for real:
  void $ H.execCli
    [ "genesis", "create"
    , "--testnet-magic", "42"
    , "--genesis-dir", tempAbsPath </> "shelley"
    , "--gen-genesis-keys", show @Int (numBftNodes testnetOptions)
    , "--start-time", formatIso8601 startTime
    , "--gen-utxo-keys", show @Int (numPoolNodes testnetOptions)
    ]
#ifdef UNIX
  --TODO: Remove me after #1948 is merged.
  let vrfPath n = tempAbsPath </> "shelley" </> "delegate-keys" </> "delegate" <> n <> ".vrf.skey"
  H.evalIO . forM_ bftNodesN $ \n -> setFileMode (vrfPath (show @Int n)) ownerModes
#endif
  -- Generated genesis keys and genesis files
  H.noteEachM_ . H.listDirectory $ tempAbsPath </> "shelley"

  -- Generated genesis.json
  H.cat $ tempAbsPath </> "shelley/genesis.json"

  -- Make the pool operator cold keys
  -- This was done already for the BFT nodes as part of the genesis creation
  forM_ poolNodes $ \node -> do
    void $ H.execCli
      [ "node", "key-gen"
      , "--cold-verification-key-file", tempAbsPath </> node </> "shelley/operator.vkey"
      , "--cold-signing-key-file", tempAbsPath </> node </> "shelley/operator.skey"
      , "--operational-certificate-issue-counter-file", tempAbsPath </> node </> "shelley/operator.counter"
      ]

    void $ H.execCli
      [ "node", "key-gen-VRF"
      , "--verification-key-file", tempAbsPath </> node </> "shelley/vrf.vkey"
      , "--signing-key-file", tempAbsPath </> node </> "shelley/vrf.skey"
      ]
#ifdef UNIX
    --TODO: Remove me after #1948 is merged.
    H.evalIO $ setFileMode (tempAbsPath </> node </> "shelley/vrf.skey") ownerModes
#endif
  -- Symlink the BFT operator keys from the genesis delegates, for uniformity
  forM_ bftNodesN $ \n -> do
    H.createFileLink (tempAbsPath </> "shelley/delegate-keys/delegate" <> show @Int n <> ".skey") (tempAbsPath </> "node-bft" <> show @Int n </> "shelley/operator.skey")
    H.createFileLink (tempAbsPath </> "shelley/delegate-keys/delegate" <> show @Int n <> ".vkey") (tempAbsPath </> "node-bft" <> show @Int n </> "shelley/operator.vkey")
    H.createFileLink (tempAbsPath </> "shelley/delegate-keys/delegate" <> show @Int n <> ".counter") (tempAbsPath </> "node-bft" <> show @Int n </> "shelley/operator.counter")
    H.createFileLink (tempAbsPath </> "shelley/delegate-keys/delegate" <> show @Int n <> ".vrf.vkey") (tempAbsPath </> "node-bft" <> show @Int n </> "shelley/vrf.vkey")
    H.createFileLink (tempAbsPath </> "shelley/delegate-keys/delegate" <> show @Int n <> ".vrf.skey") (tempAbsPath </> "node-bft" <> show @Int n </> "shelley/vrf.skey")

  -- Make hot keys and for all nodes
  forM_ allNodes $ \node -> do
    void $ H.execCli
      [ "node", "key-gen-KES"
      , "--verification-key-file", tempAbsPath </> node </> "shelley/kes.vkey"
      , "--signing-key-file",      tempAbsPath </> node </> "shelley/kes.skey"
      ]

    void $ H.execCli
      [ "node", "issue-op-cert"
      , "--kes-period", "0"
      , "--kes-verification-key-file", tempAbsPath </> node </> "shelley/kes.vkey"
      , "--cold-signing-key-file", tempAbsPath </> node </> "shelley/operator.skey"
      , "--operational-certificate-issue-counter-file", tempAbsPath </> node </> "shelley/operator.counter"
      , "--out-file", tempAbsPath </> node </> "shelley/node.cert"
      ]

  -- Generated node operator keys (cold, hot) and operational certs
  forM_ allNodes $ \node -> H.noteEachM_ . H.listDirectory $ tempAbsPath </> node </> "byron"

  -- Make some payment and stake addresses
  -- user1..n:       will own all the funds in the system, we'll set this up from
  --                 initial utxo the
  -- pool-owner1..n: will be the owner of the pools and we'll use their reward
  --                 account for pool rewards
  let userAddrs = ("user" <>) . show @Int <$> userPoolN
  let poolAddrs = ("pool-owner" <>) . show @Int <$> poolNodesN
  let addrs = userAddrs <> poolAddrs

  H.createDirectoryIfMissing $ tempAbsPath </> "addresses"

  forM_ addrs $ \addr -> do
    -- Payment address keys
    void $ H.execCli
      [ "address", "key-gen"
      , "--verification-key-file", tempAbsPath </> "addresses/" <> addr <> ".vkey"
      , "--signing-key-file", tempAbsPath </> "addresses/" <> addr <> ".skey"
      ]

    -- Stake address keys
    void $ H.execCli
      [ "stake-address", "key-gen"
      , "--verification-key-file", tempAbsPath </> "addresses/" <> addr <> "-stake.vkey"
      , "--signing-key-file", tempAbsPath </> "addresses/" <> addr <> "-stake.skey"
      ]

    -- Payment addresses
    void $ H.execCli
      [ "address", "build"
      , "--payment-verification-key-file", tempAbsPath </> "addresses/" <> addr <> ".vkey"
      , "--stake-verification-key-file", tempAbsPath </> "addresses/" <> addr <> "-stake.vkey"
      , "--testnet-magic", "42"
      , "--out-file", tempAbsPath </> "addresses/" <> addr <> ".addr"
      ]

    -- Stake addresses
    void $ H.execCli
      [ "stake-address", "build"
      , "--stake-verification-key-file", tempAbsPath </> "addresses/" <> addr <> "-stake.vkey"
      , "--testnet-magic", "42"
      , "--out-file", tempAbsPath </> "addresses/" <> addr <> "-stake.addr"
      ]

    -- Stake addresses registration certs
    void $ H.execCli
      [ "stake-address", "registration-certificate"
      , "--stake-verification-key-file", tempAbsPath </> "addresses/" <> addr <> "-stake.vkey"
      , "--out-file", tempAbsPath </> "addresses/" <> addr <> "-stake.reg.cert"
      ]

  -- user N will delegate to pool N
  forM_ userPoolN $ \n -> do
    -- Stake address delegation certs
    void $ H.execCli
      [ "stake-address", "delegation-certificate"
      , "--stake-verification-key-file", tempAbsPath </> "addresses/user" <> show @Int n <> "-stake.vkey"
      , "--cold-verification-key-file", tempAbsPath </> "node-pool" <> show @Int n </> "shelley/operator.vkey"
      , "--out-file", tempAbsPath </> "addresses/user" <> show @Int n <> "-stake.deleg.cert"
      ]

    H.createFileLink (tempAbsPath </> "addresses/pool-owner" <> show @Int n <> "-stake.vkey") (tempAbsPath </> "node-pool" <> show @Int n </> "owner.vkey")
    H.createFileLink (tempAbsPath </> "addresses/pool-owner" <> show @Int n <> "-stake.skey") (tempAbsPath </> "node-pool" <> show @Int n </> "owner.skey")

  -- Generated payment address keys, stake address keys,
  -- stake address regitration certs, and stake address delegatation certs
  H.noteEachM_ . H.listDirectory $ tempAbsPath </> "addresses"

  -- Next is to make the stake pool registration cert
  forM_ poolNodes $ \node -> do
    H.execCli
      [ "stake-pool", "registration-certificate"
      , "--testnet-magic", "42"
      , "--pool-pledge", "0", "--pool-cost", "0", "--pool-margin", "0"
      , "--cold-verification-key-file", tempAbsPath </> node </> "shelley/operator.vkey"
      , "--vrf-verification-key-file", tempAbsPath </> node </> "shelley/vrf.vkey"
      , "--reward-account-verification-key-file", tempAbsPath </> node </> "owner.vkey"
      , "--pool-owner-stake-verification-key-file", tempAbsPath </> node </> "owner.vkey"
      , "--out-file", tempAbsPath </> node </> "registration.cert"
      ]

  -- Generated stake pool registration certs
  forM_ poolNodes $ \node -> H.assertIO . IO.doesFileExist $ tempAbsPath </> node </> "registration.cert"

  -- Now we'll construct one whopper of a transaction that does everything
  -- just to show off that we can, and to make the script shorter

  do
    -- We'll transfer all the funds to the user1, which delegates to pool1
    -- We'll register certs to:
    --  1. register the pool-owner1 stake address
    --  2. register the stake pool 1
    --  3. register the user1 stake address
    --  4. delegate from the user1 stake address to the stake pool
    txIn <- H.noteShow . S.strip =<< H.execCli
      [ "genesis", "initial-txin"
      , "--testnet-magic", "42"
      , "--verification-key-file", tempAbsPath </> "shelley/utxo-keys/utxo1.vkey"
      ]

    H.note_ txIn

    user1Addr <- H.readFile $ tempAbsPath </> "addresses/user1.addr"

    void $ H.execCli
      [ "transaction", "build-raw"
      , "--invalid-hereafter", "1000"
      , "--fee", "0"
      , "--tx-in", txIn
      , "--tx-out",  user1Addr <> "+" <> show @Int maxSupply
      , "--certificate-file", tempAbsPath </> "addresses/pool-owner1-stake.reg.cert"
      , "--certificate-file", tempAbsPath </> "node-pool1/registration.cert"
      , "--certificate-file", tempAbsPath </> "addresses/user1-stake.reg.cert"
      , "--certificate-file", tempAbsPath </> "addresses/user1-stake.deleg.cert"
      , "--out-file", tempAbsPath </> "tx1.txbody"
      ]

  -- TODO: this will become the transaction to register the pool, etc.
  -- We'll need to pick the tx-in from the actual UTxO since it contains the txid,
  -- we'll have to query this via cardano-cli query utxo.

  {-  cardano-cli transaction build-raw \
          --invalid-hereafter 1000000 --fee 0 \
          --tx-in 67209bfcdf78f8cd86f649da75053a80fb9bb3fad68465554f9301c31b496c65#0 \
          --tx-out $(cat example/addresses/user1.addr)+450000000 \
          --certificate-file example/addresses/pool-owner1-stake.reg.cert \
          --certificate-file example/node-pool1/registration.cert \
          --certificate-file example/addresses/user1-stake.reg.cert \
          --certificate-file example/addresses/user1-stake.deleg.cert \
          --out-file example/register-pool.txbody
  -}

  {-  cardano-cli address convert \
          --byron-key-file example/byron/payment-keys.000.key \
          --signing-key-file example/byron/payment-keys.000-converted.key
  -}

  {-  cardano-cli transaction sign \
          --tx-body-file example/register-pool.txbody \
          --testnet-magic 42 \
          --signing-key-file example/byron/payment-keys.000-converted.key \
          --signing-key-file example/shelley/utxo-keys/utxo1.skey \
          --signing-key-file example/addresses/user1-stake.skey \
          --signing-key-file example/node-pool1/owner.skey \
          --signing-key-file example/node-pool1/shelley/operator.skey \
          --out-file example/register-pool.tx
  -}

  {-  cardano-cli transaction submit \
          --tx-file example/register-pool.tx --testnet-magic 42
  -}

  -- So we'll need to sign this with a bunch of keys:
  -- 1. the initial utxo spending key, for the funds
  -- 2. the user1 stake address key, due to the delegatation cert
  -- 3. the pool1 owner key, due to the pool registration cert
  -- 3. the pool1 operator key, due to the pool registration cert
  void $ H.execCli
    [ "transaction", "sign"
    , "--signing-key-file", tempAbsPath </> "shelley/utxo-keys/utxo1.skey"
    , "--signing-key-file", tempAbsPath </> "addresses/user1-stake.skey"
    , "--signing-key-file", tempAbsPath </> "node-pool1/owner.skey"
    , "--signing-key-file", tempAbsPath </> "node-pool1/shelley/operator.skey"
    , "--testnet-magic", "42"
    , "--tx-body-file", tempAbsPath </> "tx1.txbody"
    , "--out-file", tempAbsPath </> "tx1.tx"
    ]

  -- Generated a signed 'do it all' transaction:
  H.assertIO . IO.doesFileExist $ tempAbsPath </> "tx1.tx"

  --------------------------------
  -- Launch cluster of three nodes

  forM_ bftNodes $ \node -> do
    dbDir <- H.noteShow $ tempAbsPath </> "db/" <> node
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ node <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> node)

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing $ tempBaseAbsPath </> socketDir

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    portString <- fmap S.strip . H.readFile $ tempAbsPath </> node </> "port"

    (_, _, _, hProcess, _) <- H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--config",  tempAbsPath </> "configuration.yaml"
        , "--topology",  tempAbsPath </> node </> "topology.json"
        , "--database-path", tempAbsPath </> node </> "db"
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--shelley-kes-key", tempAbsPath </> node </> "shelley/kes.skey"
        , "--shelley-vrf-key", tempAbsPath </> node </> "shelley/vrf.skey"
        , "--shelley-operational-certificate", tempAbsPath </> node </> "shelley/node.cert"
        , "--port",  portString
        , "--delegation-certificate",  tempAbsPath </> node </> "byron/delegate.cert"
        , "--signing-key", tempAbsPath </> node </> "byron/delegate.key"
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

    H.noteShowM_ $ H.getPid hProcess

    when (OS.os `L.elem` ["darwin", "linux"]) $ do
      H.onFailure . H.noteIO_ $ IO.readProcess "lsof" ["-iTCP:" <> portString, "-sTCP:LISTEN", "-n", "-P"] ""

  H.threadDelay 100000

  forM_ poolNodes $ \node -> do
    dbDir <- H.noteShow $ tempAbsPath </> "db/" <> node
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ node <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> node)

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing $ tempBaseAbsPath </> socketDir

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    portString <- fmap S.strip . H.readFile $ tempAbsPath </> node </> "port"

    void $ H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--config", tempAbsPath </> "configuration.yaml"
        , "--topology", tempAbsPath </> node </> "topology.json"
        , "--database-path", tempAbsPath </> node </> "db"
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--shelley-kes-key", tempAbsPath </> node </> "shelley/kes.skey"
        , "--shelley-vrf-key", tempAbsPath </> node </> "shelley/vrf.skey"
        , "--shelley-operational-certificate", tempAbsPath </> node </> "shelley/node.cert"
        , "--host-addr", ifaceAddress
        , "--port",  portString
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

    when (OS.os `L.elem` ["darwin", "linux"]) $ do
      H.onFailure . H.noteIO_ $ IO.readProcess "lsof" ["-iTCP:" <> portString, "-sTCP:LISTEN", "-n", "-P"] ""

  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 90 now

  forM_ allNodes $ \node -> do
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> node)
    _spocketSystemNameFile <- H.noteShow $ IO.sprocketSystemName sprocket
    H.waitByDeadlineM deadline $ H.doesSprocketExist sprocket

  forM_ allNodes $ \node -> do
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    H.assertByDeadlineIO deadline $ IO.fileContains "until genesis start time at" nodeStdoutFile
    H.assertByDeadlineIO deadline $ IO.fileContains "Chain extended, new tip" nodeStdoutFile

  H.noteShowIO_ DTC.getCurrentTime

  return allNodes
