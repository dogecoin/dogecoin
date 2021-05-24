{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


module Cardano.Tracing.Config
  ( TraceOptions (..)
  , TraceSelection (..)
  , traceConfigParser
  , OnOff (..)
  ) where

import           Cardano.Prelude

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Text (pack)

import           Cardano.BM.Tracing (TracingVerbosity (..))

import           Cardano.Node.Orphans ()


data TraceOptions
  = TracingOff
  | TracingOn TraceSelection
  deriving (Eq, Show)

type TraceAcceptPolicy = ("TraceAcceptPolicy" :: Symbol)
type TraceBlockchainTime = ("TraceBlockchainTime" :: Symbol)
type TraceBlockFetchClient = ("TraceBlockFetchClient" :: Symbol)
type TraceBlockFetchDecisions = ("TraceBlockFetchDecisions" :: Symbol)
type TraceBlockFetchProtocol = ("TraceBlockFetchProtocol" :: Symbol)
type TraceBlockFetchProtocolSerialised = ("TraceBlockFetchProtocolSerialised" :: Symbol)
type TraceBlockFetchServer = ("TraceBlockFetchServer" :: Symbol)
type TraceChainDB = ("TraceChainDB" :: Symbol)
type TraceChainSyncClient = ("TraceChainSyncClient" :: Symbol)
type TraceChainSyncBlockServer = ("TraceChainSyncBlockServer" :: Symbol)
type TraceChainSyncHeaderServer = ("TraceChainSyncHeaderServer" :: Symbol)
type TraceChainSyncProtocol = ("TraceChainSyncProtocol" :: Symbol)
type TraceDiffusionInitialization = ("TraceDiffusionInitialization" :: Symbol)
type TraceDnsResolver = ("TraceDnsResolver" :: Symbol)
type TraceDnsSubscription = ("TraceDnsSubscription" :: Symbol)
type TraceErrorPolicy = ("TraceErrorPolicy" :: Symbol)
type TraceForge = ("TraceForge" :: Symbol)
type TraceForgeStateInfo = ("TraceForgeStateInfo" :: Symbol)
type TraceHandshake = ("TraceHandshake" :: Symbol)
type TraceIpSubscription = ("TraceIpSubscription" :: Symbol)
type TraceKeepAliveClient = ("TraceKeepAliveClient" :: Symbol)
type TraceLocalChainSyncProtocol = ("TraceLocalChainSyncProtocol" :: Symbol)
type TraceLocalErrorPolicy = ("TraceLocalErrorPolicy" :: Symbol)
type TraceLocalHandshake = ("TraceLocalHandshake" :: Symbol)
type TraceLocalTxSubmissionProtocol = ("TraceLocalTxSubmissionProtocol" :: Symbol)
type TraceLocalTxSubmissionServer = ("TraceLocalTxSubmissionServer" :: Symbol)
type TraceLocalStateQueryProtocol = ("TraceLocalStateQueryProtocol" :: Symbol)
type TraceMempool = ("TraceMempool" :: Symbol)
type TraceMux = ("TraceMux" :: Symbol)
type TraceLocalMux = ("TraceLocalMux" :: Symbol)
type TraceTxInbound = ("TraceTxInbound" :: Symbol)
type TraceTxOutbound = ("TraceTxOutbound" :: Symbol)
type TraceTxSubmissionProtocol = ("TraceTxSubmissionProtocol" :: Symbol)
type TraceTxSubmission2Protocol = ("TraceTxSubmission2Protocol" :: Symbol)

newtype OnOff (name :: Symbol) = OnOff { isOn :: Bool } deriving (Eq, Show)

instance FromJSON (OnOff a) where
    parseJSON (Data.Aeson.Bool b)= return $ OnOff b
    parseJSON _ = mzero

getName :: forall name. KnownSymbol name => OnOff name -> Text
getName _ = pack (symbolVal (Proxy @name))

data TraceSelection
  = TraceSelection
  { traceVerbosity :: !TracingVerbosity

  -- Per-trace toggles, alpha-sorted.
  , traceAcceptPolicy :: OnOff TraceAcceptPolicy
  , traceBlockFetchClient :: OnOff TraceBlockFetchClient
  , traceBlockFetchDecisions :: OnOff TraceBlockFetchDecisions
  , traceBlockFetchProtocol :: OnOff TraceBlockFetchProtocol
  , traceBlockFetchProtocolSerialised :: OnOff TraceBlockFetchProtocolSerialised
  , traceBlockFetchServer :: OnOff TraceBlockFetchServer
  , traceBlockchainTime :: OnOff TraceBlockchainTime
  , traceChainDB :: OnOff TraceChainDB
  , traceChainSyncBlockServer :: OnOff TraceChainSyncBlockServer
  , traceChainSyncClient :: OnOff TraceChainSyncClient
  , traceChainSyncHeaderServer :: OnOff TraceChainSyncHeaderServer
  , traceChainSyncProtocol :: OnOff TraceChainSyncProtocol
  , traceDiffusionInitialization :: OnOff TraceDiffusionInitialization
  , traceDnsResolver :: OnOff TraceDnsResolver
  , traceDnsSubscription :: OnOff TraceDnsSubscription
  , traceErrorPolicy :: OnOff TraceErrorPolicy
  , traceForge :: OnOff TraceForge
  , traceForgeStateInfo :: OnOff TraceForgeStateInfo
  , traceHandshake :: OnOff TraceHandshake
  , traceIpSubscription :: OnOff TraceIpSubscription
  , traceKeepAliveClient :: OnOff TraceKeepAliveClient
  , traceLocalChainSyncProtocol :: OnOff TraceLocalChainSyncProtocol
  , traceLocalErrorPolicy :: OnOff TraceLocalErrorPolicy
  , traceLocalHandshake :: OnOff TraceLocalHandshake
  , traceLocalStateQueryProtocol :: OnOff TraceLocalStateQueryProtocol
  , traceLocalTxSubmissionProtocol :: OnOff TraceLocalTxSubmissionProtocol
  , traceLocalTxSubmissionServer :: OnOff TraceLocalTxSubmissionServer
  , traceMempool :: OnOff TraceMempool
  , traceMux :: OnOff TraceMux
  , traceLocalMux :: OnOff TraceLocalMux
  , traceTxInbound :: OnOff TraceTxInbound
  , traceTxOutbound :: OnOff TraceTxOutbound
  , traceTxSubmissionProtocol :: OnOff TraceTxSubmissionProtocol
  , traceTxSubmission2Protocol :: OnOff TraceTxSubmission2Protocol
  } deriving (Eq, Show)


traceConfigParser :: Object -> Parser TraceOptions
traceConfigParser v =
  let acceptPolicy :: OnOff TraceAcceptPolicy
      acceptPolicy = OnOff False
      blockFetchClient :: OnOff TraceBlockFetchClient
      blockFetchClient = OnOff False
      blockFetchDecisions :: OnOff TraceBlockFetchDecisions
      blockFetchDecisions = OnOff True
      blockFetchProtocol :: OnOff TraceBlockFetchProtocol
      blockFetchProtocol = OnOff False
      blockFetchProtocolSerialised :: OnOff TraceBlockFetchProtocolSerialised
      blockFetchProtocolSerialised = OnOff False
      blockFetchServer :: OnOff TraceBlockFetchServer
      blockFetchServer = OnOff False
      blockchainTime :: OnOff TraceBlockchainTime
      blockchainTime = OnOff False
      chainDB :: OnOff TraceChainDB
      chainDB = OnOff True
      chainSyncBlockServer :: OnOff TraceChainSyncBlockServer
      chainSyncBlockServer = OnOff False
      chainSyncClient :: OnOff TraceChainSyncClient
      chainSyncClient = OnOff True
      chainSyncHeaderServer :: OnOff TraceChainSyncHeaderServer
      chainSyncHeaderServer = OnOff False
      chainSyncProtocol :: OnOff TraceChainSyncProtocol
      chainSyncProtocol = OnOff False
      diffusionInitialization :: OnOff TraceDiffusionInitialization
      diffusionInitialization = OnOff False
      dnsResolver :: OnOff TraceDnsResolver
      dnsResolver = OnOff False
      dnsSubscription :: OnOff TraceDnsSubscription
      dnsSubscription = OnOff True
      errorPolicy :: OnOff TraceErrorPolicy
      errorPolicy = OnOff True
      forge :: OnOff TraceForge
      forge = OnOff True
      forgeStateInfo :: OnOff TraceForgeStateInfo
      forgeStateInfo = OnOff True
      handshake :: OnOff TraceHandshake
      handshake = OnOff False
      ipSubscription :: OnOff TraceIpSubscription
      ipSubscription = OnOff True
      keepAliveClient :: OnOff TraceKeepAliveClient
      keepAliveClient = OnOff False
      localChainSyncProtocol :: OnOff TraceLocalChainSyncProtocol
      localChainSyncProtocol = OnOff False
      localErrorPolicy :: OnOff TraceLocalErrorPolicy
      localErrorPolicy = OnOff True
      localHandshake :: OnOff TraceLocalHandshake
      localHandshake = OnOff False
      localStateQueryProtocol :: OnOff TraceLocalStateQueryProtocol
      localStateQueryProtocol = OnOff False
      localTxSubmissionProtocol :: OnOff TraceLocalTxSubmissionProtocol
      localTxSubmissionProtocol = OnOff False
      localTxSubmissionServer :: OnOff TraceLocalTxSubmissionServer
      localTxSubmissionServer = OnOff False
      mempool :: OnOff TraceMempool
      mempool = OnOff True
      mux :: OnOff TraceMux
      mux = OnOff True
      localMux :: OnOff TraceLocalMux
      localMux = OnOff False
      txInbound :: OnOff TraceTxInbound
      txInbound = OnOff False
      txOutbound :: OnOff TraceTxOutbound
      txOutbound = OnOff False
      txSubmissionProtocol :: OnOff TraceTxSubmissionProtocol
      txSubmissionProtocol = OnOff False
      txSubmission2Protocol :: OnOff TraceTxSubmission2Protocol
      txSubmission2Protocol = OnOff False in

  TracingOn <$> (TraceSelection
    <$> v .:? "TracingVerbosity" .!= NormalVerbosity
    -- Per-trace toggles, alpha-sorted.
    <*> v .:? getName acceptPolicy .!= acceptPolicy
    <*> v .:? getName blockFetchClient .!=  blockFetchClient
    <*> v .:? getName blockFetchDecisions .!= blockFetchDecisions
    <*> v .:? getName blockFetchProtocol .!= blockFetchProtocol
    <*> v .:? getName blockFetchProtocolSerialised .!= blockFetchProtocolSerialised
    <*> v .:? getName blockFetchServer .!= blockFetchServer
    <*> v .:? getName blockchainTime .!= blockchainTime
    <*> v .:? getName chainDB .!=  chainDB
    <*> v .:? getName chainSyncBlockServer .!= chainSyncBlockServer
    <*> v .:? getName chainSyncClient .!= chainSyncClient
    <*> v .:? getName chainSyncHeaderServer .!= chainSyncHeaderServer
    <*> v .:? getName chainSyncProtocol .!= chainSyncProtocol
    <*> v .:? getName diffusionInitialization .!= diffusionInitialization
    <*> v .:? getName dnsResolver .!= dnsResolver
    <*> v .:? getName dnsSubscription .!= dnsSubscription
    <*> v .:? getName errorPolicy .!=  errorPolicy
    <*> v .:? getName forge .!= forge
    <*> v .:? getName forgeStateInfo .!= forgeStateInfo
    <*> v .:? getName handshake .!= handshake
    <*> v .:? getName ipSubscription .!= ipSubscription
    <*> v .:? getName keepAliveClient .!= keepAliveClient
    <*> v .:? getName localChainSyncProtocol .!= localChainSyncProtocol
    <*> v .:? getName localErrorPolicy .!= localErrorPolicy
    <*> v .:? getName localHandshake .!= localHandshake
    <*> v .:? getName localStateQueryProtocol .!= localStateQueryProtocol
    <*> v .:? getName localTxSubmissionProtocol .!= localTxSubmissionProtocol
    <*> v .:? getName localTxSubmissionServer .!= localTxSubmissionServer
    <*> v .:? getName mempool .!= mempool
    <*> v .:? getName mux .!= mux
    <*> v .:? getName localMux .!= localMux
    <*> v .:? getName txInbound .!= txInbound
    <*> v .:? getName txOutbound .!= txOutbound
    <*> v .:? getName txSubmissionProtocol .!= txSubmissionProtocol
    <*> v .:? getName txSubmission2Protocol .!= txSubmission2Protocol)
