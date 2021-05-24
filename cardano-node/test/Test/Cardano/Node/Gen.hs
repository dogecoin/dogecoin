{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Cardano.Node.Gen
  ( genNetworkTopology
  , genNodeHostIPv4Address
  , genNodeHostIPv6Address
  , genNodeHostIPAddress
  , genNodeIPAddress
  , genNodeIPv4Address
  , genNodeIPv6Address
  , genNodeSetup
  ) where

import           Cardano.Prelude

import           Cardano.Node.Configuration.Topology (NetworkTopology (..), NodeSetup (..),
                     RemoteAddress (..))
import           Cardano.Node.Types (NodeAddress' (..), NodeHostIPAddress (..),
                   NodeHostIPv4Address (..), NodeHostIPv6Address (..),
                   NodeIPAddress, NodeIPv4Address, NodeIPv6Address)

import qualified Data.IP as IP

import           Hedgehog (Gen)
import           Hedgehog.Corpus (cooking)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Internal.Gen ()
import qualified Hedgehog.Range as Range

genNetworkTopology :: Gen NetworkTopology
genNetworkTopology =
  Gen.choice
    [ MockNodeTopology <$> Gen.list (Range.linear 0 10) genNodeSetup
    , RealNodeTopology <$> Gen.list (Range.linear 0 10) genRemoteAddress
    ]

genNodeAddress' :: Gen addr -> Gen (NodeAddress' addr)
genNodeAddress' genAddr =
  NodeAddress
    <$> genAddr
    <*> fmap fromIntegral (Gen.word16 $ Range.linear 100 20000)

genNodeHostIPv4Address :: Gen NodeHostIPv4Address
genNodeHostIPv4Address =
    NodeHostIPv4Address . IP.toIPv4w <$> Gen.enumBounded

genNodeHostIPv6Address :: Gen NodeHostIPv6Address
genNodeHostIPv6Address =
    NodeHostIPv6Address . IP.toIPv6w <$> genFourWord32
  where
    genFourWord32 :: Gen (Word32, Word32, Word32, Word32)
    genFourWord32 =
       (,,,) <$> Gen.enumBounded
             <*> Gen.enumBounded
             <*> Gen.enumBounded
             <*> Gen.enumBounded

genNodeHostIPAddress :: Gen NodeHostIPAddress
genNodeHostIPAddress =
  NodeHostIPAddress
    <$> Gen.choice
          [ IP.IPv4 . unNodeHostIPv4Address <$> genNodeHostIPv4Address
          , IP.IPv6 . unNodeHostIPv6Address <$> genNodeHostIPv6Address
          ]

genNodeIPAddress :: Gen NodeIPAddress
genNodeIPAddress = genNodeAddress' genNodeHostIPAddress

genNodeIPv4Address :: Gen NodeIPv4Address
genNodeIPv4Address = genNodeAddress' genNodeHostIPv4Address

genNodeIPv6Address :: Gen NodeIPv6Address
genNodeIPv6Address = genNodeAddress' genNodeHostIPv6Address

genNodeSetup :: Gen NodeSetup
genNodeSetup =
  NodeSetup
    <$> Gen.word64 (Range.linear 0 10000)
    <*> Gen.maybe (genNodeAddress' genNodeHostIPv4Address)
    <*> Gen.maybe (genNodeAddress' genNodeHostIPv6Address)
    <*> Gen.list (Range.linear 0 6) genRemoteAddress

genRemoteAddress :: Gen RemoteAddress
genRemoteAddress =
  RemoteAddress
    <$> Gen.element cooking
    <*> fmap fromIntegral (Gen.word16 $ Range.linear 100 20000)
    <*> Gen.int (Range.linear 0 100)

