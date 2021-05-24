{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Api.Crypto
  ( tests
  )
where

import           Cardano.Prelude
import           Prelude (String)

import           Cardano.Api.Crypto.Ed25519Bip32 (Ed25519Bip32DSIGN, SignKeyDSIGN (..))

import           Cardano.Crypto.DSIGN
import           Cardano.Crypto.Util (SignableRepresentation (..))

import           Test.Crypto.Util
import           Test.QuickCheck (Arbitrary (..), Gen, Property, (=/=), (===), (==>))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import qualified Cardano.Crypto.Wallet as CC

--
-- The list of all tests
--
tests :: TestTree
tests =
  testGroup "Cardano.Api.Crypto"
    [ testDSIGNAlgorithm (Proxy :: Proxy Ed25519Bip32DSIGN) "Ed25519Bip32DSIGN"
    ]

testDSIGNAlgorithm
  :: forall proxy v. ( DSIGNAlgorithm v
                     , ToCBOR (VerKeyDSIGN v)
                     , FromCBOR (VerKeyDSIGN v)
                     , ToCBOR (SignKeyDSIGN v)
                     , FromCBOR (SignKeyDSIGN v)
                     , Eq (SignKeyDSIGN v)   -- no Eq for signing keys normally
                     , ToCBOR (SigDSIGN v)
                     , FromCBOR (SigDSIGN v)
                     , Signable v ~ SignableRepresentation
                     , ContextDSIGN v ~ ()
                     )
  => proxy v
  -> String
  -> TestTree
testDSIGNAlgorithm _ n =
  testGroup n
    [ testGroup "serialisation"
      [ testGroup "raw"
        [ testProperty "VerKey"  $ prop_raw_serialise @(VerKeyDSIGN v)
                                                      rawSerialiseVerKeyDSIGN
                                                      rawDeserialiseVerKeyDSIGN
        , testProperty "SignKey" $ prop_raw_serialise @(SignKeyDSIGN v)
                                                      rawSerialiseSignKeyDSIGN
                                                      rawDeserialiseSignKeyDSIGN
        , testProperty "Sig"     $ prop_raw_serialise @(SigDSIGN v)
                                                      rawSerialiseSigDSIGN
                                                      rawDeserialiseSigDSIGN
        ]

      , testGroup "size"
        [ testProperty "VerKey"  $ prop_size_serialise @(VerKeyDSIGN v)
                                                       rawSerialiseVerKeyDSIGN
                                                       (sizeVerKeyDSIGN (Proxy @ v))
        , testProperty "SignKey" $ prop_size_serialise @(SignKeyDSIGN v)
                                                       rawSerialiseSignKeyDSIGN
                                                       (sizeSignKeyDSIGN (Proxy @ v))
        , testProperty "Sig"     $ prop_size_serialise @(SigDSIGN v)
                                                       rawSerialiseSigDSIGN
                                                       (sizeSigDSIGN (Proxy @ v))
        ]

      , testGroup "direct CBOR"
        [ testProperty "VerKey"  $ prop_cbor_with @(VerKeyDSIGN v)
                                                  encodeVerKeyDSIGN
                                                  decodeVerKeyDSIGN
        , testProperty "SignKey" $ prop_cbor_with @(SignKeyDSIGN v)
                                                  encodeSignKeyDSIGN
                                                  decodeSignKeyDSIGN
        , testProperty "Sig"     $ prop_cbor_with @(SigDSIGN v)
                                                  encodeSigDSIGN
                                                  decodeSigDSIGN
        ]

      , testGroup "To/FromCBOR class"
        [ testProperty "VerKey"  $ prop_cbor @(VerKeyDSIGN v)
        , testProperty "SignKey" $ prop_cbor @(SignKeyDSIGN v)
        , testProperty "Sig"     $ prop_cbor @(SigDSIGN v)
        ]

      , testGroup "ToCBOR size"
        [ testProperty "VerKey"  $ prop_cbor_size @(VerKeyDSIGN v)
        , testProperty "SignKey" $ prop_cbor_size @(SignKeyDSIGN v)
        , testProperty "Sig"     $ prop_cbor_size @(SigDSIGN v)
        ]

      , testGroup "direct matches class"
        [ testProperty "VerKey"  $ prop_cbor_direct_vs_class @(VerKeyDSIGN v)
                                                             encodeVerKeyDSIGN
        , testProperty "SignKey" $ prop_cbor_direct_vs_class @(SignKeyDSIGN v)
                                                             encodeSignKeyDSIGN
        , testProperty "Sig"     $ prop_cbor_direct_vs_class @(SigDSIGN v)
                                                             encodeSigDSIGN
        ]
      ]

    , testGroup "verify"
      [ testProperty "verify positive" $ prop_dsign_verify_pos @v
      , testProperty "verify negative (wrong key)" $ prop_dsign_verify_neg_key @v
      , testProperty "verify negative (wrong message)" $ prop_dsign_verify_neg_msg @v
      ]

    , testGroup "NoUnexpectedThunks"
      [ testProperty "VerKey"  $ prop_no_thunks @(VerKeyDSIGN v)
      , testProperty "SignKey" $ prop_no_thunks @(SignKeyDSIGN v)
      , testProperty "Sig"     $ prop_no_thunks @(SigDSIGN v)
      ]
    ]


-- | If we sign a message @a@ with the signing key, then we can verify the
-- signature using the corresponding verification key.
--
-- TODO: Export this from @Test.Crypto.Util@.
--
prop_dsign_verify_pos
  :: forall v. (DSIGNAlgorithm v, ContextDSIGN v ~ (), Signable v ~ SignableRepresentation)
  => Message
  -> SignKeyDSIGN v
  -> Property
prop_dsign_verify_pos a sk =
  let sig = signDSIGN () a sk
      vk = deriveVerKeyDSIGN sk
  in verifyDSIGN () vk a sig === Right ()

-- | If we sign a message @a@ with one signing key, if we try to verify the
-- signature (and message @a@) using a verification key corresponding to a
-- different signing key, then the verification fails.
--
-- TODO: Export this from @Test.Crypto.Util@.
--
prop_dsign_verify_neg_key
  :: forall v. (DSIGNAlgorithm v, Eq (SignKeyDSIGN v),
                ContextDSIGN v ~ (), Signable v ~ SignableRepresentation)
  => Message
  -> SignKeyDSIGN v
  -> SignKeyDSIGN v
  -> Property
prop_dsign_verify_neg_key a sk sk' =
  sk /= sk' ==>
    let sig = signDSIGN () a sk
        vk' = deriveVerKeyDSIGN sk'
    in verifyDSIGN () vk' a sig =/= Right ()


-- | If we sign a message @a@ with one signing key, if we try to verify the
-- signature with a message other than @a@, then the verification fails.
--
-- TODO: Export this from @Test.Crypto.Util@.
--
prop_dsign_verify_neg_msg
  :: forall v. (DSIGNAlgorithm v,
                ContextDSIGN v ~ (), Signable v ~ SignableRepresentation)
  => Message
  -> Message
  -> SignKeyDSIGN v
  -> Property
prop_dsign_verify_neg_msg a a' sk =
  a /= a' ==>
    let sig = signDSIGN () a sk
        vk = deriveVerKeyDSIGN sk
    in verifyDSIGN () vk a' sig =/= Right ()

--
-- Arbitrary instances
--

instance DSIGNAlgorithm v => Arbitrary (VerKeyDSIGN v) where
  arbitrary = deriveVerKeyDSIGN <$> arbitrary
  shrink = const []

instance DSIGNAlgorithm v => Arbitrary (SignKeyDSIGN v) where
  arbitrary = genKeyDSIGN <$> arbitrarySeedOfSize seedSize
    where
      seedSize = seedSizeDSIGN (Proxy :: Proxy v)
  shrink = const []

instance (DSIGNAlgorithm v,
          ContextDSIGN v ~ (), Signable v ~ SignableRepresentation)
      => Arbitrary (SigDSIGN v) where
  arbitrary = signDSIGN () <$> (arbitrary :: Gen Message) <*> arbitrary
  shrink = const []

--
-- Signing key Eq instances for testing
--

instance Eq (SignKeyDSIGN Ed25519Bip32DSIGN) where
  (SignKeyEd25519Bip32DSIGN x) == (SignKeyEd25519Bip32DSIGN y) =
    CC.unXPrv x == CC.unXPrv y

  (SignKeyEd25519Bip32DSIGN x) /= (SignKeyEd25519Bip32DSIGN y) =
    CC.unXPrv x /= CC.unXPrv y
