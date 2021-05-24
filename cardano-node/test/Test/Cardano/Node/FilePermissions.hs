{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Test.Cardano.Node.FilePermissions
  ( tests
  ) where

import           Cardano.Prelude

import           System.Directory (removeFile)

import           Cardano.Api
import           Cardano.Node.Run (checkVRFFilePermissions)
import           Hedgehog (Property, PropertyT, property, success)
import qualified Hedgehog
import           Hedgehog.Internal.Property (Group (..), failWith)

#ifdef UNIX
import           Cardano.Node.Types (VRFPrivateKeyFilePermissionError (..))

import           System.Posix.Files
import           System.Posix.IO (closeFd, createFile)
import           System.Posix.Types (FileMode)

import           Hedgehog (Gen, classify, forAll)
import qualified Hedgehog.Gen as Gen
#endif

{- HLINT ignore "Use fewer imports" -}

prop_createVRFFileWithOwnerPermissions :: Property
prop_createVRFFileWithOwnerPermissions =
  property $ do
    let vrfSign = "vrf-signing-key"
    vrfSkey <- liftIO $ generateSigningKey AsVrfKey
    createFileWithOwnerPermissions vrfSign vrfSkey

    fResult <- liftIO . runExceptT $ checkVRFFilePermissions vrfSign
    case fResult of
      Left err -> failWith Nothing $ show err
      Right () -> liftIO (removeFile vrfSign) >> success

createFileWithOwnerPermissions :: HasTextEnvelope a => FilePath -> a -> PropertyT IO ()
createFileWithOwnerPermissions targetfp value = do
  result <- liftIO $ writeFileTextEnvelopeWithOwnerPermissions targetfp Nothing value
  case result of
    Left err -> failWith Nothing $ displayError err
    Right () -> return ()

#ifdef UNIX

-- | This property ensures that 'checkVRFFilePermissions' checks the
-- file permissions & ownership correctly.
prop_sanityCheck_checkVRFFilePermissions :: Property
prop_sanityCheck_checkVRFFilePermissions =
  property $ do
    -- Correct case: only owner has read permission
    let correctPermission = ownerReadMode
        vrfPrivateKeyCorrect = "vrf-private-key-correct"
    correctResult <-
      liftIO $ bracket  (createFile vrfPrivateKeyCorrect correctPermission)
                        (\h -> closeFd h >> removeFile vrfPrivateKeyCorrect)
                        (const . liftIO . runExceptT $ checkVRFFilePermissions vrfPrivateKeyCorrect)
    case correctResult of
      Left err ->
        failWith Nothing $ "checkVRFFilePermissions should not have failed with error: "
                         <> show err
      Right () -> success

    -- Error case: owner has read permissions & various combinations of other permissions
    let vrfPrivateKeyOther = "vrf-private-key-other"
    oPermissions <- forAll genOtherPermissions
    classify "VRF File has one other permission" $ length oPermissions == 1
    classify "VRF File has two other permissions" $ length oPermissions == 2
    classify "VRF File has three other permissions" $ length oPermissions == 3
    otherResult <-
      -- Creating a file with other permissions appears to not work
      -- it instead creates a file with owner permissions. Therefore we must
      -- create a file with no permissions and then set other permissions
      liftIO $ bracket  (do h <- createFile vrfPrivateKeyOther nullFileMode
                            setFileMode vrfPrivateKeyOther $ createPermissions oPermissions
                            return h)
                        (\h -> closeFd h >> removeFile vrfPrivateKeyOther)
                        (const .liftIO . runExceptT $ checkVRFFilePermissions vrfPrivateKeyOther)
    case otherResult of
      Left (OtherPermissionsExist _) -> success
      Left err ->
        failWith Nothing $ "checkVRFFilePermissions should not have failed with error: "
                         <> show err
      Right () ->
        failWith Nothing "This should have failed as Other permissions exist"

    -- Error case: owner has read permissions & various combinations of group permissions
    let vrfPrivateKeyGroup = "vrf-private-key-group"
    gPermissions <- forAll genGroupPermissions
    classify "VRF File has one group permission" $ length gPermissions == 1
    classify "VRF File has two group permissions" $ length gPermissions == 2
    classify "VRF File has three group permissions" $ length gPermissions == 3
    groupResult <-
      -- Creating a file with group permissions appears to not work
      -- it instead creates a file with owner permissions. Therefore we must
      -- create a file with no permissions and then set group permissions.
      liftIO $ bracket  (do h <- createFile vrfPrivateKeyGroup nullFileMode
                            setFileMode vrfPrivateKeyGroup $ createPermissions gPermissions
                            return h)
                        (\h -> closeFd h >> removeFile vrfPrivateKeyGroup)
                        (const . liftIO . runExceptT $ checkVRFFilePermissions vrfPrivateKeyGroup)
    case groupResult of
      Left (GroupPermissionsExist _) -> success
      Left err ->
        failWith Nothing $ "checkVRFFilePermissions should not have failed with error: "
                         <> show err
      Right () ->
        failWith Nothing "This should have failed as Group permissions exist"

createPermissions :: [FileMode] -> FileMode
createPermissions = foldl' unionFileModes (ownerReadMode `unionFileModes` ownerWriteMode)

genGroupPermissions :: Gen [FileMode]
genGroupPermissions =
  let gPermissions = [groupReadMode, groupWriteMode, groupExecuteMode]
  in do subSeq <- Gen.filter (not . null) $ Gen.subsequence gPermissions
        Gen.frequency [(3, return gPermissions), (12, return subSeq)]

genOtherPermissions :: Gen [FileMode]
genOtherPermissions =
  let oPermissions = [otherReadMode, otherWriteMode, otherExecuteMode]
  in do subSeq <- Gen.filter (not . null) $ Gen.subsequence oPermissions
        Gen.frequency [(3, return oPermissions), (12, return subSeq)]
#endif

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $ Group "Test.Cardano.Node.FilePermissons"
#ifdef UNIX
    [ ("prop_createVRFFileWithOwnerPermissions", prop_createVRFFileWithOwnerPermissions)
    , ("prop_sanityCheck_checkVRFFilePermissions", prop_sanityCheck_checkVRFFilePermissions)
    ]
#else
    [("prop_createVRFFileWithOwnerPermissions", prop_createVRFFileWithOwnerPermissions)]
#endif
