{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Node.FilePermissions
  ( tests
  ) where

import           Cardano.Prelude

import           Control.Exception (bracket_)
import           System.Directory (removeFile)
import           System.Posix.Files
import           System.Posix.IO (createFile)
import           System.Posix.Types (FileMode)

import           Cardano.Node.Run (checkVRFFilePermissions)
import           Cardano.Node.Types (VRFPrivateKeyFilePermissionError (..))
import           Hedgehog (Property, classify, discover, forAll, property, success)
import qualified Hedgehog
import           Hedgehog.Internal.Property (failWith)

import           Test.Cardano.Node.Gen (genGroupPermissions, genOtherPermissions)

-- | This property ensures that 'checkVRFFilePermissions' sets the
-- file permissions & ownership correctly.
prop_sanityCheck_checkVRFFilePermissions :: Property
prop_sanityCheck_checkVRFFilePermissions =
  property $ do
    -- Correct case: only owner has read permission
    let correctPermission = ownerReadMode
        vrfPrivateKeyCorrect = "vrf-private-key-correct"
    correctResult <-
      liftIO $ bracket_ (createFile vrfPrivateKeyCorrect correctPermission)
                        (removeFile vrfPrivateKeyCorrect)
                        (liftIO . runExceptT $ checkVRFFilePermissions $ Just vrfPrivateKeyCorrect)
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
      liftIO $ bracket_ (do _ <- createFile vrfPrivateKeyOther nullFileMode
                            setFileMode vrfPrivateKeyOther $ createPermissions oPermissions)
                        (removeFile vrfPrivateKeyOther)
                        (liftIO . runExceptT $ checkVRFFilePermissions $ Just vrfPrivateKeyOther)
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
      liftIO $ bracket_ (createFile vrfPrivateKeyGroup $ createPermissions gPermissions)
                        (removeFile vrfPrivateKeyGroup)
                        (liftIO . runExceptT $ checkVRFFilePermissions $ Just vrfPrivateKeyGroup)
    case groupResult of
      Left (GroupPermissionsExist _) -> success
      Left err ->
        failWith Nothing $ "checkVRFFilePermissions should not have failed with error: "
                         <> show err
      Right () ->
        failWith Nothing "This should have failed as Group permissions exist"


createPermissions :: [FileMode] -> FileMode
createPermissions = foldl' unionFileModes (ownerReadMode `unionFileModes` ownerWriteMode)

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
