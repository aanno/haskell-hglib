{-# LANGUAGE OverloadedStrings #-}

-- Test/HgLib/MyUpdateSpec.hs
module Test.HgLib.MyUpdateSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE

import Test.Hspec
import Test.HgLib.Common
import qualified HgLib.Commands as C
import HgLib.Types

spec :: Spec  
spec = describe "Update" $ do

  it "should update to specific revision" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      -- Create two commits
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client (mkDefaultCommitOptions "first") { commitAddRemove = True }
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client $ mkDefaultCommitOptions "second"
      
      -- Update to first revision
      (updated, merged, removed, unresolved) <- C.update client 
        (defaultUpdateOptions { updateRev = Just (show rev0) })
      
      updated `shouldBe` 1
      merged `shouldBe` 0
      removed `shouldBe` 0
      unresolved `shouldBe` 0
      
      -- Verify we're at the right revision
      parents <- C.parents client []
      case parents of
        [parent] -> revNode parent `shouldBe` TE.encodeUtf8 node0
        _ -> expectationFailure "Expected exactly one parent"
  
  it "should handle merge conflicts" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      -- Create divergent history
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client (mkDefaultCommitOptions "first") { commitAddRemove = True }
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client $ mkDefaultCommitOptions "second"
      
      -- Update back and create conflicting change
      C.update client (defaultUpdateOptions { updateRev = Just (show rev0) })
      commonAppendFile "a" "b"
      
      -- Try to update - should create unresolved conflict
      (updated, merged, removed, unresolved) <- C.update client defaultUpdateOptions
      
      unresolved `shouldBe` 1
      
      -- Check status shows modified file
      status <- C.status client defaultStatusOptions
      any (\s -> statusCode s == 'M' && statusFile s == "a") status `shouldBe` True

  it "should handle clean flag" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      commonCreateFile "a" "original"
      (rev0, node0) <- C.commit client (mkDefaultCommitOptions "first") { commitAddRemove = True }
      
      -- Modify file
      commonCreateFile "a" "modified"
      
      -- Clean update should restore original content
      (updated, merged, removed, unresolved) <- C.update client 
        (defaultUpdateOptions { updateClean = True })
      
      updated `shouldBe` 1
      content <- BS.readFile "a"
      content `shouldBe` "original"

