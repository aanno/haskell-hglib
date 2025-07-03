{-# LANGUAGE OverloadedStrings #-}

-- Test/HgLib/UpdateSpec.hs
module Test.HgLib.UpdateSpec (spec) where

import Test.Hspec
import Test.HgLib.Common
import qualified HgLib.Commands as C
import HgLib.Types

spec :: Spec  
spec = describe "Update" $ do
  
  it "should update to specific revision" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      -- Create two commits
      appendFile "a" "a"
      (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      appendFile "a" "a"
      (rev1, node1) <- C.commit client "second" C.defaultCommitOptions
      
      -- Update to first revision
      (updated, merged, removed, unresolved) <- C.update client 
        (C.defaultUpdateOptions { C.updateRev = Just (show rev0) })
      
      updated `shouldBe` 1
      merged `shouldBe` 0
      removed `shouldBe` 0
      unresolved `shouldBe` 0
      
      -- Verify we're at the right revision
      parents <- C.parents client Nothing []
      case parents of
        Just [parent] -> revNode parent `shouldBe` node0
        _ -> expectationFailure "Expected exactly one parent"
  
  it "should handle merge conflicts" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      -- Create divergent history
      appendFile "a" "a"
      (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      appendFile "a" "a"
      (rev1, node1) <- C.commit client "second" C.defaultCommitOptions
      
      -- Update back and create conflicting change
      C.update client (C.defaultUpdateOptions { C.updateRev = Just (show rev0) })
      appendFile "a" "b"
      
      -- Try to update - should create unresolved conflict
      (updated, merged, removed, unresolved) <- C.update client C.defaultUpdateOptions
      
      unresolved `shouldBe` 1
      
      -- Check status shows modified file
      status <- C.status client C.defaultStatusOptions
      any (\s -> statusCode s == 'M' && statusFile s == "a") status `shouldBe` True
  
  it "should handle clean flag" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      createFile "a" "original"
      (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      
      -- Modify file
      createFile "a" "modified"
      
      -- Clean update should restore original content
      (updated, merged, removed, unresolved) <- C.update client 
        (C.defaultUpdateOptions { C.updateClean = True })
      
      updated `shouldBe` 1
      content <- BS.readFile "a"
      content `shouldBe` "original"

