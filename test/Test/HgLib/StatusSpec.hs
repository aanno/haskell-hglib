{-# LANGUAGE OverloadedStrings #-}

-- Test/HgLib/StatusSpec.hs  
module Test.HgLib.StatusSpec (spec) where

import Test.Hspec
import Test.HgLib.Common
import qualified HgLib.Commands as C
import HgLib.Types

spec :: Spec
spec = describe "Status" $ do
  
  it "should return empty status for clean repository" $ do
    withTestRepo $ \bt -> do
      status <- C.status (btClient bt) C.defaultStatusOptions
      status `shouldBe` []
  
  it "should detect all status types" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      -- Setup files in different states
      commonCreateFile ".hgignore" "ignored"
      commonCreateFile "ignored" "a"
      commonCreateFile "clean" "a"  
      commonCreateFile "modified" "a"
      commonCreateFile "removed" "a"
      commonCreateFile "missing" "a"
      
      -- Initial commit
      C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      
      -- Modify files to create different statuses
      commonAppendFile "modified" "a"
      commonCreateFile "added" "a"
      C.add client ["added"] []
      commonRemoveFile "missing"
      C.remove client ["removed"] []
      commonCreateFile "untracked" "content"
      
      -- Get status with all types
      status <- C.status client (C.defaultStatusOptions { C.statusAll = True })
      
      -- Check each status type is present
      let hasStatus code = any (\s -> statusCode s == code) status
      hasStatus 'M' `shouldBe` True  -- modified
      hasStatus 'A' `shouldBe` True  -- added
      hasStatus 'R' `shouldBe` True  -- removed
      hasStatus 'C' `shouldBe` True  -- clean
      hasStatus '!' `shouldBe` True  -- missing
      hasStatus '?' `shouldBe` True  -- untracked
      hasStatus 'I' `shouldBe` True  -- ignored
      
  it "should handle file copies" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      commonCreateFile "source" "a"
      C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      C.copy client "source" "dest" []
      
      status <- C.status client (C.defaultStatusOptions { C.statusCopies = True })
      
      let copyEntries = filter (\s -> statusCode s `elem` ['A', ' ']) status
      length copyEntries `shouldBe` 2
