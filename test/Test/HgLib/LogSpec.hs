{-# LANGUAGE OverloadedStrings #-}

-- Test/HgLib/LogSpec.hs
module Test.HgLib.LogSpec (spec) where

import Test.Hspec
import Test.HgLib.Common
import qualified HgLib.Commands as C
import HgLib.Types
import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = describe "Log" $ do
  
  it "should return empty log for empty repository" $ do
    withTestRepo $ \bt -> do
      revs <- C.log_ (btClient bt) C.defaultLogOptions
      revs `shouldBe` []
  
  it "should return commit information" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      -- Create two commits
      appendFile "a" "a"
      (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      appendFile "a" "a"  
      (rev1, node1) <- C.commit client "second" C.defaultCommitOptions
      
      revs <- C.log_ client C.defaultLogOptions
      
      length revs `shouldBe` 2
      let [rev1', rev0'] = revs  -- log returns newest first
      
      revNode rev1' `shouldBe` node1
      revNode rev0' `shouldBe` node0
      revDesc rev0' `shouldBe` "first"
      revDesc rev1' `shouldBe` "second"
      revBranch rev0' `shouldBe` "default"
      revBranch rev1' `shouldBe` "default"
  
  it "should filter by specific revision" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      appendFile "a" "a"
      (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      appendFile "a" "a"
      (rev1, node1) <- C.commit client "second" C.defaultCommitOptions
      
      revs <- C.log_ client (C.defaultLogOptions { C.logRev = Just "0" })
      
      length revs `shouldBe` 1
      revNode (head revs) `shouldBe` node0
  
  it "should handle files parameter" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      createFile "a" "a"
      createFile "b" "b"
      C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      
      appendFile "a" "a"
      C.commit client "second on a" C.defaultCommitOptions
      
      appendFile "b" "b"  
      C.commit client "third on b" C.defaultCommitOptions
      
      -- Log should show all commits
      allRevs <- C.log client C.defaultLogOptions
      length allRevs `shouldBe` 3
      
      -- Log for file 'a' should show 2 commits
      aRevs <- C.log client (C.defaultLogOptions { C.logFiles = ["a"] })
      length aRevs `shouldBe` 2

