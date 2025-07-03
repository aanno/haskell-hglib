{-# LANGUAGE OverloadedStrings #-}

-- Test/HgLib/SummarySpec.hs
module Test.HgLib.SummarySpec (spec) where

import Test.Hspec
import Test.HgLib.Common
import qualified HgLib.Commands as C
import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = describe "Summary" $ do
  
  it "should handle empty repository" $ do
    withTestRepo $ \bt -> do
      summary <- C.summary (btClient bt) []
      let commit = lookup "commit" summary
      commit `shouldSatisfy` maybe False ("(clean)" `T.isInfixOf`)
  
  it "should handle basic repository with one commit" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      -- Create and commit a file
      appendFile "a" "a"
      (rev, node) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      
      summary <- C.summary client []
      let parent = lookup "parent" summary
          branch = lookup "branch" summary  
          commit = lookup "commit" summary
          
      parent `shouldSatisfy` maybe False (T.isPrefixOf "0:")
      branch `shouldBe` Just "default"
      commit `shouldSatisfy` maybe False ("(clean)" `T.isInfixOf`)
  
  it "should detect dirty working directory" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      -- Create and commit a file
      appendFile "a" "a"
      (rev, node) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      
      -- Modify the file
      appendFile "a" "b"
      
      summary <- C.summary client []
      let commit = lookup "commit" summary
      commit `shouldSatisfy` maybe False (not . ("(clean)" `T.isInfixOf`))
  
  it "should handle update information" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      -- Create two commits
      appendFile "a" "a"
      (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      appendFile "a" "a"
      (rev1, node1) <- C.commit client "second" C.defaultCommitOptions
      
      -- Update to first revision
      C.update client (C.defaultUpdateOptions { C.updateRev = Just (show rev0) })
      
      summary <- C.summary client []
      let update = lookup "update" summary
      update `shouldSatisfy` maybe False (T.isPrefixOf "1")
