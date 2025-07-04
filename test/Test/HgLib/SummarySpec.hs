{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.SummarySpec (spec) where

import Test.Hspec
import Test.HgLib.Common
import qualified HgLib.Commands as C
import HgLib.Types (SummaryInfo(..))
import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = describe "Summary" $ do  
  it "should handle empty repository" $ do
    withTestRepo $ \bt -> do
      summary <- C.summary (btClient bt) []
      -- Access the SummaryInfo fields directly
      summaryCommitClean summary `shouldBe` True
  
  it "should handle basic repository with one commit" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      -- Create and commit a file
      commonAppendFile "a" "a"
      let options = mkTestCommitOptions "msg2"
      (rev, node) <- C.commit client (options { C.commitAddRemove = True })
      
      summary <- C.summary client []
      -- Check SummaryInfo fields
      summaryBranch summary `shouldBe` "default"
      summaryCommitClean summary `shouldBe` True
      length (summaryParents summary) `shouldBe` 1
  
  it "should detect dirty working directory" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      -- Create and commit a file
      commonAppendFile "a" "a"
      let options = mkTestCommitOptions "msg1"
      (rev, node) <- C.commit client ( options { C.commitAddRemove = True })
      
      -- Modify the file
      commonAppendFile "a" "b"
      
      summary <- C.summary client []
      -- Should not be clean anymore
      summaryCommitClean summary `shouldBe` False
  
  -- Commented out until UpdateOptions is implemented
  -- it "should handle update information" $ do
  --   withTestRepo $ \bt -> do
  --     let client = btClient bt
  --     
  --     -- Create two commits
  --     appendcommonAppendFileFile "a" "a"
  --     (rev0, node0) <- C.commit client (C.defaultCommitOptions { C.commitAddRemove = True })
  --     commonAppendFile "a" "a"
  --     (rev1, node1) <- C.commit client C.defaultCommitOptions
  --     
  --     -- Update to first revision (need UpdateOptions implementation)
  --     -- C.update client (C.defaultUpdateOptions { C.updateRev = Just (show rev0) })
  --     
  --     summary <- C.summary client []
  --     -- Should show that 1 update is available
  --     summaryUpdateCount summary `shouldBe` 1

-- TODO: updateRev is not implemented
--   it "should handle update information" $ do
--     withTestRepo $ \bt -> do
--       let client = btClient bt
      
--       -- Create two commits
--       commonAppendFile "a" "a"
--       (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
--       commonAppendFile "a" "a"
--       (rev1, node1) <- C.commit client "second" C.defaultCommitOptions
      
--       -- Update to first revision
--       C.update client (C.defaultUpdateOptions { C.updateRev = Just (show rev0) })
      
--       summary <- C.summary client []
--       let update = lookup "update" summary
--       update `shouldSatisfy` maybe False (T.isPrefixOf "1")
