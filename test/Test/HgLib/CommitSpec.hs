{-# LANGUAGE OverloadedStrings #-}

-- Test/HgLib/CommitSpec.hs
module Test.HgLib.CommitSpec (spec) where

import Test.Hspec
import Test.HgLib.Common
import qualified HgLib.Commands as C
import HgLib.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

spec :: Spec
spec = describe "Commit" $ do
  
  it "should create commit with custom user" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      appendFile "a" "a"
      (rev, node) <- C.commit client "first" 
        (C.defaultCommitOptions { C.commitAddRemove = True, C.commitUser = Just "foo" })
      
      revs <- C.log_ client (C.defaultLogOptions { C.logRev = Just (T.unpack $ TE.decodeUtf8 node) })
      case revs of
        [rev'] -> revAuthor rev' `shouldBe` "foo"
        _ -> expectationFailure "Expected exactly one revision"
  
  it "should fail with empty user" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      appendFile "a" "a"
      result <- try $ C.commit client "first" 
        (C.defaultCommitOptions { C.commitAddRemove = True, C.commitUser = Just "" })
      
      result `shouldSatisfy` isLeft
  
  it "should handle custom date" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      appendFile "a" "a"
      now <- getCurrentTime
      let dateStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
      
      (rev, node) <- C.commit client "first" 
        (C.defaultCommitOptions 
          { C.commitAddRemove = True
          , C.commitDate = Just dateStr 
          })
      
      revs <- C.log client (C.defaultLogOptions { C.logRev = Just (T.unpack $ TE.decodeUtf8 node) })
      case revs of
        [rev'] -> do
          let commitTime = revDate rev'
          let timeDiff = diffUTCTime commitTime now
          abs timeDiff `shouldSatisfy` (< 60)  -- Within 1 minute
        _ -> expectationFailure "Expected exactly one revision"
  
  it "should close branch" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      -- Create initial commit
      appendFile "a" "a"
      (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      
      -- Create new branch
      C.branch client "foo" []
      appendFile "a" "a"
      (rev1, node1) <- C.commit client "second" C.defaultCommitOptions
      
      -- Close branch
      (revClose, nodeClose) <- C.commit client "closing foo" 
        (C.defaultCommitOptions { C.commitCloseBranch = True })
      
      -- Check branches (should only show default)
      branches <- C.branches client []
      map (\(name, _, _) -> name) branches `shouldBe` ["default"]
      
      -- Check closed branches
      allBranches <- C.branches client ["--closed"]
      length allBranches `shouldBe` 2
  
  it "should amend previous commit" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      appendFile "a" "a"
      (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      
      -- Amend the commit
      appendFile "a" "a"
      (rev1, node1) <- C.commit client "amended" (C.defaultCommitOptions { C.commitAmend = True })
      
      -- Should still only have one commit
      revs <- C.log client C.defaultLogOptions
      length revs `shouldBe` 1
      
      -- But different node
      node0 `shouldNotBe` node1
      
      -- And new message
      revDesc (head revs) `shouldBe` "amended"

-- Additional test modules would continue following this pattern...
-- Each module covers the functionality from the corresponding Python test
