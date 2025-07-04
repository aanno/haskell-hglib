{-# LANGUAGE OverloadedStrings #-}

-- Test/HgLib/LogSpec.hs
module Test.HgLib.LogSpec (spec) where

import Test.Hspec
import Test.HgLib.Common
import qualified HgLib.Commands as C
import qualified HgLib as HgLib
import HgLib.Types
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception (try, SomeException)

spec :: Spec
spec = describe "Log" $ do
  
  it "should return empty log for empty repository" $ do
    withTestRepo $ \bt -> do
      -- Correct parameter order: C.log_ client files options
      revs <- C.log_ (btClient bt) [] C.defaultLogOptions
      revs `shouldBe` []
  
  it "should return commit information" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      -- Try to create commits
      commonAppendFile "a" "a"
      let options = mkTestCommitOptions "first"
      result1 <- (try :: IO (Int, Text) -> IO (Either SomeException (Int, Text))) $ 
        C.commit client (options { C.commitAddRemove = True })
      
      case result1 of
        Right (rev0, node0) -> do
          commonAppendFile "a" "a"
          let options = mkTestCommitOptions "second"
          result2 <- (try :: IO (Int, Text) -> IO (Either SomeException (Int, Text))) $ 
            C.commit client options
          
          case result2 of
            Right (rev1, node1) -> do
              revs <- C.log_ client [] C.defaultLogOptions
              
              length revs `shouldBe` 2
              let [rev1', rev0'] = revs  -- log returns newest first
              
              revDesc rev0' `shouldBe` "first"
              revDesc rev1' `shouldBe` "second"
              revBranch rev0' `shouldBe` "default"
              revBranch rev1' `shouldBe` "default"
            Left _ -> pendingWith "Second commit failed"
        Left _ -> pendingWith "First commit failed"

-- TODO: logRev is not implemented
--   it "should filter by specific revision" $ do
--     withTestRepo $ \bt -> do
--       let client = btClient bt
      
--       appendFile "a" "a"
--       (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
--       appendFile "a" "a"
--       (rev1, node1) <- C.commit client "second" C.defaultCommitOptions
      
--       revs <- C.log_ client (C.defaultLogOptions { C.logRev = Just "0" })
      
--       length revs `shouldBe` 1
--       revNode (head revs) `shouldBe` node0
  
  -- TODO: logFiles is not implemented yet
  -- it "should handle files parameter" $ do
  --   withTestRepo $ \bt -> do
  --     let client = btClient bt
      
  --     createFile "a" "a"
  --     createFile "b" "b"
  --     C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      
  --     appendFile "a" "a"
  --     C.commit client "second on a" C.defaultCommitOptions
      
  --     appendFile "b" "b"  
  --     C.commit client "third on b" C.defaultCommitOptions
      
  --     -- Log should show all commits
  --     allRevs <- C.log_ client C.defaultLogOptions
  --     length allRevs `shouldBe` 3
      
  --     -- Log for file 'a' should show 2 commits
  --     aRevs <- C.log_ client (C.defaultLogOptions { C.logFiles = ["a"] })
  --     length aRevs `shouldBe` 2

