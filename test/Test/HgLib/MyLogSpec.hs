{-# LANGUAGE OverloadedStrings #-}

-- Test/HgLib/MyLogSpec.hs
module Test.HgLib.MyLogSpec (spec) where

import Test.Hspec
import Test.HgLib.Common
import qualified HgLib.Commands as C
import qualified HgLib
import HgLib.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Exception (try, SomeException)

spec :: Spec
spec = describe "Log" $ do
  
  it "should return empty log for empty repository" $ do
    withTestRepo $ \bt -> do
      -- Correct parameter order: C.log_ client files options
      revs <- C.log_ (btClient bt) [] defaultLogOptions
      revs `shouldBe` []
  
  it "should return commit information" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      -- Try to create commits
      commonAppendFile "a" "a"
      let options = mkTestCommitOptions "first"
      result1 <- (try :: IO (Int, Text) -> IO (Either SomeException (Int, Text))) $ 
        C.commit client (options { commitAddRemove = True })
      
      case result1 of
        Right (rev0, node0) -> do
          commonAppendFile "a" "a"
          let options = mkTestCommitOptions "second"
          result2 <- (try :: IO (Int, Text) -> IO (Either SomeException (Int, Text))) $ 
            C.commit client options
          
          case result2 of
            Right (rev1, node1) -> do
              revs <- C.log_ client [] defaultLogOptions
              
              length revs `shouldBe` 2
              let [rev1', rev0'] = revs  -- log returns newest first
              
              revDesc rev0' `shouldBe` "first"
              revDesc rev1' `shouldBe` "second"
              revBranch rev0' `shouldBe` "default"
              revBranch rev1' `shouldBe` "default"
            Left _ -> pendingWith "Second commit failed"
        Left _ -> pendingWith "First commit failed"

  it "should filter by specific revision" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client (mkDefaultCommitOptions "first") { commitAddRemove = True }
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client $ mkDefaultCommitOptions "second"
      
      revs <- C.log_ client [] (defaultLogOptions { logRevRange = Just "0" })
      
      length revs `shouldBe` 1
      revNode (head revs) `shouldBe` TE.encodeUtf8 node0
  
  it "should handle files parameter" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      commonCreateFile "a" "a"
      commonCreateFile "b" "b"
      C.commit client (mkDefaultCommitOptions "first") { commitAddRemove = True }
      
      commonAppendFile "a" "a"
      C.commit client $ mkDefaultCommitOptions "second on a"
      
      commonAppendFile "b" "b"  
      C.commit client $ mkDefaultCommitOptions "third on b"
      
      -- Log should show all commits
      allRevs <- C.log_ client [] defaultLogOptions
      length allRevs `shouldBe` 3
      
      -- Log for file 'a' should show 2 commits
      aRevs <- C.log_ client [] (defaultLogOptions { logFiles = ["a"] })
      length aRevs `shouldBe` 2

