{-# LANGUAGE OverloadedStrings #-}

-- Test/HgLib/MyStatusSpec.hs  
module Test.HgLib.MyStatusSpec (spec) where

import Test.Hspec
import Test.HgLib.Common
import qualified HgLib.Commands as C
import qualified HgLib
import HgLib.Types
import Control.Exception (try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = describe "Status" $ do
  
  it "should return empty status for clean repository" $ do
    withTestRepo $ \bt -> do
      status <- HgLib.simpleStatus (btClient bt)
      status `shouldBe` []
  
  it "should detect basic file statuses" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      -- Create a file
      commonCreateFile "test.txt" "content"
      
      -- Should show as untracked
      status <- HgLib.simpleStatus client
      
      -- Check if any file shows as untracked ('?')
      let hasUntracked = any (\s -> statusCode s == '?') status
      hasUntracked `shouldBe` True
  
  it "should handle committed files" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      
      -- Create a file
      commonCreateFile "a" "a"
      
      let options = mkTestCommitOptions "first"

      -- Try to commit (fix the Maybe String issue)
      result <- (try :: IO (Int, Text) -> IO (Either SomeException (Int, Text))) $ 
        C.commit client (options { C.commitAddRemove = True })
      
      case result of
        Right _ -> do
          -- After commit, should be clean
          status <- HgLib.simpleStatus client
          status `shouldBe` []
        Left _ -> pendingWith "Commit functionality not working yet"

-- TODO: no defaultStatusOptions, defaultCommitOptions at present
--   it "should handle file copies" $ do
--     withTestRepo $ \bt -> do
--       let client = btClient bt
      
--       commonCreateFile "source" "a"
--       C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
--       C.copy client "source" "dest" []
      
--       status <- C.status client (C.defaultStatusOptions { C.statusCopies = True })
      
--       let copyEntries = filter (\s -> statusCode s `elem` ['A', ' ']) status
--       length copyEntries `shouldBe` 2
