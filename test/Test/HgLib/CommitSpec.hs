{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.CommitSpec (spec) where

import Test.Hspec
import Test.HgLib.Common
import qualified HgLib.Commands as C
import HgLib.Types (SummaryInfo(..), Revision(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Control.Exception (try, SomeException)

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "Commit" $ do
  it "should handle commit with custom user" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev, node) <- C.commit client (mkUpdateableCommitOptions "first" $ \opts -> opts { C.commitAddRemove = True } { C.commitUser = Just "foo" })
      rev <- head <$> C.log_ client [node] C.defaultLogOptions
      revAuthor rev `shouldBe` "foo"

  it "should fail with empty user" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      result <- (try :: IO (Int, Text) -> IO (Either SomeException (Int, Text))) $ C.commit client mkUpdateableCommitOptions "first" $ \opts -> opts { C.commitUser = Just "" }
      result `shouldSatisfy` isLeft

  it "should close branch" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client (mkUpdateableCommitOptions "first" $ \opts -> opts { C.commitAddRemove = True })
      C.branch client (Just "foo") []
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client (mkTestCommitOptions "second")
      revclose <- C.commit client $ mkUpdateableCommitOptions "closing foo" $ \opts -> opts { C.commitCloseBranch = True }
      [rev0, rev1, revclose] <- C.log_ client [[node0, node1, revclose !! 1]] C.defaultLogOptions
      -- TODO: complex assertEqual
      -- TODO: complex assertEqual
      return ()

  it "should handle message and logfile conflicts" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      result <- (try :: IO (Int, Text) -> IO (Either SomeException (Int, Text))) $ C.commit client mkUpdateableCommitOptions "foo" $ \opts -> opts { C.commitLogFile = Just "bar" }
      result `shouldSatisfy` isLeft
      result <- (try :: IO (Int, Text) -> IO (Either SomeException (Int, Text))) $ C.commit client mkTestCommitOptions "default"
      result `shouldSatisfy` isLeft

  it "should handle custom date" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      now <- return -- TODO: method call -- TODO: attr access datetime.datetime.now(...) -- TODO: handle replace with 1 args
      (rev0, node0) <- C.commit client (mkUpdateableCommitOptions "first" $ \opts -> opts { C.commitAddRemove = True } { C.commitDate = Just -- TODO: method call -- TODO: method call now.isoformat(...).encode(...) })
      now `shouldBe` revDate C.tip client

  it "should amend previous commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      now <- return -- TODO: method call -- TODO: attr access datetime.datetime.now(...) -- TODO: handle replace with 1 args
      (rev0, node0) <- C.commit client (mkUpdateableCommitOptions "first" $ \opts -> opts { C.commitAddRemove = True } { C.commitDate = Just -- TODO: method call -- TODO: method call now.isoformat(...).encode(...) })
      now `shouldBe` revDate C.tip client
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client (mkUpdateableCommitOptions "default" $ \opts -> opts { C.commitAmend = True })
      now `shouldBe` revDate C.tip client
      node0 `shouldNotBe` node1
      -- TODO: complex assertEqual

  it "should prevent null injection" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      result <- (try :: IO (Int, Text) -> IO (Either SomeException (Int, Text))) $ C.commit client $ mkTestCommitOptions "fail\0-A"
      result `shouldSatisfy` isLeft
      -- TODO: complex assertEqual


