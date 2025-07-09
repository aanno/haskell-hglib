{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.CommitSpec (spec) where

import Control.Exception (try, SomeException)
import Data.Text (Text)
import HgLib.Types
import Test.HgLib.Common
import Test.Hspec
import qualified Data.Text as T
import qualified HgLib.Commands as C
import qualified System.FilePath

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "Commit" $ do

-- Conversion notes:
-- TODO: Unhandled module attribute: self.client.commit
-- TODO: Unhandled module attribute: self.client.commit
-- TODO: Unhandled module attribute: self.client.commit
-- TODO: Unhandled method call: now.isoformat
-- TODO: Unhandled method call: now.isoformat

  it "should handle commit with custom user" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev, node) <- C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True, commitUser = Just "foo" })
      rev <- head C.log_ client node
      revAuthor rev `shouldBe` "foo"

  it "should fail with empty user" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      -- TODO: self.client.commit "first" -- TODO: keyword arg user="" `shouldThrow` anyException

  it "should close branch" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True })
      C.branch client "foo"
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client (mkDefaultCommitOptions "second")
      revclose <- C.commit client ((mkDefaultCommitOptions "closing foo") { commitCloseBranch = True })
      (rev0, rev1, revclose) <- C.log_ client [node0, node1, (C.commit client ((mkDefaultCommitOptions "closing foo") { commitCloseBranch = True }) !! 1)]
      C.branches client  `shouldBe` [(revBranch rev0, read revRev rev0, take 12 revNode rev0)]
      C.branches client  (C.defaultBranchesOptions { -- TODO: options closed=True }) `shouldBe` [(revBranch revclose, read revRev revclose, take 12 revNode revclose), (revBranch rev0, read revRev rev0, take 12 revNode rev0)]

  it "should handle message and logfile conflicts" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: self.client.commit "foo" -- TODO: keyword arg logfile="bar" `shouldThrow` anyException
      -- TODO: self.client.commit `shouldThrow` anyException
      pendingWith "Test not implemented yet"

  it "should handle custom date" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      now <- -- TODO: replace chained call on getCurrentTime -- TODO: keyword arg microsecond=0
      (rev0, node0) <- C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True, commitDate = Just -- TODO: encode chained call on -- TODO: now.isoformat "latin-1" })
      -- TODO: replace chained call on getCurrentTime -- TODO: keyword arg microsecond=0 `shouldBe` revDate (C.tip client )

  it "should amend previous commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      now <- -- TODO: replace chained call on getCurrentTime -- TODO: keyword arg microsecond=0
      (rev0, node0) <- C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True, commitDate = Just -- TODO: encode chained call on -- TODO: now.isoformat "latin-1" })
      -- TODO: replace chained call on getCurrentTime -- TODO: keyword arg microsecond=0 `shouldBe` revDate (C.tip client )
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client (mkDefaultCommitOptions "" { commitAmend = True })
      -- TODO: replace chained call on getCurrentTime -- TODO: keyword arg microsecond=0 `shouldBe` revDate (C.tip client )
      node0 `shouldNotBe` node1
      1 `shouldBe` length C.log_ client []

  it "should prevent null injection" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (\_-> C.commit client (mkDefaultCommitOptions "fail\0-A")) `shouldThrow` anyException
      0 `shouldBe` length C.log_ client []


-- TODOS:
-- Unhandled module attribute: self.client.commit
-- Unhandled module attribute: self.client.commit
-- Unhandled module attribute: self.client.commit
-- Unhandled method call: now.isoformat
-- Unhandled method call: now.isoformat
