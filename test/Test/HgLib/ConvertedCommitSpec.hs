{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.ConvertedCommitSpec (spec) where

import Test.Hspec
import Test.HgLib.Common
import qualified HgLib.Commands as C
import HgLib.Types (SummaryInfo(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Control.Exception (try, SomeException)

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "Commit" $ do
  it "should handle commit with custom user" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "'a'" "'a'"
      (rev, node) <- C.commit client (mkTestCommitOptions "'first'" { C.commitAddRemove = True } { C.commitUser = Just "'foo'" })
      rev <- head <$> self.client.log(node)
      rev.author `shouldBe` "'foo'"

  it "should fail with empty user" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "'a'" "'a'"
      -- TODO: assertRaises with 4 args

  it "should close branch" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "'a'" "'a'"
      (rev0, node0) <- C.commit client (mkTestCommitOptions "'first'" { C.commitAddRemove = True })
      C.branch client (Just "'foo'") []
      commonAppendFile "'a'" "'a'"
      (rev1, node1) <- C.commit client (mkTestCommitOptions "'second'")
      revclose <- C.commit client (mkTestCommitOptions "'closing foo'" { C.commitCloseBranch = True })
      [rev0, rev1, revclose] <- C.log_ client [[node0, node1, revclose[1]]] C.defaultLogOptions
      self.client.branches() `shouldBe` [-- TODO: expr Paren {paren_expr = Tuple {tuple_exprs = [Dot {dot]
      self.client.branches(closed=True) `shouldBe` [-- TODO: expr Paren {paren_expr = Tuple {tuple_exprs = [Dot {dot, -- TODO: expr Paren {paren_expr = Tuple {tuple_exprs = [Dot {dot]

  it "should handle message and logfile conflicts" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: assertRaises with 4 args
      -- TODO: assertRaises with 2 args

  it "should handle custom date" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "'a'" "'a'"
      now <- return datetime.datetime.now() -- TODO: handle replace
      (rev0, node0) <- C.commit client (mkTestCommitOptions "'first'" { C.commitAddRemove = True } { C.commitDate = Just now.isoformat("' '").encode("'latin-1'") })
      now `shouldBe` self.client.tip().date

  it "should amend previous commit" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "'a'" "'a'"
      now <- return datetime.datetime.now() -- TODO: handle replace
      (rev0, node0) <- C.commit client (mkTestCommitOptions "'first'" { C.commitAddRemove = True } { C.commitDate = Just now.isoformat("' '").encode("'latin-1'") })
      now `shouldBe` self.client.tip().date
      commonAppendFile "'a'" "'a'"
      (rev1, node1) <- C.commit client (mkTestCommitOptions "default" { C.commitAmend = True })
      now `shouldBe` self.client.tip().date
      node0 `shouldNotBe` node1
      1 `shouldBe` length self.client.log()

  it "should prevent null injection" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "'a'" "'a'"
      -- TODO: assertRaises with 2 args
      0 `shouldBe` length self.client.log()
