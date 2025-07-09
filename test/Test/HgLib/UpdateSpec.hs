{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.UpdateSpec (spec) where

import Control.Exception (try, SomeException)
import Data.Text (Text)
import HgLib.Types
import Test.HgLib.Common
import Test.Hspec
import [Ident {ident_string = "os", ident_annot = SpanCoLinear {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_update.py", span_row = 86, span_start_column = 16, span_end_column = 17}}]
import qualified Data.Text as T
import qualified HgLib.Commands as C

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "Update" $ do

-- Conversion notes:
-- TODO: Unhandled module call: common.basetest.setUp
-- TODO: With statement conversion
-- TODO: Unhandled method call: old.encode
-- TODO: Complex assertion: assertRaises with 4 args
-- TODO: With statement conversion
-- TODO: Complex assertion: assertRaises with 3 args
-- TODO: With statement conversion
-- TODO: Unhandled method call: self.test_basic

  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup:
      -- TODO: common.basetest.setUp
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client "second"
      (u, m, r, ur) <- C.update client rev0
      u `shouldBe` 1
      m `shouldBe` 0
      r `shouldBe` 0
      ur `shouldBe` 0

  it "should unresolved" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup:
      -- TODO: common.basetest.setUp
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client "second"
      C.update client rev0
      commonAppendFile "a" "b"
      (u, m, r, ur) <- C.update client 
      u `shouldBe` 0
      m `shouldBe` 0
      r `shouldBe` 0
      ur `shouldBe` 1
      elem ("M", "a") C.status client  `shouldBe` True

  it "should merge" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup:
      -- TODO: common.basetest.setUp
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client "second"
      commonAppendFile "a" "\n\n\n\nb"
      (rev2, node2) <- C.commit client "third"
      commonAppendFile "a" "b"
      C.commit client "fourth"
      C.update client rev2
      -- TODO: with statement
      f <- -- TODO: withFile "a" -- TODO: unknown mode "wb" $ \h ->
      hPutStrLn f "a" ++ -- TODO: old.encode
      -- TODO: close f (handled by withFile)
      (u, m, r, ur) <- C.update client 
      u `shouldBe` 0
      m `shouldBe` 1
      r `shouldBe` 0
      ur `shouldBe` 0
      C.status client  `shouldBe` [("M", "a")]

  it "should tip" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup:
      -- TODO: common.basetest.setUp
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client "second"
      C.update client rev0
      (u, m, r, ur) <- C.update client 
      u `shouldBe` 1
      revNode (head C.parents client ) `shouldBe` node1
      C.update client rev0
      commonAppendFile "a" "b"
      (rev2, node2) <- C.commit client "new head"
      C.update client rev0
      C.update client 
      revNode (head C.parents client ) `shouldBe` node2

  it "should check_clean" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup:
      -- TODO: common.basetest.setUp
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client "second"
      -- TODO: complex assertRaises

  it "should clean" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup:
      -- TODO: common.basetest.setUp
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client "second"
      -- TODO: with statement
      commonAppendFile "a" "b"
      -- TODO: complex assertRaises
      (u, m, r, ur) <- C.update client  -- TODO: options clean=True
      u `shouldBe` 1
      -- TODO: with statement

  it "should basic_plain" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup:
      -- TODO: common.basetest.setUp
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client "second"
      f <- -- TODO: withFile ".hg/hgrc" AppendMode $ \h ->
      hPutStrLn f "[defaults]\nupdate=-v\n"
      -- TODO: close f (handled by withFile)
      -- TODO: self.test_basic


-- TODOS:
-- Unhandled module call: common.basetest.setUp
-- With statement conversion
-- Unhandled method call: old.encode
-- Complex assertion: assertRaises with 4 args
-- With statement conversion
-- Complex assertion: assertRaises with 3 args
-- With statement conversion
-- Unhandled method call: self.test_basic
