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
-- WARNING: Non-variable in assignment target
-- WARNING: Non-variable in assignment target
-- WARNING: Non-variable in assignment target
-- WARNING: Non-variable in assignment target
-- TODO: Unhandled module call: common.basetest.setUp
-- TODO: Unhandled module attribute: self.rev0
-- TODO: Unhandled module attribute: self.rev0
-- TODO: With statement conversion
-- TODO: Unhandled method call: old.encode
-- TODO: Unhandled module attribute: self.rev0
-- TODO: Unhandled module attribute: self.node1
-- TODO: Unhandled module attribute: self.rev0
-- TODO: Unhandled module attribute: self.rev0
-- TODO: Complex assertion: assertRaises with 4 args
-- TODO: With statement conversion
-- TODO: Complex assertion: assertRaises with 3 args
-- TODO: With statement conversion
-- TODO: Unhandled method call: self.test_basic

  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: common.basetest.setUp
      -- Setup: -- TODO: appendFile "a" "a"
      -- Setup: (_, _) <- C.commit client "first" -- TODO: options addremove=True
      -- Setup: -- TODO: appendFile "a" "a"
      -- Setup: (_, _) <- C.commit client "second"
      (u, m, r, ur) <- C.update client -- TODO: self.rev0
      u `shouldBe` 1
      m `shouldBe` 0
      r `shouldBe` 0
      ur `shouldBe` 0

  it "should unresolved" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: common.basetest.setUp
      -- Setup: -- TODO: appendFile "a" "a"
      -- Setup: (_, _) <- C.commit client "first" -- TODO: options addremove=True
      -- Setup: -- TODO: appendFile "a" "a"
      -- Setup: (_, _) <- C.commit client "second"
      C.update client -- TODO: self.rev0
      -- TODO: appendFile "a" "b"
      (u, m, r, ur) <- C.update client 
      u `shouldBe` 0
      m `shouldBe` 0
      r `shouldBe` 0
      ur `shouldBe` 1
      elem ("M", "a") C.status client  `shouldBe` True

  it "should merge" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: common.basetest.setUp
      -- Setup: -- TODO: appendFile "a" "a"
      -- Setup: (_, _) <- C.commit client "first" -- TODO: options addremove=True
      -- Setup: -- TODO: appendFile "a" "a"
      -- Setup: (_, _) <- C.commit client "second"
      -- TODO: appendFile "a" "\n\n\n\nb"
      (rev2, node2) <- C.commit client "third"
      -- TODO: appendFile "a" "b"
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
      -- Setup: -- TODO: common.basetest.setUp
      -- Setup: -- TODO: appendFile "a" "a"
      -- Setup: (_, _) <- C.commit client "first" -- TODO: options addremove=True
      -- Setup: -- TODO: appendFile "a" "a"
      -- Setup: (_, _) <- C.commit client "second"
      C.update client -- TODO: self.rev0
      (u, m, r, ur) <- C.update client 
      u `shouldBe` 1
      revNode (head C.parents client ) `shouldBe` -- TODO: self.node1
      C.update client -- TODO: self.rev0
      -- TODO: appendFile "a" "b"
      (rev2, node2) <- C.commit client "new head"
      C.update client -- TODO: self.rev0
      C.update client 
      revNode (head C.parents client ) `shouldBe` node2

  it "should check_clean" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: common.basetest.setUp
      -- Setup: -- TODO: appendFile "a" "a"
      -- Setup: (_, _) <- C.commit client "first" -- TODO: options addremove=True
      -- Setup: -- TODO: appendFile "a" "a"
      -- Setup: (_, _) <- C.commit client "second"
      -- TODO: complex assertRaises
      pendingWith "Test not implemented yet"

  it "should clean" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: common.basetest.setUp
      -- Setup: -- TODO: appendFile "a" "a"
      -- Setup: (_, _) <- C.commit client "first" -- TODO: options addremove=True
      -- Setup: -- TODO: appendFile "a" "a"
      -- Setup: (_, _) <- C.commit client "second"
      -- TODO: with statement
      -- TODO: appendFile "a" "b"
      -- TODO: complex assertRaises
      (u, m, r, ur) <- C.update client  -- TODO: options clean=True
      u `shouldBe` 1
      -- TODO: with statement

  it "should basic_plain" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: common.basetest.setUp
      -- Setup: -- TODO: appendFile "a" "a"
      -- Setup: (_, _) <- C.commit client "first" -- TODO: options addremove=True
      -- Setup: -- TODO: appendFile "a" "a"
      -- Setup: (_, _) <- C.commit client "second"
      f <- -- TODO: withFile ".hg/hgrc" AppendMode $ \h ->
      hPutStrLn f "[defaults]\nupdate=-v\n"
      -- TODO: close f (handled by withFile)
      -- TODO: self.test_basic


-- WARNINGS:
-- Non-variable in assignment target
-- Non-variable in assignment target
-- Non-variable in assignment target
-- Non-variable in assignment target
-- TODOS:
-- Unhandled module call: common.basetest.setUp
-- Unhandled module attribute: self.rev0
-- Unhandled module attribute: self.rev0
-- With statement conversion
-- Unhandled method call: old.encode
-- Unhandled module attribute: self.rev0
-- Unhandled module attribute: self.node1
-- Unhandled module attribute: self.rev0
-- Unhandled module attribute: self.rev0
-- Complex assertion: assertRaises with 4 args
-- With statement conversion
-- Complex assertion: assertRaises with 3 args
-- With statement conversion
-- Unhandled method call: self.test_basic
