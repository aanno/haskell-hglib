{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.StatusSpec (spec) where

import Control.Exception (try, SomeException)
import Data.Text (Text)
import HgLib.Types
import Test.HgLib.Common
import Test.Hspec
import [Ident {ident_string = "os", ident_annot = SpanCoLinear {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_status.py", span_row = 1, span_start_column = 8, span_end_column = 9}}]
import qualified Data.Text as T
import qualified HgLib.Commands as C

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "Status" $ do

-- Conversion notes:
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- TODO: Unhandled method call: os.remove
-- TODO: Unhandled statement: For {for_targets = [Var {var_ident = Ident {ident_

  it "should handle empty repository" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      C.status client  `shouldBe` []

  it "should one_of_each" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      -- TODO: append
      -- TODO: append
      -- TODO: append
      -- TODO: append
      -- TODO: append
      C.commit client "first" -- TODO: options addremove=True
      -- TODO: append
      -- TODO: append
      C.add client ["added"]
      -- TODO: os.remove
      C.remove client ["removed"]
      -- TODO: append
      l <- [("M", "modified"), ("A", "added"), ("R", "removed"), ("C", ".hgignore"), ("C", "clean"), ("!", "missing"), ("?", "untracked"), ("I", "ignored")]
      st <- C.status client  -- TODO: options all=True
      -- TODO: For {for_targets = [Var {var_ident = Ident {ident_

  it "should copy" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      C.commit client "first" -- TODO: options addremove=True
      C.copy client "source" "dest"
      l <- [("A", "dest"), (" ", "source")]
      C.status client  -- TODO: options copies=True `shouldBe` [("A", "dest"), (" ", "source")]

  it "should copy_origin_space" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      C.commit client "first" -- TODO: options addremove=True
      C.copy client "s ource" "dest"
      l <- [("A", "dest"), (" ", "s ource")]
      C.status client  -- TODO: options copies=True `shouldBe` [("A", "dest"), (" ", "s ource")]


-- WARNINGS:
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- TODOS:
-- Unhandled method call: os.remove
-- Unhandled statement: For {for_targets = [Var {var_ident = Ident {ident_
