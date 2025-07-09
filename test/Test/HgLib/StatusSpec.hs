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
-- TODO: Unhandled method call: os.remove
-- TODO: Complex self.append() call
-- TODO: Unhandled statement: For {for_targets = [Var {var_ident = Ident {ident_

  it "should handle empty repository" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      C.status client  `shouldBe` []

  it "should one_of_each" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: appendFile ".hgignore" "ignored"
      -- TODO: appendFile "ignored" "a"
      -- TODO: appendFile "clean" "a"
      -- TODO: appendFile "modified" "a"
      -- TODO: appendFile "removed" "a"
      -- TODO: appendFile "missing" "a"
      C.commit client "first" -- TODO: options addremove=True
      -- TODO: appendFile "modified" "a"
      -- TODO: appendFile "added" "a"
      C.add client ["added"]
      -- TODO: os.remove
      C.remove client ["removed"]
      -- TODO: complex self.append() call
      l <- [("M", "modified"), ("A", "added"), ("R", "removed"), ("C", ".hgignore"), ("C", "clean"), ("!", "missing"), ("?", "untracked"), ("I", "ignored")]
      st <- C.status client  -- TODO: options all=True
      -- TODO: For {for_targets = [Var {var_ident = Ident {ident_

  it "should copy" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: appendFile "source" "a"
      C.commit client "first" -- TODO: options addremove=True
      C.copy client "source" "dest"
      l <- [("A", "dest"), (" ", "source")]
      C.status client  -- TODO: options copies=True `shouldBe` [("A", "dest"), (" ", "source")]

  it "should copy_origin_space" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: appendFile "s ource" "a"
      C.commit client "first" -- TODO: options addremove=True
      C.copy client "s ource" "dest"
      l <- [("A", "dest"), (" ", "s ource")]
      C.status client  -- TODO: options copies=True `shouldBe` [("A", "dest"), (" ", "s ource")]


-- TODOS:
-- Unhandled method call: os.remove
-- Complex self.append() call
-- Unhandled statement: For {for_targets = [Var {var_ident = Ident {ident_
