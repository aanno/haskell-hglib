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
-- WARNING: Unknown client method: add
-- WARNING: Unknown client method: remove
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown client method: copy
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown client method: copy
-- TODO: Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- TODO: Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- TODO: Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- TODO: Unhandled expression: List {list_exprs = [], expr_annot = SpanCoLinear {
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- TODO: Unhandled statement: For {for_targets = [Var {var_ident = Ident {ident_
-- TODO: Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- TODO: Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup

  it "should handle empty repository" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      C.status client  `shouldBe` -- TODO: List {list_exprs = [], expr_annot = SpanCoLinear {

  it "should one_of_each" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      -- TODO: append
      -- TODO: append
      -- TODO: append
      -- TODO: append
      -- TODO: append
      C.commit client b "first" -- TODO: options addremove=True
      -- TODO: append
      -- TODO: append
      -- TODO: client.add
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: client.remove
      -- TODO: append
      l <- -- TODO: List {list_exprs = [Paren {paren_expr = Tuple {tup
      st <- C.status client  -- TODO: options all=True
      -- TODO: For {for_targets = [Var {var_ident = Ident {ident_

  it "should copy" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      C.commit client b "first" -- TODO: options addremove=True
      -- TODO: client.copy
      l <- -- TODO: List {list_exprs = [Paren {paren_expr = Tuple {tup
      C.status client  -- TODO: options copies=True `shouldBe` -- TODO: List {list_exprs = [Paren {paren_expr = Tuple {tup

  it "should copy_origin_space" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      C.commit client b "first" -- TODO: options addremove=True
      -- TODO: client.copy
      l <- -- TODO: List {list_exprs = [Paren {paren_expr = Tuple {tup
      C.status client  -- TODO: options copies=True `shouldBe` -- TODO: List {list_exprs = [Paren {paren_expr = Tuple {tup


-- WARNINGS:
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown client method: add
-- Unknown client method: remove
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown client method: copy
-- Unknown assertion method: append
-- Unknown client method: copy
-- TODOS:
-- Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- Unhandled expression: List {list_exprs = [], expr_annot = SpanCoLinear {
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- Unhandled statement: For {for_targets = [Var {var_ident = Ident {ident_
-- Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
