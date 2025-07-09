{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.LogSpec (spec) where

import Control.Exception (try, SomeException)
import Data.Text (Text)
import HgLib.Types
import Test.HgLib.Common
import Test.Hspec
import [Ident {ident_string = "hglib", ident_annot = SpanCoLinear {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_log.py", span_row = 2, span_start_column = 8, span_end_column = 12}}]
import [Ident {ident_string = "subprocess", ident_annot = SpanCoLinear {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_log.py", span_row = 4, span_start_column = 8, span_end_column = 17}}]
import qualified Data.Text as T
import qualified HgLib.Commands as C

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "Log" $ do

-- Conversion notes:
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- TODO: Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- TODO: Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: BinaryOp {operator = Equality {op_annot = SpanCoLi
-- TODO: Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Var {var_
-- TODO: Unhandled expression: Subscript {subscriptee = Var {var_ident = Ident {i
-- TODO: Unhandled expression: Subscript {subscriptee = Call {call_fun = Dot {dot
-- TODO: Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- TODO: Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- TODO: Unhandled expression: BinaryOp {operator = Equality {op_annot = SpanCoLi
-- TODO: Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Var {var_
-- TODO: Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- TODO: Unhandled expression: BinaryOp {operator = Equality {op_annot = SpanCoLi
-- TODO: Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Var {var_
-- TODO: With statement conversion
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Var {var_

  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      (rev0, node0) <- C.commit client b "first" -- TODO: options addremove=True
      -- TODO: append
      (rev1, node1) <- C.commit client b "second"
      revs <- C.log_ client 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: BinaryOp {operator = Equality {op_annot = SpanCoLi `shouldBe` True
      -- TODO: Dot {dot_expr = Subscript {subscriptee = Var {var_ `shouldBe` node1
      -- TODO: Subscript {subscriptee = Var {var_ident = Ident {i `shouldBe` -- TODO: Subscript {subscriptee = Call {call_fun = Dot {dot
      C.log_ client  `shouldBe` C.log_ client  -- TODO: options files=-- TODO: List {list_exprs = [Call {call_fun = Var {var_iden
      C.log_ client  `shouldBe` C.log_ client  -- TODO: options hidden=True

  it "should dash_in_filename" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      C.commit client b "first" -- TODO: options addremove=True
      revs <- C.log_ client  -- TODO: options files=-- TODO: List {list_exprs = [Call {call_fun = Var {var_iden
      -- TODO: BinaryOp {operator = Equality {op_annot = SpanCoLi `shouldBe` True
      -- TODO: Dot {dot_expr = Subscript {subscriptee = Var {var_ `shouldBe` b "0"

  it "should empty_short_option" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      C.commit client b "first" -- TODO: options addremove=True
      revs <- C.log_ client  -- TODO: options keyword=b "" files=-- TODO: List {list_exprs = [Call {call_fun = Var {var_iden
      -- TODO: BinaryOp {operator = Equality {op_annot = SpanCoLi `shouldBe` True
      -- TODO: Dot {dot_expr = Subscript {subscriptee = Var {var_ `shouldBe` b "0"

  it "should null_byte" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      -- TODO: with statement
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      revs <- C.log_ client  -- TODO: options revrange=b "."
      -- TODO: Dot {dot_expr = Subscript {subscriptee = Var {var_ `shouldBe` b "some message\0more stuff"


-- WARNINGS:
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- TODOS:
-- Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: BinaryOp {operator = Equality {op_annot = SpanCoLi
-- Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Var {var_
-- Unhandled expression: Subscript {subscriptee = Var {var_ident = Ident {i
-- Unhandled expression: Subscript {subscriptee = Call {call_fun = Dot {dot
-- Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- Unhandled expression: BinaryOp {operator = Equality {op_annot = SpanCoLi
-- Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Var {var_
-- Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- Unhandled expression: BinaryOp {operator = Equality {op_annot = SpanCoLi
-- Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Var {var_
-- With statement conversion
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Var {var_
