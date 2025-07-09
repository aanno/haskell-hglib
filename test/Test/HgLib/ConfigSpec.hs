{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.ConfigSpec (spec) where

import Control.Exception (try, SomeException)
import Data.Text (Text)
import HgLib.Types
import Test.HgLib.Common
import Test.Hspec
import [Ident {ident_string = "hglib", ident_annot = SpanCoLinear {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_config.py", span_row = 2, span_start_column = 12, span_end_column = 16}}]
import [Ident {ident_string = "os", ident_annot = SpanCoLinear {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_config.py", span_row = 2, span_start_column = 8, span_end_column = 9}}]
import qualified Data.Text as T
import qualified HgLib.Commands as C

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "Config" $ do

-- Conversion notes:
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Complex assignment pattern
-- TODO: Unhandled expression: BinaryOp {operator = In {op_annot = SpanCoLinear {
-- TODO: Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- TODO: Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- TODO: Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- TODO: Complex assertion: assertRaises with 3 args
-- TODO: Unhandled expression: BinaryOp {operator = In {op_annot = SpanCoLinear {

  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
      -- Setup: f <- open ".hg/hgrc" "a"
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- Setup: -- TODO: complex assignment
      config <- C.config client 
      -- TODO: BinaryOp {operator = In {op_annot = SpanCoLinear { `shouldBe` True
      -- TODO: List {list_exprs = [Paren {paren_expr = Tuple {tup `shouldBe True` C.config client b "section"
      -- TODO: List {list_exprs = [Paren {paren_expr = Tuple {tup `shouldBe True` C.config client -- TODO: List {list_exprs = [Call {call_fun = Var {var_iden
      -- TODO: complex assertRaises

  it "should show_source" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
      -- Setup: f <- open ".hg/hgrc" "a"
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- Setup: -- TODO: complex assignment
      config <- C.config client  -- TODO: options showsource=True
      -- TODO: BinaryOp {operator = In {op_annot = SpanCoLinear { `shouldBe` True


-- TODOS:
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Complex assignment pattern
-- Unhandled expression: BinaryOp {operator = In {op_annot = SpanCoLinear {
-- Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- Complex assertion: assertRaises with 3 args
-- Unhandled expression: BinaryOp {operator = In {op_annot = SpanCoLinear {
