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
-- WARNING: Unknown client method: add
-- WARNING: Unknown client method: remove
-- WARNING: Unknown client method: copy
-- WARNING: Unknown client method: copy
-- TODO: Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- TODO: Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- TODO: Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- TODO: Unhandled statement: For {for_targets = [Var {var_ident = Ident {ident_
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should handle empty repository" $
    withTestRepo $ \bt ->
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      pendingWith "Test not implemented yet"

  it "should one_of_each" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      C.commit client b "first" addremove=True
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: client.add
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: client.remove
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      l <- -- TODO: List {list_exprs = [Paren {paren_expr = Tuple {tup
      st <- C.status client all=True
      -- TODO: For {for_targets = [Var {var_ident = Ident {ident_

  it "should copy" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      C.commit client b "first" addremove=True
      -- TODO: client.copy
      l <- -- TODO: List {list_exprs = [Paren {paren_expr = Tuple {tup
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should copy_origin_space" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      C.commit client b "first" addremove=True
      -- TODO: client.copy
      l <- -- TODO: List {list_exprs = [Paren {paren_expr = Tuple {tup
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 


-- WARNINGS:
-- Unknown client method: add
-- Unknown client method: remove
-- Unknown client method: copy
-- Unknown client method: copy
-- TODOS:
-- Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- Unhandled statement: For {for_targets = [Var {var_ident = Ident {ident_
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
