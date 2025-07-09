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
-- TODO: Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- TODO: Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: With statement conversion
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      (rev0, node0) <- C.commit client b "first" addremove=True
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      (rev1, node1) <- C.commit client b "second"
      revs <- C.log_ client 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should dash_in_filename" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      C.commit client b "first" addremove=True
      revs <- C.log_ client files=-- TODO: List {list_exprs = [Call {call_fun = Var {var_iden
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should empty_short_option" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      C.commit client b "first" addremove=True
      revs <- C.log_ client keyword=b "" files=-- TODO: List {list_exprs = [Call {call_fun = Var {var_iden
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should null_byte" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: with statement
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      revs <- C.log_ client revrange=b "."
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 


-- TODOS:
-- Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: List {list_exprs = [Call {call_fun = Var {var_iden
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- With statement conversion
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
