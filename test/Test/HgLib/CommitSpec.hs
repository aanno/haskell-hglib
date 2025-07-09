{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.CommitSpec (spec) where

import Control.Exception (try, SomeException)
import Data.Text (Text)
import HgLib.Types
import Test.HgLib.Common
import Test.Hspec
import [Ident {ident_string = "datetime", ident_annot = SpanCoLinear {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_commit.py", span_row = 2, span_start_column = 15, span_end_column = 22}}]
import [Ident {ident_string = "hglib", ident_annot = SpanCoLinear {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_commit.py", span_row = 2, span_start_column = 8, span_end_column = 12}}]
import qualified Data.Text as T
import qualified HgLib.Commands as C

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "Commit" $ do

-- Conversion notes:
-- TODO: Unhandled expression: Subscript {subscriptee = Call {call_fun = Dot {dot
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Subscript {subscriptee = Call {call_fun = Dot {dot
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: List {list_exprs = [Var {var_ident = Ident {ident_
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should handle commit with custom user" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      (rev, node) <- C.commit client b "first" addremove=True user=b "foo"
      rev <- -- TODO: Subscript {subscriptee = Call {call_fun = Dot {dot
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should fail with empty user" $
    withTestRepo $ \bt ->
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      pendingWith "Test not implemented yet"

  it "should close branch" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      (rev0, node0) <- C.commit client b "first" addremove=True
      C.branch client b "foo"
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      (rev1, node1) <- C.commit client b "second"
      revclose <- C.commit client b "closing foo" closebranch=True
      (rev0, rev1, revclose) <- C.log_ client -- TODO: List {list_exprs = [Var {var_ident = Ident {ident_
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should handle message and logfile conflicts" $
    withTestRepo $ \bt ->
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      pendingWith "Test not implemented yet"

  it "should handle custom date" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      now <- -- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun = 
      (rev0, node0) <- C.commit client b "first" addremove=True date=-- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should amend previous commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      now <- -- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun = 
      (rev0, node0) <- C.commit client b "first" addremove=True date=-- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      (rev1, node1) <- C.commit client amend=True
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should prevent null injection" $
    withTestRepo $ \bt ->
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      pendingWith "Test not implemented yet"


-- TODOS:
-- Unhandled expression: Subscript {subscriptee = Call {call_fun = Dot {dot
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Subscript {subscriptee = Call {call_fun = Dot {dot
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: List {list_exprs = [Var {var_ident = Ident {ident_
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
