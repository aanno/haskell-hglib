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
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- TODO: Unhandled expression: Subscript {subscriptee = Call {call_fun = Dot {dot
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Subscript {subscriptee = Call {call_fun = Dot {dot
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Complex assertion: assertRaises with 4 args
-- TODO: Unhandled expression: Subscript {subscriptee = Var {var_ident = Ident {i
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden
-- TODO: Complex assertion: assertRaises with 4 args
-- TODO: Unhandled expression: Dot {dot_expr = Dot {dot_expr = Var {var_ident = I
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Dot {dot_expr = Call {call_fun = Dot {dot_expr = D
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Dot {dot_expr = Call {call_fun = Dot {dot_expr = D
-- TODO: Unhandled expression: Dot {dot_expr = Call {call_fun = Dot {dot_expr = D
-- TODO: Unhandled expression: Lambda {lambda_args = [], lambda_body = Call {call

  it "should handle commit with custom user" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      (rev, node) <- C.commit client "first" -- TODO: options addremove=True user="foo"
      rev <- -- TODO: Subscript {subscriptee = Call {call_fun = Dot {dot
      -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri `shouldBe` "foo"

  it "should fail with empty user" $
    withTestRepo $ \bt ->
      let client = btClient bt
      -- TODO: append
      -- TODO: complex assertRaises
      pendingWith "Test not implemented yet"

  it "should close branch" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      (rev0, node0) <- C.commit client "first" -- TODO: options addremove=True
      C.branch client "foo"
      -- TODO: append
      (rev1, node1) <- C.commit client "second"
      revclose <- C.commit client "closing foo" -- TODO: options closebranch=True
      (rev0, rev1, revclose) <- C.log_ client [node0, node1, -- TODO: Subscript {subscriptee = Var {var_ident = Ident {i]
      C.branches client  `shouldBe` [(-- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri, int -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri, -- TODO: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden)]
      C.branches client  -- TODO: options closed=True `shouldBe` [(-- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri, int -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri, -- TODO: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden), (-- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri, int -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri, -- TODO: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden)]

  it "should handle message and logfile conflicts" $
    withTestRepo $ \bt ->
      let client = btClient bt
      -- TODO: complex assertRaises
      -- TODO: Dot {dot_expr = Dot {dot_expr = Var {var_ident = I `shouldThrow` anyException
      pendingWith "Test not implemented yet"

  it "should handle custom date" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      now <- -- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun = 
      (rev0, node0) <- C.commit client "first" -- TODO: options addremove=True date=-- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun = 
      -- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun =  `shouldBe` -- TODO: Dot {dot_expr = Call {call_fun = Dot {dot_expr = D

  it "should amend previous commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      now <- -- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun = 
      (rev0, node0) <- C.commit client "first" -- TODO: options addremove=True date=-- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun = 
      -- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun =  `shouldBe` -- TODO: Dot {dot_expr = Call {call_fun = Dot {dot_expr = D
      -- TODO: append
      (rev1, node1) <- C.commit client  -- TODO: options amend=True
      -- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun =  `shouldBe` -- TODO: Dot {dot_expr = Call {call_fun = Dot {dot_expr = D
      node0 `shouldNotBe` node1
      1 `shouldBe` len C.log_ client 

  it "should prevent null injection" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      -- TODO: Lambda {lambda_args = [], lambda_body = Call {call `shouldThrow` anyException
      0 `shouldBe` len C.log_ client 


-- WARNINGS:
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- TODOS:
-- Unhandled expression: Subscript {subscriptee = Call {call_fun = Dot {dot
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Subscript {subscriptee = Call {call_fun = Dot {dot
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Complex assertion: assertRaises with 4 args
-- Unhandled expression: Subscript {subscriptee = Var {var_ident = Ident {i
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden
-- Complex assertion: assertRaises with 4 args
-- Unhandled expression: Dot {dot_expr = Dot {dot_expr = Var {var_ident = I
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Dot {dot_expr = Call {call_fun = Dot {dot_expr = D
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Dot {dot_expr = Call {call_fun = Dot {dot_expr = D
-- Unhandled expression: Dot {dot_expr = Call {call_fun = Dot {dot_expr = D
-- Unhandled expression: Lambda {lambda_args = [], lambda_body = Call {call
