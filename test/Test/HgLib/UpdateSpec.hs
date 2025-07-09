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
-- WARNING: Unknown assertion method: append
-- WARNING: Non-variable in assignment target
-- WARNING: Non-variable in assignment target
-- WARNING: Unknown assertion method: append
-- WARNING: Non-variable in assignment target
-- WARNING: Non-variable in assignment target
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: test_basic
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: BinaryOp {operator = In {op_annot = SpanCoLinear {
-- TODO: With statement conversion
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Call {cal
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Call {cal
-- TODO: Complex assertion: assertRaises with 4 args
-- TODO: With statement conversion
-- TODO: Complex assertion: assertRaises with 3 args
-- TODO: With statement conversion
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client b "first" -- TODO: options addremove=True
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client b "second"
      (u, m, r, ur) <- C.update client -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri
      u `shouldBe` 1
      m `shouldBe` 0
      r `shouldBe` 0
      ur `shouldBe` 0

  it "should unresolved" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client b "first" -- TODO: options addremove=True
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client b "second"
      C.update client -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri
      -- TODO: append
      (u, m, r, ur) <- C.update client 
      u `shouldBe` 0
      m `shouldBe` 0
      r `shouldBe` 0
      ur `shouldBe` 1
      -- TODO: BinaryOp {operator = In {op_annot = SpanCoLinear { `shouldBe` True

  it "should merge" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client b "first" -- TODO: options addremove=True
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client b "second"
      -- TODO: append
      (rev2, node2) <- C.commit client b "third"
      -- TODO: append
      C.commit client b "fourth"
      C.update client rev2
      -- TODO: with statement
      f <- open "a" "wb"
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      (u, m, r, ur) <- C.update client 
      u `shouldBe` 0
      m `shouldBe` 1
      r `shouldBe` 0
      ur `shouldBe` 0
      C.status client  `shouldBe` -- TODO: List {list_exprs = [Paren {paren_expr = Tuple {tup

  it "should tip" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client b "first" -- TODO: options addremove=True
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client b "second"
      C.update client -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri
      (u, m, r, ur) <- C.update client 
      u `shouldBe` 1
      -- TODO: Dot {dot_expr = Subscript {subscriptee = Call {cal `shouldBe` -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri
      C.update client -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri
      -- TODO: append
      (rev2, node2) <- C.commit client b "new head"
      C.update client -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri
      C.update client 
      -- TODO: Dot {dot_expr = Subscript {subscriptee = Call {cal `shouldBe` node2

  it "should check_clean" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client b "first" -- TODO: options addremove=True
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client b "second"
      -- TODO: complex assertRaises
      pendingWith "Test not implemented yet"

  it "should clean" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client b "first" -- TODO: options addremove=True
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client b "second"
      -- TODO: with statement
      -- TODO: append
      -- TODO: complex assertRaises
      (u, m, r, ur) <- C.update client  -- TODO: options clean=True
      u `shouldBe` 1
      -- TODO: with statement

  it "should basic_plain" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client b "first" -- TODO: options addremove=True
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client b "second"
      f <- open ".hg/hgrc" "a"
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: test_basic


-- WARNINGS:
-- Unknown assertion method: append
-- Non-variable in assignment target
-- Non-variable in assignment target
-- Unknown assertion method: append
-- Non-variable in assignment target
-- Non-variable in assignment target
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: test_basic
-- TODOS:
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: BinaryOp {operator = In {op_annot = SpanCoLinear {
-- With statement conversion
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Call {cal
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Call {cal
-- Complex assertion: assertRaises with 4 args
-- With statement conversion
-- Complex assertion: assertRaises with 3 args
-- With statement conversion
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
