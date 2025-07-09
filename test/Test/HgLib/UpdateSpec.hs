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
-- TODO: With statement conversion
-- TODO: Unhandled method call: old.encode
-- TODO: Unhandled binary operator: Plus {op_annot = SpanPoint {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_update.py", span_row = 39, span_column = 24}}
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

  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client "first" -- TODO: options addremove=True
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client "second"
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
      -- Setup: (_, _) <- C.commit client "first" -- TODO: options addremove=True
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client "second"
      C.update client -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri
      -- TODO: append
      (u, m, r, ur) <- C.update client 
      u `shouldBe` 0
      m `shouldBe` 0
      r `shouldBe` 0
      ur `shouldBe` 1
      elem ("M", "a") C.status client  `shouldBe` True

  it "should merge" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client "first" -- TODO: options addremove=True
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client "second"
      -- TODO: append
      (rev2, node2) <- C.commit client "third"
      -- TODO: append
      C.commit client "fourth"
      C.update client rev2
      -- TODO: with statement
      f <- -- TODO: withFile "a" -- TODO: unknown mode "wb" $ \h ->
      hPutStrLn f "a" -- TODO: Plus {op_annot = SpanPoint {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_update.py", span_row = 39, span_column = 24}} -- TODO: old.encode
      -- TODO: close f (handled by withFile)
      (u, m, r, ur) <- C.update client 
      u `shouldBe` 0
      m `shouldBe` 1
      r `shouldBe` 0
      ur `shouldBe` 0
      C.status client  `shouldBe` [("M", "a")]

  it "should tip" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client "first" -- TODO: options addremove=True
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client "second"
      C.update client -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri
      (u, m, r, ur) <- C.update client 
      u `shouldBe` 1
      -- TODO: Dot {dot_expr = Subscript {subscriptee = Call {cal `shouldBe` -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri
      C.update client -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri
      -- TODO: append
      (rev2, node2) <- C.commit client "new head"
      C.update client -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri
      C.update client 
      -- TODO: Dot {dot_expr = Subscript {subscriptee = Call {cal `shouldBe` node2

  it "should check_clean" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client "first" -- TODO: options addremove=True
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client "second"
      -- TODO: complex assertRaises
      pendingWith "Test not implemented yet"

  it "should clean" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client "first" -- TODO: options addremove=True
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client "second"
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
      -- Setup: (_, _) <- C.commit client "first" -- TODO: options addremove=True
      -- Setup: -- TODO: append
      -- Setup: (_, _) <- C.commit client "second"
      f <- -- TODO: withFile ".hg/hgrc" AppendMode $ \h ->
      hPutStrLn f "[defaults]\nupdate=-v\n"
      -- TODO: close f (handled by withFile)
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
-- With statement conversion
-- Unhandled method call: old.encode
-- Unhandled binary operator: Plus {op_annot = SpanPoint {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_update.py", span_row = 39, span_column = 24}}
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
