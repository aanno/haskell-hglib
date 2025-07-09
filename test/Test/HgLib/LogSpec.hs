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
-- TODO: Unhandled method call: revs.reverse
-- TODO: Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Var {var_
-- TODO: Unhandled expression: Subscript {subscriptee = Var {var_ident = Ident {i
-- TODO: Unhandled expression: Subscript {subscriptee = Call {call_fun = Dot {dot
-- TODO: Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Var {var_
-- TODO: Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Var {var_
-- TODO: With statement conversion
-- TODO: Unhandled method call: subprocess.check_call
-- TODO: Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Var {var_

  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      (rev0, node0) <- C.commit client "first" -- TODO: options addremove=True
      -- TODO: append
      (rev1, node1) <- C.commit client "second"
      revs <- C.log_ client 
      -- TODO: revs.reverse
      len C.log_ client  == 2 `shouldBe` True
      -- TODO: Dot {dot_expr = Subscript {subscriptee = Var {var_ `shouldBe` node1
      -- TODO: Subscript {subscriptee = Var {var_ident = Ident {i `shouldBe` -- TODO: Subscript {subscriptee = Call {call_fun = Dot {dot
      C.log_ client  `shouldBe` C.log_ client  -- TODO: options files=["a"]
      C.log_ client  `shouldBe` C.log_ client  -- TODO: options hidden=True

  it "should dash_in_filename" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      C.commit client "first" -- TODO: options addremove=True
      revs <- C.log_ client  -- TODO: options files=["-a"]
      len C.log_ client  -- TODO: options files=["-a"] == 1 `shouldBe` True
      -- TODO: Dot {dot_expr = Subscript {subscriptee = Var {var_ `shouldBe` "0"

  it "should empty_short_option" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      C.commit client "first" -- TODO: options addremove=True
      revs <- C.log_ client  -- TODO: options keyword="" files=["foobar"]
      len C.log_ client  -- TODO: options keyword="" files=["foobar"] == 1 `shouldBe` True
      -- TODO: Dot {dot_expr = Subscript {subscriptee = Var {var_ `shouldBe` "0"

  it "should null_byte" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      -- TODO: with statement
      -- TODO: subprocess.check_call
      revs <- C.log_ client  -- TODO: options revrange="."
      -- TODO: Dot {dot_expr = Subscript {subscriptee = Var {var_ `shouldBe` "some message\0more stuff"


-- WARNINGS:
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- TODOS:
-- Unhandled method call: revs.reverse
-- Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Var {var_
-- Unhandled expression: Subscript {subscriptee = Var {var_ident = Ident {i
-- Unhandled expression: Subscript {subscriptee = Call {call_fun = Dot {dot
-- Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Var {var_
-- Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Var {var_
-- With statement conversion
-- Unhandled method call: subprocess.check_call
-- Unhandled expression: Dot {dot_expr = Subscript {subscriptee = Var {var_
