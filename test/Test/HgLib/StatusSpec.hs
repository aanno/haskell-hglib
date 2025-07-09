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
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled method call: os.remove
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled statement: For {for_targets = [Var {var_ident = Ident {ident_
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca

  it "should handle empty repository" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      C.status client  `shouldBe` []

  it "should one_of_each" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      -- TODO: append
      -- TODO: append
      -- TODO: append
      -- TODO: append
      -- TODO: append
      C.commit client "first" -- TODO: options addremove=True
      -- TODO: append
      -- TODO: append
      -- TODO: client.add
      -- TODO: os.remove
      -- TODO: client.remove
      -- TODO: append
      l <- [-- TODO: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca, -- TODO: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca, -- TODO: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca, -- TODO: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca, -- TODO: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca, -- TODO: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca, -- TODO: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca, -- TODO: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca]
      st <- C.status client  -- TODO: options all=True
      -- TODO: For {for_targets = [Var {var_ident = Ident {ident_

  it "should copy" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      C.commit client "first" -- TODO: options addremove=True
      -- TODO: client.copy
      l <- [-- TODO: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca, -- TODO: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca]
      C.status client  -- TODO: options copies=True `shouldBe` [-- TODO: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca, -- TODO: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca]

  it "should copy_origin_space" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      C.commit client "first" -- TODO: options addremove=True
      -- TODO: client.copy
      l <- [-- TODO: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca, -- TODO: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca]
      C.status client  -- TODO: options copies=True `shouldBe` [-- TODO: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca, -- TODO: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca]


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
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled method call: os.remove
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled statement: For {for_targets = [Var {var_ident = Ident {ident_
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Call {ca
