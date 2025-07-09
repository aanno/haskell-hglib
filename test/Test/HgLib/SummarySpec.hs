{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.SummarySpec (spec) where

import Control.Exception (try, SomeException)
import Data.Text (Text)
import HgLib.Types
import Test.HgLib.Common
import Test.Hspec
import [Ident {ident_string = "hglib", ident_annot = SpanCoLinear {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_summary.py", span_row = 3, span_start_column = 8, span_end_column = 12}}]
import [Ident {ident_string = "unittest", ident_annot = SpanCoLinear {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_summary.py", span_row = 1, span_start_column = 8, span_end_column = 15}}]
import qualified Data.Text as T
import qualified HgLib.Commands as C

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "Summary" $ do

-- Conversion notes:
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown client method: phase
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown client method: clone
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown client method: bookmark
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown assertion method: append
-- WARNING: Unknown client method: merge
-- TODO: Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- TODO: Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- TODO: Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- TODO: Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- TODO: Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- TODO: Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- TODO: Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- TODO: If statement conversion
-- TODO: Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- TODO: If statement conversion
-- TODO: If statement conversion
-- TODO: Unhandled expression: Subscript {subscriptee = Var {var_ident = Ident {i
-- TODO: Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- TODO: If statement conversion
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Int {int
-- TODO: Complex assignment pattern
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Int {int
-- TODO: Complex assignment pattern
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Int {int
-- TODO: Complex assignment pattern
-- TODO: If statement conversion
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Int {int
-- TODO: Complex assignment pattern
-- TODO: If statement conversion
-- TODO: Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- TODO: Complex assignment pattern
-- TODO: If statement conversion
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- TODO: If statement conversion

  it "should handle empty repository" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      d <- -- TODO: Dictionary {dict_mappings = [DictMappingPair (Call
      C.summary client  `shouldBe` -- TODO: Dictionary {dict_mappings = [DictMappingPair (Call

  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      (rev, node) <- C.commit client b "first" -- TODO: options addremove=True
      d <- -- TODO: Dictionary {dict_mappings = [DictMappingPair (Call
      -- TODO: if statement
      C.summary client  `shouldBe` -- TODO: Dictionary {dict_mappings = [DictMappingPair (Call

  it "should detect dirty working directory" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      (rev, node) <- C.commit client b "first" -- TODO: options addremove=True
      -- TODO: append
      d <- -- TODO: Dictionary {dict_mappings = [DictMappingPair (Call
      -- TODO: if statement
      C.summary client  `shouldBe` -- TODO: Dictionary {dict_mappings = [DictMappingPair (Call

  it "should handle secret commit clean" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: if statement
      -- TODO: append
      (rev, node) <- C.commit client b "first" -- TODO: options addremove=True
      -- TODO: client.phase
      e <- C.summary client 
      -- TODO: Subscript {subscriptee = Var {var_ident = Ident {i `shouldBe` True

  it "should handle update" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      (rev, node) <- C.commit client b "first" -- TODO: options addremove=True
      -- TODO: append
      C.commit client b "second"
      C.update client 0
      d <- -- TODO: Dictionary {dict_mappings = [DictMappingPair (Call
      -- TODO: if statement
      C.summary client  `shouldBe` -- TODO: Dictionary {dict_mappings = [DictMappingPair (Call

  it "should handle remote" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      (rev, node) <- C.commit client b "first" -- TODO: options addremove=True
      -- TODO: client.clone
      other <- -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      d <- -- TODO: Dictionary {dict_mappings = [DictMappingPair (Call
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident =  `shouldBe` -- TODO: Dictionary {dict_mappings = [DictMappingPair (Call
      -- TODO: append
      C.commit client b "second"
      -- TODO: complex assignment
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident =  `shouldBe` -- TODO: Dictionary {dict_mappings = [DictMappingPair (Call
      -- TODO: client.bookmark
      -- TODO: complex assignment
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident =  `shouldBe` -- TODO: Dictionary {dict_mappings = [DictMappingPair (Call
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: complex assignment
      -- TODO: if statement
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident =  `shouldBe` -- TODO: Dictionary {dict_mappings = [DictMappingPair (Call
      -- TODO: append
      (rev, node) <- -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: complex assignment
      -- TODO: if statement
      -- TODO: complex assignment
      -- TODO: if statement
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident =  `shouldBe` -- TODO: Dictionary {dict_mappings = [DictMappingPair (Call

  it "should handle two parents" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: append
      (rev0, node) <- C.commit client b "first" -- TODO: options addremove=True
      -- TODO: append
      (rev1, node1) <- C.commit client b "second"
      C.update client rev0
      -- TODO: append
      (rev2, node2) <- C.commit client b "third" -- TODO: options addremove=True
      -- TODO: client.merge
      d <- -- TODO: Dictionary {dict_mappings = [DictMappingPair (Call
      -- TODO: if statement
      C.summary client  `shouldBe` -- TODO: Dictionary {dict_mappings = [DictMappingPair (Call


-- WARNINGS:
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown client method: phase
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown client method: clone
-- Unknown assertion method: append
-- Unknown client method: bookmark
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown assertion method: append
-- Unknown client method: merge
-- TODOS:
-- Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- If statement conversion
-- Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- If statement conversion
-- If statement conversion
-- Unhandled expression: Subscript {subscriptee = Var {var_ident = Ident {i
-- Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- If statement conversion
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Int {int
-- Complex assignment pattern
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Int {int
-- Complex assignment pattern
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Int {int
-- Complex assignment pattern
-- If statement conversion
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Paren {paren_expr = Tuple {tuple_exprs = [Int {int
-- Complex assignment pattern
-- If statement conversion
-- Unhandled expression: List {list_exprs = [Paren {paren_expr = Tuple {tup
-- Complex assignment pattern
-- If statement conversion
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Dictionary {dict_mappings = [DictMappingPair (Call
-- If statement conversion
