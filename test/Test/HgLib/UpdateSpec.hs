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
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: With statement conversion
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: With statement conversion
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: With statement conversion
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      (u, m, r, ur) <- C.update client -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should unresolved" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      C.update client -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      (u, m, r, ur) <- C.update client 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should merge" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      (rev2, node2) <- C.commit client b "third"
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      C.commit client b "fourth"
      C.update client rev2
      -- TODO: with statement
      f <- open "a" "wb"
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      (u, m, r, ur) <- C.update client 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should tip" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      C.update client -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri
      (u, m, r, ur) <- C.update client 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      C.update client -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      (rev2, node2) <- C.commit client b "new head"
      C.update client -- TODO: Dot {dot_expr = Var {var_ident = Ident {ident_stri
      C.update client 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 

  it "should check_clean" $
    withTestRepo $ \bt ->
      let client = btClient bt
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      pendingWith "Test not implemented yet"

  it "should clean" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: with statement
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      (u, m, r, ur) <- C.update client clean=True
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: with statement

  it "should basic_plain" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      f <- open ".hg/hgrc" "a"
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 
      -- TODO: Call {call_fun = Dot {dot_expr = Var {var_ident = 


-- TODOS:
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- With statement conversion
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Dot {dot_expr = Var {var_ident = Ident {ident_stri
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- With statement conversion
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- With statement conversion
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Var {var_ident = 
