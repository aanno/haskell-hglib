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
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled module attribute: rev.author
-- TODO: Complex assertion: assertRaises with 4 args
-- TODO: Unhandled module attribute: rev0.branch
-- TODO: Unhandled module attribute: rev0.rev
-- TODO: Unhandled expression: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden
-- TODO: Unhandled module attribute: revclose.branch
-- TODO: Unhandled module attribute: revclose.rev
-- TODO: Unhandled expression: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden
-- TODO: Unhandled module attribute: rev0.branch
-- TODO: Unhandled module attribute: rev0.rev
-- TODO: Unhandled expression: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden
-- TODO: Complex assertion: assertRaises with 4 args
-- TODO: Unhandled module attribute: self.client.commit
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- TODO: Unhandled expression: Lambda {lambda_args = [], lambda_body = Call {call

  it "should handle commit with custom user" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: appendFile "a" "a"
      (rev, node) <- C.commit client "first" -- TODO: options addremove=True user="foo"
      rev <- head C.log_ client node
      -- TODO: rev.author `shouldBe` "foo"

  it "should fail with empty user" $
    withTestRepo $ \bt ->
      let client = btClient bt
      -- TODO: appendFile "a" "a"
      -- TODO: complex assertRaises
      pendingWith "Test not implemented yet"

  it "should close branch" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: appendFile "a" "a"
      (rev0, node0) <- C.commit client "first" -- TODO: options addremove=True
      C.branch client "foo"
      -- TODO: appendFile "a" "a"
      (rev1, node1) <- C.commit client "second"
      revclose <- C.commit client "closing foo" -- TODO: options closebranch=True
      (rev0, rev1, revclose) <- C.log_ client [node0, node1, (C.commit client "closing foo" -- TODO: options closebranch=True !! 1)]
      C.branches client  `shouldBe` [(-- TODO: rev0.branch, int -- TODO: rev0.rev, -- TODO: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden)]
      C.branches client  -- TODO: options closed=True `shouldBe` [(-- TODO: revclose.branch, int -- TODO: revclose.rev, -- TODO: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden), (-- TODO: rev0.branch, int -- TODO: rev0.rev, -- TODO: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden)]

  it "should handle message and logfile conflicts" $
    withTestRepo $ \bt ->
      let client = btClient bt
      -- TODO: complex assertRaises
      -- TODO: self.client.commit `shouldThrow` anyException
      pendingWith "Test not implemented yet"

  it "should handle custom date" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: appendFile "a" "a"
      now <- -- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun = 
      (rev0, node0) <- C.commit client "first" -- TODO: options addremove=True date=-- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun = 
      -- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun =  `shouldBe` revDate (C.tip client )

  it "should amend previous commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: appendFile "a" "a"
      now <- -- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun = 
      (rev0, node0) <- C.commit client "first" -- TODO: options addremove=True date=-- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun = 
      -- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun =  `shouldBe` revDate (C.tip client )
      -- TODO: appendFile "a" "a"
      (rev1, node1) <- C.commit client  -- TODO: options amend=True
      -- TODO: Call {call_fun = Dot {dot_expr = Call {call_fun =  `shouldBe` revDate (C.tip client )
      node0 `shouldNotBe` node1
      1 `shouldBe` len C.log_ client 

  it "should prevent null injection" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: appendFile "a" "a"
      -- TODO: Lambda {lambda_args = [], lambda_body = Call {call `shouldThrow` anyException
      0 `shouldBe` len C.log_ client 


-- TODOS:
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled module attribute: rev.author
-- Complex assertion: assertRaises with 4 args
-- Unhandled module attribute: rev0.branch
-- Unhandled module attribute: rev0.rev
-- Unhandled expression: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden
-- Unhandled module attribute: revclose.branch
-- Unhandled module attribute: revclose.rev
-- Unhandled expression: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden
-- Unhandled module attribute: rev0.branch
-- Unhandled module attribute: rev0.rev
-- Unhandled expression: SlicedExpr {slicee = Dot {dot_expr = Var {var_iden
-- Complex assertion: assertRaises with 4 args
-- Unhandled module attribute: self.client.commit
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Call {call_fun = 
-- Unhandled expression: Lambda {lambda_args = [], lambda_body = Call {call
