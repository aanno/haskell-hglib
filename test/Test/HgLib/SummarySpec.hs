{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.SummarySpec (spec) where

import Test.Hspec
import Test.HgLib.Common
import qualified HgLib.Commands as C
import HgLib.Types (SummaryInfo(..), Revision(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Control.Exception (try, SomeException)

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "Summary" $ do
  it "should handle empty repository" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Dictionary assignment for d omitted
      -- TODO: complex assertEqual
      pendingWith "Test not implemented yet"

  it "should handle basic repository with one commit" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev, node) <- C.commit client (mkTestCommitOptions "first" { C.commitAddRemove = True })
      -- Dictionary assignment for d omitted
      -- TODO: if statement with condition: BinaryOp {operator = GreaterThanEquals {op_annot =
      -- TODO: complex assertEqual
      return ()

  it "should detect dirty working directory" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev, node) <- C.commit client (mkTestCommitOptions "first" { C.commitAddRemove = True })
      commonAppendFile "a" "a"
      -- Dictionary assignment for d omitted
      -- TODO: if statement with condition: BinaryOp {operator = GreaterThanEquals {op_annot =
      -- TODO: complex assertEqual

  it "should handle secret commit clean" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: if statement with condition: BinaryOp {operator = LessThan {op_annot = SpanPoin
      commonAppendFile "a" "a"
      (rev, node) <- C.commit client (mkTestCommitOptions "first" { C.commitAddRemove = True })
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      e <- C.summary client []
      e !! "commit" `shouldBe` True

  it "should handle update" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev, node) <- C.commit client (mkTestCommitOptions "first" { C.commitAddRemove = True })
      commonAppendFile "a" "a"
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      -- Dictionary assignment for d omitted
      -- TODO: if statement with condition: BinaryOp {operator = GreaterThanEquals {op_annot =
      -- TODO: complex assertEqual

  it "should handle remote" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev, node) <- C.commit client (mkTestCommitOptions "first" { C.commitAddRemove = True })
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      -- TODO: Assign {assign_to = [Var {var_ident = Ident {ident_string = "other", ident_annot
      -- Dictionary assignment for d omitted
      -- TODO: complex assertEqual
      commonAppendFile "a" "a"
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      -- TODO: Assign {assign_to = [Subscript {subscriptee = Var {var_ident = Ident {ident_stri
      -- TODO: complex assertEqual
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      -- TODO: Assign {assign_to = [Subscript {subscriptee = Var {var_ident = Ident {ident_stri
      -- TODO: complex assertEqual
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {i
      -- TODO: Assign {assign_to = [Subscript {subscriptee = Var {var_ident = Ident {ident_stri
      -- TODO: if statement with condition: BinaryOp {operator = LessThan {op_annot = SpanPoin
      -- TODO: complex assertEqual
      commonAppendFile "other/a" "a"
      -- TODO: Assign {assign_to = [Tuple {tuple_exprs = [Var {var_ident = Ident {ident_string 
      -- TODO: Assign {assign_to = [Subscript {subscriptee = Var {var_ident = Ident {ident_stri
      -- TODO: if statement with condition: BinaryOp {operator = LessThan {op_annot = SpanPoin
      -- TODO: Assign {assign_to = [Subscript {subscriptee = Var {var_ident = Ident {ident_stri
      -- TODO: if statement with condition: BinaryOp {operator = GreaterThanEquals {op_annot =
      -- TODO: complex assertEqual

  it "should handle two parents" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev0, node) <- C.commit client (mkTestCommitOptions "first" { C.commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client mkTestCommitOptions "second"
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      commonAppendFile "b" "a"
      (rev2, node2) <- C.commit client (mkTestCommitOptions "third" { C.commitAddRemove = True })
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      -- Dictionary assignment for d omitted
      -- TODO: if statement with condition: BinaryOp {operator = GreaterThanEquals {op_annot =
      -- TODO: complex assertEqual
      return ()


