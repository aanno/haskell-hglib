{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.SummarySpec (spec) where

import Test.Hspec
import Test.HgLib.Common
import qualified HgLib.Commands as C
import HgLib.Types (SummaryInfo(..), Revision(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Control.Exception (try, SomeException)

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "Summary" $ do
  it "should handle empty repository" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Dictionary assignment for d omitted
      -- TODO: complex assertEqual (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      pendingWith "Test not implemented yet"

  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev, node) <- C.commit client (mkTestCommitOptions "first") -- TODO: options addremove
      -- Dictionary assignment for d omitted
      -- TODO: if statement with complex condition
      -- TODO: complex assertEqual (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      return ()

  it "should detect dirty working directory" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev, node) <- C.commit client (mkTestCommitOptions "first") -- TODO: options addremove
      commonAppendFile "a" "a"
      -- Dictionary assignment for d omitted
      -- TODO: if statement with complex condition
      -- TODO: complex assertEqual (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)

  it "should handle secret commit clean" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: if statement with complex condition
      commonAppendFile "a" "a"
      (rev, node) <- C.commit client (mkTestCommitOptions "first") -- TODO: options addremove
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      e <- C.summary client []
      summaryCommitClean e `shouldBe` True

  it "should handle update" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev, node) <- C.commit client (mkTestCommitOptions "first") -- TODO: options addremove
      commonAppendFile "a" "a"
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      -- Dictionary assignment for d omitted
      -- TODO: if statement with complex condition
      -- TODO: complex assertEqual (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)

  it "should handle remote" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev, node) <- C.commit client (mkTestCommitOptions "first") -- TODO: options addremove
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      -- TODO: statement not implemented (AST: Assign {assign_to = [Var {var_ident = Ident {ident_string = ...)
      -- Dictionary assignment for d omitted
      -- TODO: complex assertEqual (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      commonAppendFile "a" "a"
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      -- TODO: statement not implemented (AST: Assign {assign_to = [Subscript {subscriptee = Var {var_ident...)
      -- TODO: complex assertEqual (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      -- TODO: statement not implemented (AST: Assign {assign_to = [Subscript {subscriptee = Var {var_ident...)
      -- TODO: complex assertEqual (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      -- TODO: statement not implemented (AST: Assign {assign_to = [Subscript {subscriptee = Var {var_ident...)
      -- TODO: if statement with complex condition
      -- TODO: complex assertEqual (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      commonAppendFile "other/a" "a"
      -- TODO: statement not implemented (AST: Assign {assign_to = [Tuple {tuple_exprs = [Var {var_ident = ...)
      -- TODO: statement not implemented (AST: Assign {assign_to = [Subscript {subscriptee = Var {var_ident...)
      -- TODO: if statement with complex condition
      -- TODO: statement not implemented (AST: Assign {assign_to = [Subscript {subscriptee = Var {var_ident...)
      -- TODO: if statement with complex condition
      -- TODO: complex assertEqual (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)

  it "should handle two parents" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev0, node) <- C.commit client (mkTestCommitOptions "first") -- TODO: options addremove
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client (mkTestCommitOptions "second")
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      commonAppendFile "b" "a"
      (rev2, node2) <- C.commit client (mkTestCommitOptions "third") -- TODO: options addremove
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      -- Dictionary assignment for d omitted
      -- TODO: if statement with complex condition
      -- TODO: complex assertEqual (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      return ()


