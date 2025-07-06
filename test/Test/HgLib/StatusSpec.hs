{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.StatusSpec (spec) where

import Test.Hspec
import Test.HgLib.Common
import qualified HgLib.Commands as C
import HgLib.Types (SummaryInfo(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Control.Exception (try, SomeException)

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "Status" $ do
  it "should handle empty repository" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: complex assertEqual - -- TODO: client method status() should equal []
      pendingWith "Test not implemented yet"

  it "test_one_of_each" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "'.hgignore'" "'ignored'"
      commonAppendFile "'ignored'" "'a'"
      commonAppendFile "'clean'" "'a'"
      commonAppendFile "'modified'" "'a'"
      commonAppendFile "'removed'" "'a'"
      commonAppendFile "'missing'" "'a'"
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      commonAppendFile "'modified'" "'a'"
      commonAppendFile "'added'" "'a'"
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {i
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {i
      -- TODO: Assign {assign_to = [Var {var_ident = Ident {ident_string = "l", ident_annot = S
      -- TODO: Assign {assign_to = [Var {var_ident = Ident {ident_string = "st", ident_annot = 
      -- TODO: For {for_targets = [Var {var_ident = Ident {ident_string = "i", ident_annot = Sp

  it "test_copy" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "'source'" "'a'"
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      -- TODO: Assign {assign_to = [Var {var_ident = Ident {ident_string = "l", ident_annot = S
      -- TODO: complex assertEqual - -- TODO: client method status(copies=True) should equal l

  it "test_copy_origin_space" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "'s ource'" "'a'"
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      -- TODO: Assign {assign_to = [Var {var_ident = Ident {ident_string = "l", ident_annot = S
      -- TODO: complex assertEqual - -- TODO: client method status(copies=True) should equal l


