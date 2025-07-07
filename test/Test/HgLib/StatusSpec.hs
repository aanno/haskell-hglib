{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.StatusSpec (spec) where

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
spec = describe "Status" $ do
  it "should handle empty repository" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      do
        statusResult <- C.status client C.defaultStatusOptions
        statusResult `shouldBe` []

  it "test_one_of_each" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile ".hgignore" "ignored"
      commonAppendFile "ignored" "a"
      commonAppendFile "clean" "a"
      commonAppendFile "modified" "a"
      commonAppendFile "removed" "a"
      commonAppendFile "missing" "a"
      _ <- C.commit client (mkTestCommitOptions "first")
      commonAppendFile "modified" "a"
      commonAppendFile "added" "a"
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      -- TODO: statement not implemented (AST: Assign {assign_to = [Var {var_ident = Ident {ident_string = ...)
      -- TODO: statement not implemented (AST: Assign {assign_to = [Var {var_ident = Ident {ident_string = ...)
      -- TODO: statement not implemented (AST: For {for_targets = [Var {var_ident = Ident {ident_string = "...)

  it "test_copy" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "source" "a"
      _ <- C.commit client (mkTestCommitOptions "first")
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      -- TODO: statement not implemented (AST: Assign {assign_to = [Var {var_ident = Ident {ident_string = ...)
      do
        statusResult <- C.status client C.defaultStatusOptions
        statusResult `shouldBe` l

  it "test_copy_origin_space" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "s ource" "a"
      _ <- C.commit client (mkTestCommitOptions "first")
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      -- TODO: statement not implemented (AST: Assign {assign_to = [Var {var_ident = Ident {ident_string = ...)
      do
        statusResult <- C.status client C.defaultStatusOptions
        statusResult `shouldBe` l


