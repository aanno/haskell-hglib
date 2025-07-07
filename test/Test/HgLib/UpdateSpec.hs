{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.UpdateSpec (spec) where

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
spec = describe "Update" $ do
  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: statement not implemented (AST: Assign {assign_to = [Tuple {tuple_exprs = [Var {var_ident = ...)
      u `shouldBe` 1
      m `shouldBe` 0
      r `shouldBe` 0
      ur `shouldBe` 0

  it "test_unresolved" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      commonAppendFile "a" "b"
      -- TODO: statement not implemented (AST: Assign {assign_to = [Tuple {tuple_exprs = [Var {var_ident = ...)
      u `shouldBe` 0
      m `shouldBe` 0
      r `shouldBe` 0
      ur `shouldBe` 1
      -- TODO: complex tuple <op> -- TODO: client method status() `shouldBe` True

  it "test_merge" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "\n\n\n\nb"
      (rev2, node2) <- C.commit client (mkTestCommitOptions "third")
      commonAppendFile "a" "b"
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      -- TODO: statement not implemented (AST: With {with_context = [(Call {call_fun = Var {var_ident = Ide...)
      -- TODO: statement not implemented (AST: Assign {assign_to = [Var {var_ident = Ident {ident_string = ...)
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      -- TODO: statement not implemented (AST: Assign {assign_to = [Tuple {tuple_exprs = [Var {var_ident = ...)
      u `shouldBe` 0
      m `shouldBe` 1
      r `shouldBe` 0
      ur `shouldBe` 0
      -- TODO: complex assertEqual (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)

  it "test_tip" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      -- TODO: statement not implemented (AST: Assign {assign_to = [Tuple {tuple_exprs = [Var {var_ident = ...)
      u `shouldBe` 1
      -- TODO: complex assertEqual (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      commonAppendFile "a" "b"
      (rev2, node2) <- C.commit client (mkTestCommitOptions "new head")
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      TE.decodeUtf8 (revNode (undefined {- TODO: subscript -})) `shouldBe` node2

  it "test_check_clean" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      result <- (try :: IO (Int, Text) -> IO (Either SomeException IO (Int, Text))) $ C.update client C.defaultLogOptions -- TODO: UpdateOptions not implemented, got ["clean","check"]
      result `shouldSatisfy` isLeft

  it "test_clean" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: statement not implemented (AST: With {with_context = [(Call {call_fun = Var {var_ident = Ide...)
      commonAppendFile "a" "b"
      result <- (try :: IO (Int, Text) -> IO (Either SomeException IO (Int, Text))) $ C.update client C.defaultLogOptions -- TODO: UpdateOptions not implemented, got ["check"]
      result `shouldSatisfy` isLeft
      -- TODO: statement not implemented (AST: Assign {assign_to = [Tuple {tuple_exprs = [Var {var_ident = ...)
      u `shouldBe` 1
      -- TODO: statement not implemented (AST: With {with_context = [(Call {call_fun = Var {var_ident = Ide...)

  it "test_basic_plain" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: statement not implemented (AST: Assign {assign_to = [Var {var_ident = Ident {ident_string = ...)
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      pendingWith "Test not implemented yet"


