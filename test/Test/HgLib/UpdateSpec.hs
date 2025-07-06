{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.UpdateSpec (spec) where

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
spec = describe "Update" $ do
  it "should handle basic repository with one commit" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Assign {assign_to = [Tuple {tuple_exprs = [Var {var_ident = Ident {ident_string 
      u `shouldBe` 1
      m `shouldBe` 0
      r `shouldBe` 0
      ur `shouldBe` 0

  it "test_unresolved" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      commonAppendFile "a" "b"
      -- TODO: Assign {assign_to = [Tuple {tuple_exprs = [Var {var_ident = Ident {ident_string 
      u `shouldBe` 0
      m `shouldBe` 0
      r `shouldBe` 0
      ur `shouldBe` 1
      -- TODO: complex tuple <op> -- TODO: client method status() `shouldBe` True

  it "test_merge" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "\n\n\n\nb"
      (rev2, node2) <- C.commit client mkTestCommitOptions "third"
      commonAppendFile "a" "b"
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      -- TODO: With {with_context = [(Call {call_fun = Var {var_ident = Ident {ident_string = "
      -- TODO: Assign {assign_to = [Var {var_ident = Ident {ident_string = "f", ident_annot = S
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {i
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {i
      -- TODO: Assign {assign_to = [Tuple {tuple_exprs = [Var {var_ident = Ident {ident_string 
      u `shouldBe` 0
      m `shouldBe` 1
      r `shouldBe` 0
      ur `shouldBe` 0
      -- TODO: complex assertEqual

  it "test_tip" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      -- TODO: Assign {assign_to = [Tuple {tuple_exprs = [Var {var_ident = Ident {ident_string 
      u `shouldBe` 1
      -- TODO: complex assertEqual
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      commonAppendFile "a" "b"
      (rev2, node2) <- C.commit client mkTestCommitOptions "new head"
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      revNode (-- TODO: subscript) `shouldBe` node2

  it "test_check_clean" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      result <- (try :: IO a -> IO (Either SomeException a)) $ C.update client (C.defaultUpdateOptions { C.updateClean = True } { C.updateCheck = True })
      result `shouldSatisfy` isLeft

  it "test_clean" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: With {with_context = [(Call {call_fun = Var {var_ident = Ident {ident_string = "
      commonAppendFile "a" "b"
      result <- (try :: IO a -> IO (Either SomeException a)) $ C.update client (C.defaultUpdateOptions { C.updateCheck = True })
      result `shouldSatisfy` isLeft
      -- TODO: Assign {assign_to = [Tuple {tuple_exprs = [Var {var_ident = Ident {ident_string 
      u `shouldBe` 1
      -- TODO: With {with_context = [(Call {call_fun = Var {var_ident = Ident {ident_string = "

  it "test_basic_plain" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Assign {assign_to = [Var {var_ident = Ident {ident_string = "f", ident_annot = S
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {i
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {i
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {i
      pendingWith "Test not implemented yet"


