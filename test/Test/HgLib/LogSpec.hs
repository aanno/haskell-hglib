{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.LogSpec (spec) where

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
spec = describe "Log" $ do
  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client (mkUpdateableCommitOptions "first" $ \opts -> opts { C.commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client (mkTestCommitOptions "second")
      revs <- C.log_ client [] C.defaultLogOptions
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      length revs == 2 `shouldBe` True
      TE.decodeUtf8 (revNode ((revs !! 1))) `shouldBe` node1
      -- TODO: complex assertEqual (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      -- TODO: complex assertEqual (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      -- TODO: complex assertEqual (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)

  it "test_dash_in_filename" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "-a" "-a"
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      revs <- C.log_ client [] C.defaultLogOptions -- TODO: with options
      length revs == 1 `shouldBe` True
      show (revRev ((revs !! 0))) `shouldBe` "0"

  it "test_empty_short_option" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "foobar" "foobar"
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {...)
      revs <- C.log_ client [] C.defaultLogOptions -- TODO: with options
      length revs == 1 `shouldBe` True
      show (revRev ((revs !! 0))) `shouldBe` "0"

  it "test_null_byte" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      -- TODO: statement not implemented (AST: With {with_context = [(Call {call_fun = Var {var_ident = Ide...)
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      revs <- C.log_ client [] C.defaultLogOptions -- TODO: with options
      revDesc ((revs !! 0)) `shouldBe` "some message\0more stuff"


