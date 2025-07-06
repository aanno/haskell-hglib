{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.LogSpec (spec) where

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
spec = describe "Log" $ do
  it "should handle basic repository with one commit" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client (mkTestCommitOptions "first" { C.commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client mkTestCommitOptions "second"
      revs <- C.log_ client [] C.defaultLogOptions
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {i
      length revs == 2 `shouldBe` True
      revNode (revs !! 1) `shouldBe` node1
      revs !! 0 `shouldBe` C.log_ client ["0"] C.defaultLogOptions !! 0
      C.log_ client [] C.defaultLogOptions `shouldBe` C.log_ client -- TODO: log with options C.defaultLogOptions
      C.log_ client [] C.defaultLogOptions `shouldBe` C.log_ client -- TODO: log with options C.defaultLogOptions

  it "test_dash_in_filename" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "-a" "-a"
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      revs <- C.log_ client -- TODO: log with options C.defaultLogOptions
      length revs == 1 `shouldBe` True
      revRev (revs !! 0) `shouldBe` "0"

  it "test_empty_short_option" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "foobar" "foobar"
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Dot {dot_expr = Var {var_
      revs <- C.log_ client -- TODO: log with options C.defaultLogOptions
      length revs == 1 `shouldBe` True
      revRev (revs !! 0) `shouldBe` "0"

  it "test_null_byte" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      -- TODO: With {with_context = [(Call {call_fun = Var {var_ident = Ident {ident_string = "
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {i
      revs <- C.log_ client -- TODO: log with options C.defaultLogOptions
      revDesc (revs !! 0) `shouldBe` "some message\0more stuff"


