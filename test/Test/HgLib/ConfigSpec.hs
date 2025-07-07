{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.ConfigSpec (spec) where

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
spec = describe "Config" $ do
  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Assign {assign_to = [Var {var_ident = Ident {ident_string = "config", ident_anno
      -- TODO: complex tuple <op> -- TODO: client method config() `shouldBe` True
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {i
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {i
      result <- (try :: IO (Int, Text) -> IO (Either SomeException (Int, Text))) $ C.config client -- TODO: complex list
      result `shouldSatisfy` isLeft

  it "test_show_source" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Assign {assign_to = [Var {var_ident = Ident {ident_string = "config", ident_anno
      -- TODO: complex tuple <op> config `shouldBe` True
      pendingWith "Test not implemented yet"


