{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.ConfigSpec (spec) where

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
spec = describe "Config" $ do
  it "should handle basic repository with one commit" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Assign {assign_to = [Var {var_ident = Ident {ident_string = "config", ident_anno
      ("'section'", "'key'", "'value'") <op> -- TODO: client method config() `shouldBe` True
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {i
      -- TODO: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {var_ident = Ident {i
      result <- (try :: IO a -> IO (Either SomeException a)) $ C.config client ["'a.b'", "'foo'"]
      result `shouldSatisfy` isLeft

  it "test_show_source" $ do
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- TODO: Assign {assign_to = [Var {var_ident = Ident {ident_string = "config", ident_anno
      (-- TODO: method call -- TODO: attr access os.path.abspath(...) <op> "':2'", "'section'", "'key'", "'value'") <op> config `shouldBe` True


