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
      config <- C.config client [] []
      -- TODO: complex tuple <op> C.config client [] [] `shouldBe` True
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      -- TODO: statement not implemented (AST: StmtExpr {stmt_expr = Call {call_fun = Dot {dot_expr = Var {...)
      result <- (try :: IO [(Text, Text, Text)] -> IO (Either SomeException IO [(Text, Text, Text)])) $ C.config client -- TODO: complex list
      result `shouldSatisfy` isLeft

  it "test_show_source" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      config <- C.config client [] []
      -- TODO: complex tuple <op> config `shouldBe` True
      return ()


