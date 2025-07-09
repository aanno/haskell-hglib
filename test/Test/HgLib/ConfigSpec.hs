{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.ConfigSpec (spec) where

import Control.Exception (try, SomeException)
import Data.Text (Text)
import HgLib.Types
import Test.HgLib.Common
import Test.Hspec
import [Ident {ident_string = "hglib", ident_annot = SpanCoLinear {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_config.py", span_row = 2, span_start_column = 12, span_end_column = 16}}]
import [Ident {ident_string = "os", ident_annot = SpanCoLinear {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_config.py", span_row = 2, span_start_column = 8, span_end_column = 9}}]
import qualified Data.Text as T
import qualified HgLib.Commands as C

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "Config" $ do

-- Conversion notes:
-- TODO: Unhandled method call: hglib.open
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
-- TODO: Unhandled method call: hglib.open
-- TODO: Unhandled expression: Dot {dot_expr = Dot {dot_expr = Var {var_ident = I
-- TODO: Unhandled expression: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
-- TODO: Unhandled binary operator: Plus {op_annot = SpanPoint {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_config.py", span_row = 29, span_column = 57}}

  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
      -- Setup: f <- -- TODO: withFile ".hg/hgrc" AppendMode $ \h ->
      -- Setup: hPutStrLn f "[section]\nkey=value\n"
      -- Setup: -- TODO: close f (handled by withFile)
      config <- C.config client 
      elem ("section", "key", "value") C.config client  `shouldBe` True
      [("section", "key", "value")] `shouldBe True` C.config client "section"
      [("section", "key", "value")] `shouldBe True` C.config client ["section", "foo"]
      -- TODO: Dot {dot_expr = Dot {dot_expr = Var {var_ident = I ["a.b", "foo"] `shouldThrow` anyException

  it "should show_source" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup: -- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
      -- Setup: f <- -- TODO: withFile ".hg/hgrc" AppendMode $ \h ->
      -- Setup: hPutStrLn f "[section]\nkey=value\n"
      -- Setup: -- TODO: close f (handled by withFile)
      config <- C.config client  -- TODO: options showsource=True
      elem (-- TODO: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V -- TODO: Plus {op_annot = SpanPoint {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_config.py", span_row = 29, span_column = 57}} ":2", "section", "key", "value") C.config client  -- TODO: options showsource=True `shouldBe` True


-- TODOS:
-- Unhandled method call: hglib.open
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
-- Unhandled method call: hglib.open
-- Unhandled expression: Dot {dot_expr = Dot {dot_expr = Var {var_ident = I
-- Unhandled expression: Call {call_fun = Dot {dot_expr = Dot {dot_expr = V
-- Unhandled binary operator: Plus {op_annot = SpanPoint {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_config.py", span_row = 29, span_column = 57}}
