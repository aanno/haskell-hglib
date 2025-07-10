{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.ConfigSpec (spec) where

import Control.Exception (try, SomeException)
import Data.Text (Text)
import HgLib.Types
import System.IO
import System.OsPath (OsPath)
import Test.HgLib.Common
import Test.Hspec
import qualified Data.Text as T
import qualified HgLib.Commands as C
import qualified System.FilePath
import qualified System.OsPath as OsPath

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "Config" $ do

-- Conversion notes:
-- TODO: Unhandled method call: hglib.open
-- TODO: Unhandled module call: common.basetest.setUp
-- TODO: Unhandled method call: hglib.open
-- TODO: Unhandled module attribute: self.client.config

  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup:
      -- TODO: common.basetest.setUp
      withFile ".hg/hgrc" AppendMode $ \h -> do
        hPutStrLn h "[section]\nkey=value\n"
        -- TODO: close f (handled by withFile)
        config <- C.config client 
        let hasValue = any (\item -> item == ("section", "key", "value")) C.config client 
        hasValue `shouldBe` True
        [("section", "key", "value")] `shouldBe` C.config client "section"
        [("section", "key", "value")] `shouldBe` C.config client ["section", "foo"]
        -- TODO: self.client.config ["a.b", "foo"] `shouldThrow` anyException
      closeClient client
      client' <- openClient nonInteractiveConfig
      -- ... continue with client' ...
      closeClient client'

  it "should show_source" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup:
      -- TODO: common.basetest.setUp
      withFile ".hg/hgrc" AppendMode $ \h -> do
        hPutStrLn h "[section]\nkey=value\n"
        -- TODO: close f (handled by withFile)
        config <- C.config client  (C.defaultConfigOptions { -- TODO: options showsource=True })
        let hasValue = any (\item -> item == (System.FilePath.normalise ".hg/hgrc" ++ ":2", "section", "key", "value")) C.config client  (C.defaultConfigOptions { -- TODO: options showsource=True })
        hasValue `shouldBe` True
      closeClient client
      client' <- openClient nonInteractiveConfig
      -- ... continue with client' ...
      closeClient client'


-- TODOS:
-- Unhandled method call: hglib.open
-- Unhandled module call: common.basetest.setUp
-- Unhandled method call: hglib.open
-- Unhandled module attribute: self.client.config
