{-# LANGUAGE OverloadedStrings #-}

-- Test/HgLib/ConfigSpec.hs
module Test.HgLib.ConfigSpec (spec) where

import System.Exit
import Test.Hspec
import Test.HgLib.Common
import qualified HgLib.Commands as C
import HgLib.Protocol
import HgLib.Types
import System.IO

spec :: Spec
spec = describe "Config" $ do
  it "should read configuration" $ do
    exitCode <- withTestRepo $ \bt -> do
      -- Write config
      withFile ".hg/hgrc" AppendMode $ \h ->
        hPutStrLn h "[section]\nkey=value"
      -- Reopen client to see config changes
      closeClient (btClient bt)
      client <- openClient nonInteractiveConfig
      config <- C.config client [] []
      let hasConfig = any (\(s, k, v) -> s == "section" && k == "key" && v == "value") config
      hasConfig `shouldBe` True
      closeClient client
    -- exitCode shouldBe ExitSuccess
    return ()

  it "should filter by section" $ do
    exitCode <- withTestRepo $ \bt -> do
      withFile ".hg/hgrc" AppendMode $ \h -> do
        hPutStrLn h "[section]\nkey=value"
        hPutStrLn h "[other]\nother=val"
      closeClient (btClient bt)
      client <- openClient nonInteractiveConfig
      sectionConfig <- C.config client ["section"] []
      all (\(s, _, _) -> s == "section") sectionConfig `shouldBe` True
      closeClient client
    return ()
