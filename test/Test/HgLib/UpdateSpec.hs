{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.UpdateSpec (spec) where

import Control.Exception (try, SomeException)
import Data.Text (Text)
import HgLib.Types
import System.IO
import Test.HgLib.Common
import Test.Hspec
import qualified Data.Text as T
import qualified HgLib.Commands as C
import qualified System.FilePath

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "Update" $ do

-- Conversion notes:
-- TODO: Unhandled module call: common.basetest.setUp
-- TODO: Complex open() call in with statement
-- TODO: Unhandled method call: old.encode
-- TODO: Unhandled module attribute: self.client.update
-- TODO: Complex open() call in with statement
-- TODO: Unhandled module attribute: self.client.update
-- TODO: Complex open() call in with statement
-- TODO: Unhandled method call: self.test_basic

  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup:
      -- TODO: common.basetest.setUp
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client (mkDefaultCommitOptions "second")
      (u, m, r, ur) <- C.update client rev0
      u `shouldBe` 1
      m `shouldBe` 0
      r `shouldBe` 0
      ur `shouldBe` 0

  it "should unresolved" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup:
      -- TODO: common.basetest.setUp
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client (mkDefaultCommitOptions "second")
      C.update client rev0
      commonAppendFile "a" "b"
      (u, m, r, ur) <- C.update client 
      u `shouldBe` 0
      m `shouldBe` 0
      r `shouldBe` 0
      ur `shouldBe` 1
      let hasValue = any (\item -> item == ("M", "a")) C.status client []
      hasValue `shouldBe` True

  it "should merge" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup:
      -- TODO: common.basetest.setUp
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client (mkDefaultCommitOptions "second")
      commonAppendFile "a" "\n\n\n\nb"
      (rev2, node2) <- C.commit client (mkDefaultCommitOptions "third")
      commonAppendFile "a" "b"
      C.commit client (mkDefaultCommitOptions "fourth")
      C.update client rev2
      -- TODO: complex with open() call
      withFile "a" WriteMode $ \h -> do
        hPutStrLn h "a" ++ -- TODO: old.encode
        -- TODO: close f (handled by withFile)
        (u, m, r, ur) <- C.update client 
        u `shouldBe` 0
        m `shouldBe` 1
        r `shouldBe` 0
        ur `shouldBe` 0
        C.status client [] `shouldBe` [("M", "a")]

  it "should tip" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup:
      -- TODO: common.basetest.setUp
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client (mkDefaultCommitOptions "second")
        C.update client rev0
        (u, m, r, ur) <- C.update client 
        u `shouldBe` 1
        revNode (head C.parents client ) `shouldBe` node1
        C.update client rev0
        commonAppendFile "a" "b"
        (rev2, node2) <- C.commit client (mkDefaultCommitOptions "new head")
        C.update client rev0
        C.update client 
        revNode (head C.parents client ) `shouldBe` node2

  it "should check_clean" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup:
      -- TODO: common.basetest.setUp
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client (mkDefaultCommitOptions "second")
        -- TODO: self.client.update -- TODO: keyword arg clean=True -- TODO: keyword arg check=True `shouldThrow` anyException

  it "should clean" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup:
      -- TODO: common.basetest.setUp
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client (mkDefaultCommitOptions "second")
        -- TODO: complex with open() call
        commonAppendFile "a" "b"
        -- TODO: self.client.update -- TODO: keyword arg check=True `shouldThrow` anyException
        (u, m, r, ur) <- C.update client  (C.defaultUpdateOptions { -- TODO: options clean=True })
        u `shouldBe` 1
        -- TODO: complex with open() call

  it "should basic_plain" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      -- Setup:
      -- TODO: common.basetest.setUp
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client (mkDefaultCommitOptions "second")
        withFile ".hg/hgrc" AppendMode $ \h -> do
          hPutStrLn h "[defaults]\nupdate=-v\n"
          -- TODO: close f (handled by withFile)
          -- TODO: self.test_basic


-- TODOS:
-- Unhandled module call: common.basetest.setUp
-- Complex open() call in with statement
-- Unhandled method call: old.encode
-- Unhandled module attribute: self.client.update
-- Complex open() call in with statement
-- Unhandled module attribute: self.client.update
-- Complex open() call in with statement
-- Unhandled method call: self.test_basic
