{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.StatusSpec (spec) where

import Control.Exception (try, SomeException)
import Data.Text (Text)
import HgLib.Types
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
spec = describe "Status" $ do

-- Conversion notes:
-- TODO: Unhandled method call: os.remove
-- TODO: Complex self.append() call
-- TODO: Unhandled statement: For {for_targets = [Var {var_ident = Ident {ident_

  it "should handle empty repository" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      C.status client [] `shouldBe` []

  it "should one_of_each" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile ".hgignore" "ignored"
      commonAppendFile "ignored" "a"
      commonAppendFile "clean" "a"
      commonAppendFile "modified" "a"
      commonAppendFile "removed" "a"
      commonAppendFile "missing" "a"
      C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True })
      commonAppendFile "modified" "a"
      commonAppendFile "added" "a"
      C.add client ["added"]
      -- TODO: os.remove
      C.remove client ["removed"]
      -- TODO: complex self.append() call
      l <- [("M", "modified"), ("A", "added"), ("R", "removed"), ("C", ".hgignore"), ("C", "clean"), ("!", "missing"), ("?", "untracked"), ("I", "ignored")]
      st <- C.status client [] (C.defaultStatusOptions { -- TODO: options all=True })
      -- TODO: For {for_targets = [Var {var_ident = Ident {ident_

  it "should copy" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "source" "a"
      C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True })
      sourcePath <- OsPath.encodeUtf "source"
      destPath <- OsPath.encodeUtf "dest"
      C.copy client [sourcePath] destPath []
      l <- [("A", "dest"), (" ", "source")]
      C.status client [] (C.defaultStatusOptions { -- TODO: options copies=True }) `shouldBe` [("A", "dest"), (" ", "source")]

  it "should copy_origin_space" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "s ource" "a"
      C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True })
      sourcePath <- OsPath.encodeUtf "s ource"
      destPath <- OsPath.encodeUtf "dest"
      C.copy client [sourcePath] destPath []
      l <- [("A", "dest"), (" ", "s ource")]
      C.status client [] (C.defaultStatusOptions { -- TODO: options copies=True }) `shouldBe` [("A", "dest"), (" ", "s ource")]


-- TODOS:
-- Unhandled method call: os.remove
-- Complex self.append() call
-- Unhandled statement: For {for_targets = [Var {var_ident = Ident {ident_
