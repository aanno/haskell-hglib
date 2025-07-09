{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.LogSpec (spec) where

import Control.Exception (try, SomeException)
import Data.Text (Text)
import HgLib.Types
import Test.HgLib.Common
import Test.Hspec
import [Ident {ident_string = "hglib", ident_annot = SpanCoLinear {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_log.py", span_row = 2, span_start_column = 8, span_end_column = 12}}]
import [Ident {ident_string = "subprocess", ident_annot = SpanCoLinear {span_filename = "/workspaces/ghc/tmp/python-hglib/tests/test_log.py", span_row = 4, span_start_column = 8, span_end_column = 17}}]
import qualified Data.Text as T
import qualified HgLib.Commands as C

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "Log" $ do

-- Conversion notes:
-- TODO: With statement conversion
-- TODO: Unhandled method call: subprocess.check_call

  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client "second"
      revs <- C.log_ client 
      -- TODO: revs <- return (reverse revs) -- Note: Python reverse() is in-place, Haskell reverse is not
      length C.log_ client  == 2 `shouldBe` True
      revNode ((C.log_ client  !! 1)) `shouldBe` node1
      head C.log_ client  `shouldBe` head C.log_ client "0"
      C.log_ client  `shouldBe` C.log_ client  (C.defaultLogOptions { C.logFiles = ["a"] })
      C.log_ client  `shouldBe` C.log_ client  (C.defaultLogOptions { C.logHidden = True })

  it "should dash_in_filename" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "-a" "-a"
      C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      revs <- C.log_ client  (C.defaultLogOptions { C.logFiles = ["-a"] })
      length C.log_ client  (C.defaultLogOptions { C.logFiles = ["-a"] }) == 1 `shouldBe` True
      revRev (head C.log_ client  (C.defaultLogOptions { C.logFiles = ["-a"] })) `shouldBe` "0"

  it "should empty_short_option" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "foobar" "foobar"
      C.commit client "first" (C.defaultCommitOptions { C.commitAddRemove = True })
      revs <- C.log_ client  (C.defaultLogOptions { C.logKeyword = Just "", C.logFiles = ["foobar"] })
      length C.log_ client  (C.defaultLogOptions { C.logKeyword = Just "", C.logFiles = ["foobar"] }) == 1 `shouldBe` True
      revRev (head C.log_ client  (C.defaultLogOptions { C.logKeyword = Just "", C.logFiles = ["foobar"] })) `shouldBe` "0"

  it "should null_byte" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      -- TODO: with statement
      -- TODO: subprocess.check_call
      revs <- C.log_ client  (C.defaultLogOptions { C.logRevRange = Just "." })
      revDesc (head C.log_ client  (C.defaultLogOptions { C.logRevRange = Just "." })) `shouldBe` "some message\0more stuff"


-- TODOS:
-- With statement conversion
-- Unhandled method call: subprocess.check_call
