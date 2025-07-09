{-# LANGUAGE OverloadedStrings #-}

module Test.HgLib.LogSpec (spec) where

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
spec = describe "Log" $ do

-- Conversion notes:
-- TODO: Unhandled method call: subprocess.check_call

  it "should handle basic repository with one commit" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      (rev0, node0) <- C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True })
      commonAppendFile "a" "a"
      (rev1, node1) <- C.commit client (mkDefaultCommitOptions "second")
      revs <- C.log_ client []
      -- TODO: C.log_ client [] <- return (reverse C.log_ client []) -- Note: Python reverse() is in-place, Haskell reverse is not
      length C.log_ client [] == 2 `shouldBe` True
      revNode ((C.log_ client [] !! 1)) `shouldBe` node1
      head C.log_ client [] `shouldBe` head C.log_ client "0"
      C.log_ client [] `shouldBe` C.log_ client [] (defaultLogOptions { C.logFiles = ["a"] })
      C.log_ client [] `shouldBe` C.log_ client [] (defaultLogOptions { C.logHidden = True })

  it "should dash_in_filename" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "-a" "-a"
      C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True })
      revs <- C.log_ client [] (defaultLogOptions { C.logFiles = ["-a"] })
      length C.log_ client [] (defaultLogOptions { C.logFiles = ["-a"] }) == 1 `shouldBe` True
      revRev (head C.log_ client [] (defaultLogOptions { C.logFiles = ["-a"] })) `shouldBe` "0"

  it "should empty_short_option" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "foobar" "foobar"
      C.commit client ((mkDefaultCommitOptions "first") { commitAddRemove = True })
      revs <- C.log_ client [] (defaultLogOptions { C.logKeyword = Just "", C.logFiles = ["foobar"] })
      length C.log_ client [] (defaultLogOptions { C.logKeyword = Just "", C.logFiles = ["foobar"] }) == 1 `shouldBe` True
      revRev (head C.log_ client [] (defaultLogOptions { C.logKeyword = Just "", C.logFiles = ["foobar"] })) `shouldBe` "0"

  it "should null_byte" $
    withTestRepo $ \bt -> do
      let client = btClient bt
      commonAppendFile "a" "a"
      withFile "commitmessagenullbyte" WriteMode $ \h -> do
        hPutStrLn f "some message\0more stuff"
      -- TODO: subprocess.check_call
      revs <- C.log_ client [] (defaultLogOptions { C.logRevRange = Just "." })
      revDesc (head C.log_ client [] (defaultLogOptions { C.logRevRange = Just "." })) `shouldBe` "some message\0more stuff"


-- TODOS:
-- Unhandled method call: subprocess.check_call
