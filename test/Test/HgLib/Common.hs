{-# LANGUAGE OverloadedStrings #-}

-- Test/HgLib/Common.hs
module Test.HgLib.Common
    ( BaseTest(..)
    , withTestRepo
    , withTwoRepos
    , commonAppendFile
    , commonCreateFile
    , commonRemoveFile
    , hspec
    ) where

import Control.Exception (bracket, finally)
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, 
                        getCurrentDirectory, setCurrentDirectory, doesDirectoryExist, removeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Test.Hspec
import Test.Hspec.Core.Spec (SpecM)

import HgLib
import qualified HgLib.Commands as C

-- | Base test environment
data BaseTest = BaseTest
    { btClient :: HgClient
    , btTempDir :: FilePath
    } deriving (Show)

-- | Setup a test repository and run tests
withTestRepo :: (BaseTest -> IO a) -> IO a
withTestRepo action = 
    withSystemTempDirectory "hglib-test" $ \tmpDir -> do
        oldDir <- getCurrentDirectory
        bracket_
            (setCurrentDirectory tmpDir >> callProcess "hg" ["init"])
            (setCurrentDirectory oldDir)
            $ do
                client <- openClient defaultConfig
                let baseTest = BaseTest client tmpDir
                finally (action baseTest) (closeClient client >> return ())

-- | Setup two test repositories for push/pull tests
withTwoRepos :: (BaseTest -> BaseTest -> IO a) -> IO a
withTwoRepos action = 
    withSystemTempDirectory "hglib-test-1" $ \tmpDir1 ->
        withSystemTempDirectory "hglib-test-2" $ \tmpDir2 -> do
            oldDir <- getCurrentDirectory
            
            -- Setup first repo
            setCurrentDirectory tmpDir1
            callProcess "hg" ["init"]
            client1 <- openClient defaultConfig
            let baseTest1 = BaseTest client1 tmpDir1
            
            -- Setup second repo  
            setCurrentDirectory tmpDir2
            callProcess "hg" ["init"]
            client2 <- openClient defaultConfig
            let baseTest2 = BaseTest client2 tmpDir2
            
            finally 
                (action baseTest1 baseTest2)
                (do closeClient client1 >> return ()
                    closeClient client2 >> return ()
                    setCurrentDirectory oldDir)

-- | Utility to bracket directory changes
bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ setup teardown action = bracket setup (const teardown) (const action)

-- | Append content to a file
commonAppendFile :: FilePath -> ByteString -> IO ()
commonAppendFile path content = BS.appendFile path content

-- | Create a file with content
commonCreateFile :: FilePath -> ByteString -> IO ()
commonCreateFile path content = BS.writeFile path content

-- | Remove a file
commonRemoveFile :: FilePath -> IO ()
commonRemoveFile = System.Directory.removeFile
