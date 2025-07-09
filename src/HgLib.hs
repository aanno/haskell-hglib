{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell port of python-hglib
--
-- This module provides a complete Haskell interface to Mercurial's command server,
-- offering fast and efficient communication with Mercurial repositories.
--
-- Example usage:
--
-- @
-- import HgLib
-- 
-- main :: IO ()
-- main = do
--   config <- return $ defaultConfigWithPath "/path/to/repo"
--   withClient config $ \client -> do
--     -- Get repository status
--     statuses <- C.status client C.defaultStatusOptions
--     print statuses
--     
--     -- Get recent log entries
--     revisions <- C.log_ client [] C.defaultLogOptions { C.logLimit = Just 10 }
--     mapM_ (putStrLn . T.unpack . formatRevision) revisions
--     
--     -- Add and commit files
--     success <- C.add client ["newfile.txt"] C.defaultAddOptions
--     when success $ do
--       (rev, node) <- C.commit client C.defaultCommitOptions 
--         { C.commitMessage = Just "Add new file" }
--       putStrLn $ "Committed as revision " ++ show rev
-- @

module HgLib
    ( -- * Re-exports
      module HgLib.Types
    , module C
    , module HgLib.Protocol
    , module HgLib.Error
    
    -- * Convenience functions
    , simpleClone
    , simpleCommit
    , simpleStatus
    , simpleLog
    , getCurrentRevision
    , isCleanWorkingDirectory
    , getRepositoryInfo
    
    -- * Repository information
    , RepositoryInfo(..)
    ) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import HgLib.Types
import qualified HgLib.Commands as C
import HgLib.Protocol
import HgLib.Error

-- | Summary information about a repository
data RepositoryInfo = RepositoryInfo
    { repoRoot :: !FilePath
    , repoCurrentRevision :: !Revision
    , repoBranch :: !Text
    , repoIsClean :: !Bool
    , repoUpdateCount :: !Int
    } deriving (Show, Eq)

-- | Simple repository cloning
simpleClone :: String -> FilePath -> IO ()
simpleClone source dest = do
    withClient defaultConfig $ \client ->
        C.clone client source (Just dest) []

-- | Simple commit with just a message
simpleCommit :: HgClient -> String -> IO (Int, Text)
simpleCommit client message = 
    C.commit client $ C.mkDefaultCommitOptions message

-- | Simple status check (all files)
simpleStatus :: HgClient -> IO [HgStatus]
simpleStatus client = C.status client C.defaultStatusOptions

-- | Simple log (last 10 revisions)
simpleLog :: HgClient -> IO [Revision]
simpleLog client = C.log_ client [] C.defaultLogOptions { C.logLimit = Just 10 }

-- | Get the current (tip) revision
getCurrentRevision :: HgClient -> IO Revision
getCurrentRevision = C.tip

-- | Check if working directory is clean (no uncommitted changes)
isCleanWorkingDirectory :: HgClient -> IO Bool
isCleanWorkingDirectory client = do
    statuses <- simpleStatus client
    return $ null statuses

-- | Get comprehensive repository information
getRepositoryInfo :: HgClient -> IO RepositoryInfo
getRepositoryInfo client = do
    repoRoot <- C.root client
    repoCurrentRevision <- getCurrentRevision client
    repoBranch <- C.branch client C.defaultBranchOptions
    
    statuses <- simpleStatus client
    let repoIsClean = null statuses
    
    summaryInfo <- C.summary client []
    let repoUpdateCount = summaryUpdateCount summaryInfo
    
    return RepositoryInfo{..}
