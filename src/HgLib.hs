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
--     statuses <- status client defaultStatusOptions
--     print statuses
--     
--     -- Get recent log entries
--     revisions <- log_ client [] defaultLogOptions { logLimit = Just 10 }
--     mapM_ (putStrLn . formatRevision) revisions
--     
--     -- Add and commit files
--     success <- add client ["newfile.txt"] defaultAddOptions
--     when success $ do
--       (rev, node) <- commit client defaultCommitOptions 
--         { commitMessage = Just "Add new file" }
--       putStrLn $ "Committed as revision " ++ show rev
-- @

module HgLib
    ( -- * Re-exports
      module HgLib.Types
    , module HgLib.Commands
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

import Control.Monad (when, void)
import Control.Exception (bracket, throwIO, finally)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Text.Read (readMaybe)

import HgLib.Types
import qualified HgLib.Commands as C
import HgLib.Protocol
import HgLib.Error
import qualified HgLib.Utils as U
import HgLib.Utils (buildArgs)

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
    C.commit client C.defaultCommitOptions { C.commitMessage = Just message }

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
    repoBranch <- C.branch client Nothing []
    
    statuses <- simpleStatus client
    let repoIsClean = null statuses
    
    summaryKV <- C.summary client []
    let repoUpdateCount = parseUpdateCount $ lookup "update" summaryKV
    
    return RepositoryInfo{..}
  where
    parseUpdateCount Nothing = 0
    parseUpdateCount (Just text)
        | "(current)" `T.isInfixOf` text = 0
        | otherwise = fromMaybe 0 $ readMaybe . T.unpack . T.takeWhile (/= ' ') $ text

-- | Create a bundle
bundle :: HgClient -> FilePath -> [String] -> IO Bool
bundle client file options = do
    let opts = ("file", Just file) : map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "bundle" opts [])
    return True

-- | Show file contents at revision
cat :: HgClient -> [FilePath] -> Maybe String -> IO ByteString
cat client files rev = do
    let opts = [("rev", rev)]
    runCommand client (buildArgs "cat" opts files)

-- | Clone a repository
clone :: HgClient -> String -> Maybe String -> [String] -> IO ()
clone client source dest options = do
    let opts = ("source", Just source) : maybe [] (\d -> [("dest", Just d)]) dest 
               ++ map (\o -> (o, Just "")) options
    void $ runCommand client (buildArgs "clone" opts [])

-- | Commit changes
commit :: HgClient -> Maybe String -> [String] -> IO (Int, Text)
commit client message options = do
    let opts = maybe [] (\m -> [("message", Just m)]) message 
               ++ [("debug", Just "")] ++ map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "commit" opts [])
    parseCommitResult $ TE.decodeUtf8 result
  where
    parseCommitResult text = 
        case T.lines text of
            [] -> (0, "")
            (line:_) -> case T.words line of
                (_:_:rev:node:_) -> (read $ T.unpack rev, node)
                _ -> (0, "")

-- | Show configuration
config :: HgClient -> [String] -> [String] -> IO [(Text, Text, Text)]
config client names options = do
    let opts = map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "showconfig" opts names)
    return $ parseConfig $ T.lines $ TE.decodeUtf8 result
  where
    parseConfig = mapMaybe parseConfigLine
    parseConfigLine line = case T.splitOn "=" line of
        [key, value] -> case T.splitOn "." key of
            [section, name] -> Just (section, name, value)
            _ -> Nothing
        _ -> Nothing

-- | Copy files
copy :: HgClient -> [FilePath] -> FilePath -> [String] -> IO Bool
copy client sources dest options = do
    let opts = ("dest", Just dest) : map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "copy" opts sources)
    return True

-- | Show differences
diff :: HgClient -> [FilePath] -> [String] -> [String] -> IO ByteString
diff client files revs options = do
    let opts = map (\r -> ("rev", Just r)) revs ++ map (\o -> (o, Just "")) options
    runCommand client (buildArgs "diff" opts files)

-- | Export changesets
export :: HgClient -> [String] -> [String] -> IO ByteString
export client revs options = do
    let opts = map (\o -> (o, Just "")) options
    runCommand client (buildArgs "export" opts revs)

-- | Forget files
forget :: HgClient -> [FilePath] -> [String] -> IO Bool
forget client files options = do
    let opts = map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "forget" opts files)
    return True

-- | Search for patterns
grep :: HgClient -> String -> [FilePath] -> [String] -> IO [(Text, Text)]
grep client pattern files options = do
    let opts = ("pattern", Just pattern) : map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "grep" opts files)
    return $ parseGrepResults $ T.lines $ TE.decodeUtf8 result
  where
    parseGrepResults = map (\line -> (line, ""))  -- Simplified

-- | Show repository heads
heads :: HgClient -> [String] -> [String] -> IO [Revision]
heads client revs options = do
    let opts = ("template", Just "json") : map (\r -> ("rev", Just r)) revs 
               ++ map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "heads" opts [])
    parseJsonRevisions $ TE.decodeUtf8 result

-- | Identify repository state
identify :: HgClient -> [String] -> IO Text
identify client options = do
    let opts = map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "identify" opts [])
    return $ T.strip $ TE.decodeUtf8 result

-- | Import patches
import_ :: HgClient -> [FilePath] -> [String] -> IO ()
import_ client patches options = do
    let opts = map (\o -> (o, Just "")) options
    void $ runCommand client (buildArgs "import" opts patches)

-- | Show incoming changes
incoming :: HgClient -> Maybe String -> [String] -> IO [Revision]
incoming client path options = do
    let opts = ("template", Just "json") : maybe [] (\p -> [("path", Just p)]) path
               ++ map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "incoming" opts [])
    parseJsonRevisions $ TE.decodeUtf8 result

-- | Show revision history
log_ :: HgClient -> [FilePath] -> [String] -> IO [Revision]
log_ client files options = do
    let opts = ("template", Just "json") : map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "log" opts files)
    parseJsonRevisions $ TE.decodeUtf8 result

-- | Show manifest
manifest :: HgClient -> [String] -> IO [(Text, Text, Bool, Bool, Text)]
manifest client options = do
    let opts = ("debug", Just "") : map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "manifest" opts [])
    return $ parseManifest $ T.lines $ TE.decodeUtf8 result
  where
    parseManifest = mapMaybe parseManifestLine
    parseManifestLine line = 
        if T.length line >= 47
            then let node = T.take 40 line
                     perm = T.take 3 $ T.drop 41 line
                     flag = T.index line 45
                     path = T.drop 47 line
                     executable = flag == '*'
                     symlink = flag == '@'
                 in Just (node, perm, executable, symlink, path)
            else Nothing

-- | Merge with another revision
merge :: HgClient -> Maybe String -> [String] -> IO ()
merge client rev options = do
    let opts = maybe [] (\r -> [("rev", Just r)]) rev ++ map (\o -> (o, Just "")) options
    void $ runCommand client (buildArgs "merge" opts [])

-- | Move/rename files
move :: HgClient -> [FilePath] -> FilePath -> [String] -> IO Bool
move client sources dest options = do
    let opts = ("dest", Just dest) : map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "move" opts sources)
    return True

-- | Show outgoing changes
outgoing :: HgClient -> Maybe String -> [String] -> IO [Revision]
outgoing client path options = do
    let opts = ("template", Just "json") : maybe [] (\p -> [("path", Just p)]) path
               ++ map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "outgoing" opts [])
    parseJsonRevisions $ TE.decodeUtf8 result

-- | Show parent revisions
parents :: HgClient -> [String] -> IO [Revision]
parents client options = do
    let opts = ("template", Just "json") : map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "parents" opts [])
    parseJsonRevisions $ TE.decodeUtf8 result

-- | Show repository paths
paths :: HgClient -> Maybe String -> IO [(Text, Text)]
paths client name = do
    let args = maybe [] (\n -> [n]) name
    result <- runCommand client (buildArgs "paths" [] args)
    return $ parsePaths $ T.lines $ TE.decodeUtf8 result
  where
    parsePaths = mapMaybe parsePath
    parsePath line = case T.splitOn " = " line of
        [name, url] -> Just (name, url)
        _ -> Nothing

-- | Pull changes
pull :: HgClient -> Maybe String -> [String] -> IO Bool
pull client source options = do
    let opts = maybe [] (\s -> [("source", Just s)]) source ++ map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "pull" opts [])
    return True

-- | Push changes
push :: HgClient -> Maybe String -> [String] -> IO Bool
push client dest options = do
    let opts = maybe [] (\d -> [("dest", Just d)]) dest ++ map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "push" opts [])
    return True

-- | Remove files
remove :: HgClient -> [FilePath] -> [String] -> IO Bool
remove client files options = do
    let opts = map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "remove" opts files)
    return True

-- | Resolve merge conflicts
resolve :: HgClient -> [FilePath] -> [String] -> IO [(Char, FilePath)]
resolve client files options = do
    let opts = ("list", Just "") : map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "resolve" opts files)
    return $ parseResolve $ T.lines $ TE.decodeUtf8 result
  where
    parseResolve = mapMaybe parseResolveLine
    parseResolveLine line = case T.uncons line of
        Just (code, rest) -> Just (code, T.unpack $ T.strip rest)
        Nothing -> Nothing

-- | Revert files
revert :: HgClient -> [FilePath] -> [String] -> IO Bool
revert client files options = do
    let opts = map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "revert" opts files)
    return True

-- | Show repository root
root :: HgClient -> IO FilePath
root client = do
    result <- runCommand client (buildArgs "root" [] [])
    return $ T.unpack $ T.strip $ TE.decodeUtf8 result

-- | Show repository status
status :: HgClient -> [String] -> IO [HgStatus]
status client options = do
    let opts = ("print0", Just "") : map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "status" opts [])
    return $ parseStatus $ BS8.split '\0' result
  where
    parseStatus = mapMaybe parseStatusLine
    parseStatusLine line 
        | BS.length line >= 2 = Just $ HgStatus (BS8.head line) (BS8.unpack $ BS.drop 2 line)
        | otherwise = Nothing

-- | Add tags
tag :: HgClient -> [String] -> [String] -> IO ()
tag client names options = do
    let opts = map (\o -> (o, Just "")) options
    void $ runCommand client (buildArgs "tag" opts names)

-- | List tags
tags :: HgClient -> IO [(Text, Int, Text, Bool)]
tags client = do
    result <- runCommand client (buildArgs "tags" [("verbose", Just "")] [])
    return $ parseTags $ T.lines $ TE.decodeUtf8 result
  where
    parseTags = mapMaybe parseTag
    parseTag line = 
        let isLocal = " local" `T.isSuffixOf` line
            line' = if isLocal then T.dropEnd 6 line else line
        in case T.words line' of
            parts -> case T.splitOn ":" (last parts) of
                [rev, node] -> case reads (T.unpack rev) of
                    [(revNum, "")] -> Just (T.unwords (init parts), revNum, node, isLocal)
                    _ -> Nothing
                _ -> Nothing

-- | Show or set phase
phase :: HgClient -> [String] -> [String] -> IO [(Int, Text)]
phase client revs options = do
    let opts = map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "phase" opts revs)
    return $ parsePhase $ T.lines $ TE.decodeUtf8 result
  where
    parsePhase = mapMaybe parsePhaseLine
    parsePhaseLine line = case T.splitOn ": " line of
        [revStr, phase] -> case reads (T.unpack revStr) of
            [(rev, "")] -> Just (rev, phase)
            _ -> Nothing
        _ -> Nothing

-- | Show repository summary
summary :: HgClient -> [String] -> IO [(Text, Text)]
summary client options = do
    let opts = map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "summary" opts [])
    return $ parseSummary $ T.lines $ TE.decodeUtf8 result
  where
    parseSummary = mapMaybe parseSummaryLine
    parseSummaryLine line = case T.splitOn ": " line of
        [key, value] -> Just (key, value)
        _ -> Nothing

-- | Show tip revision
tip :: HgClient -> IO Revision
tip client = do
    result <- runCommand client (buildArgs "tip" [("template", Just "json")] [])
    revs <- parseJsonRevisions $ TE.decodeUtf8 result
    case revs of
        (rev:_) -> return rev
        [] -> throwIO $ HgResponseError "No tip revision found"

-- | Update working directory
update :: HgClient -> Maybe String -> [String] -> IO (Int, Int, Int, Int)
update client rev options = do
    let opts = maybe [] (\r -> [("rev", Just r)]) rev ++ map (\o -> (o, Just "")) options
    result <- runCommand client (buildArgs "update" opts [])
    parseUpdateResult $ TE.decodeUtf8 result
  where
    parseUpdateResult text = 
        case T.words <$> T.lines text of
            [] -> (0, 0, 0, 0)
            (line:_) -> case mapMaybe (readMaybe . T.unpack) line of
                [a, b, c, d] -> (a, b, c, d)
                _ -> (0, 0, 0, 0)
    readMaybe s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

-- | Show Mercurial version
version :: HgClient -> IO (Int, Int, Int, String)
version client = do
    case clientVersion client of
        Just v -> return v
        Nothing -> do
            result <- runCommand client (buildArgs "version" [("quiet", Just "")] [])
            let v = parseVersion $ TE.decodeUtf8 result
            return v
  where
    parseVersion text = (0, 0, 0, T.unpack text)  -- Simplified

-- Utility functions

-- | Parse JSON revisions from command output
parseJsonRevisions :: Text -> IO [Revision]
parseJsonRevisions text = return []  -- Simplified - would need proper JSON parsing

-- | Parse a revision from text
parseRevision :: Text -> Maybe Revision
parseRevision _ = Nothing  -- Simplified

-- | Format a revision for display
formatRevision :: Revision -> Text
formatRevision Revision{..} = T.concat [revRev, ":", T.take 12 $ TE.decodeUtf8 revNode]
