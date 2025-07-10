{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HgLib.Commands
    ( -- * Repository Operations
      add
    , addRemove
    , annotate
    , archive
    , backout
    , bookmark
    , bookmarks
    , branch
    , branches
    , bundle
    , cat
    , clone
    , commit
    , config
    , copy
    , diff
    , export
    , forget
    , grep
    , heads
    , identify
    , import_
    , incoming
    , log_
    , manifest
    , merge
    , move
    , outgoing
    , parents
    , paths
    , pull
    , push
    , remove
    , resolve
    , revert
    , root
    , status
    , tag
    , tags
    , phase
    , summary
    , tip
    , update
    , version    
    ) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Text.Read (readMaybe)
import System.OsPath (OsPath)
import qualified System.OsPath as OsPath

import HgLib.Types
import HgLib.Protocol
import HgLib.Error
import HgLib.Utils
import Logging

-- | Add files to the repository
add :: HgClient -> [OsPath] -> AddOptions -> IO Bool
add client files AddOptions{..} = do
    args <- buildArgsWithPaths "add" 
            [ ("dry-run", boolFlag addDryRun)
            , ("subrepos", boolFlag addSubrepos)
            , ("include", addInclude)
            , ("exclude", addExclude)
            ] files
    
    (result, _, exitCode) <- runCommand client args
    return (exitCode == 0)

-- | Add new files and remove missing files
addRemove :: HgClient -> [OsPath] -> Maybe Int -> Bool -> Maybe String -> Maybe String -> IO Bool
addRemove client files similarity dryrun include exclude = do
    args <- buildArgsWithPaths "addremove"
            [ ("similarity", show <$> similarity)
            , ("dry-run", boolFlag dryrun)
            , ("include", include)
            , ("exclude", exclude)
            ] files
    
    (result, _, exitCode) <- runCommand client args
    return (exitCode == 0)

-- | Show changeset information by line for each file
annotate :: HgClient -> [OsPath] -> Maybe String -> [String] -> IO [AnnotationLine]
annotate client files rev options = do
    args <- buildArgsWithPaths "annotate"
            (("rev", rev) : map (\o -> (o, Just "")) options) files
    
    result <- rawCommand client args
    return $ parseAnnotationLines $ TE.decodeUtf8 result

-- | Create an archive of the repository
archive :: HgClient -> OsPath -> Maybe String -> [String] -> IO ()
archive client dest rev options = do
    destOption <- osPathToStringOption ("dest", Just dest)
    let args = buildArgs "archive"
            (destOption : ("rev", rev) : map (\o -> (o, Just "")) options) []
    
    void $ rawCommand client args

-- | Backout a changeset
backout :: HgClient -> String -> [String] -> IO ()
backout client rev options = do
    let args = buildArgs "backout"
            (("rev", Just rev) : map (\o -> (o, Just "")) options) []
    
    void $ rawCommand client args

-- | Set or list bookmarks
bookmark :: HgClient -> Maybe String -> [String] -> IO ()
bookmark client name options = do
    let args = buildArgs "bookmark"
            (maybe [] (\n -> [("name", Just n)]) name ++ map (\o -> (o, Just "")) options) []
    
    void $ rawCommand client args

-- | List bookmarks
bookmarks :: HgClient -> IO [BookmarkInfo]
bookmarks client = do
    result <- rawCommand client ["bookmarks"]
    return $ parseBookmarks $ TE.decodeUtf8 result

-- | Get or set branch name
branch :: HgClient -> BranchOptions -> IO Text
branch client BranchOptions{..} = do
    let args = buildArgs "branch"
            [ ("force", boolFlag branchForce)
            , ("clean", boolFlag branchClean)
            ] (maybe [] (\n -> [n]) branchName)
    
    result <- rawCommand client args
    return $ T.strip $ TE.decodeUtf8 result

-- | List branches
branches :: HgClient -> BranchesOptions -> IO [BranchInfo]
branches client BranchesOptions{..} = do
    let args = buildArgs "branches"
            (map (\r -> ("rev", Just r)) branchesRev ++
             [ ("closed", boolFlag branchesClosed)
             , ("template", branchesTemplate)
             ]) []
    
    result <- rawCommand client args
    return $ parseBranches $ TE.decodeUtf8 result

-- | Create a bundle
bundle :: HgClient -> OsPath -> [String] -> IO Bool
bundle client file options = do
    fileOption <- osPathToStringOption ("file", Just file)
    let args = buildArgs "bundle"
            (fileOption : map (\o -> (o, Just "")) options) []
    
    result <- rawCommand client args
    return True

-- | Show file contents at revision
cat :: HgClient -> [OsPath] -> Maybe String -> IO ByteString
cat client files rev = do
    args <- buildArgsWithPaths "cat" [("rev", rev)] files
    rawCommand client args

-- | Clone a repository
clone :: HgClient -> String -> Maybe String -> [String] -> IO ()
clone client source dest options = do
    let args = buildArgs "clone"
            (("source", Just source) : maybe [] (\d -> [("dest", Just d)]) dest 
             ++ map (\o -> (o, Just "")) options) []
    
    void $ rawCommand client args

-- | Commit changes
commit :: HgClient -> CommitOptions -> IO (Int, Text)
commit client CommitOptions{..} = do
    logfileOption <- osPathToStringOption ("logfile", commitLogfile)
    let args = buildArgs "commit"
            [ ("message", Just commitMessage)
            , logfileOption
            , ("addremove", boolFlag commitAddRemove)
            , ("close-branch", boolFlag commitCloseBranch)
            , ("date", commitDate)
            , ("user", commitUser)
            , ("include", commitInclude)
            , ("exclude", commitExclude)
            , ("amend", boolFlag commitAmend)
            , ("debug", Just "")
            ] []
    
    result <- rawCommand client args
    parseCommitResult $ TE.decodeUtf8 result

-- | Show configuration
config :: HgClient -> ConfigOptions -> IO [(Text, Text, Text)]
config client ConfigOptions{..} = do
    let args = buildArgs "showconfig"
            [ ("untrusted", boolFlag configUntrusted)
            , ("edit", boolFlag configEdit)
            , ("local", boolFlag configLocal)
            , ("source", boolFlag configSource)
            , ("global", boolFlag configGlobal)
            , ("template", configTemplate)
            ] configNames
    
    result <- rawCommand client args
    return $ parseConfig $ TE.decodeUtf8 result

-- | Copy files
copy :: HgClient -> [OsPath] -> OsPath -> [String] -> IO Bool
copy client sources dest options = do
    destOption <- osPathToStringOption ("dest", Just dest)
    args <- buildArgsWithPaths "copy"
            (destOption : map (\o -> (o, Just "")) options) sources
    
    result <- rawCommand client args
    return True

-- | Show differences
diff :: HgClient -> [OsPath] -> DiffOptions -> IO ByteString
diff client files DiffOptions{..} = do
    args <- buildArgsWithPaths "diff"
            (map (\r -> ("rev", Just r)) diffRevs ++
             [ ("change", diffChange)
             , ("text", boolFlag diffText)
             , ("git", boolFlag diffGit)
             , ("nodates", boolFlag diffNoDates)
             , ("show-function", boolFlag diffShowFunction)
             , ("reverse", boolFlag diffReverse)
             , ("ignore-all-space", boolFlag diffIgnoreAllSpace)
             , ("ignore-space-change", boolFlag diffIgnoreSpaceChange)
             , ("ignore-blank-lines", boolFlag diffIgnoreBlankLines)
             , ("unified", show <$> diffUnified)
             , ("stat", boolFlag diffStat)
             , ("subrepos", boolFlag diffSubrepos)
             , ("include", diffInclude)
             , ("exclude", diffExclude)
             ]) files
    
    rawCommand client args

-- | Export changesets
export :: HgClient -> [String] -> [String] -> IO ByteString
export client revs options = do
    let args = buildArgs "export" (map (\o -> (o, Just "")) options) revs
    rawCommand client args

-- | Forget files
forget :: HgClient -> [OsPath] -> [String] -> IO Bool
forget client files options = do
    args <- buildArgsWithPaths "forget" (map (\o -> (o, Just "")) options) files
    result <- rawCommand client args
    return True

-- | Search for patterns
grep :: HgClient -> String -> [OsPath] -> [String] -> IO [(Text, Text)]
grep client pattern files options = do
    args <- buildArgsWithPaths "grep"
            (("pattern", Just pattern) : map (\o -> (o, Just "")) options) files
    
    result <- rawCommand client args
    return $ parseGrepResults $ TE.decodeUtf8 result

-- | Show repository heads
heads :: HgClient -> [String] -> [String] -> IO [Revision]
heads client revs options = do
    let args = buildArgs "heads"
            (("template", Just "json") : map (\r -> ("rev", Just r)) revs 
             ++ map (\o -> (o, Just "")) options) []
    
    result <- rawCommand client args
    parseJsonRevisions $ TE.decodeUtf8 result

-- | Identify repository state
identify :: HgClient -> [String] -> IO Text
identify client options = do
    let args = buildArgs "identify" (map (\o -> (o, Just "")) options) []
    result <- rawCommand client args
    return $ T.strip $ TE.decodeUtf8 result

-- | Import patches
import_ :: HgClient -> [OsPath] -> [String] -> IO ()
import_ client patches options = do
    args <- buildArgsWithPaths "import" (map (\o -> (o, Just "")) options) patches
    void $ rawCommand client args

-- | Show incoming changes
incoming :: HgClient -> Maybe String -> [String] -> IO [Revision]
incoming client path options = do
    let args = buildArgs "incoming"
            (("template", Just "json") : maybe [] (\p -> [("path", Just p)]) path
             ++ map (\o -> (o, Just "")) options) []
    
    result <- rawCommand client args
    parseJsonRevisions $ TE.decodeUtf8 result

-- | Show revision history
log_ :: HgClient -> [OsPath] -> LogOptions -> IO [Revision]
log_ client files LogOptions{..} = do
    args <- buildArgsWithPaths "log"
            (("template", Just "json") :
             [ ("rev", logRevRange)
             , ("follow", boolFlag logFollow)
             , ("follow-first", boolFlag logFollowFirst)
             , ("date", logDate)
             , ("copies", boolFlag logCopies)
             , ("keyword", logKeyword)
             , ("removed", boolFlag logRemoved)
             , ("only-merges", boolFlag logOnlyMerges)
             , ("user", logUser)
             , ("branch", logBranch)
             , ("prune", logPrune)
             , ("limit", show <$> logLimit)
             , ("no-merges", boolFlag logNoMerges)
             , ("include", logInclude)
             , ("exclude", logExclude)
             ]) files
    
    result <- rawCommand client args
    parseJsonRevisions $ TE.decodeUtf8 result

-- | Show manifest
manifest :: HgClient -> [String] -> IO [ManifestEntry]
manifest client options = do
    let args = buildArgs "manifest" 
            (("debug", Just "") : map (\o -> (o, Just "")) options) []
    
    result <- rawCommand client args
    parseManifest $ TE.decodeUtf8 result

-- | Merge with another revision
merge :: HgClient -> Maybe String -> [String] -> IO ()
merge client rev options = do
    let args = buildArgs "merge"
            (maybe [] (\r -> [("rev", Just r)]) rev ++ map (\o -> (o, Just "")) options) []
    
    void $ rawCommand client args

-- | Move/rename files
move :: HgClient -> [OsPath] -> OsPath -> [String] -> IO Bool
move client sources dest options = do
    destOption <- osPathToStringOption ("dest", Just dest)
    args <- buildArgsWithPaths "move"
            (destOption : map (\o -> (o, Just "")) options) sources
    
    result <- rawCommand client args
    return True

-- | Show outgoing changes
outgoing :: HgClient -> Maybe String -> [String] -> IO [Revision]
outgoing client path options = do
    let args = buildArgs "outgoing"
            (("template", Just "json") : maybe [] (\p -> [("path", Just p)]) path
             ++ map (\o -> (o, Just "")) options) []
    
    result <- rawCommand client args
    parseJsonRevisions $ TE.decodeUtf8 result

-- | Show parent revisions
parents :: HgClient -> [String] -> IO [Revision]
parents client options = do
    let args = buildArgs "parents"
            (("template", Just "json") : map (\o -> (o, Just "")) options) []
    
    result <- rawCommand client args
    parseJsonRevisions $ TE.decodeUtf8 result

-- | Show repository paths
paths :: HgClient -> Maybe String -> IO [(Text, Text)]
paths client name = do
    let args = buildArgs "paths" [] (maybe [] (\n -> [n]) name)
    result <- rawCommand client args
    return $ parsePaths $ TE.decodeUtf8 result

-- | Pull changes
pull :: HgClient -> Maybe String -> [String] -> IO Bool
pull client source options = do
    let args = buildArgs "pull"
            (maybe [] (\s -> [("source", Just s)]) source ++ map (\o -> (o, Just "")) options) []
    
    result <- rawCommand client args
    return True

-- | Push changes
push :: HgClient -> Maybe String -> [String] -> IO Bool
push client dest options = do
    let args = buildArgs "push"
            (maybe [] (\d -> [("dest", Just d)]) dest ++ map (\o -> (o, Just "")) options) []
    
    result <- rawCommand client args
    return True

-- | Remove files
remove :: HgClient -> [OsPath] -> [String] -> IO Bool
remove client files options = do
    args <- buildArgsWithPaths "remove" (map (\o -> (o, Just "")) options) files
    result <- rawCommand client args
    return True

-- | Resolve merge conflicts
resolve :: HgClient -> [OsPath] -> [String] -> IO [ResolveStatus]
resolve client files options = do
    args <- buildArgsWithPaths "resolve"
            (("list", Just "") : map (\o -> (o, Just "")) options) files
    
    result <- rawCommand client args
    parseResolve $ TE.decodeUtf8 result

-- | Revert files
revert :: HgClient -> [OsPath] -> [String] -> IO Bool
revert client files options = do
    args <- buildArgsWithPaths "revert" (map (\o -> (o, Just "")) options) files
    result <- rawCommand client args
    return True

-- | Show repository root
root :: HgClient -> IO OsPath
root client = do
    result <- rawCommand client ["root"]
    let pathStr = T.unpack $ T.strip $ TE.decodeUtf8 result
    osPathResult <- (try $ OsPath.encodeFS pathStr) :: IO (Either SomeException OsPath)
    case osPathResult of
        Left _ -> error $ "Invalid path from root command: " ++ pathStr
        Right osPath -> return osPath

-- | Show repository status
status :: HgClient -> StatusOptions -> IO [HgStatus]
status client StatusOptions{..} = do
    -- First get the repository root
    repoRoot <- root client

    -- Then run status command
    let args = buildArgs "status"
            [ ("rev", statusRev)
            , ("change", statusChange)
            , ("all", boolFlag statusAll)
            , ("modified", boolFlag statusModified)
            , ("added", boolFlag statusAdded)
            , ("removed", boolFlag statusRemoved)
            , ("deleted", boolFlag statusDeleted)
            , ("clean", boolFlag statusClean)
            , ("unknown", boolFlag statusUnknown)
            , ("ignored", boolFlag statusIgnored)
            , ("copies", boolFlag statusCopies)
            , ("subrepos", boolFlag statusSubrepos)
            , ("include", statusInclude)
            , ("exclude", statusExclude)
            , ("print0", Just "")
            ] []

    (result, _, _) <- runCommand client args
    logDebug $ "status: args: " ++ show args
    logDebug $ "calling parseStatusWithRoot with result"
    parseStatusWithRoot repoRoot result

-- | Add tags
tag :: HgClient -> [String] -> [String] -> IO ()
tag client names options = do
    let args = buildArgs "tag" (map (\o -> (o, Just "")) options) names
    void $ rawCommand client args

-- | List tags
tags :: HgClient -> IO [TagInfo]
tags client = do
    result <- rawCommand client ["tags", "--verbose"]
    return $ parseTags $ TE.decodeUtf8 result

-- | Show or set phase
phase :: HgClient -> [String] -> [String] -> IO [(Int, Text)]
phase client revs options = do
    let args = buildArgs "phase" (map (\o -> (o, Just "")) options) revs
    result <- rawCommand client args
    return $ parsePhase $ TE.decodeUtf8 result

-- | Show repository C.summary
summary :: HgClient -> [String] -> IO SummaryInfo
summary client options = do
    let args = buildArgs "C.summary" (map (\o -> (o, Just "")) options) []
    result <- rawCommand client args
    parseSummary $ TE.decodeUtf8 result

-- | Show tip revision
tip :: HgClient -> IO Revision
tip client = do
    result <- rawCommand client ["tip", "--template", "json"]
    revs <- parseJsonRevisions $ TE.decodeUtf8 result
    case revs of
        (rev:_) -> return rev
        [] -> throwHgError $ mkResponseError "No tip revision found"

-- | Update working directory
update :: HgClient -> UpdateOptions -> IO (Int, Int, Int, Int)
update client UpdateOptions{..} = do
    let args = buildArgs "update"
            [ ("rev", updateRev)
            , ("clean", boolFlag updateClean)
            , ("check", boolFlag updateCheck)
            , ("merge", boolFlag updateMerge)
            , ("date", updateDate)
            , ("tool", updateTool)
            ] []
    
    result <- rawCommand client args
    return $ parseUpdateResult $ TE.decodeUtf8 result

-- | Show Mercurial version
version :: HgClient -> IO (Int, Int, Int, String)
version client = do
    case clientVersion client of
        Just v -> return v
        Nothing -> do
            result <- rawCommand client ["version", "--quiet"]
            let v = parseVersion $ TE.decodeUtf8 result
            return v

-- Helper function for boolean flags
boolFlag :: Bool -> Maybe String
boolFlag True = Just ""
boolFlag False = Nothing
