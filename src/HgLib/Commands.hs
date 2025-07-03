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
    
    -- * Command Options
    , AddOptions(..)
    , CommitOptions(..)
    , LogOptions(..)
    , StatusOptions(..)
    , DiffOptions(..)
    , defaultAddOptions
    , defaultCommitOptions
    , defaultLogOptions
    , defaultStatusOptions
    , defaultDiffOptions
    ) where

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

import HgLib.Types
import HgLib.Protocol
import HgLib.Error
import HgLib.Utils

-- | Options for the add command
data AddOptions = AddOptions
    { addDryRun :: !Bool
    , addSubrepos :: !Bool
    , addInclude :: !(Maybe String)
    , addExclude :: !(Maybe String)
    } deriving (Show, Eq)

defaultAddOptions :: AddOptions
defaultAddOptions = AddOptions False False Nothing Nothing

-- | Options for the commit command
data CommitOptions = CommitOptions
    { commitMessage :: !(Maybe String)
    , commitLogfile :: !(Maybe FilePath)
    , commitAddRemove :: !Bool
    , commitCloseBranch :: !Bool
    , commitDate :: !(Maybe String)
    , commitUser :: !(Maybe String)
    , commitInclude :: !(Maybe String)
    , commitExclude :: !(Maybe String)
    , commitAmend :: !Bool
    } deriving (Show, Eq)

defaultCommitOptions :: CommitOptions
defaultCommitOptions = CommitOptions Nothing Nothing False False Nothing Nothing Nothing Nothing False

-- | Options for the log command
data LogOptions = LogOptions
    { logRevRange :: !(Maybe String)
    , logFollow :: !Bool
    , logFollowFirst :: !Bool
    , logDate :: !(Maybe String)
    , logCopies :: !Bool
    , logKeyword :: !(Maybe String)
    , logRemoved :: !Bool
    , logOnlyMerges :: !Bool
    , logUser :: !(Maybe String)
    , logBranch :: !(Maybe String)
    , logPrune :: !(Maybe String)
    , logLimit :: !(Maybe Int)
    , logNoMerges :: !Bool
    , logInclude :: !(Maybe String)
    , logExclude :: !(Maybe String)
    } deriving (Show, Eq)

defaultLogOptions :: LogOptions
defaultLogOptions = LogOptions Nothing False False Nothing False Nothing False False Nothing Nothing Nothing Nothing False Nothing Nothing

-- | Options for the status command
data StatusOptions = StatusOptions
    { statusRev :: !(Maybe String)
    , statusChange :: !(Maybe String)
    , statusAll :: !Bool
    , statusModified :: !Bool
    , statusAdded :: !Bool
    , statusRemoved :: !Bool
    , statusDeleted :: !Bool
    , statusClean :: !Bool
    , statusUnknown :: !Bool
    , statusIgnored :: !Bool
    , statusCopies :: !Bool
    , statusSubrepos :: !Bool
    , statusInclude :: !(Maybe String)
    , statusExclude :: !(Maybe String)
    } deriving (Show, Eq)

defaultStatusOptions :: StatusOptions
defaultStatusOptions = StatusOptions Nothing Nothing False False False False False False False False False False Nothing Nothing

-- | Options for the diff command
data DiffOptions = DiffOptions
    { diffRevs :: ![String]
    , diffChange :: !(Maybe String)
    , diffText :: !Bool
    , diffGit :: !Bool
    , diffNoDates :: !Bool
    , diffShowFunction :: !Bool
    , diffReverse :: !Bool
    , diffIgnoreAllSpace :: !Bool
    , diffIgnoreSpaceChange :: !Bool
    , diffIgnoreBlankLines :: !Bool
    , diffUnified :: !(Maybe Int)
    , diffStat :: !Bool
    , diffSubrepos :: !Bool
    , diffInclude :: !(Maybe String)
    , diffExclude :: !(Maybe String)
    } deriving (Show, Eq)

defaultDiffOptions :: DiffOptions
defaultDiffOptions = DiffOptions [] Nothing False False False False False False False False Nothing False False Nothing Nothing

-- | Add files to the repository
add :: HgClient -> [FilePath] -> AddOptions -> IO Bool
add client files AddOptions{..} = do
    let args = buildArgs "add" 
            [ ("dry-run", boolFlag addDryRun)
            , ("subrepos", boolFlag addSubrepos)
            , ("include", addInclude)
            , ("exclude", addExclude)
            ] files
    
    (result, _, exitCode) <- runCommand client args
    return (exitCode == 0)

-- | Add new files and remove missing files
addRemove :: HgClient -> [FilePath] -> Maybe Int -> Bool -> Maybe String -> Maybe String -> IO Bool
addRemove client files similarity dryrun include exclude = do
    let args = buildArgs "addremove"
            [ ("similarity", show <$> similarity)
            , ("dry-run", boolFlag dryrun)
            , ("include", include)
            , ("exclude", exclude)
            ] files
    
    (result, _, exitCode) <- runCommand client args
    return (exitCode == 0)

-- | Show changeset information by line for each file
annotate :: HgClient -> [FilePath] -> Maybe String -> [String] -> IO [AnnotationLine]
annotate client files rev options = do
    let args = buildArgs "annotate"
            (("rev", rev) : map (\o -> (o, Just "")) options) files
    
    result <- rawCommand client args
    return $ parseAnnotationLines $ TE.decodeUtf8 result

-- | Create an archive of the repository
archive :: HgClient -> FilePath -> Maybe String -> [String] -> IO ()
archive client dest rev options = do
    let args = buildArgs "archive"
            (("dest", Just dest) : ("rev", rev) : map (\o -> (o, Just "")) options) []
    
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
branch :: HgClient -> Maybe String -> [String] -> IO Text
branch client name options = do
    let args = buildArgs "branch"
            (maybe [] (\n -> [("name", Just n)]) name ++ map (\o -> (o, Just "")) options) []
    
    result <- rawCommand client args
    return $ T.strip $ TE.decodeUtf8 result

-- | List branches
branches :: HgClient -> [String] -> IO [BranchInfo]
branches client options = do
    let args = buildArgs "branches" (map (\o -> (o, Just "")) options) []
    
    result <- rawCommand client args
    return $ parseBranches $ TE.decodeUtf8 result

-- | Create a bundle
bundle :: HgClient -> FilePath -> [String] -> IO Bool
bundle client file options = do
    let args = buildArgs "bundle"
            (("file", Just file) : map (\o -> (o, Just "")) options) []
    
    result <- rawCommand client args
    return True

-- | Show file contents at revision
cat :: HgClient -> [FilePath] -> Maybe String -> IO ByteString
cat client files rev = do
    let args = buildArgs "cat" [("rev", rev)] files
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
    let args = buildArgs "commit"
            [ ("message", commitMessage)
            , ("logfile", commitLogfile)
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
config :: HgClient -> [String] -> [String] -> IO [(Text, Text, Text)]
config client names options = do
    let args = buildArgs "showconfig" (map (\o -> (o, Just "")) options) names
    
    result <- rawCommand client args
    return $ parseConfig $ TE.decodeUtf8 result

-- | Copy files
copy :: HgClient -> [FilePath] -> FilePath -> [String] -> IO Bool
copy client sources dest options = do
    let args = buildArgs "copy"
            (("dest", Just dest) : map (\o -> (o, Just "")) options) sources
    
    result <- rawCommand client args
    return True

-- | Show differences
diff :: HgClient -> [FilePath] -> DiffOptions -> IO ByteString
diff client files DiffOptions{..} = do
    let args = buildArgs "diff"
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
forget :: HgClient -> [FilePath] -> [String] -> IO Bool
forget client files options = do
    let args = buildArgs "forget" (map (\o -> (o, Just "")) options) files
    result <- rawCommand client args
    return True

-- | Search for patterns
grep :: HgClient -> String -> [FilePath] -> [String] -> IO [(Text, Text)]
grep client pattern files options = do
    let args = buildArgs "grep"
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
import_ :: HgClient -> [FilePath] -> [String] -> IO ()
import_ client patches options = do
    let args = buildArgs "import" (map (\o -> (o, Just "")) options) patches
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
log_ :: HgClient -> [FilePath] -> LogOptions -> IO [Revision]
log_ client files LogOptions{..} = do
    let args = buildArgs "log"
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
    return $ parseManifest $ TE.decodeUtf8 result

-- | Merge with another revision
merge :: HgClient -> Maybe String -> [String] -> IO ()
merge client rev options = do
    let args = buildArgs "merge"
            (maybe [] (\r -> [("rev", Just r)]) rev ++ map (\o -> (o, Just "")) options) []
    
    void $ rawCommand client args

-- | Move/rename files
move :: HgClient -> [FilePath] -> FilePath -> [String] -> IO Bool
move client sources dest options = do
    let args = buildArgs "move"
            (("dest", Just dest) : map (\o -> (o, Just "")) options) sources
    
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
remove :: HgClient -> [FilePath] -> [String] -> IO Bool
remove client files options = do
    let args = buildArgs "remove" (map (\o -> (o, Just "")) options) files
    result <- rawCommand client args
    return True

-- | Resolve merge conflicts
resolve :: HgClient -> [FilePath] -> [String] -> IO [ResolveStatus]
resolve client files options = do
    let args = buildArgs "resolve"
            (("list", Just "") : map (\o -> (o, Just "")) options) files
    
    result <- rawCommand client args
    return $ parseResolve $ TE.decodeUtf8 result

-- | Revert files
revert :: HgClient -> [FilePath] -> [String] -> IO Bool
revert client files options = do
    let args = buildArgs "revert" (map (\o -> (o, Just "")) options) files
    result <- rawCommand client args
    return True

-- | Show repository root
root :: HgClient -> IO FilePath
root client = do
    result <- rawCommand client ["root"]
    return $ T.unpack $ T.strip $ TE.decodeUtf8 result

-- | Show repository status
status :: HgClient -> StatusOptions -> IO [HgStatus]
status client StatusOptions{..} = do
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
    
    result <- rawCommand client args
    return $ parseStatus result

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

-- | Show repository summary
summary :: HgClient -> [String] -> IO SummaryInfo
summary client options = do
    let args = buildArgs "summary" (map (\o -> (o, Just "")) options) []
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
update :: HgClient -> Maybe String -> [String] -> IO (Int, Int, Int, Int)
update client rev options = do
    let args = buildArgs "update"
            (maybe [] (\r -> [("rev", Just r)]) rev ++ map (\o -> (o, Just "")) options) []
    
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
