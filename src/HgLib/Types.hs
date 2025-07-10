{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HgLib.Types
    ( -- * Core Types
      HgClient(..)
    , HgConfig(..)
    , Revision(..)
    , HgStatus(..)
    , HgPhase(..)
    , BookmarkInfo(..)
    , BranchInfo(..)
    , TagInfo(..)
    , ManifestEntry(..)
    , AnnotationLine(..)
    , ResolveStatus(..)
    , SummaryInfo(..)

    -- * Command Options
    , AddOptions(..)
    , BranchesOptions(..)
    , BranchOptions(..)
    , CommitOptions(..)
    , ConfigOptions(..)
    , DiffOptions(..)
    , LogOptions(..)
    , StatusOptions(..)
    , UpdateOptions(..)
    
    -- * Configuration
    , defaultAddOptions
    , defaultBranchesOptions
    , defaultBranchOptions
    -- , defaultCommitOptions
    , mkDefaultCommitOptions
    , defaultConfig
    , defaultConfigOptions
    , defaultConfigWithPath
    , defaultDiffOptions
    , defaultLogOptions
    , defaultStatusOptions
    , defaultUpdateOptions
    
    -- * Utilities
    , parseRevision
    , formatRevision
    , statusCodeToChar
    , charToStatusCode
    , debugChannelCommands
    ) where

import Control.Exception (Exception)
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), object, (.=), eitherDecodeStrict)
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import System.IO (Handle)
import System.OsPath (OsPath)
import qualified System.OsPath as OsPath
import System.Process (ProcessHandle)

-- | Mercurial client handle containing connection and state information
data HgClient = HgClient
    { clientProcess :: !ProcessHandle
    , clientStdin :: !Handle
    , clientStdout :: !Handle
    , clientStderr :: !Handle
    , clientCapabilities :: ![ByteString]
    , clientEncoding :: !ByteString
    , clientConfig :: !HgConfig
    , clientVersion :: !(Maybe (Int, Int, Int, String))
    , clientDebug :: !Bool -- from hgDebug of HgConfig
    } deriving stock (Generic)

instance Show HgClient where
    show HgClient{..} = 
        "HgClient { capabilities = " ++ show clientCapabilities ++
        ", encoding = " ++ show clientEncoding ++
        ", config = " ++ show clientConfig ++
        ", version = " ++ show clientVersion ++
        ", debug = " ++ show clientDebug ++ " }"

-- | Configuration for creating an HgClient
data HgConfig = HgConfig
    { hgPath :: !(Maybe OsPath)        -- ^ Repository path (Nothing for current directory)
    , hgEncoding :: !(Maybe String)    -- ^ Character encoding (Nothing for default)
    , hgConfigs :: ![String]           -- ^ Additional hg config options
    , hgHidden :: !Bool                -- ^ Include hidden changesets
    , hgHgPath :: !String              -- ^ Path to hg executable
    , hgDebug :: !Bool                 -- ^ Set `--logfile=-` is command supports that;
                                       -- ^ this will enable the debug channel
    } deriving stock (Show, Eq, Generic)

-- `--logfile` is supported on:
-- * commit - Attention: abort: cannot specify both --message and --logfile, 
--            hence logfile is unsupported!
-- * backout
-- * import
-- * amend
-- * close-head
-- * fetch
-- * qcommit
-- * qnew
-- * qrefresh
-- * qfold
-- * qsave
-- * rebase
-- * record
-- * uncommit

debugChannelCommands :: [ByteString]
debugChannelCommands = [
    "backout"
    , "import"
    , "amend"
    , "close-head"
    , "fetch"
    , "qcommit"
    , "qnew"
    , "qrefresh"
    , "qfold"
    , "qsave"
    , "rebase"
    , "record"
    , "uncommit"
    ]

-- | Default configuration using current directory and system defaults
defaultConfig :: HgConfig
defaultConfig = HgConfig
    { hgPath = Nothing
    , hgEncoding = Nothing
    , hgConfigs = []
    , hgHidden = False
    , hgHgPath = "hg"
    , hgDebug = True
    }

-- | Default configuration with specified repository path
defaultConfigWithPath :: OsPath -> HgConfig
defaultConfigWithPath path = defaultConfig { hgPath = Just path }

-- | Represents a Mercurial revision with all metadata
data Revision = Revision
    { revRev :: !Text           -- ^ Local revision number
    , revNode :: !ByteString    -- ^ Global revision hash
    , revTags :: !Text          -- ^ Space-separated tags
    , revBranch :: !Text        -- ^ Branch name
    , revAuthor :: !Text        -- ^ Author information
    , revDesc :: !Text          -- ^ Commit description
    , revDate :: !UTCTime       -- ^ Commit timestamp
    } deriving stock (Show, Eq, Generic)

instance FromJSON Revision where
    parseJSON = withObject "Revision" $ \o -> do
        revRev <- o .: "rev"
        revNode <- TE.encodeUtf8 <$> o .: "node"
        revTags <- T.intercalate " " <$> o .: "tags"
        revBranch <- o .: "branch"
        revAuthor <- o .: "user"
        revDesc <- o .: "desc"
        dateArray <- o .: "date"
        revDate <- case dateArray of
            [timestamp, _timezone] -> return $ parseTimeFromUnix timestamp
            _ -> fail "Invalid date format"
        return Revision{..}
      where
        parseTimeFromUnix :: Double -> UTCTime
        parseTimeFromUnix = posixSecondsToUTCTime . realToFrac
        -- parseTimeFromUnix = error "TODO: implement proper time parsing"

instance ToJSON Revision where
    toJSON Revision{..} = object
        [ "rev" .= revRev
        , "node" .= TE.decodeUtf8 revNode
        , "tags" .= T.words revTags
        , "branch" .= revBranch
        , "user" .= revAuthor
        , "desc" .= revDesc
        , "date" .= formatTime defaultTimeLocale "%s" revDate
        ]

-- | File status in working directory
data HgStatus = HgStatus
    { statusCode :: !Char      -- ^ Status code (M, A, R, C, !, ?, I, or space)
    , statusFile :: !OsPath  -- ^ File path
    } deriving stock (Show, Eq, Generic)

-- | Convert status code character to human-readable description
statusCodeToChar :: Char -> Text
statusCodeToChar = \case
    'M' -> "modified"
    'A' -> "added"
    'R' -> "removed"
    'C' -> "clean"
    '!' -> "missing"
    '?' -> "untracked"
    'I' -> "ignored"
    ' ' -> "copy source"
    c   -> "unknown (" <> T.singleton c <> ")"

-- | Parse status code from character
charToStatusCode :: Char -> Maybe HgStatus -> Maybe HgStatus
charToStatusCode c mPrev = case c of
    'M' -> Just $ HgStatus 'M' mempty
    'A' -> Just $ HgStatus 'A' mempty
    'R' -> Just $ HgStatus 'R' mempty
    'C' -> Just $ HgStatus 'C' mempty
    '!' -> Just $ HgStatus '!' mempty
    '?' -> Just $ HgStatus '?' mempty
    'I' -> Just $ HgStatus 'I' mempty
    ' ' -> Just $ HgStatus ' ' mempty
    _   -> mPrev

-- | Changeset phase (public, draft, secret)
data HgPhase = PublicPhase | DraftPhase | SecretPhase
    deriving stock (Show, Eq, Ord, Enum, Generic)

instance FromJSON HgPhase where
    parseJSON v = do
        text <- parseJSON v
        case (text :: ByteString) of
            "public" -> return PublicPhase
            "draft"  -> return DraftPhase
            "secret" -> return SecretPhase
            _        -> fail $ "Unknown phase: " ++ show text

instance FromJSON ByteString where
    parseJSON v = do
        parseJSON v

instance ToJSON HgPhase where
    toJSON PublicPhase = "public"
    toJSON DraftPhase  = "draft"
    toJSON SecretPhase = "secret"

-- | Bookmark information
data BookmarkInfo = BookmarkInfo
    { bookmarkName :: !Text
    , bookmarkRev :: !Int
    , bookmarkNode :: !Text
    , bookmarkActive :: !Bool
    } deriving stock (Show, Eq, Generic)

-- | Branch information  
data BranchInfo = BranchInfo
    { branchInfoName :: !Text
    , branchInfoRev :: !Int
    , branchInfoNode :: !Text
    , branchInfoActive :: !Bool
    } deriving stock (Show, Eq, Generic)

-- | Tag information
data TagInfo = TagInfo
    { tagName :: !Text
    , tagRev :: !Int
    , tagNode :: !Text
    , tagLocal :: !Bool
    } deriving stock (Show, Eq, Generic)

-- | Manifest entry (file in repository at specific revision)
data ManifestEntry = ManifestEntry
    { manifestNode :: !Text
    , manifestPermissions :: !Text
    , manifestExecutable :: !Bool
    , manifestSymlink :: !Bool
    , manifestPath :: !OsPath
    } deriving stock (Show, Eq, Generic)

-- | Line from annotate command
data AnnotationLine = AnnotationLine
    { annotationInfo :: !Text        -- ^ Revision/author/date info
    , annotationContent :: !ByteString  -- ^ Line content (unknown encoding)
    } deriving stock (Show, Eq, Generic)

-- | File resolve status for merge conflicts
data ResolveStatus = ResolveStatus
    { resolveCode :: !Char      -- ^ 'R' for resolved, 'U' for unresolved
    , resolvePath :: !OsPath  -- ^ File path
    } deriving stock (Show, Eq, Generic)

-- | Repository C.summary information
data SummaryInfo = SummaryInfo
    { summaryParents :: ![(Int, Text, Maybe Text, Maybe Text)]  -- ^ (rev, node, tags, message)
    , summaryBranch :: !Text
    , summaryCommitClean :: !Bool
    , summaryUpdateCount :: !Int
    , summaryRemote :: !(Maybe (Int, Int, Int, Int))  -- ^ (incoming, incoming bookmarks, outgoing, outgoing bookmarks)
    } deriving stock (Show, Eq, Generic)

-- | Parse a revision from a text representation
-- | Parse a single revision from a JSON Text (array of objects).
parseRevision :: Text -> Maybe Revision
parseRevision t =
    case eitherDecodeStrict (BS8.pack $ T.unpack t) of
        Right (rev:_) -> Just rev
        _             -> Nothing

-- | Format a revision for display (short form)
formatRevision :: Revision -> Text
formatRevision Revision{..} = 
    revRev <> ":" <> T.take 12 (TE.decodeUtf8 revNode)

-- | Format a revision for display (long form)
formatRevisionLong :: Revision -> Text
formatRevisionLong Revision{..} = T.unlines
    [ "changeset:   " <> revRev <> ":" <> TE.decodeUtf8 revNode
    , "branch:      " <> revBranch
    , "user:        " <> revAuthor
    , "date:        " <> T.pack (formatTime defaultTimeLocale "%c" revDate)
    , "C.summary:     " <> T.takeWhile (/= '\n') revDesc
    ]

-- | Options for the add command
data AddOptions = AddOptions
    { addFiles :: ![String]               -- ^ file argument
    , addDryRun :: !Bool
    , addSubrepos :: !Bool
    , addInclude :: !(Maybe String)
    , addExclude :: !(Maybe String)
    } deriving (Show, Eq)

defaultAddOptions :: AddOptions
defaultAddOptions = AddOptions [] False False Nothing Nothing

-- | Options for the commit command
data CommitOptions = CommitOptions
    { commitFiles :: ![String]            -- ^ file argument
    , commitMessage :: !String            -- mandatory
    , commitLogfile :: !(Maybe OsPath)
    , commitAddRemove :: !Bool
    , commitCloseBranch :: !Bool
    , commitDate :: !(Maybe String)
    , commitUser :: !(Maybe String)
    , commitInclude :: !(Maybe String)
    , commitExclude :: !(Maybe String)
    , commitAmend :: !Bool
    } deriving (Show, Eq)

-- should NOT be exported
mkDefaultCommitOptions :: String -> CommitOptions
mkDefaultCommitOptions msg = defaultCommitOptions { commitMessage = msg }

defaultCommitOptions :: CommitOptions
defaultCommitOptions = CommitOptions [] "" Nothing False False Nothing Nothing Nothing Nothing False

-- | Options for the log command
data LogOptions = LogOptions
    { logFiles :: ![String]               -- ^ file argument
    , logRevRange :: !(Maybe String)
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
defaultLogOptions = LogOptions [] Nothing False False Nothing False Nothing False False Nothing Nothing Nothing Nothing False Nothing Nothing

-- | Options for the status command
data StatusOptions = StatusOptions
    { statusFiles :: ![String]
    , statusRev :: !(Maybe String)
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
defaultStatusOptions = StatusOptions [] Nothing Nothing False False False False False False False False False False Nothing Nothing

-- | Options for the diff command
data DiffOptions = DiffOptions
    { diffFiles :: ![String]
    , diffRevs :: ![String]
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
defaultDiffOptions = DiffOptions [] [] Nothing False False False False False False False False Nothing False False Nothing Nothing

-- | Options for the update command
data UpdateOptions = UpdateOptions
    { updateRev :: !(Maybe String)       -- ^ -r --rev REV
    , updateClean :: !Bool               -- ^ -C --clean
    , updateCheck :: !Bool               -- ^ -c --check  
    , updateMerge :: !Bool               -- ^ -m --merge
    , updateDate :: !(Maybe String)      -- ^ -d --date DATE
    , updateTool :: !(Maybe String)      -- ^ -t --tool TOOL
    } deriving (Show, Eq)

defaultUpdateOptions :: UpdateOptions
defaultUpdateOptions = UpdateOptions Nothing False False False Nothing Nothing

-- | Options for the branch command
data BranchOptions = BranchOptions
    { branchName :: !(Maybe String)      -- ^ Branch name argument
    , branchForce :: !Bool               -- ^ -f --force
    , branchClean :: !Bool               -- ^ -C --clean
    } deriving (Show, Eq)

defaultBranchOptions :: BranchOptions
defaultBranchOptions = BranchOptions Nothing False False

-- | Options for the branches command  
data BranchesOptions = BranchesOptions
    { branchesRev :: ![String]           -- ^ -r --rev VALUE [+]
    , branchesClosed :: !Bool            -- ^ -c --closed
    , branchesTemplate :: !(Maybe String) -- ^ -T --template TEMPLATE
    } deriving (Show, Eq)

defaultBranchesOptions :: BranchesOptions
defaultBranchesOptions = BranchesOptions [] False Nothing

-- | Options for the config/showconfig command
data ConfigOptions = ConfigOptions
    { configNames :: ![String]           -- ^ NAME arguments
    , configUntrusted :: !Bool           -- ^ -u --untrusted
    , configEdit :: !Bool                -- ^ -e --edit
    , configLocal :: !Bool               -- ^ -l --local
    , configSource :: !Bool              -- ^ --source
    , configGlobal :: !Bool              -- ^ -g --global
    , configTemplate :: !(Maybe String) -- ^ -T --template TEMPLATE
    } deriving (Show, Eq)

defaultConfigOptions :: ConfigOptions
defaultConfigOptions = ConfigOptions [] False False False False False Nothing
