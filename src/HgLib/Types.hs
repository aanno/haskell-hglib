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
    
    -- * Configuration
    , defaultConfig
    , defaultConfigWithPath
    
    -- * Utilities
    , parseRevision
    , formatRevision
    , statusCodeToChar
    , charToStatusCode
    , debugChannelCommands
    ) where

import Control.Exception (Exception)
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import System.IO (Handle)
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
    { hgPath :: !(Maybe FilePath)      -- ^ Repository path (Nothing for current directory)
    , hgEncoding :: !(Maybe String)    -- ^ Character encoding (Nothing for default)
    , hgConfigs :: ![String]           -- ^ Additional hg config options
    , hgHidden :: !Bool                -- ^ Include hidden changesets
    , hgHgPath :: !String              -- ^ Path to hg executable
    , hgDebug :: !Bool                 -- ^ Set `--logfile=-` is command supports that;
                                       -- ^ this will enable the debug channel
    } deriving stock (Show, Eq, Generic)

-- `--logfile` is supported on:
-- * commit
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
    "commit"
    , "backout"
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
defaultConfigWithPath :: FilePath -> HgConfig
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
    , statusFile :: !FilePath  -- ^ File path
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
    'M' -> Just $ HgStatus 'M' ""
    'A' -> Just $ HgStatus 'A' ""
    'R' -> Just $ HgStatus 'R' ""
    'C' -> Just $ HgStatus 'C' ""
    '!' -> Just $ HgStatus '!' ""
    '?' -> Just $ HgStatus '?' ""
    'I' -> Just $ HgStatus 'I' ""
    ' ' -> Just $ HgStatus ' ' ""
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
    { branchName :: !Text
    , branchRev :: !Int
    , branchNode :: !Text
    , branchActive :: !Bool
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
    , manifestPath :: !FilePath
    } deriving stock (Show, Eq, Generic)

-- | Line from annotate command
data AnnotationLine = AnnotationLine
    { annotationInfo :: !Text     -- ^ Revision/author/date info
    , annotationContent :: !Text  -- ^ Line content
    } deriving stock (Show, Eq, Generic)

-- | File resolve status for merge conflicts
data ResolveStatus = ResolveStatus
    { resolveCode :: !Char      -- ^ 'R' for resolved, 'U' for unresolved
    , resolvePath :: !FilePath  -- ^ File path
    } deriving stock (Show, Eq, Generic)

-- | Repository summary information
data SummaryInfo = SummaryInfo
    { summaryParents :: ![(Int, Text, Maybe Text, Maybe Text)]  -- ^ (rev, node, tags, message)
    , summaryBranch :: !Text
    , summaryCommitClean :: !Bool
    , summaryUpdateCount :: !Int
    , summaryRemote :: !(Maybe (Int, Int, Int, Int))  -- ^ (incoming, incoming bookmarks, outgoing, outgoing bookmarks)
    } deriving stock (Show, Eq, Generic)

-- | Parse a revision from a text representation
parseRevision :: Text -> Maybe Revision
parseRevision text = 
    -- This would parse various revision formats like:
    -- "123:abcdef123456" 
    -- "abcdef123456"
    -- "tip"
    -- etc.
    Nothing  -- TODO: Implement proper parsing

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
    , "summary:     " <> T.takeWhile (/= '\n') revDesc
    ]
