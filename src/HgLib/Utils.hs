{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module HgLib.Utils
    ( -- * Command Building
      buildArgs
    , buildArgList
    
    -- * Parsing Functions
    , parseJsonRevisions
    , parseAnnotationLines
    , parseBookmarks
    , parseBranches
    , parseConfig
    , parseGrepResults
    , parseManifest
    , parsePaths
    , parseResolve
    , parseTags
    , parsePhase
    , parseStatusWithRoot
    , parseCommitResult
    , parseUpdateResult
    , parseVersion
    , parseSummary
    
    -- * Text Utilities
    , textToByteString
    , byteStringToText
    , splitLines
    , stripEmptyLines
    , parseKeyValue
    
    -- * Time Utilities
    , parseHgTime
    , formatHgTime
    ) where

import Control.Monad (mzero)
import Data.Aeson (decode, Value(..), Object, (.:))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
-- import Text.Read (readMaybe)
import Data.Aeson (Value(..), Object, Array, decode, (.:), Result(..), withObject)
import Data.Aeson.Types (parse)
import qualified Data.Vector as V
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.FilePath (takeFileName, normalise, makeRelative)
import Data.List (isInfixOf)
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (isSpace, isDigit)
import Data.Maybe (mapMaybe)

import HgLib.Types
import Logging

-- | Build command arguments from options and positional arguments
buildArgs :: String -> [(String, Maybe String)] -> [String] -> [ByteString]
buildArgs cmd options positional = 
    let cmdArg = TE.encodeUtf8 $ T.pack cmd
        optArgs = concatMap buildOption options
        posArgs = map (TE.encodeUtf8 . T.pack) positional
    in cmdArg : optArgs ++ posArgs

-- | Build a single command line argument from option
buildOption :: (String, Maybe String) -> [ByteString]
buildOption (name, value) = case value of
    Nothing -> []
    Just "" -> [TE.encodeUtf8 $ T.pack $ "--" ++ name]
    Just val -> [TE.encodeUtf8 $ T.pack $ "--" ++ name ++ "=" ++ val]

-- | Build argument list for commands that take multiple values
buildArgList :: String -> [String] -> [String] -> [ByteString]
buildArgList cmd options args = map (TE.encodeUtf8 . T.pack) (cmd : options ++ args)

-- | Parse JSON revisions from command output  
parseJsonRevisions :: Text -> IO [Revision]
parseJsonRevisions text = 
    case decode (LBS.fromStrict $ TE.encodeUtf8 text) of
        Nothing -> do
            putStrLn $ "Failed to parse JSON: " ++ T.unpack text
            return []
        Just (Array revArray) -> return $ mapMaybe parseJsonRevision $ V.toList revArray
        Just _ -> return []

-- | Parse a single JSON revision object
parseJsonRevision :: Value -> Maybe Revision
parseJsonRevision val = 
    case parse parseRev val of
        Success rev -> Just rev
        Error _ -> Nothing
  where
    parseRev = withObject "Revision" $ \obj -> do
        rev <- obj .: "rev"
        node <- obj .: "node"
        tags <- obj .: "tags"
        branch <- obj .: "branch"
        author <- obj .: "user"
        desc <- obj .: "desc"
        dateArray <- obj .: "date"
        
        timestamp <- case dateArray of
            Array arr | V.length arr >= 1 -> 
                case V.head arr of
                    Number n -> return $ realToFrac n
                    _ -> fail "Invalid timestamp"
            _ -> fail "Invalid date format"
        
        let utcTime = posixSecondsToUTCTime timestamp
            tagsText = T.intercalate " " tags
        
        return $ Revision
            { revRev = T.pack $ show (rev :: Int)
            , revNode = TE.encodeUtf8 node
            , revTags = tagsText
            , revBranch = branch
            , revAuthor = author
            , revDesc = desc
            , revDate = utcTime
            }

-- | Parse annotation lines from annotate command output
parseAnnotationLines :: Text -> [AnnotationLine]
parseAnnotationLines = mapMaybe parseAnnotationLine . T.lines
  where
    parseAnnotationLine line = 
        case T.splitOn ": " line of
            [info, content] -> Just $ AnnotationLine info content
            _ -> Nothing

-- | Parse bookmarks from bookmarks command output
parseBookmarks :: Text -> [BookmarkInfo]
parseBookmarks = mapMaybe parseBookmark . T.lines
  where
    parseBookmark line =
        let (marker, rest) = T.splitAt 3 line
            isActive = "*" `T.isInfixOf` marker
            parts = T.words rest
        in case parts of
            (name:revNode:_) -> case T.splitOn ":" revNode of
                [revStr, node] -> case readMaybe (T.unpack revStr) of
                    Just rev -> Just $ BookmarkInfo name rev node isActive
                    Nothing -> Nothing
                _ -> Nothing
            _ -> Nothing

-- | Parse branches from branches command output
parseBranches :: Text -> [BranchInfo]
parseBranches = mapMaybe parseBranch . T.lines
  where
    parseBranch line = 
        let parts = T.words line
        in case reverse parts of
            (nodeInfo:revStr:nameWords) -> 
                let name = T.unwords (reverse nameWords)
                    node = T.takeWhile (/= ' ') nodeInfo
                in case readMaybe (T.unpack revStr) of
                    Just rev -> Just $ BranchInfo name rev node True
                    Nothing -> Nothing
            _ -> Nothing

-- | Parse configuration from showconfig command output
parseConfig :: Text -> [(Text, Text, Text)]
parseConfig = mapMaybe parseConfigLine . T.lines
  where
    parseConfigLine line = 
        case T.splitOn "=" line of
            [key, value] -> case T.splitOn "." key of
                [section, name] -> Just (section, name, value)
                _ -> Nothing
            _ -> Nothing

-- | Parse grep results from grep command output
parseGrepResults :: Text -> [(Text, Text)]
parseGrepResults = map (\line -> (line, "")) . T.lines  -- Simplified for now

-- | Parse manifest entries from manifest command output
parseManifest :: Text -> [ManifestEntry]
parseManifest = mapMaybe parseManifestLine . T.lines
  where
    parseManifestLine line 
        | T.length line >= 47 =
            let node = T.take 40 line
                perm = T.take 3 $ T.drop 41 line
                flag = T.index line 45
                path = T.unpack $ T.drop 47 line
                executable = flag == '*'
                symlink = flag == '@'
            in Just $ ManifestEntry node perm executable symlink path
        | otherwise = Nothing

-- | Parse paths from paths command output
parsePaths :: Text -> [(Text, Text)]
parsePaths = mapMaybe parsePath . T.lines
  where
    parsePath line = 
        case T.splitOn " = " line of
            [name, url] -> Just (name, url)
            _ -> Nothing

-- | Parse resolve status from resolve command output
parseResolve :: Text -> [ResolveStatus]
parseResolve = mapMaybe parseResolveLine . T.lines
  where
    parseResolveLine line = 
        case T.uncons line of
            Just (code, rest) -> Just $ ResolveStatus code (T.unpack $ T.strip rest)
            Nothing -> Nothing

-- | Parse tags from tags command output
parseTags :: Text -> [TagInfo]
parseTags = mapMaybe parseTag . T.lines
  where
    parseTag line = 
        let isLocal = " local" `T.isSuffixOf` line
            line' = if isLocal then T.dropEnd 6 line else line
            parts = T.words line'
        in case reverse parts of
            (revNode:nameWords) -> 
                let name = T.unwords (reverse nameWords)
                in case T.splitOn ":" revNode of
                    [revStr, node] -> case readMaybe (T.unpack revStr) of
                        Just rev -> Just $ TagInfo name rev node isLocal
                        Nothing -> Nothing
                    _ -> Nothing
            _ -> Nothing

-- | Parse phase information from phase command output
parsePhase :: Text -> [(Int, Text)]
parsePhase = mapMaybe parsePhaseLine . T.lines
  where
    parsePhaseLine line = 
        case T.splitOn ": " line of
            [revStr, phase] -> case readMaybe (T.unpack revStr) of
                Just rev -> Just (rev, phase)
                Nothing -> Nothing
            _ -> Nothing

-- | Parse status relative to repository root
parseStatusWithRoot :: FilePath -> ByteString -> [HgStatus]
parseStatusWithRoot repoRoot = mapMaybe parseStatusLine . BS8.split '\0'
  where
    parseStatusLine line 
        | BS.length line >= 2 =
            let code = BS8.head line
                rawPath = BS8.unpack $ BS.drop 2 line
                cleanPath = takeWhile (`notElem` ['\n', '\r']) rawPath
                -- Extract everything after the last occurrence of repoRoot/
                relativePath = extractRelative (repoRoot ++ "/") cleanPath
            in Just $ HgStatus code relativePath
        | otherwise = Nothing

    extractRelative prefix fullPath =
        case findLastIndex prefix fullPath of
            Just idx -> drop (idx + length prefix) fullPath
            Nothing -> takeFileName fullPath  -- fallback to just filename

    findLastIndex needle haystack =
        let indices = [i | i <- [0..length haystack - length needle],
                          take (length needle) (drop i haystack) == needle]
        in case indices of
            [] -> Nothing
            _ -> Just (last indices)

-- | Parse commit result to extract revision number and node
parseCommitResult :: Text -> IO (Int, Text)
parseCommitResult text = 
    case T.lines text of
        [] -> return (0, "")
        (line:_) -> 
            let words' = T.words line
            in case words' of
                (_:_:rev:node:_) -> 
                    case readMaybe (T.unpack rev) of
                        Just r -> return (r, node)
                        Nothing -> return (0, "")
                _ -> return (0, "")

-- | Parse update result to extract file counts
parseUpdateResult :: Text -> (Int, Int, Int, Int)
parseUpdateResult text = 
    case T.lines text of
        [] -> (0, 0, 0, 0)
        (line:_) -> 
            let numbers = mapMaybe (readMaybe . T.unpack) (T.words line)
            in case numbers of
                [a, b, c, d] -> (a, b, c, d)
                _ -> (0, 0, 0, 0)

-- | Parse version information
parseVersion :: Text -> (Int, Int, Int, String)
parseVersion text = 
    let cleanText = T.strip text
        parts = T.splitOn "." cleanText
    in case parts of
        (major:minor:rest) -> 
            let majorNum = fromMaybe 0 $ readMaybe $ T.unpack major
                minorNum = fromMaybe 0 $ readMaybe $ T.unpack minor
                (microNum, extra) = case rest of
                    [] -> (0, "")
                    (micro:_) -> 
                        let microStr = T.unpack micro
                            (digits, suffix) = span (`elem` ['0' .. '9']) microStr
                        in (fromMaybe 0 $ readMaybe digits, suffix)
            in (majorNum, minorNum, microNum, extra)
        _ -> (0, 0, 0, T.unpack cleanText)

-- | Parses the parent field from hg summary output.
parseParents :: Maybe Text -> [(Int, Text, Maybe Text, Maybe Text)]
parseParents Nothing = []
parseParents (Just txt) =
    mapMaybe parseParentLine (T.lines txt)
  where
    -- Parses a single parent line.
    parseParentLine :: Text -> Maybe (Int, Text, Maybe Text, Maybe Text)
    parseParentLine line =
        let line' = T.strip line
            -- Find the revision and node (format: rev:node)
            (revNode, rest) = T.break isSpace line'
            (revTxt, nodeTxt) = T.breakOn ":" revNode
            rev = readMaybe (T.unpack revTxt) :: Maybe Int
            node = T.drop 1 nodeTxt  -- remove the ':'
            rest' = T.strip rest
            -- Extract tag/bookmark if present in parentheses at end
            (msgPart, tagPart) = T.breakOnEnd "(" rest'
            tags = if T.null tagPart
                   then Nothing
                   else Just . T.strip . T.dropAround (\c -> c == '(' || c == ')') $ tagPart
            -- Remove tag from message, if present
            message = let m = T.strip . T.dropWhileEnd (/= ')') $ rest'
                      in if T.null m then Just rest' else Just (T.strip $ T.dropEnd (T.length tagPart) rest')
        in case rev of
            Just r -> Just (r, node, tags, message)
            Nothing -> Nothing

-- Safe read for Int
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

-- | Parse summary information from summary command output
parseSummary :: Text -> IO SummaryInfo
parseSummary text = do
    let lines' = T.lines text
        summaryMap = parseKeyValueLines lines'
    
    let parents = parseParents $ lookup "parent" summaryMap
        branch = fromMaybe "default" $ lookup "branch" summaryMap
        commitClean = maybe False ("(clean)" `T.isInfixOf`) $ lookup "commit" summaryMap
        updateCount = maybe 0 parseUpdateCount $ lookup "update" summaryMap
        remote = parseRemote $ lookup "remote" summaryMap
    
    return SummaryInfo {
        summaryParents = parents
        , summaryBranch = branch
        , summaryCommitClean = commitClean
        , summaryUpdateCount = updateCount
        , summaryRemote = remote
    }
  where
    parseUpdateCount text 
        | "(current)" `T.isInfixOf` text = 0
        | otherwise = fromMaybe 0 $ readMaybe . T.unpack . T.takeWhile (/= ' ') $ text
    
    parseRemote Nothing = Nothing
    parseRemote (Just text)
        | "(synced)" `T.isInfixOf` text = Just (0, 0, 0, 0)
        | otherwise = Nothing  -- TODO: Parse complex remote status

-- | Parse key-value lines from command output
parseKeyValueLines :: [Text] -> [(Text, Text)]
parseKeyValueLines = mapMaybe parseKeyValue

-- | Parse a single key-value line
parseKeyValue :: Text -> Maybe (Text, Text)
parseKeyValue line = 
    case T.splitOn ": " line of
        [key, value] -> Just (key, value)
        _ -> Nothing

-- | Convert Text to ByteString using UTF-8
textToByteString :: Text -> ByteString
textToByteString = TE.encodeUtf8

-- | Convert ByteString to Text using UTF-8
byteStringToText :: ByteString -> Text
byteStringToText = TE.decodeUtf8

-- | Split text into lines
splitLines :: Text -> [Text]
splitLines = T.lines

-- | Remove empty lines from a list
stripEmptyLines :: [Text] -> [Text]
stripEmptyLines = filter (not . T.null . T.strip)

-- | Parse Mercurial timestamp format
parseHgTime :: Text -> Maybe UTCTime
parseHgTime timeStr = 
    case T.words timeStr of
        [timestamp, _timezone] -> 
            case readMaybe (T.unpack timestamp) of
                Just (seconds :: Double) -> Just $ posixSecondsToUTCTime $ realToFrac seconds
                Nothing -> Nothing
        _ -> parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" (T.unpack timeStr)

-- | Format UTCTime for Mercurial
formatHgTime :: UTCTime -> Text
formatHgTime time = T.pack $ show $ round $ utcTimeToPOSIXSeconds time

-- | Safe read function that returns Maybe
safeRead :: Read a => String -> Maybe a
safeRead s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

-- | Parse space-separated integers
parseIntegers :: Text -> [Int]
parseIntegers = mapMaybe (readMaybe . T.unpack) . T.words

-- | Join text with spaces
joinWithSpaces :: [Text] -> Text
joinWithSpaces = T.intercalate " "

-- | Trim whitespace and filter empty strings
cleanLines :: [Text] -> [Text]
cleanLines = filter (not . T.null) . map T.strip

-- | Parse boolean from Mercurial output
parseHgBool :: Text -> Bool
parseHgBool text = 
    let lower = T.toLower $ T.strip text
    in lower `elem` ["true", "yes", "1", "on"]

-- | Format boolean for Mercurial
formatHgBool :: Bool -> Text
formatHgBool True = "true"
formatHgBool False = "false"

-- | Extract revision number from various formats
extractRevision :: Text -> Maybe Int
extractRevision text = 
    case T.splitOn ":" text of
        [rev, _] -> readMaybe $ T.unpack rev
        _ -> readMaybe $ T.unpack text

-- | Extract node hash from various formats  
extractNode :: Text -> Maybe Text
extractNode text = 
    case T.splitOn ":" text of
        [_, node] -> Just node
        _ -> if T.length text >= 12 then Just text else Nothing

-- | Escape shell arguments (basic implementation)
escapeShellArg :: String -> String
escapeShellArg arg = "'" ++ concatMap escape arg ++ "'"
  where
    escape '\'' = "'\"'\"'"
    escape c = [c]

-- | Parse error messages to extract useful information
parseErrorMessage :: ByteString -> Text
parseErrorMessage = T.strip . TE.decodeUtf8

-- | Check if output indicates success
isSuccessOutput :: ByteString -> Bool
isSuccessOutput = BS.null . BS8.strip

-- | Parse numeric range (e.g., "1:5" -> [1,2,3,4,5])
parseRange :: Text -> [Int]
parseRange range = 
    case T.splitOn ":" range of
        [start, end] -> case (readMaybe $ T.unpack start, readMaybe $ T.unpack end) of
            (Just s, Just e) -> [s..e]
            _ -> []
        _ -> maybe [] (:[]) $ readMaybe $ T.unpack range
