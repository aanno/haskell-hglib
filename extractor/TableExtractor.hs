{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Char (toUpper, toLower, isAlpha, isAlphaNum)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)
import Control.Exception (try, SomeException)

-- | Represents a command from the markdown table
data HgCommand = HgCommand
    { cmdName :: !Text
    , cmdMandatoryArgs :: ![Text]
    , cmdOptionalFlags :: ![Flag]
    , cmdSideEffects :: !Text
    , cmdOutputFormat :: !Text
    , cmdPossibleErrors :: !Text
    , cmdReturnValue :: !Text
    } deriving (Show, Eq)

-- | Represents a command line flag
data Flag = Flag
    { flagShort :: !(Maybe Text)    -- ^ Short form (e.g., "f")
    , flagLong :: !Text             -- ^ Long form (e.g., "force")
    , flagType :: !FlagType         -- ^ Type of flag
    , flagDesc :: !(Maybe Text)     -- ^ Description/argument name
    } deriving (Show, Eq)

-- | Types of flags
data FlagType
    = BooleanFlag     -- ^ Simple boolean flag
    | StringFlag      -- ^ Takes a string argument
    | MultiStringFlag -- ^ Takes multiple string arguments
    deriving (Show, Eq)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile] -> processTable inputFile
        _ -> do
            hPutStrLn stderr "Usage: table-extractor-exe <input-file>"
            exitFailure

-- | Process the markdown table file
processTable :: FilePath -> IO ()
processTable inputFile = do
    content <- TIO.readFile inputFile
    commands <- mapM enhanceCommandWithHelpInfo =<< return (parseMarkdownTable content)
    
    putStrLn "-- Generated Option Types for Types.hs"
    mapM_ generateOptionType commands
    
    putStrLn "\n-- Generated Default Constructors for Types.hs"
    mapM_ generateDefaultConstructor commands
    
    putStrLn "\n-- Generated buildArgs Patterns for Commands.hs"
    mapM_ generateBuildArgsPattern commands

-- | Enhance command with information from hg command -h
enhanceCommandWithHelpInfo :: HgCommand -> IO HgCommand
enhanceCommandWithHelpInfo cmd = do
    helpInfo <- getHgCommandHelp (cmdName cmd)
    let enhancedFlags = enhanceFlags (cmdOptionalFlags cmd) helpInfo
    return cmd { cmdOptionalFlags = enhancedFlags }

-- | Get help information for a command
getHgCommandHelp :: Text -> IO (Maybe Text)
getHgCommandHelp cmdName = do
    result <- try $ readProcess "hg" [T.unpack cmdName, "-h"] ""
    case result of
        Right output -> return $ Just $ T.pack output
        Left (_ :: SomeException) -> do
            hPutStrLn stderr $ "Warning: Could not get help for command: " ++ T.unpack cmdName
            return Nothing

-- | Enhance flags with type information from help output
enhanceFlags :: [Flag] -> Maybe Text -> [Flag]
enhanceFlags flags Nothing = flags
enhanceFlags flags (Just helpText) = 
    let helpFlags = parseHelpFlags helpText
    in map (enhanceFlagWithHelp helpFlags) flags

-- | Parse flags from help output
parseHelpFlags :: Text -> [(Text, FlagType)]
parseHelpFlags helpText = 
    let optionsSection = extractOptionsSection helpText
        optionLines = filter isOptionLine $ T.lines optionsSection
    in mapMaybe parseHelpOptionLine optionLines

-- | Extract the options section from help text
extractOptionsSection :: Text -> Text
extractOptionsSection helpText = 
    let textLines = T.lines helpText
        optionsStart = dropWhile (not . T.isInfixOf "options") textLines
    in T.unlines $ takeWhile (not . T.null) $ drop 1 optionsStart

-- | Check if a line is an option line
isOptionLine :: Text -> Bool
isOptionLine line = 
    let trimmed = T.strip line
    in T.isPrefixOf " -" trimmed || T.isPrefixOf "    --" trimmed

-- | Parse a single help option line
parseHelpOptionLine :: Text -> Maybe (Text, FlagType)
parseHelpOptionLine line = 
    let trimmed = T.strip line
        -- Handle continuation lines that are indented
        isMainOptionLine = T.isPrefixOf " -" trimmed || T.isPrefixOf "    --" trimmed
    in if not isMainOptionLine
       then Nothing
       else
         let words' = T.words trimmed
         in case words' of
             [] -> Nothing
             (flagPart:rest) -> do
                 flagName <- extractLongFlagName flagPart
                 let flagType = determineFlagType (flagPart:rest)
                 return (flagName, flagType)

-- | Extract long flag name from flag part (e.g., "-m --message" -> "message")
extractLongFlagName :: Text -> Maybe Text
extractLongFlagName flagPart = 
    let parts = T.words flagPart
        longFlags = filter (T.isPrefixOf "--") parts
    in case longFlags of
        [] -> 
            -- Handle case where only short flag exists
            let shortFlags = filter (T.isPrefixOf "-") parts
            in case shortFlags of
                [] -> Nothing
                (shortFlag:_) -> Just $ T.drop 1 shortFlag
        (longFlag:_) -> Just $ T.drop 2 longFlag

-- | Determine flag type from remaining words
determineFlagType :: [Text] -> FlagType
determineFlagType words'
    | any isArgumentPlaceholder words' = 
        if any isMultiValueIndicator words' 
        then MultiStringFlag 
        else StringFlag
    | otherwise = BooleanFlag
  where
    -- Check for uppercase placeholders like TEXT, DATE, FILE, etc.
    isArgumentPlaceholder w = 
        T.all (`elem` (['A'..'Z'] ++ ['0'..'9'])) w && T.length w > 1
    -- Check for multi-value indicators
    isMultiValueIndicator w = 
        T.isSuffixOf "[+]" w || T.isSuffixOf "..." w || 
        T.isInfixOf "[+]" w || w == "[+]"

-- | Enhance a flag with help information
enhanceFlagWithHelp :: [(Text, FlagType)] -> Flag -> Flag
enhanceFlagWithHelp helpFlags flag = 
    case lookup (flagLong flag) helpFlags of
        Just newType -> flag { flagType = newType }
        Nothing -> flag { flagType = hardcodedFlagType (flagLong flag) }

-- | Hardcoded flag type mappings for common patterns
hardcodedFlagType :: Text -> FlagType
hardcodedFlagType flagName
    | flagName `elem` stringFlags = StringFlag
    | flagName `elem` multiStringFlags = MultiStringFlag
    | otherwise = BooleanFlag
  where
    stringFlags = 
        [ "message", "logfile", "date", "user", "rev", "change", "template"
        , "name", "tool", "output", "file", "dest", "source", "type", "prefix"
        , "similarity", "base", "branch", "bookmark", "keyword", "pattern"
        , "limit", "unified", "encoding", "style", "remotecmd", "ssh"
        , "address", "port", "certificate", "pid-file", "daemon-postexec"
        , "accesslog", "errorlog", "webdir-conf", "web-conf", "templates"
        ]
    multiStringFlags =
        [ "include", "exclude", "configs", "revs", "branches", "bookmarks"
        , "tags", "prune", "files", "patterns", "keywords", "users"
        ]

-- | Parse the markdown table into HgCommand records
parseMarkdownTable :: Text -> [HgCommand]
parseMarkdownTable content = 
    let tableLines = filter isTableRow $ T.lines content
        dataRows = drop 2 tableLines  -- Skip header and separator
    in mapMaybe parseTableRow dataRows

-- | Check if a line is part of the table
isTableRow :: Text -> Bool
isTableRow line = T.isPrefixOf "|" (T.strip line) && not (T.isPrefixOf "|-" (T.strip line))

-- | Parse a single table row
parseTableRow :: Text -> Maybe HgCommand
parseTableRow line = 
    let columns = map T.strip $ T.split (== '|') line
        cleanColumns = filter (not . T.null) columns
    in case cleanColumns of
        [cmd, mandArgs, optFlags, sideEffects, outputFormat, errors, returnVal] -> 
            Just HgCommand
                { cmdName = T.strip $ T.filter (/= '`') cmd
                , cmdMandatoryArgs = parseMandatoryArgs mandArgs
                , cmdOptionalFlags = parseOptionalFlags optFlags
                , cmdSideEffects = sideEffects
                , cmdOutputFormat = outputFormat
                , cmdPossibleErrors = errors
                , cmdReturnValue = returnVal
                }
        _ -> Nothing

-- | Parse mandatory arguments
parseMandatoryArgs :: Text -> [Text]
parseMandatoryArgs text
    | T.strip text == "None" = []
    | otherwise = map T.strip $ T.split (== ',') text

-- | Parse optional flags from the flags column
parseOptionalFlags :: Text -> [Flag]
parseOptionalFlags text
    | T.strip text == "None" = []
    | otherwise = 
        let flagStrs = T.split (== ',') text
        in mapMaybe parseFlag $ map T.strip flagStrs

-- | Parse a single flag string like "-f/--force" or "--template TEMPLATE"
parseFlag :: Text -> Maybe Flag
parseFlag flagStr
    | T.null flagStr = Nothing
    | otherwise = 
        let cleanFlag = T.strip $ T.filter (/= '`') flagStr
        in case T.words cleanFlag of
            [] -> Nothing
            [flag] -> parseFlagOnly flag
            (flag:args) -> parseFlagWithArgs flag args

-- | Parse a flag without arguments
parseFlagOnly :: Text -> Maybe Flag
parseFlagOnly flag = do
    (short, long) <- extractFlagNames flag
    return Flag
        { flagShort = short
        , flagLong = long
        , flagType = BooleanFlag
        , flagDesc = Nothing
        }

-- | Parse a flag with arguments
parseFlagWithArgs :: Text -> [Text] -> Maybe Flag
parseFlagWithArgs flag args = do
    (short, long) <- extractFlagNames flag
    let argText = T.unwords args
        flagType = if T.isSuffixOf " [+]" argText || T.isSuffixOf "..." argText || T.isInfixOf " [+]" argText
                   then MultiStringFlag
                   else StringFlag
    return Flag
        { flagShort = short
        , flagLong = long
        , flagType = flagType
        , flagDesc = Just argText
        }

-- | Extract short and long flag names from "-f/--force" format
extractFlagNames :: Text -> Maybe (Maybe Text, Text)
extractFlagNames flag = 
    case T.splitOn "/" flag of
        [short, long] -> 
            let shortName = T.strip $ T.dropWhile (== '-') short
                longName = T.strip $ T.dropWhile (== '-') long
            in if T.null longName
                then Nothing
                else Just (if T.null shortName then Nothing else Just shortName, longName)
        [long] -> 
            let longName = T.strip $ T.dropWhile (== '-') long
            in if T.null longName
                then Nothing
                else Just (Nothing, longName)
        _ -> Nothing

-- | Generate Haskell option type
generateOptionType :: HgCommand -> IO ()
generateOptionType HgCommand{..} = do
    let typeName = toTypeName cmdName
    putStrLn $ "data " ++ typeName ++ " = " ++ typeName
    putStrLn "    {"
    mapM_ (putStrLn . ("    , " ++)) (generateFields cmdName cmdOptionalFlags)
    putStrLn "    } deriving (Show, Eq)"
    putStrLn ""

-- | Generate field declarations for the option type
generateFields :: Text -> [Flag] -> [String]
generateFields cmdName flags = 
    let baseField = toFieldName cmdName ++ "Files :: ![String]"
    in baseField : map (generateField cmdName) flags

-- | Generate a single field
generateField :: Text -> Flag -> String
generateField cmdName Flag{..} = 
    let fieldName = toFieldName cmdName ++ sanitizeFieldName (T.unpack flagLong)
        fieldType = case flagType of
            BooleanFlag -> "!Bool"
            StringFlag -> "!(Maybe String)"
            MultiStringFlag -> "![String]"
    in fieldName ++ " :: " ++ fieldType

-- | Generate default constructor
generateDefaultConstructor :: HgCommand -> IO ()
generateDefaultConstructor HgCommand{..} = do
    let typeName = toTypeName cmdName
        constructorName = "default" ++ typeName
    putStrLn $ constructorName ++ " :: " ++ typeName
    putStrLn $ constructorName ++ " = " ++ typeName
    putStrLn "    {"
    mapM_ (putStrLn . ("    , " ++)) (generateDefaults cmdName cmdOptionalFlags)
    putStrLn "    }"
    putStrLn ""

-- | Generate default values for fields
generateDefaults :: Text -> [Flag] -> [String]
generateDefaults cmdName flags = 
    let baseDefault = toFieldName cmdName ++ "Files = []"
    in baseDefault : map (generateDefault cmdName) flags

-- | Generate default value for a single field
generateDefault :: Text -> Flag -> String
generateDefault cmdName Flag{..} = 
    let fieldName = toFieldName cmdName ++ sanitizeFieldName (T.unpack flagLong)
        defaultValue = case flagType of
            BooleanFlag -> "False"
            StringFlag -> "Nothing"
            MultiStringFlag -> "[]"
    in fieldName ++ " = " ++ defaultValue

-- | Generate buildArgs pattern
generateBuildArgsPattern :: HgCommand -> IO ()
generateBuildArgsPattern HgCommand{..} = do
    let typeName = toTypeName cmdName
        functionName = T.unpack cmdName
    putStrLn $ functionName ++ " :: HgClient -> " ++ typeName ++ " -> IO ()"
    putStrLn $ functionName ++ " client " ++ typeName ++ "{..} = do"
    putStrLn $ "    let args = buildArgs \"" ++ T.unpack cmdName ++ "\""
    putStrLn "            ["
    mapM_ (putStrLn . ("            , " ++)) (generateArgBuilders cmdName cmdOptionalFlags)
    putStrLn ("            ] " ++ toFieldName cmdName ++ "Files")
    putStrLn "    void $ rawCommand client args"
    putStrLn ""

-- | Generate argument builders for buildArgs
generateArgBuilders :: Text -> [Flag] -> [String]
generateArgBuilders cmdName flags = 
    map (generateArgBuilder cmdName) flags

-- | Generate argument builder for a single flag
generateArgBuilder :: Text -> Flag -> String
generateArgBuilder cmdName Flag{..} = 
    let fieldName = toFieldName cmdName ++ sanitizeFieldName (T.unpack flagLong)
        flagName = T.unpack flagLong
        builder = case flagType of
            BooleanFlag -> "(\"" ++ flagName ++ "\", boolFlag " ++ fieldName ++ ")"
            StringFlag -> "(\"" ++ flagName ++ "\", " ++ fieldName ++ ")"
            MultiStringFlag -> "-- Multi-string: map (\\v -> (\"" ++ flagName ++ "\", Just v)) " ++ fieldName
    in builder

-- | Convert command name to type name (e.g., "log" -> "LogOptions")
toTypeName :: Text -> String
toTypeName name = toTitleCase (T.unpack name) ++ "Options"

-- | Convert command name to field prefix (e.g., "log" -> "log")
toFieldName :: Text -> String
toFieldName name = T.unpack name

-- | Convert string to title case
toTitleCase :: String -> String
toTitleCase [] = []
toTitleCase (c:cs) = toUpper c : cs

-- | Sanitize field name to be valid Haskell identifier
sanitizeFieldName :: String -> String
sanitizeFieldName = toTitleCase . filter isValidFieldChar . concatMap replaceDash
  where
    replaceDash '-' = "_"
    replaceDash c = [c]
    isValidFieldChar c = isAlphaNum c || c == '_'

-- | Helper function for boolean flags
boolFlag :: Bool -> Maybe String
boolFlag True = Just ""
boolFlag False = Nothing