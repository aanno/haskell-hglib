{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Demo application showing how to use the Haskell HgLib
module Main where

import Control.Monad (when, unless, forM_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)

import HgLib
import qualified HgLib.Commands as C
import Logging

-- | Command line options
data Options = Options
    { optRepository :: !(Maybe FilePath)
    , optCommand :: !Command
    } deriving (Show)

-- | Available commands
data Command
    = CmdStatus
    | CmdLog { logLimit :: !(Maybe Int) }
    | CmdInfo
    | CmdCommit { commitMessage :: !String }
    | CmdAdd { addFiles :: ![FilePath] }
    | CmdClone { cloneSource :: !String, cloneDestination :: !FilePath }
    deriving (Show)

-- | Command line parser
optionsParser :: Parser Options
optionsParser = Options
    <$> optional (strOption
        ( long "repo"
       <> short 'R'
       <> metavar "PATH"
       <> help "Repository path" ))
    <*> commandParser

-- | Command parser
commandParser :: Parser Command
commandParser = subparser
    ( command "status" (info (pure CmdStatus) (progDesc "Show repository status"))
   <> command "log" (info logParser (progDesc "Show revision history"))
   <> command "info" (info (pure CmdInfo) (progDesc "Show repository information"))
   <> command "commit" (info commitParser (progDesc "Commit changes"))
   <> command "add" (info addParser (progDesc "Add files"))
   <> command "clone" (info cloneParser (progDesc "Clone repository"))
    )

logParser :: Parser Command
logParser = CmdLog
    <$> optional (option auto
        ( long "limit"
       <> short 'l'
       <> metavar "N"
       <> help "Limit number of revisions" ))

commitParser :: Parser Command
commitParser = CmdCommit
    <$> strOption
        ( long "message"
       <> short 'm'
       <> metavar "MESSAGE"
       <> help "Commit message" )

addParser :: Parser Command
addParser = CmdAdd
    <$> many (strArgument (metavar "FILES..."))

cloneParser :: Parser Command
cloneParser = CmdClone
    <$> strArgument (metavar "SOURCE")
    <*> strArgument (metavar "DEST")

-- | Main application
main :: IO ()
main = do
    opts <- execParser $ info (optionsParser <**> helper)
        ( fullDesc
       <> progDesc "Haskell Mercurial client demo"
       <> header "hglib-demo - demonstrate Haskell HgLib functionality" )
    
    let logConfig = defaultLogConfig 
            { minLogLevel = DEBUG
            , logFile = Nothing
            , console = Just stdout
            }

    withLogging logConfig $ runCli opts

-- | Execute the selected command
runCli :: Options -> IO ()
runCli Options{..} = do
    let config = maybe defaultConfig defaultConfigWithPath optRepository
    
    case optCommand of
        CmdClone source dest -> do
            putStrLn $ "Cloning " ++ source ++ " to " ++ dest
            simpleClone source dest
            putStrLn "Clone completed successfully"
        
        _ -> withClient config $ \client -> case optCommand of
            CmdStatus -> showStatus client
            CmdLog limit -> showLog client limit
            CmdInfo -> showInfo client
            CmdCommit message -> doCommit client message
            CmdAdd files -> doAdd client files
            CmdClone{} -> error "Clone should be handled above"

-- | Show repository status
showStatus :: HgClient -> IO ()
showStatus client = do
    putStrLn "Repository Status:"
    putStrLn "=================="
    
    statuses <- simpleStatus client
    if null statuses
        then putStrLn "Working directory is clean"
        else forM_ statuses $ \status -> do
            let codeDesc = case statusCode status of
                    'M' -> "Modified"
                    'A' -> "Added"
                    'R' -> "Removed"
                    'C' -> "Clean"
                    '!' -> "Missing"
                    '?' -> "Untracked"
                    'I' -> "Ignored"
                    c   -> "Unknown (" ++ [c] ++ ")"
            putStrLn $ codeDesc ++ ": " ++ statusFile status

-- | Show revision log
showLog :: HgClient -> Maybe Int -> IO ()
showLog client limitOpt = do
    putStrLn "Revision History:"
    putStrLn "================="
    
    let limit = maybe 10 id limitOpt
    revisions <- C.log_ client [] defaultLogOptions { logLimit = Just limit }
    
    forM_ revisions $ \rev -> do
        putStrLn $ "Revision: " ++ T.unpack (formatRevision rev)
        putStrLn $ "Author:   " ++ T.unpack (revAuthor rev)
        putStrLn $ "Branch:   " ++ T.unpack (revBranch rev)
        putStrLn $ "Date:     " ++ show (revDate rev)
        putStrLn $ "C.summary:  " ++ T.unpack (T.take 80 $ revDesc rev)
        unless (T.null $ revTags rev) $
            putStrLn $ "Tags:     " ++ T.unpack (revTags rev)
        putStrLn ""

-- | Show repository information
showInfo :: HgClient -> IO ()
showInfo client = do
    putStrLn "Repository Information:"
    putStrLn "======================"
    
    info <- getRepositoryInfo client
    
    putStrLn $ "Root:        " ++ repoRoot info
    putStrLn $ "Branch:      " ++ T.unpack (repoBranch info)
    putStrLn $ "Revision:    " ++ T.unpack (formatRevision $ repoCurrentRevision info)
    putStrLn $ "Clean:       " ++ show (repoIsClean info)
    
    when (repoUpdateCount info > 0) $
        putStrLn $ "Updates:     " ++ show (repoUpdateCount info) ++ " available"
    
    -- Show additional details
    putStrLn ""
    putStrLn "Current Revision Details:"
    let rev = repoCurrentRevision info
    putStrLn $ "Author:      " ++ T.unpack (revAuthor rev)
    putStrLn $ "Date:        " ++ show (revDate rev)
    putStrLn $ "Description: " ++ T.unpack (revDesc rev)

-- | Commit changes
doCommit :: HgClient -> String -> IO ()
doCommit client message = do
    putStrLn "Checking for changes..."
    
    statuses <- simpleStatus client
    if null statuses
        then do
            putStrLn "No changes to commit"
            exitFailure
        else do
            putStrLn $ "Found " ++ show (length statuses) ++ " changed files"
            
            putStrLn "Committing changes..."
            (rev, node) <- simpleCommit client message
            
            putStrLn $ "Committed as revision " ++ show rev
            putStrLn $ "Node: " ++ T.unpack node

-- | Add files to repository
doAdd :: HgClient -> [FilePath] -> IO ()
doAdd client files = do
    if null files
        then putStrLn "No files specified"
        else do
            putStrLn $ "Adding " ++ show (length files) ++ " files..."
            success <- C.add client files defaultAddOptions
            
            if success
                then putStrLn "Files added successfully"
                else do
                    putStrLn "Failed to add files"
                    exitFailure

-- | Example of more advanced usage
advancedExample :: IO ()
advancedExample = do
    putStrLn "Advanced HgLib Example:"
    putStrLn "======================"
    
    let config = defaultConfigWithPath "/path/to/repo"
    
    withClient config $ \client -> do
        -- Get detailed status with options
        statuses <- C.status client defaultStatusOptions
            { statusModified = True
            , statusAdded = True
            , statusRemoved = True
            }
        
        putStrLn $ "Modified/Added/Removed files: " ++ show (length statuses)
        
        -- Get log with specific options
        recent <- C.log_ client [] defaultLogOptions
            { logLimit = Just 5
            , logBranch = Just "default"
            , logNoMerges = True
            }
        
        putStrLn $ "Recent non-merge commits: " ++ show (length recent)
        
        -- Show diff for working directory
        diff <- C.diff client [] defaultDiffOptions
            { diffGit = True
            , diffShowFunction = True
            }
        
        putStrLn "Current diff:"
        putStrLn $ take 500 $ show diff
        
        -- Check if working directory is clean
        isClean <- isCleanWorkingDirectory client
        putStrLn $ "Working directory clean: " ++ show isClean
