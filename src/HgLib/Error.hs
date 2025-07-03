{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HgLib.Error
    ( -- * Error Types
      HgError(..)
    , HgErrorType(..)
    
    -- * Error Handling Utilities
    , throwHgError
    , catchHgError
    , handleHgError
    , mapHgError
    
    -- * Error Formatters
    , formatHgError
    , formatCommandError
    , isRetriableError
    , mkResponseError
    ) where

import Control.Exception (Exception(..), throwIO, catch, handle)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

-- | Types of errors that can occur
data HgErrorType
    = CommandErrorType      -- ^ Command execution failed
    | ResponseErrorType     -- ^ Invalid response from server
    | ServerErrorType       -- ^ Server process error
    | CapabilityErrorType   -- ^ Missing required capability
    | ProtocolErrorType     -- ^ Protocol communication error
    | ConfigErrorType       -- ^ Configuration error
    | RepositoryErrorType   -- ^ Repository state error
    deriving stock (Show, Eq, Ord, Enum, Generic)

-- | Comprehensive error type for all Mercurial operations
data HgError 
    = HgCommandError 
        { errorCommand :: ![String]     -- ^ Command that failed
        , errorExitCode :: !Int         -- ^ Exit code
        , errorStdout :: !ByteString    -- ^ Command stdout
        , errorStderr :: !ByteString    -- ^ Command stderr
        }
    | HgResponseError 
        { errorMessage :: !String       -- ^ Error description
        }
    | HgServerError 
        { errorMessage :: !String       -- ^ Server error description
        }
    | HgCapabilityError 
        { errorCapability :: !String    -- ^ Missing capability
        }
    | HgProtocolError
        { errorMessage :: !String       -- ^ Protocol error description
        , errorContext :: !(Maybe ByteString)  -- ^ Optional context data
        }
    | HgConfigError
        { errorMessage :: !String       -- ^ Configuration error
        , errorConfig :: !(Maybe String)  -- ^ Problematic config
        }
    | HgRepositoryError
        { errorMessage :: !String       -- ^ Repository error
        , errorPath :: !(Maybe FilePath)  -- ^ Repository path
        }
    deriving stock (Show, Eq, Generic)

instance Exception HgError where
    displayException = formatHgError

-- | Get the error type for categorization
getErrorType :: HgError -> HgErrorType
getErrorType = \case
    HgCommandError{} -> CommandErrorType
    HgResponseError{} -> ResponseErrorType
    HgServerError{} -> ServerErrorType
    HgCapabilityError{} -> CapabilityErrorType
    HgProtocolError{} -> ProtocolErrorType
    HgConfigError{} -> ConfigErrorType
    HgRepositoryError{} -> RepositoryErrorType

-- | Format an HgError for display
formatHgError :: HgError -> String
formatHgError = \case
    HgCommandError{..} -> 
        "Command failed: " ++ unwords errorCommand ++ 
        " (exit code " ++ show errorExitCode ++ ")\n" ++
        formatCommandError errorStdout errorStderr
    
    HgResponseError{..} ->
        "Protocol response error: " ++ errorMessage
    
    HgServerError{..} ->
        "Mercurial server error: " ++ errorMessage
    
    HgCapabilityError{..} ->
        "Missing required capability: " ++ errorCapability
    
    HgProtocolError{..} ->
        "Protocol error: " ++ errorMessage ++
        maybe "" (\ctx -> "\nContext: " ++ show ctx) errorContext
    
    HgConfigError{..} ->
        "Configuration error: " ++ errorMessage ++
        maybe "" (\cfg -> "\nConfig: " ++ cfg) errorConfig
    
    HgRepositoryError{..} ->
        "Repository error: " ++ errorMessage ++
        maybe "" (\path -> "\nPath: " ++ path) errorPath

-- | Format command output for error display
formatCommandError :: ByteString -> ByteString -> String
formatCommandError stdout' stderr' =
    let stdoutText = TE.decodeUtf8 stdout'
        stderrText = TE.decodeUtf8 stderr'
    in unlines $ filter (not . null) 
        [ if T.null stdoutText then "" else "stdout: " ++ T.unpack stdoutText
        , if T.null stderrText then "" else "stderr: " ++ T.unpack stderrText
        ]

-- | Check if an error might be retriable
isRetriableError :: HgError -> Bool
isRetriableError = \case
    HgCommandError{..} -> 
        -- Some exit codes might indicate temporary failures
        errorExitCode `elem` [1, 255]  -- Generic errors that might be temporary
    
    HgServerError{} -> True   -- Server might have restarted
    HgProtocolError{} -> True -- Network/communication issues
    _ -> False

-- | Throw an HgError in IO
throwHgError :: MonadIO m => HgError -> m a
throwHgError = liftIO . throwIO

-- | Catch HgError exceptions
catchHgError :: IO a -> (HgError -> IO a) -> IO a
catchHgError = catch

-- | Handle HgError exceptions
handleHgError :: (HgError -> IO a) -> IO a -> IO a
handleHgError = handle

-- | Map over HgError exceptions
mapHgError :: (HgError -> HgError) -> IO a -> IO a
mapHgError f action = action `catch` (throwIO . f)

-- | Create a command error from process results
mkCommandError :: [String] -> Int -> ByteString -> ByteString -> HgError
mkCommandError cmd exitCode stdout' stderr' = 
    HgCommandError cmd exitCode stdout' stderr'

-- | Create a response error with context
mkResponseError :: String -> HgError
mkResponseError = HgResponseError

-- | Create a server error
mkServerError :: String -> HgError
mkServerError = HgServerError

-- | Create a capability error
mkCapabilityError :: String -> HgError
mkCapabilityError = HgCapabilityError

-- | Create a protocol error with optional context
mkProtocolError :: String -> Maybe ByteString -> HgError
mkProtocolError msg ctx = HgProtocolError msg ctx

-- | Create a configuration error
mkConfigError :: String -> Maybe String -> HgError
mkConfigError msg cfg = HgConfigError msg cfg

-- | Create a repository error
mkRepositoryError :: String -> Maybe FilePath -> HgError
mkRepositoryError msg path = HgRepositoryError msg path

-- | Convert common error patterns to appropriate HgError types
classifyError :: [String] -> Int -> ByteString -> ByteString -> HgError
classifyError cmd exitCode stdout' stderr' =
    let stderrText = T.toLower $ TE.decodeUtf8 stderr'
    in if
        | T.pack "repository" `T.isInfixOf` stderrText && T.pack "not found" `T.isInfixOf` stderrText ->
            mkRepositoryError "Repository not found" Nothing
        
        | T.pack "unknown command" `T.isInfixOf` stderrText ->
            mkCapabilityError $ "Unknown command: " ++ unwords cmd
        
        | T.pack "permission denied" `T.isInfixOf` stderrText ->
            mkRepositoryError "Permission denied" Nothing
        
        | T.pack "abort:" `T.isInfixOf` stderrText ->
            mkRepositoryError (T.unpack $ T.strip stderrText) Nothing
        
        | otherwise ->
            mkCommandError cmd exitCode stdout' stderr'

-- | Helper to add context to errors
withErrorContext :: String -> IO a -> IO a
withErrorContext context action = 
    action `catch` \(err :: HgError) ->
        throwIO $ case err of
            HgResponseError msg -> HgResponseError (context ++ ": " ++ msg)
            HgServerError msg -> HgServerError (context ++ ": " ++ msg)
            HgProtocolError msg ctx -> HgProtocolError (context ++ ": " ++ msg) ctx
            HgConfigError msg cfg -> HgConfigError (context ++ ": " ++ msg) cfg
            HgRepositoryError msg path -> HgRepositoryError (context ++ ": " ++ msg) path
            other -> other

-- | Retry an action up to n times if it fails with a retriable error
retryOnError :: Int -> IO a -> IO a
retryOnError maxRetries action = go maxRetries
  where
    go 0 = action
    go n = action `catch` \err ->
        if isRetriableError err && n > 1
            then go (n - 1)
            else throwIO err
