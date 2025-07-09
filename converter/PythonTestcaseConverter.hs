{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point for the stateful converter
module Main where

import Control.Monad.State
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)
import System.FilePath (takeBaseName)

import Language.Python.Common.AST
import Language.Python.Version3 (parseModule)
import Logging

import State.ConverterState
import State.StatefulConverter

-- | Parse a Python file
parsePythonFile :: FilePath -> IO (ModuleSpan)
parsePythonFile path = do
  content <- readFile path
  case parseModule content path of
    Left err -> do
      putStrLn $ "Parse error: " ++ show err
      exitFailure
    Right (pyModule, _) -> return pyModule

-- | Main entry point using State monad
main :: IO ()
main = do
  let logConfig = defaultLogConfig { 
    minLogLevel = DEBUG
    , logFile = Nothing
    , console = Just stderr
    }

  args <- getArgs

  withLogging logConfig $
    case args of
      [pythonFile] -> do
        pyModule <- parsePythonFile pythonFile
        
        -- Run the stateful conversion
        let initialState = initialConverterState (takeBaseName pythonFile)
            (haskellCode, finalState) = runState (convertModule pythonFile pyModule) initialState
        
        -- Print the generated Haskell code
        putStrLn haskellCode
        
        -- Print any warnings, todos, or errors to stderr
        let warnings = csWarnings finalState
            todos = csTodos finalState
            errors = csErrors finalState
        
        unless (null warnings) $ do
          putStrLn "-- WARNINGS:" >> mapM_ (putStrLn . ("-- " ++)) (reverse warnings)
        
        unless (null todos) $ do
          putStrLn "-- TODOS:" >> mapM_ (putStrLn . ("-- " ++)) (reverse todos)
        
        unless (null errors) $ do
          putStrLn "-- ERRORS:" >> mapM_ (putStrLn . ("-- " ++)) (reverse errors)
          exitFailure
          
      _ -> do
        putStrLn "Usage: stateful-converter <python-test-file>"
        exitFailure

-- | Helper function for unless (not in base for older versions)
unless :: Bool -> IO () -> IO ()
unless True _ = return ()
unless False action = action
