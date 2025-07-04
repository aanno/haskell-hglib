{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (forM_, when)
import Data.List (isPrefixOf, isInfixOf, intercalate)
import Data.Maybe (mapMaybe, fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Python.Common.AST
import Language.Python.Common.SrcLocation
import Language.Python.Version3 (parseModule)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr, stdout)
import Logging

-- | Parse a Python file
parsePythonFile :: FilePath -> IO (ModuleSpan)
parsePythonFile path = do
  content <- readFile path
  case parseModule content path of
    Left err -> do
      putStrLn $ "Parse error: " ++ show err
      exitFailure
    Right (mod, _) -> return mod

-- | Extract test class name from Python module
extractTestClassName :: ModuleSpan -> Maybe String
extractTestClassName (Module stmts) = 
  findClass stmts
  where
    findClass [] = Nothing
    findClass (Class name _ _ _:_) = Just (ident_string name)
    findClass (_:xs) = findClass xs

-- | Convert Python identifier to Haskell identifier
pythonToHaskellIdent :: String -> String
pythonToHaskellIdent s = 
  case s of
    "test_empty" -> "should handle empty repository"
    "test_basic" -> "should handle basic repository with one commit"
    "test_commit_dirty" -> "should detect dirty working directory"
    "test_secret_commit_clean" -> "should handle secret commit clean"
    "test_update" -> "should handle update"
    "test_remote" -> "should handle remote"
    "test_two_parents" -> "should handle two parents"
    _ -> s

-- | Convert a Python test method to Haskell spec
convertTestMethod :: StatementSpan -> Maybe String
convertTestMethod (Fun name _ _ body _) 
  | "test_" `isPrefixOf` ident_string name = 
      Just $ generateHaskellTest (ident_string name) body
  | otherwise = Nothing
convertTestMethod _ = Nothing

-- | Generate Haskell test from Python test body
generateHaskellTest :: String -> SuiteSpan -> String
generateHaskellTest testName body = 
  let testDesc = pythonToHaskellIdent testName
      bodyLines = convertSuite body
  in unlines $
    [ "  it \"" ++ testDesc ++ "\" $ do"
    , "    withTestRepo $ \\bt -> do"
    , "      let client = btClient bt"
    ] ++ map ("      " ++) bodyLines

-- | Convert Python suite to Haskell lines
convertSuite :: SuiteSpan -> [String]
convertSuite stmts = concatMap convertStatement stmts

-- | Convert a single Python statement to Haskell
convertStatement :: StatementSpan -> [String]
convertStatement stmt = case stmt of
  -- Handle self.assertEqual
  StmtExpr (Call 
    (Dot (Var (Ident "self" _) _) (Ident "assertEqual" _) _) 
    [ArgExpr actual _, ArgExpr expected _] _) _ ->
      [convertExpr actual ++ " `shouldBe` " ++ convertExpr expected]
  
  -- Handle self.assertTrue
  StmtExpr (Call 
    (Dot (Var (Ident "self" _) _) (Ident "assertTrue" _) _) 
    [ArgExpr expr _] _) _ ->
      [convertExpr expr ++ " `shouldBe` True"]
  
  -- Handle self.append
  StmtExpr (Call 
    (Dot (Var (Ident "self" _) _) (Ident "append" _) _) 
    [ArgExpr file _, ArgExpr content _] _) _ ->
      case (file, content) of
        (Strings [fileStr] _, Strings [contentStr] _) ->
          ["commonAppendFile \"" ++ fileStr ++ "\" \"" ++ contentStr ++ "\""]
        _ -> ["-- TODO: append with non-string args"]
  
  -- Handle self.client.commit
  Assign 
    [Tuple [Var (Ident rev _) _, Var (Ident node _) _] _]
    (Call (Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident "commit" _) _) args _) _ ->
      let commitArgs = convertCommitArgs args
      in ["(" ++ rev ++ ", " ++ node ++ ") <- C.commit client " ++ commitArgs]
  
  -- Handle self.client.summary()
  Assign 
    [Var (Ident var _) _]
    (Call (Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident "summary" _) _) args _) _ ->
      [var ++ " <- C.summary client " ++ convertSummaryArgs args]
  
  -- Handle dictionary construction
  Assign 
    [Var (Ident var _) _]
    (Dictionary items _) _ ->
      ["-- Dictionary assignment for " ++ var ++ " omitted"]
  
  -- Handle if statements
  Conditional clauses elseSuite _ ->
      convertIfStatement clauses elseSuite
  
  -- Handle raise unittest.SkipTest
  Raise (RaiseV3 (Just (expr, Nothing))) _ ->
      case expr of
        Call (Dot (Var (Ident "unittest" _) _) (Ident "SkipTest" _) _) _ _ ->
          ["pendingWith \"phase not supported\""]
        _ -> ["-- TODO: raise " ++ take 30 (show expr)]
  
  -- Default case
  _ -> ["-- TODO: " ++ take 60 (show stmt)]

-- | Convert if statement
convertIfStatement :: [(ExprSpan, SuiteSpan)] -> SuiteSpan -> [String]
convertIfStatement [(cond, thenSuite)] elseSuite =
  case cond of
    -- Handle version checks
    BinaryOp op (Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident "version" _) _) (Tuple versionParts _) _ ->
      let versionCheck = convertVersionCheck op versionParts
      in ["when " ++ versionCheck ++ " $ do"] ++ 
         map ("  " ++) (convertSuite thenSuite)
    _ -> ["-- TODO: if statement"]
convertIfStatement _ _ = ["-- TODO: complex if statement"]

-- | Convert version check
convertVersionCheck :: OpSpan -> [ExprSpan] -> String
convertVersionCheck op parts = 
  let version = "(" ++ intercalate ", " (map convertExpr parts) ++ ")"
  in case op of
    LessThan _ -> "clientVersion < " ++ version
    GreaterThan _ -> "clientVersion > " ++ version
    GreaterThanEquals _ -> "clientVersion >= " ++ version
    _ -> "True"

-- | Convert commit arguments
convertCommitArgs :: [ArgumentSpan] -> String
convertCommitArgs args = 
  let opts = extractCommitOptions args
  in case lookup "message" opts of
       Just msg -> "(mkTestCommitOptions " ++ show msg ++ 
                  if lookup "addremove" opts == Just "True" 
                  then " { C.commitAddRemove = True })"
                  else ")"
       Nothing -> case args of
         [ArgExpr (Strings [msg] _) _] -> 
           "(mkTestCommitOptions " ++ show msg ++ ")"
         _ -> "[]"

-- | Convert summary arguments
convertSummaryArgs :: [ArgumentSpan] -> String
convertSummaryArgs [] = "[]"
convertSummaryArgs args = "-- TODO: summary args"

-- | Extract commit options from arguments
extractCommitOptions :: [ArgumentSpan] -> [(String, String)]
extractCommitOptions = mapMaybe extractOpt
  where
    extractOpt (ArgKeyword (Ident name _) expr _) = 
      Just (name, convertExpr expr)
    extractOpt _ = Nothing

-- | Convert Python expression to Haskell
convertExpr :: ExprSpan -> String
convertExpr expr = case expr of
  Var (Ident name _) _ -> name
  Int i _ _ -> show i
  Strings [s] _ -> show s
  Bool b _ -> show b
  Tuple exprs _ -> "(" ++ intercalate ", " (map convertExpr exprs) ++ ")"
  List exprs _ -> "[" ++ intercalate ", " (map convertExpr exprs) ++ "]"
  Dictionary items _ -> "-- TODO: dict"
  Subscript e idx _ -> convertExpr e ++ "[" ++ convertExpr idx ++ "]"
  BinaryOp op left right _ -> 
      case op of
        Equality _ -> convertExpr left ++ " == " ++ convertExpr right
        _ -> convertExpr left ++ " <op> " ++ convertExpr right
  Call (Var (Ident "b" _) _) [ArgExpr (Strings [s] _) _] _ -> show s
  _ -> "-- TODO: expr"

extractTestMethods :: StatementSpan -> [String]
extractTestMethods (Class _ _ body _) = mapMaybe convertTestMethod body
extractTestMethods _ = []

-- | Generate the Haskell module
generateHaskellModule :: String -> ModuleSpan -> String
generateHaskellModule className (Module stmts) = 
  let testMethods = concatMap extractTestMethods stmts
      moduleName = className ++ "Spec"
  in
  if null testMethods
    then "-- No test methods found."
    else unlines $
      [ "{-# LANGUAGE OverloadedStrings #-}"
      , ""
      , "module Test.HgLib." ++ moduleName ++ " (spec) where"
      , ""
      , "import Test.Hspec"
      , "import Test.HgLib.Common"
      , "import qualified HgLib.Commands as C"
      , "import HgLib.Types (SummaryInfo(..))"
      , "import Data.Text (Text)"
      , "import qualified Data.Text as T"
      , ""
      , "spec :: Spec"
      , "spec = describe \"" ++ className ++ "\" $ do"
      ] ++ testMethods

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
        case extractTestClassName pyModule of
          Just "test_summary" -> do
            let haskellCode = generateHaskellModule "Summary" pyModule
            putStrLn haskellCode
          _ -> putStrLn "Error: Could not find test_summary class"
      _ -> do
        putStrLn "Usage: PythonTestcaseConverter <python-test-file>"
        exitFailure
