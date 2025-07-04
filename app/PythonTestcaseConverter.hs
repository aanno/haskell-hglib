{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module PythonTestcaseConverter where

import Control.Monad (forM_, when)
import Data.List (isPrefixOf, isInfixOf, intercalate)
import Data.Maybe (mapMaybe, fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Python.Common.AST
import Language.Python.Common.Pretty (prettyText)
import Language.Python.Common.SrcLocation
import Language.Python.Version3 (parseModule)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- | Parse a Python file
parsePythonFile :: FilePath -> IO (ModuleSpan)
parsePythonFile path = do
  content <- readFile path
  case parseModule content path of
    Left err -> do
      putStrLn $ "Parse error: " ++ prettyText err
      exitFailure
    Right (mod, _) -> return mod

-- | Extract test class name from Python module
extractTestClassName :: ModuleSpan -> Maybe String
extractTestClassName (Module stmts) = 
  findClass stmts
  where
    findClass [] = Nothing
    findClass (ClassDef _ name _ _ _:_) = Just (ident_string name)
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
convertTestMethod (Fun _ name _ _ body _) 
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
  StmtExpr _ (Call _ 
    (Dot _ (Var _ (Ident _ "self" _) _) (Ident _ "assertEqual" _) _) 
    [ArgExpr _ actual _, ArgExpr _ expected _] _) _ ->
      [convertExpr actual ++ " `shouldBe` " ++ convertExpr expected]
  
  -- Handle self.assertTrue
  StmtExpr _ (Call _ 
    (Dot _ (Var _ (Ident _ "self" _) _) (Ident _ "assertTrue" _) _) 
    [ArgExpr _ expr _] _) _ ->
      [convertExpr expr ++ " `shouldBe` True"]
  
  -- Handle self.append
  StmtExpr _ (Call _ 
    (Dot _ (Var _ (Ident _ "self" _) _) (Ident _ "append" _) _) 
    [ArgExpr _ file _, ArgExpr _ content _] _) _ ->
      case (file, content) of
        (Strings _ [fileStr] _, Strings _ [contentStr] _) ->
          ["commonAppendFile \"" ++ fileStr ++ "\" \"" ++ contentStr ++ "\""]
        _ -> ["-- TODO: append with non-string args"]
  
  -- Handle self.client.commit
  Assign _ 
    [Tuple _ [Var _ (Ident _ rev _) _, Var _ (Ident _ node _) _] _]
    (Call _ (Dot _ (Dot _ (Var _ (Ident _ "self" _) _) (Ident _ "client" _) _) (Ident _ "commit" _) _) args _) _ ->
      let commitArgs = convertCommitArgs args
      in ["(" ++ ident_string rev ++ ", " ++ ident_string node ++ ") <- C.commit client " ++ commitArgs]
  
  -- Handle self.client.summary()
  Assign _ 
    [Var _ (Ident _ var _) _]
    (Call _ (Dot _ (Dot _ (Var _ (Ident _ "self" _) _) (Ident _ "client" _) _) (Ident _ "summary" _) _) args _) _ ->
      [ident_string var ++ " <- C.summary client " ++ convertSummaryArgs args]
  
  -- Handle dictionary construction
  Assign _ 
    [Var _ (Ident _ var _) _]
    (Dictionary _ items _) _ ->
      ["-- Dictionary assignment for " ++ ident_string var ++ " omitted"]
  
  -- Handle if statements
  Conditional _ clauses elseSuite _ ->
      convertIfStatement clauses elseSuite
  
  -- Handle raise unittest.SkipTest
  Raise _ (RaiseV3 (Just (Call _ (Dot _ (Var _ (Ident _ "unittest" _) _) (Ident _ "SkipTest" _) _) _ _))) _ ->
      ["pendingWith \"phase not supported\""]
  
  -- Default case
  _ -> ["-- TODO: " ++ take 60 (show stmt)]

-- | Convert if statement
convertIfStatement :: [(ExprSpan, SuiteSpan)] -> SuiteSpan -> [String]
convertIfStatement [(cond, thenSuite)] elseSuite =
  case cond of
    -- Handle version checks
    BinaryOp _ (Dot _ (Dot _ (Var _ (Ident _ "self" _) _) (Ident _ "client" _) _) (Ident _ "version" _) _) 
             op 
             (Tuple _ versionParts _) ->
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
         [ArgExpr _ (Strings _ [msg] _) _] -> 
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
    extractOpt (ArgKeyword _ (Ident _ name _) expr _) = 
      Just (ident_string name, convertExpr expr)
    extractOpt _ = Nothing

-- | Convert Python expression to Haskell
convertExpr :: ExprSpan -> String
convertExpr expr = case expr of
  Var _ (Ident _ name _) _ -> ident_string name
  Int _ i _ -> show i
  Strings _ [s] _ -> show s
  Bool _ b _ -> show b
  Tuple _ exprs _ -> "(" ++ intercalate ", " (map convertExpr exprs) ++ ")"
  List _ exprs _ -> "[" ++ intercalate ", " (map convertExpr exprs) ++ "]"
  Dictionary _ items _ -> "-- TODO: dict"
  Subscript _ e idx _ -> convertExpr e ++ "[" ++ convertExpr idx ++ "]"
  BinaryOp _ e1 (Equality _) e2 _ -> convertExpr e1 ++ " == " ++ convertExpr e2
  Call _ (Var _ (Ident _ "b" _) _) [ArgExpr _ (Strings _ [s] _) _] _ -> show s
  _ -> "-- TODO: expr"

-- | Generate the Haskell module
generateHaskellModule :: String -> ModuleSpan -> String
generateHaskellModule className (Module stmts) = 
  let testMethods = mapMaybe convertTestMethod stmts
      moduleName = className ++ "Spec"
  in unlines $
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

-- | Extract identifier string
ident_string :: IdentSpan -> String
ident_string (Ident s _ _) = s

main :: IO ()
main = do
  args <- getArgs
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
