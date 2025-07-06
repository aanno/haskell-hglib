{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (forM_, when)
import Data.List (isPrefixOf, isInfixOf, intercalate)
import qualified Data.List as L
import Data.Maybe (mapMaybe, fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr, stdout)
import Data.Char (toUpper, toLower)
import System.FilePath (takeBaseName)

import Logging

-- Import the language-python modules only for the converter
import Language.Python.Common.AST
import Language.Python.Common.SrcLocation
import Language.Python.Version3 (parseModule)

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
    "test_user" -> "should handle commit with custom user"
    "test_no_user" -> "should fail with empty user"
    "test_close_branch" -> "should close branch"
    "test_message_logfile" -> "should handle message and logfile conflicts"
    "test_date" -> "should handle custom date"
    "test_amend" -> "should amend previous commit"
    "test_nul_injection" -> "should prevent null injection"
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
      -- Ensure test bodies don't end with assignments and have proper final expressions
      finalBody = ensureProperTestEnding bodyLines
  in unlines $
    [ "  it \"" ++ testDesc ++ "\" $ do"
    , "    withTestRepo $ \\bt -> do"
    , "      let client = btClient bt"
    ] ++ map ("      " ++) finalBody
  where
    ensureProperTestEnding [] = ["pendingWith \"Empty test body\""]
    ensureProperTestEnding lines
      | null lines = ["pendingWith \"Empty test body\""]
      | all isDeclarationOrComment lines = lines ++ ["pendingWith \"Test not implemented yet\""]
      | not (null lines) && isAssignment (last lines) = lines ++ ["return ()"]
      | otherwise = lines
    
    isDeclarationOrComment line = 
      "let " `isPrefixOf` stripSpaces line || 
      "-- " `isPrefixOf` stripSpaces line || 
      null (stripSpaces line)
    
    isAssignment line = 
      let stripped = stripSpaces line
      in " <- " `isInfixOf` stripped && 
         not ("shouldBe" `isInfixOf` stripped) && 
         not ("-- TODO:" `isPrefixOf` stripped) &&
         not ("pendingWith" `isInfixOf` stripped)
    
    stripSpaces = dropWhile (== ' ')

-- | Convert Python suite to Haskell lines
convertSuite :: SuiteSpan -> [String]
convertSuite stmts = concatMap convertStatement stmts

-- | Convert a single Python statement to Haskell
convertStatement :: StatementSpan -> [String]
convertStatement stmt = case stmt of
  -- Handle self.assertEqual with complex expressions
  StmtExpr (Call 
    (Dot (Var (Ident "self" _) _) (Ident "assertEqual" _) _) 
    [ArgExpr actual _, ArgExpr expected _] _) _ ->
      let actualStr = convertExpr actual
          expectedStr = convertExpr expected
      in if "-- TODO:" `isPrefixOf` actualStr || "-- TODO:" `isPrefixOf` expectedStr
         then ["-- TODO: complex assertEqual"]
         else [actualStr ++ " `shouldBe` " ++ expectedStr]
  
  -- Handle self.assertTrue
  StmtExpr (Call 
    (Dot (Var (Ident "self" _) _) (Ident "assertTrue" _) _) 
    [ArgExpr expr _] _) _ ->
      [convertExpr expr ++ " `shouldBe` True"]
  
  -- Handle self.assertRaises
  StmtExpr (Call 
    (Dot (Var (Ident "self" _) _) (Ident "assertRaises" _) _) 
    args _) _ ->
      convertAssertRaises args

  -- Handle self.assertNotEqual
  StmtExpr (Call 
    (Dot (Var (Ident "self" _) _) (Ident "assertNotEqual" _) _) 
    [ArgExpr actual _, ArgExpr expected _] _) _ ->
      [convertExpr actual ++ " `shouldNotBe` " ++ convertExpr expected]
  
  -- Handle self.append
  StmtExpr (Call 
    (Dot (Var (Ident "self" _) _) (Ident "append" _) _) 
    [ArgExpr file _, ArgExpr content _] _) _ ->
      let fileStr = extractStringContent file
          contentStr = extractStringContent content
      in ["commonAppendFile " ++ fileStr ++ " " ++ contentStr]
  
  -- Handle self.client.commit with tuple assignment
  Assign 
    [Tuple [Var (Ident rev _) _, Var (Ident node _) _] _]
    (Call (Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident "commit" _) _) args _) _ ->
      let commitArgs = convertCommitArgs args
      in ["(" ++ rev ++ ", " ++ node ++ ") <- C.commit client " ++ commitArgs]
  
  -- Handle self.client.commit with single variable assignment
  Assign 
    [Var (Ident var _) _]
    (Call (Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident "commit" _) _) args _) _ ->
      let commitArgs = convertCommitArgs args
      in [var ++ " <- C.commit client " ++ commitArgs]
  
  -- Handle self.client.log assignment
  Assign 
    [Var (Ident var _) _]
    (Call (Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident "log" _) _) args _) _ ->
      case args of
        [] -> [var ++ " <- C.log_ client [] C.defaultLogOptions"]
        [ArgExpr nodeExpr _] -> [var ++ " <- C.log_ client [" ++ convertExpr nodeExpr ++ "] C.defaultLogOptions"]
        _ -> [var ++ " <- C.log_ client " ++ convertLogArgs args ++ " C.defaultLogOptions"]
  
  -- Handle self.client.branches assignment
  Assign 
    [Var (Ident var _) _]
    (Call (Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident "branches" _) _) args _) _ ->
      [var ++ " <- C.branches client " ++ convertBranchesArgs args]
  
  -- Handle self.client.tip assignment
  Assign 
    [Var (Ident var _) _]
    (Call (Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident "tip" _) _) args _) _ ->
      [var ++ " <- C.tip client"]
  
  -- Handle self.client.branch call
  StmtExpr (Call 
    (Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident "branch" _) _) 
    args _) _ ->
      case args of
        [ArgExpr branchName _] -> ["C.branch client (Just " ++ convertExpr branchName ++ ") []"]
        _ -> ["C.branch client Nothing []"]
  
  -- Handle variable assignment from indexing (e.g., rev = self.client.log(node)[0])
  Assign 
    [Var (Ident var _) _]
    (Subscript listExpr (Int 0 _ _) _) _ ->
      let listStr = convertExpr listExpr
      in if "C.log_" `isInfixOf` listStr
         then [var ++ " <- head <$> " ++ listStr]
         else if "-- TODO:" `isPrefixOf` listStr
              then ["-- TODO: head assignment from " ++ listStr]
              else [var ++ " <- return $ head " ++ listStr]
  
  -- Handle multiple variable assignment from self.client.log
  Assign 
    [Tuple vars _]
    (Call (Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident "log" _) _) args _) _ ->
      let varNames = [name | Var (Ident name _) _ <- vars]
          logArgs = convertLogArgs args
      in ["[" ++ intercalate ", " varNames ++ "] <- C.log_ client " ++ logArgs ++ " C.defaultLogOptions"]
  
  -- Handle datetime.datetime.now assignment
  Assign 
    [Var (Ident var _) _]
    (Call (Dot (Dot (Var (Ident "datetime" _) _) (Ident "datetime" _) _) (Ident "now" _) _) [] _) _ ->
      [var ++ " <- getCurrentTime"]
  
  -- Handle datetime method calls
  Assign 
    [Var (Ident var _) _]
    (Call (Dot varExpr (Ident "replace" _) _) args _) _ ->
      [var ++ " <- return " ++ convertExpr varExpr ++ " -- TODO: handle replace with " ++ show (length args) ++ " args"]
  
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
  
  -- Handle len() function calls in assertions  
  StmtExpr (Call 
    (Dot (Var (Ident "self" _) _) (Ident "assertEqual" _) _) 
    [ArgExpr expected _, ArgExpr (Call (Var (Ident "len" _) _) [ArgExpr listExpr _] _) _] _) _ ->
      let listStr = convertExpr listExpr
      in if "C.log_" `isInfixOf` listStr || "C.branches" `isInfixOf` listStr
         then ["do", "  result <- " ++ listStr, "  length result `shouldBe` " ++ convertExpr expected]
         else ["length " ++ listStr ++ " `shouldBe` " ++ convertExpr expected]
  
  -- Handle reversed len() function calls in assertions
  StmtExpr (Call 
    (Dot (Var (Ident "self" _) _) (Ident "assertEqual" _) _) 
    [ArgExpr (Call (Var (Ident "len" _) _) [ArgExpr listExpr _] _) _, ArgExpr expected _] _) _ ->
      let listStr = convertExpr listExpr
      in if "C.log_" `isInfixOf` listStr || "C.branches" `isInfixOf` listStr
         then ["do", "  result <- " ++ listStr, "  length result `shouldBe` " ++ convertExpr expected]
         else ["length " ++ listStr ++ " `shouldBe` " ++ convertExpr expected]
  
  -- Default case
  _ -> ["-- TODO: " ++ take 80 (show stmt)]

-- | Convert assertRaises calls
convertAssertRaises :: [ArgumentSpan] -> [String]
convertAssertRaises args = case args of
  -- Handle self.assertRaises(exception, self.client.method, arg1, arg2, ...)
  (ArgExpr exceptionType _):(ArgExpr (Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident method _) _) _):methodArgs ->
    let methodCall = "C." ++ method ++ " client " ++ convertMethodArgs method methodArgs
    in ["result <- (try :: IO a -> IO (Either SomeException a)) $ " ++ methodCall,
        "result `shouldSatisfy` isLeft"]
  -- Handle self.assertRaises(ValueError, self.client.commit, message, logfile=file) - simplified
  [ArgExpr exceptionType _, ArgExpr (Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident method _) _) _, ArgExpr msgArg span1, ArgKeyword (Ident kwName span2) kwValue span3] ->
    let commitArgs = [ArgExpr msgArg span1, ArgKeyword (Ident kwName span2) kwValue span3]
        methodCall = "C." ++ method ++ " client " ++ convertCommitArgs commitArgs
    in ["result <- (try :: IO a -> IO (Either SomeException a)) $ " ++ methodCall,
        "result `shouldSatisfy` isLeft"]
  -- Handle self.assertRaises(ValueError, self.client.commit)
  [ArgExpr exceptionType _, ArgExpr (Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident method _) _) _] ->
    let methodCall = "C." ++ method ++ " client C.defaultCommitOptions"
    in ["result <- (try :: IO a -> IO (Either SomeException a)) $ " ++ methodCall,
        "result `shouldSatisfy` isLeft"]
  -- Handle self.assertRaises(exception, lambda: expr)
  [ArgExpr exceptionType _, ArgExpr (Lambda _ expr _) _] ->
    let lambdaBody = convertExpr expr
    in ["result <- (try :: IO a -> IO (Either SomeException a)) $ " ++ lambdaBody,
        "result `shouldSatisfy` isLeft"]
  -- Handle other patterns
  [ArgExpr exceptionType _, ArgExpr (Call func funcArgs _) _] ->
    let funcCall = convertFunctionCall func funcArgs
    in ["result <- (try :: IO a -> IO (Either SomeException a)) $ " ++ funcCall,
        "result `shouldSatisfy` isLeft"]
  _ -> ["-- TODO: assertRaises pattern not implemented"]

-- | Convert function calls
convertFunctionCall :: ExprSpan -> [ArgumentSpan] -> String
convertFunctionCall func args = case func of
  Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident method _) _ ->
    "C." ++ method ++ " client " ++ convertMethodArgs method args
  _ -> convertExpr func ++ " " ++ intercalate " " (map convertArg args)

-- | Convert method arguments based on method name
convertMethodArgs :: String -> [ArgumentSpan] -> String
convertMethodArgs method args = case method of
  "commit" -> convertCommitArgs args
  "log" -> convertLogArgs args
  "branches" -> convertBranchesArgs args
  "update" -> convertUpdateArgs args
  _ -> intercalate " " (map convertArg args)

-- | Convert branches arguments
convertBranchesArgs :: [ArgumentSpan] -> String
convertBranchesArgs [] = "[]"
convertBranchesArgs args = 
  let opts = extractKeywordArgs args
  in if "closed" `elem` map fst opts
     then "[\"--closed\"]"
     else "[]"

-- | Convert log arguments
convertLogArgs :: [ArgumentSpan] -> String
convertLogArgs [] = "[]"
convertLogArgs args = 
  let nonKeywordArgs = [convertExpr expr | ArgExpr expr _ <- args]
      keywordOpts = extractKeywordArgs args
  in if null keywordOpts
     then "[" ++ intercalate ", " nonKeywordArgs ++ "]"
     else "-- TODO: log with options"

-- | Convert if statement
convertIfStatement :: [(ExprSpan, SuiteSpan)] -> SuiteSpan -> [String]
convertIfStatement [(cond, thenSuite)] elseSuite =
  case cond of
    -- Handle version checks
    BinaryOp op (Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident "version" _) _) (Tuple versionParts _) _ ->
      let versionCheck = convertVersionCheck op versionParts
      in ["when " ++ versionCheck ++ " $ do"] ++ 
         map ("  " ++) (convertSuite thenSuite)
    _ -> ["-- TODO: if statement with condition: " ++ take 50 (show cond)]
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
convertCommitArgs [] = "C.defaultCommitOptions"
convertCommitArgs args = 
  let opts = extractKeywordArgs args
      message = case args of
        (ArgExpr msgExpr _):_ -> convertExpr msgExpr
        _ -> "\"default\""
  in buildCommitOptions message opts

-- | Add option to commit options
addOption :: String -> (String, String) -> String
addOption base (key, value) = case key of
  "addremove" -> base ++ " { C.commitAddRemove = " ++ value ++ " }"
  "user" -> base ++ " { C.commitUser = Just " ++ value ++ " }"
  "date" -> base ++ " { C.commitDate = Just " ++ value ++ " }"
  "closebranch" -> base ++ " { C.commitCloseBranch = " ++ value ++ " }"
  "amend" -> base ++ " { C.commitAmend = " ++ value ++ " }"
  "logfile" -> base ++ " { C.commitLogFile = Just " ++ value ++ " }"
  _ -> base ++ " -- TODO: " ++ key ++ " = " ++ value

-- | Build commit options properly with parentheses
buildCommitOptions :: String -> [(String, String)] -> String
buildCommitOptions baseMsg [] = "mkTestCommitOptions " ++ baseMsg
buildCommitOptions baseMsg opts = 
  let base = "mkTestCommitOptions " ++ baseMsg
      updates = foldl (\acc (key, value) -> updateRecord acc key value) base opts
  in "(" ++ updates ++ ")"
  where
    updateRecord currentRecord key value = case key of
      "addremove" -> currentRecord ++ " { C.commitAddRemove = " ++ value ++ " }"
      "user" -> currentRecord ++ " { C.commitUser = Just " ++ value ++ " }"
      "date" -> currentRecord ++ " { C.commitDate = Just " ++ value ++ " }"
      "closebranch" -> currentRecord ++ " { C.commitCloseBranch = " ++ value ++ " }"
      "amend" -> currentRecord ++ " { C.commitAmend = " ++ value ++ " }"
      "logfile" -> currentRecord ++ " { C.commitLogFile = Just " ++ value ++ " }"
      _ -> currentRecord ++ " -- TODO: " ++ key ++ " = " ++ value

-- | Convert summary arguments
convertSummaryArgs :: [ArgumentSpan] -> String
convertSummaryArgs [] = "[]"
convertSummaryArgs args = "-- TODO: summary args"

-- | Extract keyword arguments
extractKeywordArgs :: [ArgumentSpan] -> [(String, String)]
extractKeywordArgs = mapMaybe extractOpt
  where
    extractOpt (ArgKeyword (Ident name _) expr _) = 
      Just (name, convertExpr expr)
    extractOpt _ = Nothing

-- | Convert argument to handle keyword args better
convertArg :: ArgumentSpan -> String
convertArg (ArgExpr expr _) = convertExpr expr
convertArg (ArgKeyword (Ident name _) expr _) = 
  case name of
    "files" -> convertExpr expr  -- For C.log files=["a"] -> just ["a"]
    "closed" -> convertExpr expr  -- For C.branches closed=True -> just True
    _ -> name ++ "=" ++ convertExpr expr
convertArg _ = "-- TODO: arg"

-- | Convert Python expression to Haskell
convertExpr :: ExprSpan -> String
convertExpr expr = case expr of
  Var (Ident name _) _ -> name
  Int i _ _ -> show i
  Strings [s] _ -> "\"" ++ stripQuotes s ++ "\""  -- Strip extra quotes
  Bool b _ -> show b
  Tuple exprs _ -> 
    if length exprs > 3 || any isComplexExpr exprs
    then "-- TODO: complex tuple"
    else "(" ++ intercalate ", " (map convertExpr exprs) ++ ")"
  List exprs _ -> 
    if length exprs > 5 || any isComplexExpr exprs
    then "-- TODO: complex list"
    else "[" ++ intercalate ", " (map convertExpr exprs) ++ "]"
  Dictionary items _ -> "-- TODO: dict"
  Subscript e idx _ -> 
    let baseExpr = convertExpr e
        indexExpr = convertExpr idx
    in if "-- TODO:" `isPrefixOf` baseExpr
       then "-- TODO: subscript"
       else baseExpr ++ " !! " ++ indexExpr
  BinaryOp op left right _ -> 
      case op of
        Equality _ -> convertExpr left ++ " == " ++ convertExpr right
        _ -> convertExpr left ++ " <op> " ++ convertExpr right
  -- Handle b('string') calls - extract just the string content
  Call (Var (Ident "b" _) _) [ArgExpr (Strings [s] _) _] _ -> "\"" ++ stripQuotes s ++ "\""
  Call (Var (Ident "len" _) _) [ArgExpr listExpr _] _ -> "length " ++ convertExpr listExpr
  -- Handle self.client.method() calls
  Call (Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident method _) _) args _ ->
    convertClientMethod method args
  -- Handle method calls on expressions - simplified to avoid complex parsing
  Call (Dot expr (Ident method _) _) args _ -> 
    "-- TODO: method call " ++ convertExpr expr ++ "." ++ method ++ "(...)"
  -- Handle attribute access like rev.author
  Dot expr (Ident attr _) _ -> 
    let baseExpr = convertExpr expr
    in if baseExpr == "self.client"
       then "-- TODO: client." ++ attr
       else convertAttributeAccess baseExpr attr
  -- Handle parenthesized expressions
  Paren expr _ -> convertExpr expr
  -- Handle sliced expressions - typically too complex
  SlicedExpr _ _ _ -> "-- TODO: slice"
  _ -> "-- TODO: expr"
  where
    stripQuotes s 
      | length s >= 2 && head s == '\'' && last s == '\'' = init (tail s)
      | length s >= 2 && head s == '"' && last s == '"' = init (tail s)
      | otherwise = s
    
    isComplexExpr e = case e of
      Call _ _ _ -> True
      Dot _ _ _ -> True
      SlicedExpr _ _ _ -> True
      _ -> False

-- | Extract string content, handling b('string') calls and direct strings
extractStringContent :: ExprSpan -> String
extractStringContent expr = case expr of
  Strings [s] _ -> "\"" ++ stripQuotes s ++ "\""
  Call (Var (Ident "b" _) _) [ArgExpr (Strings [s] _) _] _ -> "\"" ++ stripQuotes s ++ "\""
  _ -> convertExpr expr
  where
    stripQuotes s 
      | length s >= 2 && head s == '\'' && last s == '\'' = init (tail s)
      | length s >= 2 && head s == '"' && last s == '"' = init (tail s)
      | otherwise = s

-- | Convert client method calls
convertClientMethod :: String -> [ArgumentSpan] -> String
convertClientMethod method args = case method of
  "commit" -> "C.commit client " ++ convertCommitArgs args
  "log" -> "C.log_ client " ++ convertLogArgs args ++ " C.defaultLogOptions"
  "branches" -> "C.branches client " ++ convertBranchesArgs args  
  "tip" -> "C.tip client"
  "update" -> "C.update client " ++ convertUpdateArgs args
  "branch" -> case args of
    [ArgExpr branchName _] -> "C.branch client (Just " ++ convertExpr branchName ++ ") []"
    _ -> "C.branch client Nothing []"
  _ -> "-- TODO: client method " ++ method ++ "(" ++ intercalate ", " (map convertArg args) ++ ")"

-- | Convert update arguments
convertUpdateArgs :: [ArgumentSpan] -> String
convertUpdateArgs [] = "C.defaultUpdateOptions"
convertUpdateArgs args = 
  let opts = extractKeywordArgs args
      buildOptions base [] = base
      buildOptions base ((key, value):rest) = 
        let updated = case key of
              "clean" -> base ++ " { C.updateClean = " ++ value ++ " }"
              "check" -> base ++ " { C.updateCheck = " ++ value ++ " }"
              "rev" -> base ++ " { C.updateRev = Just " ++ value ++ " }"
              _ -> base ++ " -- TODO: " ++ key ++ " = " ++ value
        in buildOptions updated rest
  in "(" ++ buildOptions "C.defaultUpdateOptions" opts ++ ")"

-- | Convert attribute access to Haskell record accessors
convertAttributeAccess :: String -> String -> String
convertAttributeAccess baseExpr attr = case attr of
  "author" -> "revAuthor " ++ baseExpr
  "desc" -> "revDesc " ++ baseExpr
  "date" -> "revDate " ++ baseExpr
  "node" -> "revNode " ++ baseExpr
  "rev" -> "revRev " ++ baseExpr
  "branch" -> "branchName " ++ baseExpr  -- for branch info
  _ -> "-- TODO: attr access " ++ baseExpr ++ "." ++ attr

extractTestMethods :: StatementSpan -> [String]
extractTestMethods (Class _ _ body _) = mapMaybe convertTestMethod body
extractTestMethods _ = []

extractTestMethodsFromSuite :: Suite SrcSpan -> [String]
extractTestMethodsFromSuite = mapMaybe convertTestMethod

-- Convert "test_summary" → "Summary"
moduleNameFromClassName :: String -> String
moduleNameFromClassName name =
  case L.stripPrefix "test_" name of
    Just rest -> capitalize rest
    Nothing   -> capitalize name
  where
    capitalize (x:xs) = toUpper x : xs
    capitalize [] = []

-- | Generate the Haskell module
generateHaskellModule :: FilePath -> ModuleSpan -> String
generateHaskellModule filePath (Module stmts) =
  let expectedClassName = takeBaseName filePath      -- e.g., "test_summary"
      moduleName = moduleNameFromClassName expectedClassName ++ "Spec" -- e.g., SummarySpec
      classStmt = L.find (\case
                            Class (Ident name _) _ _ _ 
                              -> map toLower name == map toLower expectedClassName
                            _ -> False) stmts
      classNames = [name | Class (Ident name _) _ _ _ <- stmts]
      testMethods = case classStmt of
                      Just (Class _ _ body _) -> extractTestMethodsFromSuite body
                      _ -> []
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
      , "import Data.Time"
      , "import Control.Exception (try, SomeException)"
      , ""
      , "-- Helper function to check if Either is Left"
      , "isLeft :: Either a b -> Bool"
      , "isLeft (Left _) = True"
      , "isLeft (Right _) = False"
      , ""
      , "spec :: Spec"
      , "spec = describe \"" ++ moduleNameFromClassName expectedClassName ++ "\" $ do"
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
          Just className -> do
            let haskellCode = generateHaskellModule pythonFile pyModule
            putStrLn haskellCode
          Nothing -> do
            putStrLn $ "Error: Could not find test class in " ++ pythonFile
            exitFailure
      _ -> do
        putStrLn "Usage: PythonTestcaseConverter <python-test-file>"
        exitFailure
