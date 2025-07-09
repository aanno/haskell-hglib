{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Stateful converter using State monad
module State.StatefulConverter where

import Control.Monad.State
import Control.Monad (when)
import Data.List (isPrefixOf, isInfixOf, intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Char (toUpper)
import System.FilePath (takeBaseName)

import Language.Python.Common.AST
import Language.Python.Common.SrcLocation
import State.ConverterState

-- | Convert a Python module to Haskell using State monad
convertModule :: FilePath -> ModuleSpan -> Converter String
convertModule filePath (Module stmts) = do
  let baseName = takeBaseName filePath
      moduleName = moduleNameFromClassName baseName
  
  -- Process all statements to gather context
  mapM_ processStatement stmts
  
  -- Find test class and convert methods
  testMethods <- findAndConvertTestMethods stmts
  
  -- Generate the final module
  generateHaskellModule moduleName testMethods

-- | Process a statement to gather context (first pass)
processStatement :: StatementSpan -> Converter ()
processStatement = \case
  Class name _ body _ -> do
    let className = ident_string name
    enterClass className
    mapM_ processStatement body
    exitClass
  
  Fun name _ _ body _ -> do
    let methodName = ident_string name
    enterMethod methodName
    when ("test_" `isPrefixOf` methodName) $ do
      description <- lookupTestDescription methodName
      when (description == methodName) $ do
        -- Auto-generate description if not found
        let autoDesc = "should " ++ drop 5 methodName
        registerTestMethod methodName autoDesc
    mapM_ processStatement body
    exitMethod
  
  Assign targets expr _ -> do
    -- Track variable assignments
    mapM_ (trackAssignment expr) targets
  
  Import items _ -> do
    -- Track imports
    mapM_ processImportItem items
  
  _ -> return ()

-- | Track variable assignments
trackAssignment :: ExprSpan -> ExprSpan -> Converter ()
trackAssignment expr = \case
  Var (Ident name _) _ -> do
    -- Simple variable assignment
    haskellExpr <- convertExpr expr
    addVariable name haskellExpr
  _ -> return ()

-- | Process import items
processImportItem :: ImportItemSpan -> Converter ()
processImportItem (ImportItem name _ _) = do
  let moduleName = show name  -- Convert DottedName to string for now
  addImport moduleName

-- | Find and convert test methods (second pass)
findAndConvertTestMethods :: [StatementSpan] -> Converter [String]
findAndConvertTestMethods stmts = do
  -- Find the test class
  testClass <- findTestClass stmts
  case testClass of
    Nothing -> do
      addError "No test class found"
      return []
    Just (Class _ _ body _) -> do
      -- Convert all test methods
      mapM convertTestMethod body >>= return . concat
    _ -> return []

-- | Find the test class in statements
findTestClass :: [StatementSpan] -> Converter (Maybe StatementSpan)
findTestClass [] = return Nothing
findTestClass (stmt@(Class name _ _ _) : _) = do
  -- Assume first class is test class
  return (Just stmt)
findTestClass (_ : rest) = findTestClass rest

-- | Convert a test method using State monad
convertTestMethod :: StatementSpan -> Converter [String]
convertTestMethod = \case
  Fun name _ _ body _ -> do
    let methodName = ident_string name
    if "test_" `isPrefixOf` methodName
      then do
        enterMethod methodName
        testCode <- generateHaskellTest methodName body
        exitMethod
        return [testCode]
      else return []
  _ -> return []

-- | Generate Haskell test code using State monad
generateHaskellTest :: String -> SuiteSpan -> Converter String
generateHaskellTest testName body = do
  -- Get test description from state
  testDesc <- lookupTestDescription testName
  
  -- Convert the test body
  bodyLines <- convertSuite body
  
  -- Ensure proper test ending
  finalBody <- ensureProperTestEnding bodyLines
  
  -- Check if we need monadic context
  needsDo <- or <$> mapM needsMonadicContext finalBody
  hasSequential <- hasSequentialStatements finalBody
  
  let useMonadic = needsDo || hasSequential
  
  return $ unlines $
    [ "  it \"" ++ testDesc ++ "\" $"
    ] ++ if useMonadic
         then [ "    withTestRepo $ \\bt -> do"
              , "      let client = btClient bt"
              ] ++ map ("      " ++) finalBody
         else [ "    withTestRepo $ \\bt ->"
              , "      let client = btClient bt"
              ] ++ map ("      " ++) finalBody

-- | Convert a suite of statements
convertSuite :: SuiteSpan -> Converter [String]
convertSuite stmts = do
  results <- mapM convertStatement stmts
  return $ concat results

-- | Convert a single statement using State monad
convertStatement :: StatementSpan -> Converter [String]
convertStatement stmt = case stmt of
  -- Handle assignments
  Assign targets expr _ -> do
    convertAssignment targets expr
  
  -- Handle expressions/function calls
  StmtExpr expr _ -> do
    convertExpressionStatement expr
  
  -- Handle assertions
  StmtExpr (Call (Dot (Var (Ident "self" _) _) (Ident assertMethod _) _) args _) _ -> do
    convertAssertion assertMethod args
  
  -- Handle if statements
  Conditional clauses elseSuite _ -> do
    convertIfStatement clauses elseSuite
  
  -- Handle with statements
  With context body _ -> do
    convertWithStatement context body
  
  -- Handle raise statements
  Raise expr _ -> do
    convertRaiseStatement expr
  
  -- Default case
  _ -> do
    addTodo $ "Unhandled statement: " ++ take 50 (show stmt)
    return ["-- TODO: " ++ take 50 (show stmt)]

-- | Convert assignment statements
convertAssignment :: [ExprSpan] -> ExprSpan -> Converter [String]
convertAssignment targets expr = do
  exprStr <- convertExpr expr
  case targets of
    [Var (Ident varName _) _] -> do
      -- Simple variable assignment
      addVariable varName exprStr
      return [varName ++ " <- " ++ exprStr]
    [Tuple vars _] -> do
      -- Tuple assignment
      varNames <- mapM extractVarName vars
      let tuplePattern = "(" ++ intercalate ", " varNames ++ ")"
      return [tuplePattern ++ " <- " ++ exprStr]
    _ -> do
      addTodo "Complex assignment pattern"
      return ["-- TODO: complex assignment"]

-- | Extract variable name from expression
extractVarName :: ExprSpan -> Converter String
extractVarName (Var (Ident name _) _) = return name
extractVarName _ = do
  addWarning "Non-variable in assignment target"
  return "_"

-- | Convert expression statements
convertExpressionStatement :: ExprSpan -> Converter [String]
convertExpressionStatement expr = do
  exprStr <- convertExpr expr
  return [exprStr]

-- | Convert assertions using state lookups
convertAssertion :: String -> [ArgumentSpan] -> Converter [String]
convertAssertion assertMethod args = do
  maybeHaskellAssert <- lookupAssertionMethod assertMethod
  case maybeHaskellAssert of
    Nothing -> do
      addWarning $ "Unknown assertion method: " ++ assertMethod
      return ["-- TODO: " ++ assertMethod]
    Just haskellAssert -> do
      case args of
        [ArgExpr actual _, ArgExpr expected _] -> do
          actualStr <- convertExpr actual
          expectedStr <- convertExpr expected
          return [actualStr ++ " `" ++ haskellAssert ++ "` " ++ expectedStr]
        _ -> do
          addTodo $ "Complex assertion: " ++ assertMethod
          return ["-- TODO: complex " ++ assertMethod]

-- | Convert expressions using State monad
convertExpr :: ExprSpan -> Converter String
convertExpr expr = case expr of
  Var (Ident name _) _ -> do
    -- Check if it's a tracked variable
    maybeVar <- lookupVariable name
    return $ fromMaybe name maybeVar
  
  Int i _ _ -> return $ show i
  
  Strings [s] _ -> return $ "\"" ++ stripQuotes s ++ "\""
  
  Bool b _ -> return $ show b
  
  Call (Dot (Dot (Var (Ident "self" _) _) (Ident "client" _) _) (Ident method _) _) args _ -> do
    -- Client method call
    maybeHaskellMethod <- lookupClientMethod method
    case maybeHaskellMethod of
      Nothing -> do
        addWarning $ "Unknown client method: " ++ method
        return $ "-- TODO: client." ++ method
      Just haskellMethod -> do
        argsStr <- convertArgs args
        return $ haskellMethod ++ " client " ++ argsStr
  
  Call (Var (Ident funcName _) _) args _ -> do
    -- Regular function call
    argsStr <- convertArgs args
    return $ funcName ++ " " ++ argsStr
  
  _ -> do
    addTodo $ "Unhandled expression: " ++ take 50 (show expr)
    return $ "-- TODO: " ++ take 50 (show expr)
  where
    stripQuotes s 
      | length s >= 2 && head s == '\'' && last s == '\'' = init (tail s)
      | length s >= 2 && head s == '"' && last s == '"' = init (tail s)
      | otherwise = s

-- | Convert function arguments
convertArgs :: [ArgumentSpan] -> Converter String
convertArgs [] = return ""
convertArgs args = do
  argStrs <- mapM convertArg args
  return $ intercalate " " argStrs

-- | Convert a single argument
convertArg :: ArgumentSpan -> Converter String
convertArg (ArgExpr expr _) = convertExpr expr
convertArg (ArgKeyword (Ident name _) expr _) = do
  exprStr <- convertExpr expr
  return $ name ++ "=" ++ exprStr
convertArg _ = return "-- TODO: complex arg"

-- | Placeholder functions that need to be implemented
convertIfStatement :: [(ExprSpan, SuiteSpan)] -> SuiteSpan -> Converter [String]
convertIfStatement _ _ = do
  addTodo "If statement conversion"
  return ["-- TODO: if statement"]

convertWithStatement :: [(ExprSpan, Maybe ExprSpan)] -> SuiteSpan -> Converter [String]
convertWithStatement _ _ = do
  addTodo "With statement conversion"
  return ["-- TODO: with statement"]

convertRaiseStatement :: RaiseExprSpan -> Converter [String]
convertRaiseStatement _ = do
  addTodo "Raise statement conversion"
  return ["-- TODO: raise statement"]

-- | Helper functions
ensureProperTestEnding :: [String] -> Converter [String]
ensureProperTestEnding [] = return ["pendingWith \"Empty test body\""]
ensureProperTestEnding lines = do
  let hasContent = any (not . isDeclarationOrComment) lines
  if hasContent
    then return lines
    else return $ lines ++ ["pendingWith \"Test not implemented yet\""]

isDeclarationOrComment :: String -> Bool
isDeclarationOrComment line = 
  let stripped = dropWhile (== ' ') line
  in "let " `isPrefixOf` stripped || 
     "-- " `isPrefixOf` stripped || 
     null stripped

needsMonadicContext :: String -> Converter Bool
needsMonadicContext line = do
  let monadicIndicators = [" <- ", "shouldBe", "C.", "result"]
  return $ any (`isInfixOf` line) monadicIndicators

hasSequentialStatements :: [String] -> Converter Bool
hasSequentialStatements lines = do
  let executableLines = filter (not . isDeclarationOrComment) lines
  return $ length executableLines > 1

-- | Generate the final Haskell module
generateHaskellModule :: String -> [String] -> Converter String
generateHaskellModule moduleName testMethods = do
  requiredImports <- getRequiredImports
  warnings <- getWarnings
  todos <- getTodos
  errors <- getErrors
  
  let moduleHeader = unlines $
        [ "{-# LANGUAGE OverloadedStrings #-}"
        , ""
        , "module Test.HgLib." ++ moduleName ++ "Spec (spec) where"
        , ""
        ] ++ map ("import " ++) requiredImports ++
        [ ""
        , "-- Helper function to check if Either is Left"
        , "isLeft :: Either a b -> Bool"
        , "isLeft (Left _) = True"
        , "isLeft (Right _) = False"
        , ""
        , "spec :: Spec"
        , "spec = describe \"" ++ moduleName ++ "\" $ do"
        ]
  
  let commentsSection = if null warnings && null todos && null errors
        then ""
        else unlines $ ["", "-- Conversion notes:"] ++
             map ("-- WARNING: " ++) warnings ++
             map ("-- TODO: " ++) todos ++
             map ("-- ERROR: " ++) errors ++ [""]
  
  return $ moduleHeader ++ commentsSection ++ unlines testMethods

-- | Convert class name to module name
moduleNameFromClassName :: String -> String
moduleNameFromClassName name =
  case dropWhile (/= '_') name of
    '_':rest -> capitalize rest
    _ -> capitalize name
  where
    capitalize [] = []
    capitalize (x:xs) = toUpper x : xs
