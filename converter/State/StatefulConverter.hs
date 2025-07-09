{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Stateful converter using State monad
module State.StatefulConverter where

import Control.Monad.State
import Control.Monad (when)
import Data.List (isPrefixOf, isInfixOf, intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
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

-- | Convert DottedName to string
dottedNameToString :: DottedNameSpan -> String
dottedNameToString name = 
  -- For now, use show and clean it up better
  let nameStr = show name
  in case nameStr of
    s | "DottedName" `isPrefixOf` s -> extractModuleName s
    _ -> nameStr
  where
    extractModuleName s = 
      -- Extract module name from DottedName show output
      -- This is a temporary hack until we figure out the proper constructor
      case s of
        _ | "\"os\"" `isInfixOf` s -> "os"
        _ | "\"hglib\"" `isInfixOf` s -> "hglib"
        _ | "\"datetime\"" `isInfixOf` s -> "datetime"
        _ | "\"subprocess\"" `isInfixOf` s -> "subprocess"
        _ | "\"tests\"" `isInfixOf` s -> "tests"
        _ | "\"common\"" `isInfixOf` s -> "common"
        _ -> takeWhile (/= ' ') $ drop 1 $ dropWhile (/= '"') s

-- | Process import items
processImportItem :: ImportItemSpan -> Converter ()
processImportItem (ImportItem name _ _) = do
  let moduleName = dottedNameToString name
  -- Map Python modules to Haskell modules
  let haskellModule = mapPythonModuleToHaskell moduleName
  -- Only add meaningful imports, skip common test modules
  when (not $ moduleName `elem` ["tests.common", "common.basetest"]) $ do
    addImport haskellModule

-- | Map Python module names to Haskell module names
mapPythonModuleToHaskell :: String -> String
mapPythonModuleToHaskell pyModule = case pyModule of
  "os" -> "System.IO"
  "os.path" -> "System.FilePath"
  "datetime" -> "Data.Time"
  "subprocess" -> "System.Process"
  "hglib" -> "qualified HgLib"
  "hglib.util" -> "HgLib.Util"
  "hglib.error" -> "HgLib.Error"
  _ -> pyModule

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
      else if methodName == "setUp"
        then do
          -- Process setUp method to extract setup code
          setupCode <- convertSetUp body
          addSetupCode setupCode
          return []
        else return []
  _ -> return []

-- | Generate Haskell test code using State monad
generateHaskellTest :: String -> SuiteSpan -> Converter String
generateHaskellTest testName body = do
  -- Get test description from state
  testDesc <- lookupTestDescription testName
  
  -- Convert the test body
  bodyLines <- convertSuite body
  
  -- Get setup code
  setupCode <- getSetupCode
  
  -- Combine setup code with test body
  let allBodyLines = setupCode ++ bodyLines
  
  -- Ensure proper test ending
  finalBody <- ensureProperTestEnding allBodyLines
  
  -- Check if we need monadic context
  needsDo <- or <$> mapM needsMonadicContext finalBody
  hasSequential <- hasSequentialStatements finalBody
  
  -- Check if test involves config file operations
  needsClientReopen <- needsClientReopenForConfig finalBody
  
  let useMonadic = needsDo || hasSequential
  
  return $ unlines $
    [ "  it \"" ++ testDesc ++ "\" $"
    ] ++ if useMonadic
         then if needsClientReopen
              then [ "    withTestRepo $ \\bt -> do"
                   , "      let client = btClient bt"
                   ] ++ map ("      " ++) finalBody ++
                   [ "      closeClient client"
                   , "      client' <- openClient nonInteractiveConfig"
                   , "      -- ... continue with client' ..."
                   , "      closeClient client'"
                   ]
              else [ "    withTestRepo $ \\bt -> do"
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

-- | Apply indentation to statement results if in monadic context
applyIndentationToResults :: [String] -> Converter [String]
applyIndentationToResults results = do
  inMonadic <- isInMonadicContext
  if inMonadic
    then mapM applyIndentation results
    else return results

-- | Convert a single statement using State monad
convertStatement :: StatementSpan -> Converter [String]
convertStatement stmt = do
  -- Save current indentation state
  currentIndent <- getCurrentIndentLevel
  currentMonadic <- isInMonadicContext
  
  -- Convert the statement
  result <- convertStatement' stmt
  
  -- Apply indentation based on the state BEFORE processing this statement
  indentedResult <- if currentMonadic
    then mapM (\line -> do
      let indent = replicate (currentIndent * 2) ' '
      return $ indent ++ line) result
    else return result
    
  return indentedResult

convertStatement' :: StatementSpan -> Converter [String]
convertStatement' stmt = case stmt of
  -- Handle assignments
  Assign targets expr _ -> do
    convertAssignment targets expr
  
  -- Handle assertions (must come before general StmtExpr)
  StmtExpr (Call (Dot (Var (Ident "self" _) _) (Ident assertMethod _) _) args _) _ -> do
    -- Only treat as assertion if it's an actual assertion method
    maybeHaskellAssert <- lookupAssertionMethod assertMethod
    case maybeHaskellAssert of
      Just _ -> convertAssertion assertMethod args
      Nothing -> do
        -- Handle as a regular method call like self.append()
        result <- convertVariableMethodCall "self" assertMethod args
        return [result]
  
  -- Handle expressions/function calls
  StmtExpr expr _ -> do
    convertExpressionStatement expr
  
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
  case (targets, expr) of
    -- Handle file handle assignments from open() calls
    ([Var (Ident varName _) _], Call (Var (Ident "open" _) _) args _) -> do
      -- Convert open() to withFile and track handle mapping
      withFileExpr <- convertOpenCall args
      -- Map the Python variable to the lambda parameter 'h'
      addVariable varName "h"
      -- Set up indentation for subsequent statements inside the withFile block
      setMonadicContext True
      increaseIndentLevel
      return [withFileExpr ++ " do"]
    
    -- Handle regular assignments
    ([Var (Ident varName _) _], _) -> do
      -- Simple variable assignment
      exprStr <- convertExpr expr
      addVariable varName exprStr
      return [varName ++ " <- " ++ exprStr]
    ([Tuple vars _], _) -> do
      -- Tuple assignment
      exprStr <- convertExpr expr
      varNames <- mapM extractVarName vars
      let tuplePattern = "(" ++ intercalate ", " varNames ++ ")"
      return [tuplePattern ++ " <- " ++ exprStr]
    ([Dot (Var (Ident objName _) _) (Ident attrName _) _], _) -> do
      -- Attribute assignment (e.g., self.client = ...)
      exprStr <- convertExpr expr
      if objName == "self"
        then do
          -- For self.client assignments, we typically don't need them in Haskell tests
          return ["-- TODO: " ++ objName ++ "." ++ attrName ++ " = " ++ exprStr ++ " (handled by withTestRepo)"]
        else do
          addTodo $ "Attribute assignment: " ++ objName ++ "." ++ attrName
          return ["-- TODO: " ++ objName ++ "." ++ attrName ++ " <- " ++ exprStr]
    (_, _) -> do
      exprStr <- convertExpr expr
      addTodo $ "Complex assignment pattern: " ++ show targets
      return ["-- TODO: complex assignment: " ++ show targets]

-- | Extract variable name from expression
extractVarName :: ExprSpan -> Converter String
extractVarName (Var (Ident name _) _) = return name
extractVarName (Dot (Var (Ident objName _) _) (Ident attrName _) _) = do
  -- Handle self.attrName -> attrName (e.g., self.rev0 -> rev0)
  if objName == "self"
    then do
      -- Track this as a variable assignment for later reference
      let varName = attrName
      addVariable ("self." ++ attrName) varName
      return varName
    else do
      addWarning $ "Non-self attribute assignment: " ++ objName ++ "." ++ attrName
      return "_"
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
      case (assertMethod, args) of
        -- Handle assertTrue with 'in' operator - must come before single argument case
        ("assertTrue", [ArgExpr (BinaryOp (In _) left right _) _]) -> do
          leftStr <- convertExpr left
          rightStr <- convertExpr right
          return (["let hasValue = any (\\item -> item == " ++ leftStr ++ ") " ++ rightStr]
                 ++ ["hasValue `shouldBe` True"])
        -- assertTrue with single argument
        ("assertTrue", [ArgExpr actual _]) -> do
          actualStr <- convertExpr actual
          return [actualStr ++ " `shouldBe` True"]
        -- assertTrue with two arguments (actually assertEqual)
        ("assertTrue", [ArgExpr actual _, ArgExpr expected _]) -> do
          actualStr <- convertExpr actual
          expectedStr <- convertExpr expected
          return [actualStr ++ " `shouldBe` " ++ expectedStr]
        -- assertFalse with single argument  
        ("assertFalse", [ArgExpr actual _]) -> do
          actualStr <- convertExpr actual
          return [actualStr ++ " `shouldBe` False"]
        -- assertEqual with two arguments
        ("assertEqual", [ArgExpr actual _, ArgExpr expected _]) -> do
          actualStr <- convertExpr actual
          expectedStr <- convertExpr expected
          return [actualStr ++ " `shouldBe` " ++ expectedStr]
        -- assertRaises with exception type and lambda/function
        ("assertRaises", [ArgExpr excType _, ArgExpr func _]) -> do
          funcStr <- convertExpr func
          return [funcStr ++ " `shouldThrow` anyException"]
        -- assertRaises with exception type, function, and arguments
        ("assertRaises", [ArgExpr excType _, ArgExpr func _, ArgExpr args _]) -> do
          funcStr <- convertExpr func
          argsStr <- convertExpr args
          return [funcStr ++ " " ++ argsStr ++ " `shouldThrow` anyException"]
        -- assertRaises with more complex patterns
        ("assertRaises", excType : func : rest) -> do
          funcStr <- convertArg func
          if null rest
            then return [funcStr ++ " `shouldThrow` anyException"]
            else do
              restStr <- mapM convertArg rest
              return [funcStr ++ " " ++ intercalate " " restStr ++ " `shouldThrow` anyException"]
        -- Generic two-argument case
        _ | length args == 2 -> do
          case args of
            [ArgExpr actual _, ArgExpr expected _] -> do
              actualStr <- convertExpr actual
              expectedStr <- convertExpr expected
              return [actualStr ++ " `" ++ haskellAssert ++ "` " ++ expectedStr]
            _ -> do
              addTodo $ "Complex assertion args: " ++ assertMethod
              return ["-- TODO: complex " ++ assertMethod ++ " args"]
        -- Handle other cases
        _ -> do
          addTodo $ "Complex assertion: " ++ assertMethod ++ " with " ++ show (length args) ++ " args"
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
    maybeCommandMetadata <- lookupClientMethod method
    case maybeCommandMetadata of
      Nothing -> do
        addWarning $ "Unknown client method: " ++ method
        return $ "-- TODO: client." ++ method
      Just commandMetadata -> do
        (positionalArgs, keywordArgs) <- separateArgs args
        buildCommandCall commandMetadata positionalArgs keywordArgs
  
  Call (Var (Ident funcName _) _) args _ -> do
    -- Handle special function calls
    case funcName of
      "open" -> convertOpenCall args
      "b" -> convertBytesCall args  -- Handle b('string') calls
      "int" -> convertIntCall args  -- Handle int() calls
      "len" -> convertLenCall args  -- Handle len() calls
      _ -> do
        -- Regular function call
        argsStr <- convertArgs args
        return $ funcName ++ " " ++ argsStr
  
  -- Handle binary operators
  BinaryOp op left right _ -> do
    convertBinaryOp left op right
  
  -- Handle lists
  List exprs _ -> do
    convertList exprs
  
  -- Handle tuples
  Tuple exprs _ -> do
    convertTuple exprs
  
  -- Handle method calls on variables (e.g., f.write(), f.close())
  Call (Dot (Var (Ident varName _) _) (Ident methodName _) _) args _ -> do
    convertVariableMethodCall varName methodName args
  
  -- Handle module.function() calls (e.g., hglib.open(), os.path.abspath())
  Call (Dot (Dot (Var (Ident modName _) _) (Ident subModName _) _) (Ident funcName _) _) args _ -> do
    convertModuleCall modName subModName funcName args
  
  -- Handle module.function() calls with 2 levels (e.g., hglib.open())
  Call (Dot (Var (Ident modName _) _) (Ident funcName _) _) args _ -> do
    convertModuleCall modName "" funcName args
  
  -- Handle chained method calls (e.g., datetime.datetime.now().replace(...))
  Call (Dot (Call innerCallExpr innerCallArgs _) (Ident methodName _) _) args _ -> do
    innerCallStr <- convertExpr (Call innerCallExpr innerCallArgs (error "span placeholder"))
    argsStr <- convertArgs args
    return $ "-- TODO: " ++ methodName ++ " chained call on " ++ innerCallStr ++ " " ++ argsStr
  
  -- Handle parenthesized expressions
  Paren innerExpr _ -> do
    convertExpr innerExpr
  
  -- Handle attribute access (e.g., os.path.abspath, hglib.error.CommandError)
  Dot (Dot (Var (Ident modName _) _) (Ident subModName _) _) (Ident attrName _) _ -> do
    convertModuleAttr modName subModName attrName
  
  -- Handle 2-level attribute access (e.g., hglib.open, rev.author)
  Dot (Var (Ident modName _) _) (Ident attrName _) _ -> do
    -- Special case for self.attribute -> lookup in variable map
    if modName == "self"
      then do
        let selfAttrName = "self." ++ attrName
        maybeVar <- lookupVariable selfAttrName
        return $ fromMaybe ("-- TODO: " ++ selfAttrName) maybeVar
      -- Handle regular variable attribute access (e.g., rev.author -> revAuthor rev)
      else if modName `elem` ["rev", "rev0", "rev1", "revclose", "node", "node0", "node1"]
        then return $ convertDotAccess modName attrName
      else convertModuleAttr modName "" attrName
  
  -- Handle attribute access on subscript results (e.g., result[0].node)
  Dot (Subscript expr index _) (Ident attrName _) _ -> do
    subscriptStr <- convertSubscript expr index
    return $ convertDotAccess subscriptStr attrName
  
  -- Handle generic attribute access (e.g., obj.attr)
  Dot expr (Ident attrName _) _ -> do
    exprStr <- convertExpr expr
    return $ convertDotAccess exprStr attrName
  
  -- Handle subscript/indexing expressions (e.g., list[0], dict[key])
  Subscript expr index _ -> do
    convertSubscript expr index
  
  -- Handle sliced expressions (e.g., list[:12], node[:12])
  SlicedExpr expr slices _ -> do
    convertSlicedExpr expr slices
  
  -- Handle lambda expressions (e.g., lambda: self.client.commit(...))
  Lambda args body _ -> do
    convertLambda args body
  
  _ -> do
    addTodo $ "Unhandled expression: " ++ take 50 (show expr)
    return $ "-- TODO: " ++ take 50 (show expr)
  where
    stripQuotes s 
      | length s >= 2 && head s == '\'' && last s == '\'' = init (tail s)
      | length s >= 2 && head s == '"' && last s == '"' = init (tail s)
      | otherwise = s

-- | Build command call with proper argument handling using CommandMetadata
buildCommandCall :: CommandMetadata -> [ArgumentSpan] -> [ArgumentSpan] -> Converter String
buildCommandCall CommandMetadata{..} positionalArgs keywordArgs = do
  let hasRequiredArgs = not (null cmdRequiredArgs)
  let hasKeywordArgs = not (null keywordArgs)
  
  if hasKeywordArgs
    then do
      -- Convert keyword arguments to options
      optionsStr <- convertMethodOptions cmdFunction keywordArgs
      
      if hasRequiredArgs
        then do
          -- Command has required args AND keyword args - use constructor with record syntax
          case positionalArgs of
            argExprs | length argExprs == length cmdRequiredArgs -> do
              requiredArgStrs <- mapM (convertExpr . extractArgExpr) argExprs
              let constructorCall = cmdOptionsConstructor ++ " " ++ unwords requiredArgStrs
              return $ cmdFunction ++ " client ((" ++ constructorCall ++ ") { " ++ optionsStr ++ " })"
            [] | cmdFunction == "C.commit" -> do
              -- Special case: commit with only keyword args (e.g., amend=True) - no message needed
              return $ cmdFunction ++ " client (" ++ cmdOptionsConstructor ++ " \"\" { " ++ optionsStr ++ " })"
            _ -> do
              -- Wrong number of required arguments
              posArgsStr <- convertArgs positionalArgs
              let defaultArgs = intercalate " " cmdDefaultArgs
              let finalArgs = if null posArgsStr then defaultArgs else posArgsStr
              return $ cmdFunction ++ " client " ++ finalArgs ++ " (" ++ cmdOptionsConstructor ++ " { " ++ optionsStr ++ " })"
        else do
          -- No required args, just keyword args - use default constructor with record syntax  
          posArgsStr <- convertArgs positionalArgs
          let defaultArgs = intercalate " " cmdDefaultArgs
          let finalArgs = if null posArgsStr then defaultArgs else posArgsStr
          return $ cmdFunction ++ " client " ++ finalArgs ++ " (" ++ cmdOptionsConstructor ++ " { " ++ optionsStr ++ " })"
    else do
      -- No keyword arguments
      if hasRequiredArgs
        then do
          -- Command has required args but no keyword args - use constructor without record syntax
          case positionalArgs of
            argExprs | length argExprs == length cmdRequiredArgs -> do
              requiredArgStrs <- mapM (convertExpr . extractArgExpr) argExprs
              let constructorCall = cmdOptionsConstructor ++ " " ++ unwords requiredArgStrs
              return $ cmdFunction ++ " client (" ++ constructorCall ++ ")"
            _ -> do
              -- Wrong number of required arguments or additional args
              posArgsStr <- convertArgs positionalArgs
              let defaultArgs = intercalate " " cmdDefaultArgs
              let finalArgs = if null posArgsStr then defaultArgs else posArgsStr
              return $ cmdFunction ++ " client " ++ finalArgs
        else do
          -- No required args, no keyword args - simple call
          posArgsStr <- convertArgs positionalArgs
          let defaultArgs = intercalate " " cmdDefaultArgs
          let finalArgs = if null posArgsStr then defaultArgs else posArgsStr
          return $ cmdFunction ++ " client " ++ finalArgs

-- | Extract expression from argument
extractArgExpr :: ArgumentSpan -> ExprSpan
extractArgExpr (ArgExpr expr _) = expr
extractArgExpr _ = error "Only simple expressions supported in required arguments"

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
  -- For keyword arguments, we need to handle them as record syntax or function options
  -- This is a simplified conversion - may need more sophisticated handling
  return $ "-- TODO: keyword arg " ++ name ++ "=" ++ exprStr
convertArg _ = return "-- TODO: complex arg"

-- | Separate positional and keyword arguments
separateArgs :: [ArgumentSpan] -> Converter ([ArgumentSpan], [ArgumentSpan])
separateArgs args = do
  let (positional, keyword) = separateArgs' args
  return (positional, keyword)
  where
    separateArgs' [] = ([], [])
    separateArgs' (arg@(ArgExpr _ _):rest) = 
      let (pos, kw) = separateArgs' rest
      in (arg:pos, kw)
    separateArgs' (arg@(ArgKeyword _ _ _):rest) = 
      let (pos, kw) = separateArgs' rest
      in (pos, arg:kw)
    separateArgs' (arg:rest) = 
      let (pos, kw) = separateArgs' rest
      in (arg:pos, kw)

-- | Convert keyword argument to string
convertKeywordArg :: ArgumentSpan -> Converter String
convertKeywordArg (ArgKeyword (Ident name _) expr _) = do
  exprStr <- convertExpr expr
  return $ name ++ "=" ++ exprStr
convertKeywordArg _ = return "-- TODO: non-keyword arg"

-- | Convert method options to proper Haskell record syntax
convertMethodOptions :: String -> [ArgumentSpan] -> Converter String
convertMethodOptions cmdFunction keywordArgs = do
  -- Extract method name from command function (e.g., "C.commit" -> "commit")
  let method = if "C." `isPrefixOf` cmdFunction 
               then drop 2 cmdFunction 
               else cmdFunction
  case method of
    "commit" -> do
      options <- mapM convertCommitOption keywordArgs
      let optionsStr = intercalate ", " (filter (not . null) options)
      return optionsStr
    "log_" -> do  -- Note: log_ vs log due to Haskell keyword conflict
      options <- mapM convertLogOption keywordArgs
      let optionsStr = intercalate ", " (filter (not . null) options)
      return optionsStr
    _ -> do
      -- Fallback for unknown methods
      keywordStr <- mapM convertKeywordArg keywordArgs
      return $ "-- TODO: options " ++ intercalate " " keywordStr

-- | Convert commit-specific options
convertCommitOption :: ArgumentSpan -> Converter String
convertCommitOption (ArgKeyword (Ident name _) expr _) = do
  exprStr <- convertExpr expr
  case name of
    "addremove" -> return $ "C.commitAddRemove = " ++ exprStr
    "user" -> return $ "C.commitUser = Just " ++ exprStr
    "date" -> return $ "C.commitDate = Just " ++ exprStr
    "closebranch" -> return $ "C.commitCloseBranch = " ++ exprStr
    "amend" -> return $ "C.commitAmend = " ++ exprStr
    _ -> return $ "-- TODO: unknown commit option " ++ name
convertCommitOption _ = return ""

-- | Convert log-specific options
convertLogOption :: ArgumentSpan -> Converter String
convertLogOption (ArgKeyword (Ident name _) expr _) = do
  exprStr <- convertExpr expr
  case name of
    "rev" -> return $ "C.logRev = Just " ++ exprStr
    "files" -> return $ "C.logFiles = " ++ exprStr
    "hidden" -> return $ "C.logHidden = " ++ exprStr
    "revrange" -> return $ "C.logRevRange = Just " ++ exprStr
    "keyword" -> return $ "C.logKeyword = Just " ++ exprStr
    _ -> return $ "-- TODO: unknown log option " ++ name
convertLogOption _ = return ""

-- | Convert binary operators
convertBinaryOp :: ExprSpan -> OpSpan -> ExprSpan -> Converter String
convertBinaryOp left op right = do
  leftStr <- convertExpr left
  rightStr <- convertExpr right
  -- For now, handle the most common case and fall back to string representation
  let opStr = case show op of
        s | "In" `isInfixOf` s -> "elem " ++ leftStr ++ " " ++ rightStr
        s | "And" `isInfixOf` s -> leftStr ++ " && " ++ rightStr
        s | "Or" `isInfixOf` s -> leftStr ++ " || " ++ rightStr
        s | "Eq" `isInfixOf` s -> leftStr ++ " == " ++ rightStr
        s | "NotEq" `isInfixOf` s -> leftStr ++ " /= " ++ rightStr
        s | "Plus" `isInfixOf` s -> leftStr ++ " ++ " ++ rightStr
        s | "Minus" `isInfixOf` s -> leftStr ++ " - " ++ rightStr
        s | "Multiply" `isInfixOf` s -> leftStr ++ " * " ++ rightStr
        s | "Divide" `isInfixOf` s -> leftStr ++ " / " ++ rightStr
        _ -> leftStr ++ " -- TODO: " ++ show op ++ " " ++ rightStr
  let knownOps = ["elem", "&&", "||", "==", "/=", "++", " - ", " * ", " / "]
      isKnownOp = any (`isInfixOf` opStr) knownOps || "elem" `isPrefixOf` opStr
  if isKnownOp
    then return opStr
    else do
      addTodo $ "Unhandled binary operator: " ++ show op
      return opStr

-- | Convert list expressions
convertList :: [ExprSpan] -> Converter String
convertList exprs = do
  elemStrs <- mapM convertExpr exprs
  return $ "[" ++ intercalate ", " elemStrs ++ "]"

-- | Convert tuple expressions
convertTuple :: [ExprSpan] -> Converter String
convertTuple exprs = do
  elemStrs <- mapM convertExpr exprs
  return $ "(" ++ intercalate ", " elemStrs ++ ")"

-- | Convert open() calls to withFile
convertOpenCall :: [ArgumentSpan] -> Converter String
convertOpenCall args = do
  case args of
    [ArgExpr pathExpr _, ArgExpr modeExpr _] -> do
      pathStr <- convertExpr pathExpr
      modeStr <- convertExpr modeExpr
      let haskellMode = pythonModeToHaskell modeStr
      -- Add System.IO import for file operations
      addImport "System.IO"
      return $ "withFile " ++ pathStr ++ " " ++ haskellMode ++ " $ \\h ->"
    _ -> do
      addTodo $ "Complex open() call with " ++ show (length args) ++ " args"
      return "-- TODO: complex open() call"
  where
    pythonModeToHaskell mode = case mode of
      "\"r\"" -> "ReadMode"
      "\"w\"" -> "WriteMode"
      "\"a\"" -> "AppendMode"
      "\"rb\"" -> "ReadMode"      -- Read binary -> ReadMode
      "\"wb\"" -> "WriteMode"     -- Write binary -> WriteMode
      "\"ab\"" -> "AppendMode"    -- Append binary -> AppendMode
      "\"r+\"" -> "ReadWriteMode"  -- Read/write -> ReadWriteMode
      "\"w+\"" -> "ReadWriteMode"  -- Write/read -> ReadWriteMode
      "\"a+\"" -> "ReadWriteMode"  -- Append/read -> ReadWriteMode
      "\"rb+\"" -> "ReadWriteMode" -- Read/write binary -> ReadWriteMode
      "\"wb+\"" -> "ReadWriteMode" -- Write/read binary -> ReadWriteMode
      "\"ab+\"" -> "ReadWriteMode" -- Append/read binary -> ReadWriteMode
      _ -> "-- TODO: unknown mode " ++ mode

-- | Convert b() calls (bytes literals)
convertBytesCall :: [ArgumentSpan] -> Converter String
convertBytesCall args = do
  case args of
    [ArgExpr strExpr _] -> do
      strStr <- convertExpr strExpr
      return strStr  -- For now, just return the string as-is
    _ -> do
      addTodo $ "Complex b() call"
      return "-- TODO: complex b() call"

-- | Convert int() calls
convertIntCall :: [ArgumentSpan] -> Converter String
convertIntCall args = do
  case args of
    [ArgExpr expr _] -> do
      exprStr <- convertExpr expr
      return $ "read " ++ exprStr
    _ -> do
      addTodo $ "Complex int() call"
      return "-- TODO: complex int() call"

-- | Convert len() calls
convertLenCall :: [ArgumentSpan] -> Converter String
convertLenCall args = do
  case args of
    [ArgExpr expr _] -> do
      exprStr <- convertExpr expr
      return $ "length " ++ exprStr
    _ -> do
      addTodo $ "Complex len() call"
      return "-- TODO: complex len() call"

-- | Convert method calls on variables (e.g., f.write(), f.close())
convertVariableMethodCall :: String -> String -> [ArgumentSpan] -> Converter String
convertVariableMethodCall varName methodName args = do
  -- Check if the variable has a mapping (e.g., f -> h for file handles)
  actualVarName <- do
    maybeMapping <- lookupVariable varName
    case maybeMapping of
      Just mapping -> return mapping
      Nothing -> return varName
  
  case (actualVarName, methodName) of
    ("self", "append") -> do
      -- self.append(file, content) -> commonAppendFile
      case args of
        [ArgExpr fileExpr _, ArgExpr contentExpr _] -> do
          fileStr <- convertExpr fileExpr
          contentStr <- convertExpr contentExpr
          return $ "commonAppendFile " ++ fileStr ++ " " ++ contentStr
        _ -> do
          addTodo $ "Complex self.append() call"
          return "-- TODO: complex self.append() call"
    (_, "write") -> do
      case args of
        [ArgExpr contentExpr _] -> do
          contentStr <- convertExpr contentExpr
          return $ "hPutStrLn " ++ actualVarName ++ " " ++ contentStr
        _ -> do
          addTodo $ "Complex write() call"
          return "-- TODO: complex write() call"
    (_, "close") -> do
      return $ "-- TODO: close " ++ varName ++ " (handled by withFile)"
    (_, "append") -> do
      -- Generic list.append(item) -> list concatenation
      case args of
        [ArgExpr itemExpr _] -> do
          itemStr <- convertExpr itemExpr
          return $ actualVarName ++ " ++ [" ++ itemStr ++ "]"
        _ -> do
          addTodo $ "Complex append() call"
          return "-- TODO: complex append() call"
    (_, "reverse") -> do
      -- list.reverse() -> reverse list (note: Python does in-place, Haskell doesn't)
      case args of
        [] -> return $ "-- TODO: " ++ actualVarName ++ " <- return (reverse " ++ actualVarName ++ ") -- Note: Python reverse() is in-place, Haskell reverse is not"
        _ -> do
          addTodo $ "Complex reverse() call"
          return "-- TODO: complex reverse() call"
    _ -> do
      addTodo $ "Unhandled method call: " ++ varName ++ "." ++ methodName
      return $ "-- TODO: " ++ varName ++ "." ++ methodName

-- | Convert module function calls (e.g., hglib.open(), os.path.abspath())
convertModuleCall :: String -> String -> String -> [ArgumentSpan] -> Converter String
convertModuleCall modName subModName funcName args = do
  let fullName = if null subModName then modName ++ "." ++ funcName 
                                    else modName ++ "." ++ subModName ++ "." ++ funcName
  case (modName, subModName, funcName) of
    ("hglib", "", "open") -> do
      -- hglib.open() -> openClient nonInteractiveConfig
      return "openClient nonInteractiveConfig"
    ("os", "path", "abspath") -> do
      case args of
        [ArgExpr pathExpr _] -> do
          pathStr <- convertExpr pathExpr
          return $ "System.FilePath.normalise " ++ pathStr
        _ -> do
          addTodo $ "Complex os.path.abspath() call"
          return "-- TODO: complex os.path.abspath() call"
    ("hglib", "error", "CommandError") -> do
      -- This is likely an exception type
      return "CommandError"
    ("os", "", "remove") -> do
      case args of
        [ArgExpr pathExpr _] -> do
          pathStr <- convertExpr pathExpr
          return $ "commonRemoveFile " ++ pathStr
        _ -> do
          addTodo $ "Complex os.remove() call"
          return "-- TODO: complex os.remove() call"
    ("datetime", "", "now") -> do
      return "getCurrentTime"
    ("datetime", "datetime", "now") -> do
      return "getCurrentTime"
    _ -> do
      addTodo $ "Unhandled module call: " ++ fullName
      return $ "-- TODO: " ++ fullName

-- | Convert module attribute access (e.g., os.path.abspath, hglib.error.CommandError)
convertModuleAttr :: String -> String -> String -> Converter String
convertModuleAttr modName subModName attrName = do
  let fullName = if null subModName then modName ++ "." ++ attrName 
                                    else modName ++ "." ++ subModName ++ "." ++ attrName
  case (modName, subModName, attrName) of
    ("os", "path", "abspath") -> do
      return "System.FilePath.normalise"
    ("hglib", "error", "CommandError") -> do
      return "CommandError"
    ("hglib", "", "open") -> do
      return "openClient nonInteractiveConfig"
    _ -> do
      addTodo $ "Unhandled module attribute: " ++ fullName
      return $ "-- TODO: " ++ fullName

-- | Convert subscript expressions (e.g., list[0], dict[key])
convertSubscript :: ExprSpan -> ExprSpan -> Converter String
convertSubscript expr indexExpr = do
  exprStr <- convertExpr expr
  indexStr <- convertExpr indexExpr
  case indexStr of
    "0" -> return $ "head " ++ exprStr
    "1" -> return $ "(" ++ exprStr ++ " !! 1)"
    _ -> return $ "(" ++ exprStr ++ " !! " ++ indexStr ++ ")"

-- | Convert sliced expressions (e.g., list[:12], node[:12])
convertSlicedExpr :: ExprSpan -> [SliceSpan] -> Converter String
convertSlicedExpr expr slices = do
  exprStr <- convertExpr expr
  case slices of
    [SliceProper Nothing (Just (Int n _ _)) Nothing _] -> do
      -- Simple slice [:n] -> take n
      return $ "take " ++ show n ++ " " ++ exprStr
    [SliceProper (Just (Int start _ _)) (Just (Int end _ _)) Nothing _] -> do
      -- Range slice [start:end] -> take (end-start) . drop start
      let len = end - start
      return $ "take " ++ show len ++ " (drop " ++ show start ++ " " ++ exprStr ++ ")"
    _ -> do
      addTodo $ "Complex slicing pattern: " ++ show slices
      return $ "-- TODO: " ++ exprStr ++ "[sliced]"

-- | Convert lambda expressions
convertLambda :: [ParameterSpan] -> ExprSpan -> Converter String
convertLambda args body = do
  bodyStr <- convertExpr body
  case args of
    [] -> do
      -- No arguments lambda (lambda: expr) -> (\() -> expr)
      return $ "(\\_-> " ++ bodyStr ++ ")"
    _ -> do
      -- Lambda with arguments - for now, create a simple conversion
      argNames <- mapM extractParameterName args
      let argPattern = intercalate " " argNames
      return $ "(\\" ++ argPattern ++ " -> " ++ bodyStr ++ ")"
  where
    extractParameterName (Param (Ident name _) _ _ _) = return name
    extractParameterName _ = return "_"

-- | Convert dot access to record accessor functions
convertDotAccess :: String -> String -> String
convertDotAccess exprStr attrName = 
  case attrName of
    -- Revision attributes - handle both expressions and variables
    "node" -> 
      if exprStr `elem` ["rev", "rev0", "rev1", "revclose"]
        then "revNode " ++ exprStr
        else "revNode (" ++ exprStr ++ ")"
    "rev" -> 
      if exprStr `elem` ["rev", "rev0", "rev1", "revclose"]
        then "revRev " ++ exprStr
        else "revRev (" ++ exprStr ++ ")"
    "desc" -> 
      if exprStr `elem` ["rev", "rev0", "rev1", "revclose"]
        then "revDesc " ++ exprStr
        else "revDesc (" ++ exprStr ++ ")"
    "author" -> 
      if exprStr `elem` ["rev", "rev0", "rev1", "revclose"]
        then "revAuthor " ++ exprStr
        else "revAuthor (" ++ exprStr ++ ")"
    "branch" -> 
      if exprStr `elem` ["rev", "rev0", "rev1", "revclose"]
        then "revBranch " ++ exprStr
        else "revBranch (" ++ exprStr ++ ")"
    "tags" -> 
      if exprStr `elem` ["rev", "rev0", "rev1", "revclose"]
        then "revTags " ++ exprStr
        else "revTags (" ++ exprStr ++ ")"
    "date" -> 
      if exprStr `elem` ["rev", "rev0", "rev1", "revclose"]
        then "revDate " ++ exprStr
        else "revDate (" ++ exprStr ++ ")"
    "bookmarks" -> "revBookmarks (" ++ exprStr ++ ")"
    "parents" -> "revParents (" ++ exprStr ++ ")"
    "children" -> "revChildren (" ++ exprStr ++ ")"
    -- Branch attributes
    "name" -> "branchName (" ++ exprStr ++ ")"
    -- Common attribute access patterns
    "status" -> "statusCode (" ++ exprStr ++ ")"
    "path" -> "filePath (" ++ exprStr ++ ")"
    -- Method calls that look like attributes
    "encode" -> exprStr ++ " -- TODO: encode method"
    -- Python string methods
    "replace" -> "-- TODO: replace method on " ++ exprStr
    "isoformat" -> "-- TODO: isoformat method on " ++ exprStr
    _ -> "-- TODO: " ++ exprStr ++ "." ++ attrName

-- | Convert setUp method body to setup code
convertSetUp :: SuiteSpan -> Converter [String]
convertSetUp body = do
  -- Convert setUp method body to setup code
  -- This is a simple conversion - may need more sophisticated handling for complex setUp methods
  setupLines <- convertSuite body
  -- Filter out self.client assignment as it's handled by withTestRepo
  let filteredLines = filter (not . isClientAssignment) setupLines
  if null filteredLines
    then return []
    else return $ ["-- Setup:"] ++ filteredLines
  where
    isClientAssignment line = "self.client" `isInfixOf` line

-- | Convert if statements to when or if-then-else
convertIfStatement :: [(ExprSpan, SuiteSpan)] -> SuiteSpan -> Converter [String]
convertIfStatement clauses elseSuite = do
  case clauses of
    [(condExpr, thenSuite)] -> do
      -- Single if clause - convert to when
      condStr <- convertExpr condExpr
      thenLines <- convertSuite thenSuite
      elseLines <- convertSuite elseSuite
      
      if null elseLines
        then do
          -- Simple when statement
          let whenLine = "when (" ++ condStr ++ ") $ do"
          return $ [whenLine] ++ map ("  " ++) thenLines
        else do
          -- if-then-else
          let ifLine = "if " ++ condStr
          let thenLine = "  then do"
          let elseLine = "  else do"
          return $ [ifLine, thenLine] ++ map ("    " ++) thenLines ++ [elseLine] ++ map ("    " ++) elseLines
    
    _ -> do
      -- Multiple clauses (elif chains) - convert to if-then-else chain
      result <- convertIfChain clauses elseSuite
      return result
  where
    convertIfChain :: [(ExprSpan, SuiteSpan)] -> SuiteSpan -> Converter [String]
    convertIfChain [] elseSuite = do
      -- Final else clause
      elseLines <- convertSuite elseSuite
      if null elseLines
        then return []
        else return $ ["else do"] ++ map ("  " ++) elseLines
    
    convertIfChain [(condExpr, thenSuite)] elseSuite = do
      -- Last condition
      condStr <- convertExpr condExpr
      thenLines <- convertSuite thenSuite
      elseLines <- convertSuite elseSuite
      
      let ifLine = "if " ++ condStr
      let thenLine = "  then do"
      let result = [ifLine, thenLine] ++ map ("    " ++) thenLines
      
      if null elseLines
        then return result
        else return $ result ++ ["  else do"] ++ map ("    " ++) elseLines
    
    convertIfChain ((condExpr, thenSuite):rest) elseSuite = do
      -- Multiple conditions - elif chain
      condStr <- convertExpr condExpr
      thenLines <- convertSuite thenSuite
      restLines <- convertIfChain rest elseSuite
      
      let ifLine = "if " ++ condStr
      let thenLine = "  then do"
      let elseIfLine = "  else"
      
      return $ [ifLine, thenLine] ++ map ("    " ++) thenLines ++ [elseIfLine] ++ map ("  " ++) restLines

convertWithStatement :: [(ExprSpan, Maybe ExprSpan)] -> SuiteSpan -> Converter [String]
convertWithStatement contexts body = do
  case contexts of
    [(Call (Var (Ident "open" _) _) args _, Just (Var (Ident varName _) _))] -> do
      -- Handle: with open(file, mode) as f:
      case args of
        [ArgExpr pathExpr _, ArgExpr modeExpr _] -> do
          pathStr <- convertExpr pathExpr
          modeStr <- convertExpr modeExpr
          let haskellMode = pythonModeToHaskell modeStr
          bodyLines <- convertSuite body
          
          -- Add System.IO import for file operations
          addImport "System.IO"
          
          return $ ["withFile " ++ pathStr ++ " " ++ haskellMode ++ " $ \\h -> do"] 
                   ++ map ("  " ++) bodyLines
        _ -> do
          addTodo "Complex open() call in with statement"
          return ["-- TODO: complex with open() call"]
    _ -> do
      addTodo "Non-file with statement"
      return ["-- TODO: non-file with statement"]
  where
    pythonModeToHaskell mode = case mode of
      "\"r\"" -> "ReadMode"
      "\"w\"" -> "WriteMode"
      "\"a\"" -> "AppendMode"
      _ -> "-- TODO: unknown mode " ++ mode

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
  let nonEmptyLines = filter (not . null . strip) lines
  -- If we have multiple statements (declarations, executables, comments), we need do
  return $ length nonEmptyLines > 1
  where
    strip = dropWhile (== ' ')

-- Helper function to detect if config file operations require client reopening
needsClientReopenForConfig :: [String] -> Converter Bool
needsClientReopenForConfig bodyLines = do
  let hasConfigFile = any (isInfixOf ".hg/hgrc") bodyLines
      hasConfigCall = any (isInfixOf "C.config") bodyLines
  return (hasConfigFile && hasConfigCall)

-- Helper function to add required imports based on file operations
addFileOperationImports :: [String] -> Converter ()
addFileOperationImports bodyLines = do
  when (any (isInfixOf "withFile") bodyLines) $ do
    addImport "System.IO"
  when (any (isInfixOf "closeClient") bodyLines) $ do
    addImport "HgLib.Protocol"

-- | Generate the final Haskell module
generateHaskellModule :: String -> [String] -> Converter String
generateHaskellModule moduleName testMethods = do
  requiredImports <- getRequiredImports
  warnings <- getWarnings
  todos <- getTodos
  errors <- getErrors
  
  -- Filter out malformed imports and use Set for deduplication
  let filteredImports = filter isValidImport requiredImports
      -- Remove duplicates using Set
      allImports = Set.toList $ Set.fromList filteredImports
      standardImports = allImports
  
  let moduleHeader = unlines $
        [ "{-# LANGUAGE OverloadedStrings #-}"
        , ""
        , "module Test.HgLib." ++ moduleName ++ "Spec (spec) where"
        , ""
        ] ++ map ("import " ++) standardImports ++
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
  where
    isValidImport imp = 
      not ("Ident {" `isPrefixOf` imp) && 
      not ("SpanCoLinear" `isInfixOf` imp) &&
      not (null imp)

-- | Convert class name to module name
moduleNameFromClassName :: String -> String
moduleNameFromClassName name =
  case dropWhile (/= '_') name of
    '_':rest -> capitalize rest
    _ -> capitalize name
  where
    capitalize [] = []
    capitalize (x:xs) = toUpper x : xs
