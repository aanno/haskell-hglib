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

-- | Convert DottedName to string
dottedNameToString :: DottedNameSpan -> String
dottedNameToString name = 
  -- For now, use show and clean it up
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
        _ -> takeWhile (/= ' ') $ drop 1 $ dropWhile (/= '"') s

-- | Process import items
processImportItem :: ImportItemSpan -> Converter ()
processImportItem (ImportItem name _ _) = do
  let moduleName = dottedNameToString name
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
    [Dot (Var (Ident objName _) _) (Ident attrName _) _] -> do
      -- Attribute assignment (e.g., self.client = ...)
      if objName == "self"
        then do
          -- For self.client assignments, we typically don't need them in Haskell tests
          return ["-- TODO: " ++ objName ++ "." ++ attrName ++ " = " ++ exprStr ++ " (handled by withTestRepo)"]
        else do
          addTodo $ "Attribute assignment: " ++ objName ++ "." ++ attrName
          return ["-- TODO: " ++ objName ++ "." ++ attrName ++ " <- " ++ exprStr]
    _ -> do
      addTodo $ "Complex assignment pattern: " ++ show targets
      return ["-- TODO: complex assignment: " ++ show targets]

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
      case (assertMethod, args) of
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
    maybeHaskellMethod <- lookupClientMethod method
    case maybeHaskellMethod of
      Nothing -> do
        addWarning $ "Unknown client method: " ++ method
        return $ "-- TODO: client." ++ method
      Just haskellMethod -> do
        (positionalArgs, keywordArgs) <- separateArgs args
        posArgsStr <- convertArgs positionalArgs
        
        -- Handle keyword arguments as options
        if null keywordArgs
          then return $ haskellMethod ++ " client " ++ posArgsStr
          else do
            -- For now, create a TODO for proper options handling
            keywordStr <- mapM convertKeywordArg keywordArgs
            return $ haskellMethod ++ " client " ++ posArgsStr ++ " -- TODO: options " ++ intercalate " " keywordStr
  
  Call (Var (Ident funcName _) _) args _ -> do
    -- Handle special function calls
    case funcName of
      "open" -> convertOpenCall args
      "b" -> convertBytesCall args  -- Handle b('string') calls
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
  
  -- Handle parenthesized expressions
  Paren innerExpr _ -> do
    convertExpr innerExpr
  
  -- Handle attribute access (e.g., os.path.abspath, hglib.error.CommandError)
  Dot (Dot (Var (Ident modName _) _) (Ident subModName _) _) (Ident attrName _) _ -> do
    convertModuleAttr modName subModName attrName
  
  -- Handle 2-level attribute access (e.g., hglib.open)
  Dot (Var (Ident modName _) _) (Ident attrName _) _ -> do
    convertModuleAttr modName "" attrName
  
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
      return $ "-- TODO: withFile " ++ pathStr ++ " " ++ haskellMode ++ " $ \\h ->"
    _ -> do
      addTodo $ "Complex open() call with " ++ show (length args) ++ " args"
      return "-- TODO: complex open() call"
  where
    pythonModeToHaskell mode = case mode of
      "\"r\"" -> "ReadMode"
      "\"w\"" -> "WriteMode"
      "\"a\"" -> "AppendMode"
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

-- | Convert method calls on variables (e.g., f.write(), f.close())
convertVariableMethodCall :: String -> String -> [ArgumentSpan] -> Converter String
convertVariableMethodCall varName methodName args = do
  case (varName, methodName) of
    ("self", "append") -> do
      -- self.append(file, content) -> file append operation
      case args of
        [ArgExpr fileExpr _, ArgExpr contentExpr _] -> do
          fileStr <- convertExpr fileExpr
          contentStr <- convertExpr contentExpr
          return $ "-- TODO: appendFile " ++ fileStr ++ " " ++ contentStr
        _ -> do
          addTodo $ "Complex self.append() call"
          return "-- TODO: complex self.append() call"
    (_, "write") -> do
      case args of
        [ArgExpr contentExpr _] -> do
          contentStr <- convertExpr contentExpr
          return $ "hPutStrLn " ++ varName ++ " " ++ contentStr
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
          return $ varName ++ " ++ [" ++ itemStr ++ "]"
        _ -> do
          addTodo $ "Complex append() call"
          return "-- TODO: complex append() call"
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

-- | Convert dot access to record accessor functions
convertDotAccess :: String -> String -> String
convertDotAccess exprStr attrName = 
  case attrName of
    "node" -> "revNode (" ++ exprStr ++ ")"
    "rev" -> "revRev (" ++ exprStr ++ ")"
    "desc" -> "revDesc (" ++ exprStr ++ ")"
    "author" -> "revAuthor (" ++ exprStr ++ ")"
    "branch" -> "revBranch (" ++ exprStr ++ ")"
    "tags" -> "revTags (" ++ exprStr ++ ")"
    "date" -> "revDate (" ++ exprStr ++ ")"
    "bookmarks" -> "revBookmarks (" ++ exprStr ++ ")"
    "parents" -> "revParents (" ++ exprStr ++ ")"
    "children" -> "revChildren (" ++ exprStr ++ ")"
    -- Common attribute access patterns
    "status" -> "statusCode (" ++ exprStr ++ ")"
    "path" -> "filePath (" ++ exprStr ++ ")"
    "name" -> "fileName (" ++ exprStr ++ ")"
    _ -> "-- TODO: " ++ exprStr ++ "." ++ attrName

-- | Convert setUp method body to setup code
convertSetUp :: SuiteSpan -> Converter [String]
convertSetUp body = do
  -- Convert setUp method body to setup code
  -- This is a simple conversion - may need more sophisticated handling for complex setUp methods
  setupLines <- convertSuite body
  -- Filter out self.client assignment as it's handled by withTestRepo
  let filteredLines = filter (not . isClientAssignment) setupLines
  return $ map ("-- Setup: " ++) filteredLines
  where
    isClientAssignment line = "self.client" `isInfixOf` line

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
