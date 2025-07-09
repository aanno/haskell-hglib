{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | State monad for the Python to Haskell converter
module State.ConverterState where

import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | Converter state that accumulates context during conversion
data ConverterState = ConverterState
  { -- Symbol tables
    csVariables :: Map String String           -- Python var -> Haskell var
  , csImports :: Set String                    -- Imported modules
  , csClassMethods :: Map String [String]      -- Class -> [method names]
  , csTestMethods :: Map String String         -- test_name -> "description"
  
  -- Context tracking
  , csCurrentClass :: Maybe String             -- Current class being processed
  , csCurrentMethod :: Maybe String            -- Current method being processed
  , csIndentLevel :: Int                      -- Current indentation level
  , csInMonadicContext :: Bool                -- Whether we're in a do block
  
  -- Conversion mappings
  , csClientMethods :: Map String String       -- Python method -> Haskell function
  , csAssertionMethods :: Map String String    -- Python assert -> Haskell assertion
  , csBuiltinFunctions :: Map String String    -- Python builtin -> Haskell equivalent
  
  -- Error accumulation
  , csWarnings :: [String]                    -- Accumulated warnings
  , csTodos :: [String]                       -- TODOs for manual review
  , csErrors :: [String]                      -- Conversion errors
  
  -- Module context
  , csModuleName :: String                    -- Target Haskell module name
  , csRequiredImports :: Set String           -- Required Haskell imports
  
  -- Setup code from setUp method
  , csSetupCode :: [String]                   -- Setup code to inject into tests
  
  } deriving (Show, Eq)

-- | Initial converter state
initialConverterState :: String -> ConverterState
initialConverterState moduleName = ConverterState
  { csVariables = Map.empty
  , csImports = Set.empty
  , csClassMethods = Map.empty
  , csTestMethods = defaultTestMethods
  , csCurrentClass = Nothing
  , csCurrentMethod = Nothing
  , csIndentLevel = 0
  , csInMonadicContext = False
  , csClientMethods = defaultClientMethods
  , csAssertionMethods = defaultAssertionMethods
  , csBuiltinFunctions = defaultBuiltinFunctions
  , csWarnings = []
  , csTodos = []
  , csErrors = []
  , csModuleName = moduleName
  , csRequiredImports = Set.fromList defaultRequiredImports
  , csSetupCode = []
  }

-- | Default test method name mappings
defaultTestMethods :: Map String String
defaultTestMethods = Map.fromList
  [ ("test_empty", "should handle empty repository")
  , ("test_basic", "should handle basic repository with one commit")
  , ("test_commit_dirty", "should detect dirty working directory")
  , ("test_secret_commit_clean", "should handle secret commit clean")
  , ("test_update", "should handle update")
  , ("test_remote", "should handle remote")
  , ("test_two_parents", "should handle two parents")
  , ("test_user", "should handle commit with custom user")
  , ("test_no_user", "should fail with empty user")
  , ("test_close_branch", "should close branch")
  , ("test_message_logfile", "should handle message and logfile conflicts")
  , ("test_date", "should handle custom date")
  , ("test_amend", "should amend previous commit")
  , ("test_nul_injection", "should prevent null injection")
  ]

-- | Default client method mappings
defaultClientMethods :: Map String String
defaultClientMethods = Map.fromList
  [ ("commit", "C.commit")
  , ("log", "C.log_")
  , ("status", "C.status")
  , ("config", "C.config")
  , ("update", "C.update")
  , ("branches", "C.branches")
  , ("branch", "C.branch")
  , ("summary", "C.summary")
  , ("parents", "C.parents")
  , ("tip", "C.tip")
  ]

-- | Default assertion method mappings
defaultAssertionMethods :: Map String String
defaultAssertionMethods = Map.fromList
  [ ("assertEqual", "shouldBe")
  , ("assertNotEqual", "shouldNotBe")
  , ("assertTrue", "shouldBe True")
  , ("assertFalse", "shouldBe False")
  , ("assertRaises", "shouldThrow")
  , ("assertIn", "shouldContain")
  , ("assertNotIn", "shouldNotContain")
  ]

-- | Default builtin function mappings
defaultBuiltinFunctions :: Map String String
defaultBuiltinFunctions = Map.fromList
  [ ("len", "length")
  , ("str", "show")
  , ("int", "read")
  , ("bool", "id")
  , ("open", "-- TODO: file operations")
  ]

-- | Default required imports for generated Haskell modules
defaultRequiredImports :: [String]
defaultRequiredImports =
  [ "Test.Hspec"
  , "Test.HgLib.Common"
  , "qualified HgLib.Commands as C"
  , "HgLib.Types"
  , "Control.Exception (try, SomeException)"
  , "Data.Text (Text)"
  , "qualified Data.Text as T"
  ]

-- | Type alias for the converter monad
type Converter = State ConverterState

-- | State manipulation functions
addVariable :: String -> String -> Converter ()
addVariable pyVar hsVar = modify $ \s -> s { csVariables = Map.insert pyVar hsVar (csVariables s) }

lookupVariable :: String -> Converter (Maybe String)
lookupVariable pyVar = gets (Map.lookup pyVar . csVariables)

addImport :: String -> Converter ()
addImport imp = modify $ \s -> s { csRequiredImports = Set.insert imp (csRequiredImports s) }

enterClass :: String -> Converter ()
enterClass className = modify $ \s -> s { csCurrentClass = Just className }

exitClass :: Converter ()
exitClass = modify $ \s -> s { csCurrentClass = Nothing }

enterMethod :: String -> Converter ()
enterMethod methodName = modify $ \s -> s { csCurrentMethod = Just methodName }

exitMethod :: Converter ()
exitMethod = modify $ \s -> s { csCurrentMethod = Nothing }

setMonadicContext :: Bool -> Converter ()
setMonadicContext inMonadic = modify $ \s -> s { csInMonadicContext = inMonadic }

addWarning :: String -> Converter ()
addWarning warning = modify $ \s -> s { csWarnings = warning : csWarnings s }

addTodo :: String -> Converter ()
addTodo todo = modify $ \s -> s { csTodos = todo : csTodos s }

addError :: String -> Converter ()
addError error = modify $ \s -> s { csErrors = error : csErrors s }

addSetupCode :: [String] -> Converter ()
addSetupCode setupLines = modify $ \s -> s { csSetupCode = csSetupCode s ++ setupLines }

getSetupCode :: Converter [String]
getSetupCode = gets csSetupCode

-- | Lookup functions with state
lookupClientMethod :: String -> Converter (Maybe String)
lookupClientMethod method = gets (Map.lookup method . csClientMethods)

lookupAssertionMethod :: String -> Converter (Maybe String)
lookupAssertionMethod method = gets (Map.lookup method . csAssertionMethods)

lookupTestDescription :: String -> Converter String
lookupTestDescription testName = do
  testMethods <- gets csTestMethods
  return $ Map.findWithDefault testName testName testMethods

-- | Context queries
getCurrentClass :: Converter (Maybe String)
getCurrentClass = gets csCurrentClass

getCurrentMethod :: Converter (Maybe String)
getCurrentMethod = gets csCurrentMethod

isInMonadicContext :: Converter Bool
isInMonadicContext = gets csInMonadicContext

-- | Helper to register a test method with custom description
registerTestMethod :: String -> String -> Converter ()
registerTestMethod testName description = 
  modify $ \s -> s { csTestMethods = Map.insert testName description (csTestMethods s) }

-- | Get accumulated results
getWarnings :: Converter [String]
getWarnings = gets (reverse . csWarnings)

getTodos :: Converter [String]
getTodos = gets (reverse . csTodos)

getErrors :: Converter [String]
getErrors = gets (reverse . csErrors)

getRequiredImports :: Converter [String]
getRequiredImports = gets (Set.toList . csRequiredImports)
