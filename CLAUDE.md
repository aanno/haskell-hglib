# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a **Haskell port of python-hglib** that provides a complete interface to Mercurial's command server. The library enables fast, efficient communication with Mercurial repositories by maintaining persistent connections rather than spawning individual processes for each operation.

**Current Status**: Work in progress. The project includes:
- A Python-to-Haskell test converter (`converter/PythonTestcaseConverter.hs`)
- Core Haskell library implementation (`src/HgLib/`)
- Test infrastructure with both manual and converted tests
- Reference Python implementation in `tmp/python-hglib/`

## Build System and Commands

**Build System**: Uses Stack with package.yaml configuration
- **Stack snapshot**: LTS 23.26 (GHC 9.8.4)
- **Dependencies**: Key libraries include language-python, hspec, streamly, aeson

### Core Development Workflow

```bash
# Full test cycle
stack build && ./scripts/convert-tests.sh && stack test

# Individual steps
stack build                    # Build library and executables
./scripts/convert-tests.sh     # Convert Python tests to Haskell
stack test                     # Run all tests
```

### Individual Commands

```bash
# Build specific executables
stack build haskell-hglib-exe
stack build python-testcase-converter-exe

# Run converter manually
stack run python-testcase-converter-exe tmp/python-hglib/tests/test_config.py

# Run single test file
stack test --test-arguments="--match \"Config\""

# Run with debug logging
stack test  # Uses spec.log for output
```

## Architecture Overview

### Core Components

1. **Client Management (`src/HgLib.hs`)**
   - Main entry point with resource management via `withClient`
   - Convenience functions: `simpleClone`, `simpleCommit`, `simpleStatus`, `simpleLog`
   - Repository information gathering

2. **Command Layer (`src/HgLib/Commands.hs`)**
   - 49+ Mercurial commands implemented as typed functions
   - Each command has dedicated options type (e.g., `CommitOptions`, `LogOptions`)
   - Consistent API: `command :: HgClient -> [args] -> Options -> IO Result`

3. **Protocol Layer (`src/HgLib/Protocol.hs`)**
   - Low-level command server protocol implementation
   - Binary protocol with 7 channel types (Output, Error, Result, etc.)
   - Connection management with `openClient`/`closeClient`

4. **Type System (`src/HgLib/Types.hs`)**
   - Strongly typed data structures: `Revision`, `HgStatus`, `SummaryInfo`
   - Configuration types: `HgClient`, `HgConfig`
   - JSON serialization support

5. **Error Handling (`src/HgLib/Error.hs`)**
   - Comprehensive error categorization
   - Retry logic for transient failures
   - Context-aware error reporting

### Design Patterns

- **Resource Management**: Bracket pattern with `withClient`
- **Type Safety**: Strong typing prevents runtime errors
- **Command Builder**: Consistent option handling across commands
- **JSON Integration**: Structured output parsing using `--template json`

## Test Infrastructure

### Test Structure

- **Manual Tests**: `test/Test/HgLib/My*.hs` - Hand-written reference implementations
- **Converted Tests**: `test/Test/HgLib/*Spec.hs` - Generated from Python tests
- **Common Utilities**: `test/Test/HgLib/Common.hs` - Shared test infrastructure

### Test Conversion Process

The `converter/PythonTestcaseConverter.hs` tool:
- Parses Python test files using `language-python`
- Converts Python AST to Haskell test syntax
- Handles Python unittest patterns → Haskell Hspec
- Converts `self.assertEqual` → `shouldBe`, `self.assertRaises` → exception handling
- Manages mercurial client method calls: `self.client.commit` → `C.commit client`

### Test Categories

1. **Converter Issues**: Missing AST conversion features (ignore during converter work)
2. **Implementation Issues**: Missing library features (address during implementation work)
3. **Type Errors**: Incorrect type signatures in converter output (fix in converter)

## Implementation Guidelines

### Command Implementation Pattern

For each Mercurial command:
1. Define strongly-typed options record
2. Provide `mkDefault*Options` constructor for mandatory arguments
3. Don't export raw default constructors if command requires mandatory args
4. Use `buildArgs` to construct command arguments
5. Handle JSON output parsing where applicable

### Mandatory Arguments Pattern

Commands requiring mandatory arguments (like commit message) use constructor functions:

```haskell
-- Don't export defaultCommitOptions
mkDefaultCommitOptions :: String -> CommitOptions  -- Exported
```

This ensures compile-time enforcement of required parameters.

### Error Handling

- Use structured error types from `HgLib.Error`
- Categorize errors appropriately
- Implement retry logic for transient failures
- Add context to errors using `withErrorContext`

## Development Focus Areas

### Current Work: Test Converter

When working on the converter, focus on:
- Expanding Python AST → Haskell conversion coverage
- Fixing type signature generation
- Handling complex Python constructs (with statements, lambdas, etc.)
- Generating proper monadic context (do blocks when needed)

**Ignore during converter work**: Missing library implementation features

### Implementation Work

When working on the library itself:
- Fix issues revealed by converted tests
- Implement missing command options
- Add proper type signatures
- Handle edge cases in protocol communication

## Reference Resources

- **Mercurial Commands**: https://www.mercurial-scm.org/help/commands
- **Local Command Docs**: `tmp/hgcommands/help/commands/`
- **Python Reference**: `tmp/python-hglib/` (original implementation)
- **Test Examples**: `test/Test/HgLib/My*.hs` (manual implementations)

## File Locations

- **Main Library**: `src/HgLib/`
- **Test Converter**: `converter/PythonTestcaseConverter.hs`
- **Test Sources**: `tmp/python-hglib/tests/test_*.py`
- **Generated Tests**: `test/Test/HgLib/*Spec.hs`
- **Manual Tests**: `test/Test/HgLib/My*.hs`
- **Conversion Script**: `scripts/convert-tests.sh`

## Key Dependencies

- **language-python**: Python AST parsing for converter
- **hspec**: Test framework for generated tests
- **streamly**: Async I/O and streaming
- **aeson**: JSON parsing for structured command output
- **process**: Mercurial process management
- **stm**: Software transactional memory for concurrency