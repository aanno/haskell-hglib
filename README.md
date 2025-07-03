**Work in progress!** - Not tested. Not functional.

A port of `python-hglib` mostly done by [claude.ai](https://claude.ai).
Even this README is from the AI LLM!

# HgLib - Haskell Mercurial Client Library

A complete Haskell port of [python-hglib](https://www.mercurial-scm.org/wiki/PythonHglib) that provides a fast, convenient interface to Mercurial. It uses Mercurial's command server for communication with hg, offering better performance than spawning individual hg processes.

## Features

- **Fast Communication**: Uses Mercurial's binary command server protocol
- **Comprehensive API**: Covers all major Mercurial operations
- **Type Safety**: Leverages Haskell's type system for reliability
- **Resource Management**: Automatic cleanup of server connections
- **Error Handling**: Structured error types with detailed information
- ~**Well Tested**: Extensive test suite covering all functionality~

## Installation

**Not published!** Hence the following will _not_ work!

Add to your `cabal.project` or install directly:

```bash
cabal install hglib
```

Or add to your `package.yaml` or `.cabal` file:

```yaml
dependencies:
  - hglib >= 1.0
```

## Quick Start

```haskell
{-# LANGUAGE OverloadedStrings #-}
import HgLib

main :: IO ()
main = do
  -- Use default config for current directory
  withClient defaultConfig $ \client -> do
    -- Check repository status
    statuses <- simpleStatus client
    print statuses
    
    -- Get recent history
    recent <- simpleLog client
    mapM_ (putStrLn . formatRevision) recent
    
    -- Add and commit files
    success <- add client ["newfile.txt"] defaultAddOptions
    when success $ do
      (rev, node) <- simpleCommit client "Add new file"
      putStrLn $ "Committed as " ++ show rev
```

## Core Concepts

### Client Management

The library manages connections to Mercurial's command server:

```haskell
-- Manual management
client <- openClient defaultConfig
result <- status client defaultStatusOptions
closeClient client

-- Automatic management (recommended)
withClient defaultConfig $ \client -> do
  result <- status client defaultStatusOptions
  -- client is automatically closed
```

### Configuration

Configure the client for different repositories and options:

```haskell
-- Default config (current directory)
let config = defaultConfig

-- Specific repository
let config = defaultConfigWithPath "/path/to/repo"

-- Custom configuration
let config = defaultConfig
  { hgPath = Just "/path/to/repo"
  , hgEncoding = Just "utf-8"
  , hgConfigs = ["ui.username=John Doe <john@example.com>"]
  , hgHidden = True  -- Include hidden changesets
  }
```

### Error Handling

The library provides structured error handling:

```haskell
import Control.Exception (try)

result <- try $ withClient config $ \client -> do
  status client defaultStatusOptions

case result of
  Left (HgCommandError cmd exitCode stdout stderr) -> 
    putStrLn $ "Command failed: " ++ unwords cmd
  Left (HgRepositoryError msg path) -> 
    putStrLn $ "Repository error: " ++ msg
  Right statuses -> 
    print statuses
```

## API Reference

### Repository Operations

#### Status and Information
```haskell
-- Get file status
statuses <- status client defaultStatusOptions
isClean <- isCleanWorkingDirectory client
info <- getRepositoryInfo client
rootPath <- root client
```

#### History and Revisions
```haskell
-- Get revision history
revisions <- log_ client [] defaultLogOptions { logLimit = Just 10 }
tipRevision <- tip client
parents <- parents client []
heads <- heads client [] []
```

#### Working Directory Operations
```haskell
-- Add files
success <- add client ["file1.txt", "file2.txt"] defaultAddOptions

-- Commit changes
(rev, node) <- commit client defaultCommitOptions 
  { commitMessage = Just "My commit message" }

-- Update working directory
(updated, merged, removed, unresolved) <- update client (Just "tip") []
```

#### Repository Synchronization
```haskell
-- Pull changes
success <- pull client Nothing []

-- Push changes  
success <- push client Nothing []

-- Clone repository
clone client "https://hg.example.com/repo" (Just "/local/path") []
```

#### File Operations
```haskell
-- Show file contents
contents <- cat client ["README.md"] (Just "tip")

-- Copy files
success <- copy client ["old.txt"] "new.txt" []

-- Move files
success <- move client ["old.txt"] "new.txt" []

-- Remove files
success <- remove client ["unwanted.txt"] []
```

#### Branching and Tagging
```haskell
-- Branch operations
currentBranch <- branch client Nothing []
allBranches <- branches client []

-- Bookmark operations
bookmarks <- bookmarks client
bookmark client (Just "my-bookmark") []

-- Tag operations
tags <- tags client
tag client ["v1.0"] []
```

### Advanced Operations

#### Merging and Conflict Resolution
```haskell
-- Merge revisions
merge client (Just "other-branch") []

-- Resolve conflicts
conflicts <- resolve client [] ["--list"]
resolve client ["conflicted.txt"] ["--mark"]
```

#### Patches and Diffs
```haskell
-- Generate diff
diff <- HgLib.Commands.diff client [] defaultDiffOptions
  { diffGit = True
  , diffShowFunction = True
  }

-- Export patches
patches <- export client ["tip"] []

-- Import patches
import_ client ["patch.diff"] []
```

#### Search and Annotation
```haskell
-- Search in files
results <- grep client "TODO" ["*.hs"] []

-- Annotate files
annotations <- annotate client ["Main.hs"] Nothing []
```

## Command Options

The library provides option types for complex commands:

```haskell
-- Status options
let statusOpts = defaultStatusOptions
  { statusModified = True
  , statusAdded = True
  , statusInclude = Just "*.hs"
  }
statuses <- status client statusOpts

-- Log options  
let logOpts = defaultLogOptions
  { logLimit = Just 20
  , logBranch = Just "develop"
  , logKeyword = Just "bugfix"
  }
revisions <- log_ client [] logOpts

-- Diff options
let diffOpts = defaultDiffOptions
  { diffGit = True
  , diffUnified = Just 5
  , diffIgnoreAllSpace = True
  }
diff <- HgLib.Commands.diff client [] diffOpts
```

## Error Types

```haskell
data HgError 
  = HgCommandError [String] Int ByteString ByteString  -- Command failed
  | HgResponseError String                             -- Protocol error
  | HgServerError String                              -- Server error  
  | HgCapabilityError String                          -- Missing capability
  | HgProtocolError String (Maybe ByteString)         -- Protocol issue
  | HgConfigError String (Maybe String)               -- Configuration error
  | HgRepositoryError String (Maybe FilePath)         -- Repository error
```

## Performance Notes

- The command server provides significant performance improvements over subprocess calls
- Connections are reusable - keep clients open for multiple operations
- The server supports concurrent operations (within limits)
- Large repositories benefit most from the persistent connection

## Compatibility

- **Mercurial**: Requires Mercurial 1.9+ (command server support)
- **GHC**: Tested with GHC 9.8.4 ~8.10.7, 9.0.2, 9.2.8, 9.4.8, 9.6.3~
- **Stack snapshot**: lts-23.14
- **Platforms**: Linux ~, macOS, Windows (anywhere Mercurial runs)~

## Examples

See the [demo application](app/Main.hs) for comprehensive usage examples, including:

- Repository status checking
- Commit workflow
- Branch and tag management
- History browsing
- File operations

## Contributing

~Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.~

## License

This library is available under the BSD-3-Clause license, while python-hglib is MIT.

## Comparison with python-hglib

**Not tested!** Hence the following is only opinion of claude.ai!

This Haskell port aims for API compatibility while leveraging Haskell's strengths:

| Feature | python-hglib | hglib-haskell |
|---------|--------------|---------------|
| Type Safety | Dynamic | Static typing |
| Error Handling | Exceptions | Structured types |
| Memory Safety | GC | GC + no null pointers |  
| Performance | Good | Good |
| API Coverage | Complete | Complete |
| Concurrency | Threading | STM/Async |

## Related Projects

- [python-hglib](https://www.mercurial-scm.org/wiki/PythonHglib) - Original Python implementation
- [Mercurial](https://www.mercurial-scm.org/) - The Mercurial DVCS
- [CommandServer](https://www.mercurial-scm.org/wiki/CommandServer) - Protocol documentation
