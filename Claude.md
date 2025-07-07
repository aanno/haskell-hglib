# haskell-hglib hints for Claude.ai


* Try paths `/stratis/home/tpasch/dev/scm/aanno/github/haskell-hglib` and `/workspaces/ghc`
* Don't use filesystem MCP `write_file`
* Instead use vs code MCP `text_editor`
* Don't use `copilot_*'. There plain commands as well.

## General

* This is a port of python-hglib from https://repo.mercurial-scm.org/python-hglib to Haskell.
* You could find a local copy of python-hglib at `tmp/python-hglib` .
* The goal is a full blown port, with all tests running successful.
* The code is public on github at `aanno/haskell-hglib` .

## Project setup

Project is at `/stratis/home/tpasch/dev/scm/aanno/github/haskell-hglib` on host
and opened with vs code and a dev container. If possible, you should _primary_ use vs code MCP.

In vs code you will find the project at `/workspaces/ghc` because of dev container usage.

You should always read Claude.md in project first.

Currently, we fix the converter a bit.

Full test cycle is (in a vs code terminal):

```bash
stack build && ./scripts/convert-tests.sh && stack test
```

But you could also execute this 3 command sequentially.

### State

* The converter should build and run without problems.
* The converted test cases has 2 types of problems:
  + Problem that originates from missing features in the converter.
  + Problem arising from things so far not implemented in haskell-hglib itself.

When we work on the converter, we will ignore problems with the implementation.
This means we will _not_ fix implementation problems in the converter.

When we work on the implementation, we want to fixed problems that has been
revealed by the generated tests.

#### Examples

This is an error because test convertion is incomplete.
We ignore this kind of error (as we rather try to translate more of the AST structure).

```
/workspaces/ghc/test/Test/HgLib/UpdateSpec.hs:67:7: error: [GHC-88464]
    Variable not in scope: u
   |        
67 |       u `shouldBe` 1
   |       ^
```

This is an error within the converter that should be fixed:

```
/workspaces/ghc/test/Test/HgLib/UpdateSpec.hs:79:47: error: [GHC-83865]
    • Expected kind ‘* -> *’,
        but ‘Either SomeException IO’ has kind ‘*’
    • In the first argument of ‘IO’, namely
        ‘(Either SomeException IO (Int, Text))’
      In an expression type signature:
        IO (Int, Text) -> IO (Either SomeException IO (Int, Text))
      In the first argument of ‘($)’, namely
        ‘(try ::
            IO (Int, Text) -> IO (Either SomeException IO (Int, Text)))’
   |        
79 |       result <- (try :: IO (Int, Text) -> IO (Either SomeException IO (Int, Text))) $ C.update client C.defaultLogOptions -- TODO: UpdateOptions not implemented, got ["clean","check"]
   |                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

```

At the end of the line, there is a TODO from a missing feature in the converter. 
We address this as well, but normally after fixing errors.

## Test converter

* There is a Haskell tool `converter/PythonTestcaseConverter.hs` that parses 
  the python tests to AST and then converts this to Haskell tests.
* There are python tests at `tmp/python-hglib/tests/test*.py`
* The converter should make haskell test from these. If possible, if we get to 90% or 95%,
  and than have to make some manual adaptions, it ok as well.
* There are some manual converted tests at `test/Test/HgLib/My*.hs` as reference
  for implementing the converter.
* With the test, we will see better what is still missing in the actual implementation
  and we could fill the gaps.

## Implementation patterns

* mercurial commands are documented at https://www.mercurial-scm.org/help/commands
* There is a local copy of the html pages at `tmp/hgcommands/help/commands/`
* In your implementation each command gets its own options.
* The default options for the command should default to the values documented.
* There are also simplified form of each command that needs no options.

### Enforce mandartory arguments

* Some arguments to commands are mandatory, like the commit message on the `hg commit` command.
* In this case we do _not_ export the option constructor.

On the commit example, that means:

And this is because defaultCommitOptions are not exported! And I did this on purpose. 
The problem is that `hg commit` always needs an commit message (it's mandatory!). 

Because of this I decided that, instead of exporting defaultCommitOptions there is an exported 
function `mkDefaultCommitOptions :: String -> CommitOptions` that takes the commit message!

This should be an general pattern for the library: If a hg command requires mandantory options, 
we don't export a 'default', but a constructor that takes these mandatory arguments.

