# haskell-hglib hints for Claude.ai

## General

* This is a port of python-hglib from https://repo.mercurial-scm.org/python-hglib to Haskell.
* You could find a local copy of python-hglib at `tmp/python-hglib` .
* The goal is a full blown port, with all tests running successful.
* The code is public on github at `aanno/haskell-hglib` .

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
* In your implementation each command gets its own options.
* The default options for the command should default to the values documented.
* There are also simplified form of each command that need not options.

### Enforce mandartory arguments

* Some arguments to commands are mandatory, like the commit message on the `hg commit` command.
* In this case we do _not_ export the option constructor.

On the commit example, that means:

And this is because defaultCommitOptions are not exported! And I did this on purpose. The problem is that hg commit always needs an commit message (it's mandatory!). 

Because of this I decided that, instead of exporting defaultCommitOptions there is an exported function `mkDefaultCommitOptions :: String -> CommitOptions` that takes the commit message!

This should be an general pattern for the library: If a hg command requires mandantory options, we don't export a 'default', but a construct that takes that mandatory arguments.
