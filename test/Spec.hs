{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Main test runner
-- Spec.hs
module Main (main) where

import Test.Hspec
import System.IO (stdout, stderr)
import System.OsPath (OsPath, encodeFS)
import System.IO.Unsafe (unsafePerformIO)
import Logging

import qualified Test.HgLib.SummarySpec as Summary
import qualified Test.HgLib.StatusSpec as Status
import qualified Test.HgLib.LogSpec as Log
import qualified Test.HgLib.UpdateSpec as Update
-- import qualified Test.HgLib.TagsSpec as Tags
-- import qualified Test.HgLib.ResolveSpec as Resolve
-- import qualified Test.HgLib.RemoveSpec as Remove
-- import qualified Test.HgLib.PushPullSpec as PushPull
-- import qualified Test.HgLib.PhaseSpec as Phase
-- import qualified Test.HgLib.PathsSpec as Paths
-- import qualified Test.HgLib.ParentsSpec as Parents
-- import qualified Test.HgLib.OutgoingIncomingSpec as OutgoingIncoming
-- import qualified Test.HgLib.MoveSpec as Move
-- import qualified Test.HgLib.MergeSpec as Merge
-- import qualified Test.HgLib.ManifestSpec as Manifest
-- import qualified Test.HgLib.InitSpec as Init
-- import qualified Test.HgLib.ImportSpec as Import
-- import qualified Test.HgLib.HeadsSpec as Heads
-- import qualified Test.HgLib.GrepSpec as Grep
-- import qualified Test.HgLib.ForgetSpec as Forget
-- import qualified Test.HgLib.EncodingSpec as Encoding
-- import qualified Test.HgLib.DiffSpec as Diff
-- import qualified Test.HgLib.CopySpec as Copy
-- import qualified Test.HgLib.ContextSpec as Context
-- import qualified Test.HgLib.ConfigSpec as Config
-- import qualified Test.HgLib.CommitSpec as Commit
-- import qualified Test.HgLib.CloneSpec as Clone
-- import qualified Test.HgLib.BundleSpec as Bundle
-- import qualified Test.HgLib.BranchesSpec as Branches
-- import qualified Test.HgLib.BranchSpec as Branch
-- import qualified Test.HgLib.BookmarksSpec as Bookmarks
-- import qualified Test.HgLib.AnnotateSpec as Annotate
-- import qualified Test.HgLib.ClientSpec as Client

logging :: IO () -> IO ()
logging action = do
  osPath <- encodeFS "spec.log"
  let config = defaultLogConfig { 
    minLogLevel = DEBUG
    , logFile = Just $ unsafePerformIO $ encodeFS "spec.log"
    , console = Nothing
    }
  withLogging config action

main :: IO ()
main = hspec $ around_ logging $ do
  describe "HgLib" $ do
    Summary.spec
    Status.spec
    Log.spec
    Update.spec
    -- TODO: implement
    -- Tags.spec
    -- Resolve.spec
    -- Remove.spec
    -- PushPull.spec
    -- Phase.spec
    -- Paths.spec
    -- Parents.spec
    -- OutgoingIncoming.spec
    -- Move.spec
    -- Merge.spec
    -- Manifest.spec
    -- Init.spec
    -- Import.spec
    -- Heads.spec
    -- Grep.spec
    -- Forget.spec
    -- Encoding.spec
    -- Diff.spec
    -- Copy.spec
    -- Context.spec
    -- Config.spec
    -- Commit.spec
    -- Clone.spec
    -- Bundle.spec
    -- Branches.spec
    -- Branch.spec
    -- Bookmarks.spec
    -- Annotate.spec
    -- Client.spec





