#!/bin/bash

export PY_HGLIB=/workspaces/ghc/tmp/python-hglib

if [ ! -d "$PY_HGLIB" ]; then
  echo "no $PY_HGLIB, exiting..."
  exit -1
fi

stack run python-testcase-converter-exe $PY_HGLIB/tests/test_config.py  >test/Test/HgLib/ConfigSpec.hs
stack run python-testcase-converter-exe $PY_HGLIB/tests/test_commit.py  >test/Test/HgLib/CommitSpec.hs
stack run python-testcase-converter-exe $PY_HGLIB/tests/test_log.py  >test/Test/HgLib/LogSpec.hs
stack run python-testcase-converter-exe $PY_HGLIB/tests/test_status.py  >test/Test/HgLib/StatusSpec.hs
stack run python-testcase-converter-exe $PY_HGLIB/tests/test_summary.py  >test/Test/HgLib/SummarySpec.hs
stack run python-testcase-converter-exe $PY_HGLIB/tests/test_update.py  >test/Test/HgLib/UpdateSpec.hs
