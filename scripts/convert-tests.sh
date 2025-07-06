#!/bin/bash

stack run python-testcase-converter-exe /home/tpasch/tmp/python-hglib/tests/test_config.py  >test/Test/HgLib/ConfigSpec.hs
stack run python-testcase-converter-exe /home/tpasch/tmp/python-hglib/tests/test_commit.py  >test/Test/HgLib/CommitSpec.hs
stack run python-testcase-converter-exe /home/tpasch/tmp/python-hglib/tests/test_log.py  >test/Test/HgLib/LogSpec.hs
stack run python-testcase-converter-exe /home/tpasch/tmp/python-hglib/tests/test_status.py  >test/Test/HgLib/StatusSpec.hs
stack run python-testcase-converter-exe /home/tpasch/tmp/python-hglib/tests/test_summary.py  >test/Test/HgLib/SummarySpec.hs
stack run python-testcase-converter-exe /home/tpasch/tmp/python-hglib/tests/test_update.py  >test/Test/HgLib/UpdateSpec.hs
