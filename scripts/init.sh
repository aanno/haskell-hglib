#!/bin/bash

mkdir tmp || true

pushd tmp

hg clone https://repo.mercurial-scm.org/python-hglib/ || true

#wget --recursive -k http://somedomain/images/page1.html
# https://dev.to/tallesl/crawling-a-website-with-wget-6pd

mkdir hgcommands || true

pushd hgcommands

wget -k --recursive --level=3 -nH -A html \
  --wait=1 --random-wait --adjust-extension --no-clobber --continue \
  --domains=mercurial-scm.org \
  --page-requisites --convert-links \
  --reject "*.exe*" --reject "*.msi" --reject "*/news/*" \
  https://www.mercurial-scm.org/help/commands || true

popd

mkdir language-python || true

pushd language-python

wget -k -nH https://raw.githubusercontent.com/bjpop/language-python/refs/heads/master/language-python/src/Language/Python/Common/AST.hs

popd

popd
