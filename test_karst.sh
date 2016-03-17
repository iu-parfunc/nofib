#!/bin/bash

set -xe

CURRENT_DIR=`pwd`
echo "We're in ${CURRENT_DIR}"
PATH=$HOME/opt_local/ghc-7.10.3/bin:$PATH
ghc --version
