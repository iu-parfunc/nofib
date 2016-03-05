#!/bin/bash

set -xe

################
## Look here! ##
################
#
# Before running, please update this to correspond to the last
# benchmark_nofib_StrictData ${BUILD_NUM} that was run.
# 
CONFIGURABLE_BUILD_NUM=45


CURRENT_DIR=`pwd`
LOG_DIR=${HOME}/results_backup/nofib-jenkinsbuild-${CONFIGURABLE_BUILD_NUM}
GHC_TOP_DIR="${HOME}"/opt/ghc-8.1
GHC_BIN_DIR="${GHC_TOP_DIR}"/bin
export PATH="${GHC_BIN_DIR}":$PATH
( cd nofib-analyse     ; "${GHC_BIN_DIR}"/ghc Main.hs -O2 -fforce-recomp -o nofib-analyse )
( cd piecewise-analyse ; "${GHC_BIN_DIR}"/ghc Main.hs -O2 -fforce-recomp -o piecewise-analyse )
piecewise-analyse/piecewise-analyse "${LOG_DIR}"
