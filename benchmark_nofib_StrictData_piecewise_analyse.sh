#!/bin/bash

set -xe

CURRENT_DIR=`pwd`
LOG_DIR=${HOME}/results_backup/nofib-jenkinsbuild-${BUILD_NUMBER}
GHC_TOP_DIR="${HOME}"/opt/ghc-8.1
GHC_BIN_DIR="${GHC_TOP_DIR}"/bin
export PATH="${GHC_BIN_DIR}":$PATH
( cd nofib-analyse     ; "${GHC_BIN_DIR}"/ghc Main.hs -O2 -fforce-recomp -o nofib-analyse )
( cd piecewise-analyse ; "${GHC_BIN_DIR}"/ghc Main.hs -O2 -fforce-recomp -o piecewise-analyse )
piecewise-analyse/piecewise-analyse "${LOG_DIR}"
