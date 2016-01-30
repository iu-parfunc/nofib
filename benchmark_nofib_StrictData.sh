#!/bin/bash

set -xe

WORKDIR=`pwd`
GHC_TOP_DIR="${HOME}"/opt/ghc-8.0.0-20160111
LOG_DIR="${HOME}"/nofib-results
GHC_BIN_DIR="${GHC_TOP_DIR}"/bin

LOG_NORMAL=nofib-log-normal-$(date -d "today" +"%Y%m%d%H%M").log
LOG_STRICT=nofib-log-strict-$(date -d "today" +"%Y%m%d%H%M").log

COMP_NORMAL_STRICT=nofib-comp-normal-strict-$(date -d "today" +"%Y%m%d%H%M").log

export PATH=${GHC_BIN_DIR}:$PATH

( cd html-1.0.1.2/ \
  && runghc Setup configure --prefix="${GHC_TOP_DIR}" \
  && runghc Setup build \
  && runghc Setup install )

( cd mtl-2.2.1/ \
  && runghc Setup configure --prefix="${GHC_TOP_DIR}" \
  && runghc Setup build \
  && runghc Setup install )

( cd regex-base-0.93.2/ \
  && runghc Setup configure --prefix="${GHC_TOP_DIR}" \
  && runghc Setup build \
  && runghc Setup install )

( cd regex-posix-0.95.2/ \
  && runghc Setup configure --prefix="${GHC_TOP_DIR}" \
  && runghc Setup build \
  && runghc Setup install )

( cd regex-compat-0.95.1/ \
  && runghc Setup configure --prefix="${GHC_TOP_DIR}" \
  && runghc Setup build \
  && runghc Setup install )

make boot
make 2>&1 | tee ${LOG_NORMAL}

make clean
make boot
make EXTRA_HC_OPTS="-XStrictData -DSTRICT_DATA" 2>&1 | tee ${LOG_STRICT}

nofib-analyse/nofib-analyse ${LOG_NORMAL} ${LOG_STRICT} > ${COMP_NORMAL_STRICT}

cp ${LOG_NORMAL}         "${LOG_DIR}"/
cp ${LOG_STRICT}         "${LOG_DIR}"/
cp ${COMP_NORMAL_STRICT} "${LOG_DIR}"/
