#!/bin/bash

set -xe

GHC_TOP_DIR="${HOME}"/opt/ghc-unboxed-sums-1.3
LOG_DIR="${HOME}"/results_backup
GHC_BIN_DIR="${GHC_TOP_DIR}"/bin
LOG_EXT=.lgfile

LOG_NORMAL=nofib-log-normal-$(date -d "today" +"%Y%m%d%H%M")${LOG_EXT}
LOG_STRICT=nofib-log-strict-$(date -d "today" +"%Y%m%d%H%M")${LOG_EXT}
LOG_UBXSUMS=nofib-log-ubxsums-$(date -d "today" +"%Y%m%d%H%M")${LOG_EXT}
COMP_NORMAL_STRICT=nofib-comp-normal-strict-$(date -d "today" +"%Y%m%d%H%M")${LOG_EXT}
COMP_NORMAL_UBXSUMS=nofib-comp-normal-ubxsums-$(date -d "today" +"%Y%m%d%H%M")${LOG_EXT}
COMP_STRICT_UBXSUMS=nofib-comp-strict-ubxsums-$(date -d "today" +"%Y%m%d%H%M")${LOG_EXT}

STRICT_FLAGS="-XStrictData -DSTRICT_DATA"
UBXSUMS_FLAGS="${STRICT_FLAGS} -funbox-small-strict-sums=2"

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

make clean
make boot
make 2>&1 | tee ${LOG_NORMAL}

make clean
make boot
make EXTRA_HC_OPTS="${STRICT_FLAGS}" 2>&1 | tee ${LOG_STRICT}

make clean
make boot
make EXTRA_HC_OPTS="${UBXSUMS_FLAGS}" 2>&1 | tee ${LOG_UBXSUMS}

nofib-analyse/nofib-analyse ${LOG_NORMAL} ${LOG_STRICT}  > ${COMP_NORMAL_STRICT}
nofib-analyse/nofib-analyse ${LOG_NORMAL} ${LOG_UBXSUMS} > ${COMP_NORMAL_UBXSUMS}
nofib-analyse/nofib-analyse ${LOG_STRICT} ${LOG_UBXSUMS} > ${COMP_STRICT_UBXSUMS}

cp ${LOG_NORMAL}          "${LOG_DIR}"/
cp ${LOG_STRICT}          "${LOG_DIR}"/
cp ${LOG_UBXSUMS}         "${LOG_DIR}"/
cp ${COMP_NORMAL_STRICT}  "${LOG_DIR}"/
cp ${COMP_NORMAL_UBXSUMS} "${LOG_DIR}"/
cp ${COMP_STRICT_UBXSUMS} "${LOG_DIR}"/
