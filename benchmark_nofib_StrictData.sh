#!/bin/bash

set -xe

OURHOME=/u/crest-team
LOG_DIR="${OURHOME}"/results_backup
LOG_EXT=.lgfile

STOCK_GHC_TOP_DIR="${OURHOME}"/opt/ghc-8.1
STOCK_GHC_BIN_DIR="${STOCK_GHC_TOP_DIR}"/bin

UBXSUMS_GHC_TOP_DIR="${OURHOME}"/opt/ghc-unboxed-sums-1.5
UBXSUMS_GHC_BIN_DIR="${UBXSUMS_GHC_TOP_DIR}"/bin

LOG_STOCK_NORMAL=$(date -d "today" +"%Y%m%d%H%M")-nofib-log-stock-normal${LOG_EXT}
LOG_STOCK_STRICT=$(date -d "today" +"%Y%m%d%H%M")-nofib-log-stock-strict${LOG_EXT}
LOG_UBXSUMS_NORMAL=$(date -d "today" +"%Y%m%d%H%M")-nofib-log-ubxsums-normal${LOG_EXT}
LOG_UBXSUMS_STRICT=$(date -d "today" +"%Y%m%d%H%M")-nofib-log-ubxsums-strict${LOG_EXT}
LOG_UBXSUMS_FUNBOXSSS=$(date -d "today" +"%Y%m%d%H%M")-nofib-log-ubxsums-funboxsss${LOG_EXT}
COMP_STOCK_STRICT=$(date -d "today" +"%Y%m%d%H%M")-nofib-comp-stock-strict${LOG_EXT}
COMP_UBXSUMS_NORMAL=$(date -d "today" +"%Y%m%d%H%M")-nofib-comp-ubxsums-normal${LOG_EXT}
COMP_UBXSUMS_STRICT=$(date -d "today" +"%Y%m%d%H%M")-nofib-comp-ubxsums-strict${LOG_EXT}
COMP_UBXSUMS_FUNBOXSSS=$(date -d "today" +"%Y%m%d%H%M")-nofib-comp-ubxsums-funboxsss${LOG_EXT}

STRICT_FLAGS="-XStrictData -DSTRICT_DATA"
FUNBOXSSS_FLAGS="${STRICT_FLAGS} -funbox-small-strict-sums"

export PATH=${STOCK_GHC_BIN_DIR}:$PATH

( cd html-1.0.1.2/ \
  && runghc Setup clean \
  && runghc Setup configure --prefix="${STOCK_GHC_TOP_DIR}" \
  && runghc Setup build \
  && runghc Setup install )

( cd mtl-2.2.1/ \
  && runghc Setup clean \
  && runghc Setup configure --prefix="${STOCK_GHC_TOP_DIR}" \
  && runghc Setup build \
  && runghc Setup install )

( cd regex-base-0.93.2/ \
  && runghc Setup clean \
  && runghc Setup configure --prefix="${STOCK_GHC_TOP_DIR}" \
  && runghc Setup build \
  && runghc Setup install )

( cd regex-posix-0.95.2/ \
  && runghc Setup clean \
  && runghc Setup configure --prefix="${STOCK_GHC_TOP_DIR}" \
  && runghc Setup build \
  && runghc Setup install )

( cd regex-compat-0.95.1/ \
  && runghc Setup clean \
  && runghc Setup configure --prefix="${STOCK_GHC_TOP_DIR}" \
  && runghc Setup build \
  && runghc Setup install )

make clean
make boot
make 2>&1 | tee ${LOG_STOCK_NORMAL}

make clean
make boot
make EXTRA_HC_OPTS="${STRICT_FLAGS}" 2>&1 | tee ${LOG_STOCK_STRICT}

export PATH=${UBXSUMS_GHC_BIN_DIR}:$PATH

( cd html-1.0.1.2/ \
  && runghc Setup clean \
  && runghc Setup configure --prefix="${UBXSUMS_GHC_TOP_DIR}" \
  && runghc Setup build \
  && runghc Setup install )

( cd mtl-2.2.1/ \
  && runghc Setup clean \
  && runghc Setup configure --prefix="${UBXSUMS_GHC_TOP_DIR}" \
  && runghc Setup build \
  && runghc Setup install )

( cd regex-base-0.93.2/ \
  && runghc Setup clean \
  && runghc Setup configure --prefix="${UBXSUMS_GHC_TOP_DIR}" \
  && runghc Setup build \
  && runghc Setup install )

( cd regex-posix-0.95.2/ \
  && runghc Setup clean \
  && runghc Setup configure --prefix="${UBXSUMS_GHC_TOP_DIR}" \
  && runghc Setup build \
  && runghc Setup install )

( cd regex-compat-0.95.1/ \
  && runghc Setup clean \
  && runghc Setup configure --prefix="${UBXSUMS_GHC_TOP_DIR}" \
  && runghc Setup build \
  && runghc Setup install )

make clean
make boot
make 2>&1 | tee ${LOG_UBXSUMS_NORMAL}

make clean
make boot
make EXTRA_HC_OPTS="${STRICT_FLAGS}" 2>&1 | tee ${LOG_UBXSUMS_STRICT}

make clean
make boot
make EXTRA_HC_OPTS="${FUNBOXSSS_FLAGS}" 2>&1 | tee ${LOG_UBXSUMS_FUNBOXSSS}

nofib-analyse/nofib-analyse ${LOG_STOCK_NORMAL} ${LOG_STOCK_STRICT}      > ${COMP_STOCK_STRICT}
nofib-analyse/nofib-analyse ${LOG_STOCK_NORMAL} ${LOG_UBXSUMS_NORMAL}    > ${COMP_UBXSUMS_NORMAL}
nofib-analyse/nofib-analyse ${LOG_STOCK_NORMAL} ${LOG_UBXSUMS_STRICT}    > ${COMP_UBXSUMS_STRICT}
nofib-analyse/nofib-analyse ${LOG_STOCK_NORMAL} ${LOG_UBXSUMS_FUNBOXSSS} > ${COMP_UBXSUMS_FUNBOXSSS}

cp ${LOG_STOCK_NORMAL}       "${LOG_DIR}"/
cp ${LOG_STOCK_STRICT}       "${LOG_DIR}"/
cp ${LOG_UBXSUMS_NORMAL}     "${LOG_DIR}"/
cp ${LOG_UBXSUMS_STRICT}     "${LOG_DIR}"/
cp ${LOG_UBXSUMS_FUNBOXSSS}  "${LOG_DIR}"/

cp ${COMP_STOCK_STRICT}      "${LOG_DIR}"/
cp ${COMP_UBXSUMS_NORMAL}    "${LOG_DIR}"/
cp ${COMP_UBXSUMS_STRICT}    "${LOG_DIR}"/
cp ${COMP_UBXSUMS_FUNBOXSSS} "${LOG_DIR}"/
