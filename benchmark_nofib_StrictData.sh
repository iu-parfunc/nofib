#!/bin/bash

set -xe

CURRENT_DIR=`pwd`
DEPENDENCY_PKG_DIR="${CURRENT_DIR}"/register_junk_here
DEPENDENCY_BUILD_DIR="${CURRENT_DIR}"/build_junk_here
LOG_DIR=${HOME}/results_backup/nofib-jenkinsbuild-${BUILD_NUMBER}
LOG_EXT=.lgfile
DATE=$(date -d "today" +"%Y%m%d%H%M")
LOG_NAME=log-node-${NODE_NAME}-branch-${UBOX_BRANCH}-strictdata-${UBOX_STRICT_DATA}-funboxsmallstrictsums-${UBOX_FUNBOX_SMALL_STRICT_SUMS}-fdosumdmdww-${UBOX_FDO_SUM_DMD_WW}-fdosumcprww-${UBOX_FDO_SUM_CPR_WW}${LOG_EXT}

echo "Benchmark results will go in: ${LOG_NAME}"

if [ "${UBOX_BRANCH}" == "stock" ]; then
    echo "Using stock GHC"
    GHC_TOP_DIR="${HOME}"/opt/ghc-8.1
else
    echo "Using our version of GHC (with worker/wrapper)"
    GHC_TOP_DIR="${HOME}"/opt/ghc-unboxed-sums-1.10
fi

GHC_BIN_DIR="${GHC_TOP_DIR}"/bin
EXTRA_NOFIB_FLAGS=""

if [ "${UBOX_STRICT_DATA}" == "1" ]; then
    echo "Using -XStrictData"
    EXTRA_NOFIB_FLAGS+=" -XStrictData -DSTRICT_DATA"
fi

# -funbox-small-strict-sums=n
if   [[ ("${UBOX_BRANCH}" == "stock") && ("${UBOX_FUNBOX_SMALL_STRICT_SUMS}" != "none") ]]; then
    echo "Invalid configuration, amigo."
    exit 0
elif [[ ("${UBOX_BRANCH}" != "stock") && ("${UBOX_FUNBOX_SMALL_STRICT_SUMS}" != "none") ]]; then
    echo "Using -funbox-small-strict-sums=${UBOX_FUNBOX_SMALL_STRICT_SUMS}"
    EXTRA_NOFIB_FLAGS+=" -funbox-small-strict-sums=${UBOX_FUNBOX_SMALL_STRICT_SUMS}"
fi

# -fdo-sum-dmd / -fdo-sum-dmd-ww
if   [[ ("${UBOX_BRANCH}" == "stock") && ("${UBOX_FDO_SUM_DMD_WW}" == "1") ]]; then
    echo "Invalid configuration, amigo."
    exit 0
elif [[ ("${UBOX_BRANCH}" != "stock") && ("${UBOX_FDO_SUM_DMD_WW}" == "1") ]]; then
    echo "Using -fdo-sum-dmd and -fdo-sum-dmd-ww"
    EXTRA_NOFIB_FLAGS+=" -fdo-sum-dmd -fdo-sum-dmd-ww"
fi

# -fdo-sum-cpr-ww
if   [[ ("${UBOX_BRANCH}" == "stock") && ("${UBOX_FDO_SUM_CPR_WW}" == "1") ]]; then
    echo "Invalid configuration, amigo."
    exit 0
elif [[ ("${UBOX_BRANCH}" != "stock") && ("${UBOX_FDO_SUM_CPR_WW}" == "1") ]]; then
    echo "Using -fdo-sum-cpr-ww"
    EXTRA_NOFIB_FLAGS+=" -fdo-sum-cpr-ww"
fi

rm -rf "${DEPENDENCY_PKG_DIR}"
rm -rf "${DEPENDENCY_BUILD_DIR}"

"${GHC_BIN_DIR}"/ghc-pkg init "${DEPENDENCY_PKG_DIR}"
export PATH="${GHC_BIN_DIR}":$PATH

( cd html-1.0.1.2/ \
  && "${GHC_BIN_DIR}"/runghc Setup clean \
  && "${GHC_BIN_DIR}"/runghc Setup configure --package-db="${DEPENDENCY_PKG_DIR}" --prefix="${DEPENDENCY_BUILD_DIR}" --user \
  && "${GHC_BIN_DIR}"/runghc Setup build \
  && "${GHC_BIN_DIR}"/runghc Setup copy \
  && "${GHC_BIN_DIR}"/runghc Setup register )

( cd mtl-2.2.1/ \
  && "${GHC_BIN_DIR}"/runghc Setup clean \
  && "${GHC_BIN_DIR}"/runghc Setup configure --package-db="${DEPENDENCY_PKG_DIR}" --prefix="${DEPENDENCY_BUILD_DIR}" --user \
  && "${GHC_BIN_DIR}"/runghc Setup build \
  && "${GHC_BIN_DIR}"/runghc Setup copy \
  && "${GHC_BIN_DIR}"/runghc Setup register )

( cd regex-base-0.93.2/ \
  && "${GHC_BIN_DIR}"/runghc Setup clean \
  && "${GHC_BIN_DIR}"/runghc Setup configure --package-db="${DEPENDENCY_PKG_DIR}" --prefix="${DEPENDENCY_BUILD_DIR}" --user \
  && "${GHC_BIN_DIR}"/runghc Setup build \
  && "${GHC_BIN_DIR}"/runghc Setup copy \
  && "${GHC_BIN_DIR}"/runghc Setup register )

( cd regex-posix-0.95.2/ \
  && "${GHC_BIN_DIR}"/runghc Setup clean \
  && "${GHC_BIN_DIR}"/runghc Setup configure --package-db="${DEPENDENCY_PKG_DIR}" --prefix="${DEPENDENCY_BUILD_DIR}" --user \
  && "${GHC_BIN_DIR}"/runghc Setup build \
  && "${GHC_BIN_DIR}"/runghc Setup copy \
  && "${GHC_BIN_DIR}"/runghc Setup register )

( cd regex-compat-0.95.1/ \
  && "${GHC_BIN_DIR}"/runghc Setup clean \
  && "${GHC_BIN_DIR}"/runghc Setup configure --package-db="${DEPENDENCY_PKG_DIR}" --prefix="${DEPENDENCY_BUILD_DIR}" --user \
  && "${GHC_BIN_DIR}"/runghc Setup build \
  && "${GHC_BIN_DIR}"/runghc Setup copy \
  && "${GHC_BIN_DIR}"/runghc Setup register )

make clean
make EXTRA_HC_OPTS="-package-db ${DEPENDENCY_PKG_DIR} ${EXTRA_NOFIB_FLAGS}" boot
make EXTRA_HC_OPTS="-package-db ${DEPENDENCY_PKG_DIR} ${EXTRA_NOFIB_FLAGS}" --keep-going 2>&1 | tee ${LOG_NAME}

mkdir -p "${LOG_DIR}"/
cp ${LOG_NAME} "${LOG_DIR}"/
