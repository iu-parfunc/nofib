#################################################################################
#
#			    nofib/Makefile
#
#		Toplevel Makefile for the nofib project
#
# 		$Id: Makefile,v 1.6 1998/04/14 10:38:57 simonm Exp $
#
#################################################################################

TOP = .
include $(TOP)/mk/boilerplate.mk

# Set up which parts of the nofib suite that is to be
# run. See $(TOP)/../mk/config.mk, which tells you how
# to set NoFibSubDirs
#
# As usual,if you want to override these, create
# $(TOP)/../mk/build.mk containing the flags and options
# you want to use in a build tree.
SUBDIRS = runstdtest nofib-analyse $(NoFibSubDirs)

# Include the standard targets, one of which
# causes make to descend into the SUBDIRS.

#
# Creating a nofib distribution
#
SRC_DIST_DIR=$(shell pwd)/nofib
SRC_DIST_NAME=nofib
SRC_DIST_DIRS=docs fibon gc imaginary smp spectral real parallel mk shootout

dist :: nofib-dist-pre
include $(TOP)/mk/target.mk
dist :: dist-post
dist :: dist-package

GHC_TOP_DIR="$(pwd)"
GHC_INPLACE_DIR="$(GHC_TOP_DIR)"/inplace
GHC_BIN_DIR="$(GHC_BIN_DIR)"/bin
HC="$(GHC_BIN_DIR)"/ghc-stage2
BOOT_HC="$(HC)"

iu: iudependencies iunormal iustrict

iudependencies:
	# We need to install html, regex-base, regex-compat, and mtl with GHC HEAD.
	# Unfortunately, neither html, regex-base, nor regex-compat have git repos as
	# far as I am aware, so I simply made local copies of them.
	cd html-1.0.1.2/ \
	&& "$(GHC_BIN_DIR)"/runghc Setup configure --with-ghc="$(HC)" --prefix="$(GHC_INPLACE_DIR)" \
	&& "$(GHC_BIN_DIR)"/runghc Setup build \
	&& "$(GHC_BIN_DIR)"/runghc Setup install

	cd mtl/ \
	&& "$(GHC_BIN_DIR)"/runghc Setup configure --with-ghc="$(HC)" --prefix="$(GHC_INPLACE_DIR)" \
	&& "$(GHC_BIN_DIR)"/runghc Setup build \
	&& "$(GHC_BIN_DIR)"/runghc Setup install \

	cd regex-posix-0.95.2/ \
	&& "$(GHC_BIN_DIR)"/runghc Setup configure --with-ghc="$(HC)" --prefix="$(GHC_INPLACE_DIR)" \
	&& "$(GHC_BIN_DIR)"/runghc Setup build \
	&& "$(GHC_BIN_DIR)"/runghc Setup install

	cd regex-base-0.93.2/ \
	&& "$(GHC_BIN_DIR)"/runghc Setup configure --with-ghc="$(HC)" --prefix="$(GHC_INPLACE_DIR)" \
	&& "$(GHC_BIN_DIR)"/runghc Setup build \
	&& "$(GHC_BIN_DIR)"/runghc Setup install

	cd regex-compat-0.95.1/ \
	&& "$(GHC_BIN_DIR)"/runghc Setup configure --with-ghc="$(HC)" --prefix="$(GHC_INPLACE_DIR)" \
	&& "$(GHC_BIN_DIR)"/runghc Setup build \
	&& "$(GHC_BIN_DIR)"/runghc Setup install

iuboot:
	make -e boot

iunormal:
	make -e 2>&1 | tee nofib-log-normal

iustrict:
	make -e EXTRA_HC_OPTS="-XStrictData -DSTRICT_DATA" 2>&1 | tee nofib-log-strict
