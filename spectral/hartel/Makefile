TOP = ../..
include $(TOP)/mk/boilerplate.mk

# Disabled for now: transform, solid
SUBDIRS= comp_lab_zift wave4main wang typecheck \
sched nucleic2 parstof listcopy listcompr ida genfft fft \
event

SRC_MKDEPENDHS_OPTS=-fglasgow-exts

EXCLUDED_SRCS += Fast2haskell.hs Fast2haskell2.hs 

include $(TOP)/mk/target.mk

