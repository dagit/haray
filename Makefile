.PHONY: all

# This makefile assumes a lot of things. I use it on windows, your mileage may vary
OPENCL_PATH := --extra-lib-dirs="C:\Program Files\NVIDIA Corporation\OpenCL" \
               --extra-include-dirs="C:\Program Files (x86)\Intel\OpenCL SDK\1.1\include"
USE_OPENCL  := -fuse-opencl

ifeq ($(V),1)
  VERBOSITY= --verbose=3
endif

all:
	cabal-dev install $(OPENCL_PATH) $(USE_OPENCL) $(VERBOSITY)
