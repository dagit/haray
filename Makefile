.PHONY: all

# This makefile assumes a lot of things. I use it on windows, your mileage may vary
OPENCL_PATH := --extra-lib-dirs="C:\Program Files\NVIDIA Corporation\OpenCL"
USE_OPENCL  := -fuse-opencl

all:
	cabal-dev install $(OPENCL_PATH) $(USE_OPENCL)
