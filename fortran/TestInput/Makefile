# ============================================================================
# Name        : Makefile
# Author      : Don
# Version     :
# Copyright   : Your copyright notice
# Description : Makefile for Hello World in Fortran
# ============================================================================

.PHONY: all clean

# Change this line if you are using a different Fortran compiler
FORTRAN_COMPILER = gfortran

all: src/TestInput.f90
	$(FORTRAN_COMPILER) -O2 -g \
		-o bin/TestInput.exe \
		src/TestInput.f90

clean:
	rm -f bin/TestInput.exe *.mod
