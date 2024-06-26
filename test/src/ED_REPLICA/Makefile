FC=mpif90
FFLAG = -O2 -ffree-line-length-none -coverage
FPPMPI=-cpp -D_MPI
FFLAGS= ${FFLAG} ${FPPMPI}

.SUFFIXES: .f90
.PHONY: clean

GLOB_INC:=$(shell pkg-config --cflags scifor edipack2)

all:  NORMAL SUPERC

NORMAL SUPERC:
	make -C ./ED_$@

clean:
	rm -rf *.mod *.o 
