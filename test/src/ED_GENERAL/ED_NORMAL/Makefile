#TO BE CHANGED BY USER:
#$ DRIVER NAME without .f90 extension
#$ COMPILER: suppprted compilers are ifort, gnu >v4.7 or use mpif90
#$ PLATFORM: supported platform are intel, gnu
#$ EXECUTABLE TARGET DIRECTORY (default if $HOME/.bin in the PATH)
EXE=ed_general_normal
FC=mpif90
PLAT=gnu
TSTBIN=../../../bin/GENERAL/NORMAL
OBJS=../../ASSERTING.o

#NO NEED TO CHANGE DOWN HERE, only expert mode.
#########################################################################
GLOB_INC:=$(shell pkg-config --cflags edipack2 scifor)
GLOB_LIB:=$(shell pkg-config --libs   edipack2 scifor | sed  "s/;/ /g"  | sed 's/\\/  /g')


ifeq ($(PLAT),intel)
FFLAG=-O2 -ftz -coverage
OFLAG=-O3 -ftz
DFLAG=-p -O0 -g -fpe0 -warn -warn errors -debug extended -traceback -check all,noarg_temp_created
FPPFLAG =-fpp
endif
ifeq ($(PLAT),gnu)
FFLAG = -O2 -ffree-line-length-none -coverage
DFLAG = -O2 -p -g -fimplicit-none -Wsurprising  -Waliasing -fwhole-file -fcheck=all -pedantic -fbacktrace -ffree-line-length-none
OFLAG = -O3 -ffast-math -march=native -funroll-loops -ffree-line-length-none
FPPFLAG =-cpp -D_MPI
endif


##$ REVISION SOFTWARE VARIABLES
#REV=$(shell git rev-parse HEAD)
#REV=$(shell git rev-parse HEAD)
#VER = 'character(len=41),parameter :: revision = "$(REV)"' > revision.inc


##$ Extends the implicit support of the Makefile to .f90 files
.SUFFIXES: .f90 .o

all: FLAG:=${FFLAG} ${FPPFLAG}
all:
	@echo ""
	@echo "compiling $(EXE).f90 "
	@echo ""
	$(FC) $(FLAG) $(OBJS) $(EXE).f90 -o $(TSTBIN)/$(EXE).x -I../../ ${GLOB_INC} ${GLOB_LIB}
	@cp *.check $(TSTBIN)/
	@cp *.in    $(TSTBIN)/
	@echo "Done"

debug: FLAG:=${DFLAG} ${FPPFLAG}
debug:
	@echo ""
	@echo "compiling $(EXE).f90 "
	@echo ""
	$(FC) $(FLAG) $(OBJS) $(EXE).f90 -o $(TSTBIN)/$(EXE).x -I../../ ${GLOB_INC} ${GLOB_LIB}
	@cp *.check $(TSTBIN)/
	@cp *.in    $(TSTBIN)/
	@echo "Done"

clean: 
	@echo "Cleaning REPLICA NORMAL:"
	@rm -rf *.mod *.o *~
	@rm -fv  $(TSTBIN)/$(EXE)



#########################################################################
