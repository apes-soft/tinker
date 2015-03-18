#
# Makefile to make test programs
#

# Set defaults
FC=gfortran
FCFLAGS=

# MPI Fortran compiler
MPIFORT=mpiifort

# Targets that do not have a dependency of external files
.PHONY: clean

# Default target
all: allgatherv testsplit

testsplit: testsplit.f
	$(FC) $(FCFLAGS) -o $@ $<

allgatherv: allgatherv.f
	$(MPIFORT) $(FCFLAGS) -o $@ $<

# Remove unwanted files
clean:
	rm -fr testsplit *.mod

# Dependencies
testsplit.o: testsplit.f
allgatherv.o: allgatherv.o
