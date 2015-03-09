#
# Makefile to make test programs
#

# Set defaults
FC=gfortran
FCFLAGS=

# Targets that do not have a dependency of external files
.PHONY: clean

# Default target
all: testsplit

testsplit: testsplit.f
	$(FC) $(FCFLAGS) -o $@ $<

# Remove unwanted files
clean:
	rm -fr testsplit *.mod

# Dependencies
testsplit.o: testsplit.f