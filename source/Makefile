##
###################################################################
##                                                               ##
##  Makefile for Building the Tinker Molecular Modeling Package  ##
##                                                               ##
###################################################################
##
##  Invocation Options:
##
##   1. make all              Build all the TINKER executables
##   2. make rename           Move the executables to BINDIR
##   3. make create_links     Create soft links in LINKDIR
##   4. make remove_links     Remove soft links from LINKDIR
##   6. make listing          Concatenate source to tinker.txt
##   5. make clean            Delete objects and executables
##
##  Original version of this file is due to Peter Happersberger
##  and Jochen Buehler of the University of Konstanz, Jan. 1998,
##  Modifications by Reece Hart & Jay Ponder, Washington University
##
##  Note: Building TINKER requires libraries for the FFTW Fast
##  Fourier Transform package and (optionally) the APBS Poisson-
##  Boltzmann solver. These libraries are assumed to already be
##  present, and are not built by this Makefile.
##
###################################################################

###################################################################
##  Master Directory Locations; Change as Needed for Local Site  ##
###################################################################

##  TINKERDIR    TINKER Distribution Directory
##  BINDIR       Hard Copies of TINKER Executables
##  LINKDIR      Linked Copies of TINKER Executables

TINKERDIR = $(HOME)/tinker
BINDIR = $(TINKERDIR)/bin
LINKDIR = /usr/local/bin

####################################################################
##  Known Machine Types; Uncomment One of the Following Sections  ##
##  May Need Editing to Match Your Desired OS & Compiler Version  ##
####################################################################

# Set the compiler option to use by uncommenting one of the options
# below. If you are not clear what these mean look below under the
# appropriate flag to see in detail what they mean.
#
# If none specified the default is: GenericGfortranOpenMP

#COMPARCH=GenericGfortran
COMPARCH=GenericGfortranOpenMP
#COMPARCH=LinuxIntel
#COMPARCH=LinuxIntelOpenMP
#COMPARCH=MacIntel
#COMPARCH=MacIntelOpenMP
#COMPARCH=MacXeonIntelOpenMP
#COMPARCH=LinuxIntelOpenMP

ifeq ($(COMPARCH),GenericGfortran)

  ##  Machine:  Generic Linux or Macintosh
  ##  CPU Type: Intel x86 Compatibles
  ##  Compiler: GNU gfortran

  FC        = gfortran
  LIBS      = -L$(TINKERDIR)/thirdparty/fftw/lib -lfftw3_omp -lfftw3
  FCFLAGS   = -Wall -c
  OPTFLAGS  = -O3 -ffast-math 
  LIBFLAGS  = -crusv
  RANLIB    = ranlib
  LINKFLAGS = $(OPTFLAGS) -static-libgcc

else ifeq ($(COMPARCH),GenericGfortranOpenMP)

  ##  Machine:  Generic Linux or Macintosh OS X
  ##  CPU Type: Intel x86 Compatible
  ##  Compiler: GNU gfortran
  ##  Parallel: OpenMP

  FC        = gfortran
  LIBS      = -L$(TINKERDIR)/thirdparty/fftw/lib -lfftw3_omp -lfftw3
  FCFLAGS   = -Wall -c
  OPTFLAGS  = -ffast-math -fopenmp -O3
  LIBFLAGS  = -crusv
  RANLIB    = ranlib
  LINKFLAGS = $(OPTFLAGS) -static-libgcc

else ifeq ($(COMPARCH),LinuxIntel)

  ##  Machine:  Generic Linux
  ##  CPU Type: Intel x86 Compatible (also AMD)
  ##  Oper Sys: Fedora Core
  ##  Compiler: Intel Fortran for Linux 14.0

  FC        = ifort
  LIBS      =
  FCFLAGS   = -c -xHost
  OPTFLAGS  = -O3 -no-ipo -no-prec-div -recursive
  LIBFLAGS  = -crusv
  RANLIB    = ranlib
  LINKFLAGS = -static-libgcc -static-intel

else ifeq ($(COMPARCH),LinuxIntelOpenMP)

  ##  Machine:  Generic Linux
  ##  CPU Type: Intel x86 Compatible (also AMD)
  ##  Oper Sys: Fedora Core
  ##  Compiler: Intel Fortran for Linux 14.0
  ##  Parallel: OpenMP

  # Compiler warning flags that could be useful:
  #  
  # -check bounds: run time checking of array bounds.
  # -check all: turn on all checks 
  # -traceback: generate extra information in the object file 
  #             to provide source file  traceback  information
  #             when a severe error occurs at run time.
  # -fp-stack-check: generate  extra code after every
  #                function call to ensure that the floating-point  stack  is
  #                in the expected state.
  #

  FC        = ifort
  LIBS      = -L$(TINKERDIR)/thirdparty/fftw/lib -lfftw3_threads -lfftw3
  FCFLAGS   = -c -xHost
  OPTFLAGS  = -O3 -no-ipo -no-prec-div -recursive -openmp
  LIBFLAGS  = -crusv
  RANLIB    = ranlib
  LINKFLAGS = $(OPTFLAGS) -static-libgcc -static-intel

else ifeq ($(COMPARCH),MacIntelOpenMP)

  ##  Machine:  Macintosh
  ##  CPU Type: Intel Xeon
  ##  Oper Sys: OS X 10.9 (Mavericks)
  ##  Compiler: Intel Fortran for Mac 14.0

  FC        = ifort
  LIBS      =
  FCFLAGS   = -c -axSSSE3
  OPTFLAGS  = -O3 -no-ipo -no-prec-div -mdynamic-no-pic
  LIBFLAGS  = -crusv
  RANLIB    = ranlib
  LINKFLAGS = $(OPTFLAGS) -static-intel -mmacosx-version-min=10.6 \
              -Wl,-stack_size,0x10000000

else ifeq ($(COMPARCH),MacIntelXeonOpenMP)

  ##  Machine:  Macintosh
  ##  CPU Type: Intel Xeon
  ##  Oper Sys: OS X 10.9 (Mavericks)
  ##  Compiler: Intel Fortran for Mac 14.0
  ##  Parallel: OpenMP

  FC        = ifort
  LIBS      = -L$(TINKERDIR)/thirdparty/fftw/lib -lfftw3_omp -lfftw3
  FCFLAGS   = -c -axSSSE3
  OPTFLAGS  = -O3 -no-ipo -no-prec-div -openmp
  LIBFLAGS  = -crusv
  RANLIB    = /usr/bin/ranlib 
  LINKFLAGS = $(OPTFLAGS) -static-intel #-Wl,-stack_size,0x10000000

else

  ##  Machine:  Generic Linux or Macintosh OS X
  ##  CPU Type: Intel x86 Compatible
  ##  Compiler: GNU gfortran
  ##  Parallel: OpenMP

  FC        = gfortran
  LIBS      = -L$(TINKERDIR)/thirdparty/fftw/lib -lfftw3_omp -lfftw3
  FCFLAGS   = -c
  OPTFLAGS  = -O3 -ffast-math -fopenmp
  LIBFLAGS  = -crusv
  RANLIB    = ranlib
  LINKFLAGS = $(OPTFLAGS) -static-libgcc

endif


# This is a trick to always run a perl script to create a
# gitversion.f file with the current git has id. Will work
# for gmake - not sure about the other versions of make
#RUNSCRIPT:=$(shell ./version.pl)

#################################################################
##  Should not be Necessary to Change Things Below this Point  ##
#################################################################

OBJS = angbnd.o \
       angles.o \
       angpot.o \
       argue.o \
       ascii.o \
       atmlst.o \
       atomid.o \
       atoms.o \
       attach.o \
       basefile.o \
       bath.o \
       bicubic.o \
       bitor.o \
       bitors.o \
       bndpot.o \
       bndstr.o \
       bonds.o \
       bounds.o \
       boxes.o \
       calendar.o \
       cell.o \
       chgpot.o \
       chkpole.o \
       chkring.o \
       chkxyz.o \
       cholesky.o \
       chunks.o \
       column.o \
       command.o \
       connect.o \
       control.o \
       couple.o \
       cspline.o \
       cutoffs.o \
       deriv.o \
       dynamic.o \
       eangle1.o \
       ebond1.o \
       ehal1.o \
       empole1.o \
       energi.o \
       eopbend1.o \
       epitors1.o \
       erf.o \
       estrbnd1.o \
       etors1.o \
       etortor1.o \
       eurey1.o \
       evcorr.o \
       ewald.o \
       fatal.o \
       fft.o \
       fft3d.o \
       fftpack.o \
       field.o \
       fields.o \
       files.o \
       final.o \
       getkey.o \
       getnumb.o \
       getprm.o \
       getstring.o \
       gettext.o \
       getword.o \
       getxyz.o \
       gradient.o \
       image.o \
       induce.o \
       inform.o \
       initatom.o \
       initial.o \
       initprm.o \
       iounit.o \
       kangle.o \
       kangs.o \
       katom.o \
       katoms.o \
       kbond.o \
       kbonds.o \
       kewald.o \
       keys.o \
       kinetic.o \
       kmpole.o \
       kmulti.o \
       kopbend.o \
       kopbnd.o \
       korbs.o \
       kpitor.o \
       kpitors.o \
       kpolar.o \
       kpolr.o \
       kstbnd.o \
       kstrbnd.o \
       ktors.o \
       ktorsn.o \
       ktortor.o \
       ktrtor.o \
       kurey.o \
       kurybr.o \
       kvdw.o \
       kvdwpr.o \
       kvdws.o \
       lattice.o \
       light.o \
       limits.o \
       math.o \
       maxwell.o \
       mdinit.o \
       mdrest.o \
       mdsave.o \
       mdstat.o \
       mdstuf.o \
       mechanic.o \
       molcul.o \
       moldyn.o \
       molecule.o \
       mplpot.o \
       mpole.o \
       nblist.o \
       neigh.o \
       nextarg.o \
       nexttext.o \
       nspline.o \
       number.o \
       numeral.o \
       opbend.o \
       openend.o \
       openmp.o \
       output.o \
       params.o \
       pitors.o \
       pme.o \
       pmestuff.o \
       polar.o \
       polgrp.o \
       polpot.o \
       potent.o \
       pressure.o \
       prmkey.o \
       promo.o \
       prtdyn.o \
       prtxyz.o \
       ptable.o \
       random.o \
       readdyn.o \
       readprm.o \
       readxyz.o \
       replica.o \
       ring.o \
       rings.o \
       rotpole.o \
       shunt.o \
       sizes.o \
       sort.o \
       strbnd.o \
       suffix.o \
       switch.o \
       tarray.o \
       temper.o \
       titles.o \
       torphase.o \
       torpot.o \
       torque.o \
       tors.o \
       torsions.o \
       tortor.o \
       trimtext.o \
       unitcell.o \
       units.o \
       uprior.o \
       urey.o \
       usage.o \
       urypot.o \
       usolve.o \
       vdw.o \
       vdwpot.o \
       verlet.o \
       version.o \
       virial.o \
       xyzatm.o \
       zclose.o \
       zcoord.o 

EXEFILES = dynamic.x 

%.o: %.f
	${FC} ${FCFLAGS} ${OPTFLAGS} $< -o $@ 

%.o: %.c
	${CC} ${CFLAGS} ${INCLUDEDIR} ${OPTFLAGS} $<

%.x: %.o libtinker.a
	${FC} ${LINKFLAGS} -o $@ $< -L . -ltinker ${LIBS}
	# strip $@

install: all rename

all:	${EXEFILES}

clean:
	rm -f *.o *.mod *.a *.x

listing:
	cat *.i *.f *.c > tinker.txt

rename:
	mkdir -p $(BINDIR)
	mv  dynamic.x    $(BINDIR)/dynamic

remove_links:
	rm -f $(LINKDIR)/dynamic

create_links:
	ln -s $(BINDIR)/dynamic    $(LINKDIR)/dynamic

libtinker.a: ${OBJS} 
	ar ${LIBFLAGS} libtinker.a $?
	${RANLIB} libtinker.a

###############################################################
##  Next Section has Explicit Dependencies on Include Files  ##
###############################################################

angbnd.o:
angles.o: angbnd.o atmlst.o atoms.o couple.o iounit.o sizes.o
angpot.o:
argue.o:
ascii.o:
atmlst.o:
atomid.o: sizes.o
atoms.o: sizes.o
attach.o: atoms.o couple.o iounit.o sizes.o
basefile.o: ascii.o files.o
bath.o:
bicubic.o:
bitor.o:
bitors.o: angbnd.o atoms.o bitor.o couple.o iounit.o sizes.o
bndpot.o:
bndstr.o:
bonds.o: atmlst.o atoms.o bndstr.o couple.o iounit.o sizes.o
bounds.o: atomid.o atoms.o boxes.o molcul.o sizes.o
boxes.o:
calendar.o:
cell.o: sizes.o
chgpot.o:
chkpole.o: atoms.o mpole.o sizes.o
chkring.o: couple.o sizes.o
chkxyz.o: atoms.o iounit.o sizes.o
cholesky.o:
chunks.o:
column.o: sizes.o
command.o: argue.o
connect.o: atoms.o couple.o sizes.o zclose.o zcoord.o
control.o: argue.o inform.o keys.o output.o sizes.o
couple.o: sizes.o
cspline.o: iounit.o
cutoffs.o: atoms.o keys.o limits.o neigh.o polpot.o sizes.o tarray.o
deriv.o:
dynamic.o: atoms.o bath.o bndstr.o inform.o iounit.o keys.o mdstuf.o sizes.o
eangle1.o: angbnd.o angpot.o atoms.o deriv.o energi.o math.o sizes.o virial.o
ebond1.o: atoms.o bndpot.o bndstr.o deriv.o energi.o sizes.o virial.o
ehal1.o: atomid.o atoms.o couple.o deriv.o energi.o limits.o molcul.o neigh.o shunt.o sizes.o vdw.o vdwpot.o virial.o
empole1.o: atoms.o boxes.o cell.o chgpot.o couple.o deriv.o energi.o ewald.o limits.o math.o molcul.o mplpot.o mpole.o neigh.o pme.o polar.o polgrp.o polpot.o shunt.o sizes.o virial.o
energi.o:
eopbend1.o: angbnd.o angpot.o atoms.o deriv.o energi.o math.o opbend.o sizes.o virial.o
epitors1.o: atoms.o deriv.o energi.o pitors.o sizes.o torpot.o virial.o
erf.o: iounit.o math.o
estrbnd1.o: angbnd.o angpot.o atoms.o bndstr.o deriv.o energi.o math.o sizes.o strbnd.o virial.o
etors1.o: atoms.o deriv.o energi.o sizes.o torpot.o tors.o virial.o
etortor1.o: atomid.o atoms.o bitor.o couple.o deriv.o energi.o ktrtor.o math.o sizes.o torpot.o tortor.o virial.o
eurey1.o: atoms.o deriv.o energi.o sizes.o urey.o urypot.o virial.o
evcorr.o: atoms.o boxes.o limits.o math.o shunt.o sizes.o vdw.o vdwpot.o
ewald.o:
fatal.o: iounit.o
fft.o: sizes.o
fft3d.o: fft.o openmp.o pme.o sizes.o
fftpack.o: math.o
field.o: keys.o potent.o sizes.o
fields.o: sizes.o
files.o:
final.o: angbnd.o atmlst.o bitor.o bndstr.o chunks.o couple.o deriv.o inform.o iounit.o light.o molcul.o moldyn.o mpole.o neigh.o opbend.o pitors.o pme.o polar.o polgrp.o ring.o sizes.o strbnd.o tarray.o tors.o tortor.o uprior.o urey.o vdw.o
getkey.o: argue.o files.o iounit.o keys.o openmp.o sizes.o
getnumb.o: ascii.o
getprm.o: files.o iounit.o keys.o params.o sizes.o
getstring.o: ascii.o
gettext.o: ascii.o
getword.o: ascii.o
getxyz.o: inform.o iounit.o output.o
gradient.o: atoms.o deriv.o energi.o iounit.o limits.o sizes.o vdwpot.o virial.o
image.o: boxes.o cell.o sizes.o
induce.o: atoms.o boxes.o couple.o ewald.o inform.o iounit.o limits.o math.o mpole.o neigh.o openmp.o pme.o polar.o polgrp.o polpot.o shunt.o sizes.o tarray.o units.o uprior.o usolve.o
inform.o:
initatom.o: ptable.o sizes.o
initial.o: atoms.o bath.o boxes.o cell.o files.o inform.o iounit.o keys.o molcul.o neigh.o openmp.o output.o params.o sizes.o zclose.o
initprm.o: angpot.o bndpot.o chgpot.o fields.o kangs.o katoms.o kbonds.o kmulti.o kopbnd.o korbs.o kpitor.o kpolr.o kstbnd.o ktorsn.o ktrtor.o kurybr.o kvdwpr.o kvdws.o math.o mplpot.o polpot.o sizes.o torpot.o units.o urypot.o vdwpot.o
iounit.o:
kangle.o: angbnd.o angpot.o atomid.o atoms.o couple.o fields.o inform.o iounit.o kangs.o keys.o potent.o sizes.o
kangs.o:
katom.o: atomid.o atoms.o couple.o inform.o iounit.o katoms.o keys.o sizes.o
katoms.o: sizes.o
kbond.o: atomid.o atoms.o bndstr.o couple.o fields.o inform.o iounit.o kbonds.o keys.o potent.o sizes.o
kbonds.o:
kewald.o: atoms.o boxes.o chunks.o ewald.o fft.o inform.o iounit.o keys.o limits.o math.o openmp.o pme.o sizes.o
keys.o: sizes.o
kinetic.o: atomid.o atoms.o bath.o mdstuf.o moldyn.o sizes.o units.o
kmpole.o: atoms.o couple.o inform.o iounit.o keys.o kmulti.o mpole.o polar.o polgrp.o potent.o sizes.o units.o
kmulti.o:
kopbend.o: angbnd.o angpot.o atomid.o atoms.o couple.o fields.o inform.o iounit.o keys.o kopbnd.o opbend.o potent.o sizes.o
kopbnd.o:
korbs.o: sizes.o
kpitor.o:
kpitors.o: atomid.o atoms.o bndstr.o couple.o inform.o iounit.o keys.o kpitor.o pitors.o sizes.o tors.o
kpolar.o: atoms.o couple.o inform.o iounit.o keys.o kpolr.o mpole.o polar.o polgrp.o polpot.o potent.o sizes.o usolve.o
kpolr.o: sizes.o
kstbnd.o:
kstrbnd.o: angbnd.o angpot.o atmlst.o atomid.o atoms.o couple.o fields.o inform.o iounit.o keys.o kstbnd.o potent.o sizes.o strbnd.o
ktors.o: atomid.o atoms.o couple.o fields.o inform.o iounit.o keys.o ktorsn.o math.o potent.o sizes.o tors.o
ktorsn.o:
ktortor.o: atomid.o atoms.o bitor.o inform.o iounit.o keys.o ktrtor.o potent.o sizes.o tortor.o
ktrtor.o:
kurey.o: angbnd.o atomid.o atoms.o inform.o iounit.o keys.o kurybr.o potent.o sizes.o urey.o
kurybr.o:
kvdw.o: atomid.o atoms.o couple.o fields.o inform.o iounit.o keys.o kvdwpr.o kvdws.o math.o potent.o sizes.o vdw.o vdwpot.o
kvdwpr.o:
kvdws.o: sizes.o
lattice.o: boxes.o cell.o inform.o iounit.o math.o sizes.o
light.o:
limits.o:
math.o:
maxwell.o: units.o
mdinit.o: atomid.o atoms.o bath.o files.o inform.o iounit.o keys.o mdstuf.o molcul.o moldyn.o mpole.o sizes.o units.o uprior.o
mdrest.o: atomid.o atoms.o inform.o iounit.o mdstuf.o moldyn.o sizes.o units.o
mdsave.o: atomid.o atoms.o boxes.o files.o inform.o iounit.o mdstuf.o moldyn.o mpole.o output.o polar.o sizes.o titles.o units.o
mdstat.o: atoms.o bath.o boxes.o inform.o iounit.o limits.o mdstuf.o molcul.o sizes.o units.o
mdstuf.o:
mechanic.o: inform.o iounit.o limits.o vdwpot.o
molcul.o:
moldyn.o:
molecule.o: atomid.o atoms.o couple.o molcul.o sizes.o
mplpot.o:
mpole.o:
nblist.o: atoms.o boxes.o cell.o iounit.o light.o mpole.o neigh.o sizes.o vdw.o
neigh.o:
nextarg.o: argue.o
nexttext.o:
nspline.o:
number.o: inform.o iounit.o
numeral.o:
opbend.o:
openend.o:
openmp.o:
output.o:
params.o: sizes.o
pitors.o:
pme.o: sizes.o
pmestuff.o: atoms.o boxes.o chunks.o mpole.o pme.o sizes.o
polar.o:
polgrp.o:
polpot.o:
potent.o:
pressure.o: atomid.o atoms.o bath.o boxes.o math.o mdstuf.o sizes.o units.o virial.o
prmkey.o: angpot.o bndpot.o chgpot.o fields.o mplpot.o polpot.o potent.o torpot.o urypot.o vdwpot.o
promo.o: iounit.o
prtdyn.o: atoms.o boxes.o files.o mdstuf.o moldyn.o sizes.o titles.o
prtxyz.o: atomid.o atoms.o boxes.o couple.o files.o inform.o sizes.o titles.o
ptable.o: sizes.o
random.o: inform.o iounit.o keys.o math.o sizes.o
readdyn.o: atoms.o boxes.o files.o iounit.o mdstuf.o moldyn.o sizes.o
readprm.o: fields.o iounit.o kangs.o katoms.o kbonds.o kmulti.o kopbnd.o korbs.o kpitor.o kpolr.o kstbnd.o ktorsn.o ktrtor.o kurybr.o kvdwpr.o kvdws.o params.o sizes.o
readxyz.o: atomid.o atoms.o boxes.o couple.o files.o inform.o iounit.o sizes.o titles.o
replica.o: boxes.o cell.o inform.o iounit.o sizes.o
ring.o:
rings.o: angbnd.o atoms.o bitor.o bndstr.o couple.o inform.o iounit.o ring.o sizes.o tors.o
rotpole.o: atoms.o mpole.o sizes.o
shunt.o:
sizes.o:
sort.o:
strbnd.o:
suffix.o: ascii.o
switch.o: limits.o shunt.o sizes.o
tarray.o:
temper.o: atomid.o atoms.o bath.o mdstuf.o molcul.o moldyn.o sizes.o units.o
titles.o:
torphase.o:
torpot.o:
torque.o: atoms.o deriv.o mpole.o sizes.o
tors.o:
torsions.o: atoms.o bndstr.o couple.o iounit.o sizes.o tors.o
tortor.o:
trimtext.o:
unitcell.o: boxes.o iounit.o keys.o sizes.o
units.o:
uprior.o:
urey.o:
urypot.o:
usolve.o:
vdw.o:
vdwpot.o:
verlet.o: atomid.o atoms.o moldyn.o sizes.o units.o
version.o: iounit.o output.o
virial.o:
xyzatm.o: atoms.o inform.o iounit.o math.o sizes.o
zclose.o: sizes.o
zcoord.o: sizes.o
