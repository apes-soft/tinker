# Flags
# 
# gfortran based MPI compilation
#
# -fopenmp    - enable OpenMP compilation.
# -Wall       - enable all compiler warning.
# -fcheck=all - enable run time checks.
# -g          - compile with debugging information.
#
FC       = mpif90
OPTFLAGS = -g
FFLAGS   = -fopenmp -Wall -fcheck=all 
LDFLAGS  = -g -fopenmp

# Run with on indy with: 
#
#     mpirun -np 10 -mca btl tcp,sm,self ./testdecomp
#

# Note files that are used by other files must be compiled 
# before so the order below can be important.
MODULES  = sizes.o          \
           atoms.o          \
           files.o          \
           hescut.o         \
           inform.o         \
           iounit.o         \
           math.o           \
           neigh.o          \
           openmp.o         \
           output.o         \
           parallelparams.o \
           polpot.o         \
           ptable.o         \
           resdue.o         \
           socket.o         \
           tarray.o         \
           titles.o         \
           keys.o           \
           align.o          \
           argue.o          \
           atmtyp.o         \
           basefile.o       \
           bath.o           \
           boxes.o          \
           bound.o          \
           cell.o           \
           command.o        \
           control.o        \
           cutoff.o         \
           chkxyz.o         \
           couple.o         \
           fatal.o          \
           getkey.o         \
           gettext.o        \
           getword.o        \
           getxyz.o         \
           group.o          \
           initatom.o       \
           initres.o        \
           linmin.o         \
           lattice.o        \
           minima.o         \
           molcul.o         \
           mutant.o         \
           nextarg.o        \
           nexttext.o       \
           params.o         \
           pdb.o            \
           precis.o         \
           promo.o          \
           readxyz.o        \
           redistribute.o   \
           rigid.o          \
           scales.o         \
           sequen.o         \
           sort.o           \
           sortAtoms.o      \
           suffix.o         \
           trimtext.o       \
           version.o        \
           warp.o           \
           zclose.o         \
           initial.o

%.o: %.f90
	${FC} -c ${FFLAGS} ${OPTFLAGS} $< -o $@ 

testdecomp: ${MODULES} testdecomp.o 
	${FC} ${LDFLAGS} ${OPTFLAGS} $^ -o $@ 

modules: ${MODULES}
	

clean:
	rm -f *.o *~ *.mod testdecomp

