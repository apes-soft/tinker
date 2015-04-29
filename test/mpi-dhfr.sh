#!/bin/bash
# Script to run an mpi job for the JAC test/benchmark. 
# Options used for mpirun will depend on the machine used. 
# 
# Usage:
#
# mpi-dhfr.sh NumOfProcs
#
# Can add `time` in front to time execution. If NumOfProcs 
# is not specified it defaults to 1.

# Check if a number of processes has been 
# specified in the command line otherwise
# default to 1.
if [ "$#" -eq 1 ] 
then 
   nprocs=$1
else
   nprocs=1
fi

echo Using $nprocs processes.

export OMP_NUM_THREADS=1
echo Explicitly setting OMP_NUM_THREADS to $OMP_NUM_THREADS

# Command line arguments to dynamic:
#
# keyFileBaseName NumberOfTimesteps \
#                 TimeStep(fm) \
#                 TimeBetweenOuputs(ps) \
#                 Temp(K) \
#                 Press(Atms)
#

# Possible arguments for mpirun:
#
# -np - number of processes
# -mca btl tcp,sm,self - specify the message transport mechanism (for Indy)
# --mca orte_base_help_aggregate 0  - disable error message aggregation (for Indy)
# --quiet - don't print out so much stuff out

# Really want to use `hostname -s` but not universally available.
# For debugging can add the line: xterm -e gdb before the executable
# to start the program within gdb.

if [ `hostname` = "mbp-ma.local" ]; then  # Mario's mac.

  mpirun -np $nprocs --quiet \
         ../bin/dynamic dhfr 100 1.0 10.0 2 300.0

elif [ `hostname` = "mbp-ma.lan" ]; then  # Mario's mac.

  mpirun -np $nprocs --quiet \
         ../bin/dynamic dhfr 100 1.0 10.0 2 300.0

elif [ `hostname` = "indy0" ]; then       # Indy (system at EPCC).


  mpirun -np $nprocs \
         -mca btl tcp,sm,self \
         ../bin/dynamic dhfr 10 1.0 10.0 2 300.0

elif [ `hostname` = "phi.hydra" ]; then       # phi.hydra (system at EPCC).

  # Hack to bypass SLURM/Intel MPI problems
  export SLURM_JOBID=
  # Run the code
  time mpirun -np $nprocs \
         ../bin/dynamic dhfr 10 1.0 10.0 2 300.0

else

echo
echo "Unknown system :" `hostname`.
echo " Check MPI call routine."
echo

  mpirun -np $nprocs \
         -mca btl tcp,sm,self \
         ../bin/dynamic dhfr 2 1.0 10.0 2 300.0


fi

