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

# Check if a number of processes or threads has been 
# specified in the command line otherwise
# default to 1 for each case.
if [ "$#" -gt 2 ] 
then 

    echo
    echo Usage: $0 NumProcs NumThreads
    echo 
    echo If only one number is specified that is used for the
    echo number of processes, if no numbers are specified the
    echo values default to 1.
    echo
    exit

elif [ "$#" -eq 2 ] 
then 

   nprocs=$1
   nthreads=$2

elif [ "$#" -eq 1 ]
then

   nprocs=$1
   nthreads=1

else

   nprocs=1
   nthreads=1

fi

# Outfile name
outfile="out.txt"

# Dynamic command
dynamic="../bin/dynamic dhfr 10 1.0 10.0 2 300.0" 

echo | tee -a $outfile                           # Add an empty line
echo Using $nprocs processes. | tee -a $outfile  # How many procs are usd

export OMP_NUM_THREADS=$nthreads
echo Explicitly setting OMP_NUM_THREADS to $OMP_NUM_THREADS. | tee -a $outfile

#echo processes $nprocs, threads $nthreads

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
# --quiet - don't print out so much stuff out
#
# For indy:
# --mca btl tcp,sm,self - specify the message transport mechanism 
# --mca orte_base_help_aggregate 0  - disable error message aggregation


# Really want to use `hostname -s` but not universally available.
# For debugging can add the line: xterm -e gdb before the executable
# to start the program within gdb.

if [ `hostname` = "mbp-ma.local" ]; then  # Mario's mac.

  (time mpirun -np $nprocs --quiet $dynamic) 2>&1\
         | tee -a $outfile

elif [ `hostname` = "mbp-ma.lan" ]; then  # Mario's mac.

  (time mpirun -np $nprocs --quiet $dynamic) 2>&1\
         | tee -a $outfile

elif [ `hostname` = "indy0" ]; then       # Indy (system at EPCC).


  (time mpirun -np $nprocs \
         -mca btl tcp,sm,self $dynamic) 2>&1\
         | tee -a $outfile 

elif [ `hostname` = "phi.hydra" ]; then       # phi.hydra (system at EPCC).

  # Hack to bypass SLURM/Intel MPI problems
  export SLURM_JOBID=

  # Run the code
  #(time mpirun -trace -np $nprocs $dynamic) 2>&1 \
  #(time mpirun -np $nprocs inspxe-cl -collect mi2 -r rmi3 $dynamic) 2>&1\
  (time mpirun -np $nprocs $dynamic) 2>&1\
         | tee -a $outfile

else

echo
echo "Unknown system :" `hostname`.
echo " Check MPI call routine."
echo

  (time mpirun -np $nprocs \
         -mca btl tcp,sm,self $dynamic) 2>&1\
         | tee -a $outfile

fi

