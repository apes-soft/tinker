#!/bin/bash
# Script to run an mpi job for bench7. Options used for mpirun will
# depend on the machine used. Usage:
#
# bench7-mpi.sh NumOfProcs
#
# Can add `time` in front to time

# Check if a number of processes has been 
# specified in the command line otherwise
# default to 1.
if [ "$#" -eq 1 ] 
then 
   nprocs=$1
else
   nprocs=1
fi

echo Using $nprocs processes

# -np - number of processes
# -mca btl tcp,sm,self - specify the message transport mechanism
# --mca orte_base_help_aggregate 0  - disable error message aggregation

if [ `hostname` = "mbp-ma.local" ]; then
           #xterm -e ggdb -d ../source --args \
  mpirun -np $nprocs \
         ../bin/dynamic bench7 100 1.0 10.0 2 298.0
fi

if [ `hostname` = "indy0" ]; then

           #xterm -e gdb -d ../source --args \
  mpirun -np $nprocs \
         -mca btl tcp,sm,self \
         ../bin/dynamic bench7 100 1.0 10.0 2 298.0
fi
