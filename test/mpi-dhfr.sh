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

# Command line arguments to dynamic:
#
# keyFileBaseName NumberOfTimesteps TimeStep(fm) TimeBetweenOuputs(ps) Temp(K) Press(Atms)
#

# Possible arguments for mpirun:
#
# -np - number of processes
# -mca btl tcp,sm,self - specify the message transport mechanism (for Indy)
# --mca orte_base_help_aggregate 0  - disable error message aggregation (for Indy)

# Mario's mac.
if [ `hostname` = "mbp-ma.local" ]; then

  mpirun -np $nprocs \
         ../bin/dynamic dhfr 100 1.0 10.0 2 298.0
fi

# Indy (system at EPCC).
if [ `hostname` = "indy0" ]; then


  mpirun -np $nprocs \
         -mca btl tcp,sm,self \
         ../bin/dynamic dhfr 100 1.0 10.0 2 298.0
fi

