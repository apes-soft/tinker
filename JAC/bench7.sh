#!/bin/bash --login
#
# Parallel script produced by bolt
#        Resource: ARCHER (Cray XC30 (24-core per node))
#    Batch system: PBSPro_select
#
# bolt is written by EPCC (http://www.epcc.ed.ac.uk)
#
#PBS -l select=1
#PBS -N bench7_prof
#PBS -o prof_serial.out
#PBS -e prof_serial.err
#PBS -A e282
#PBS -l walltime=00:30:0


# Switch to current working directory
cd $PBS_O_WORKDIR

export KMP_AFFINITY=disabled

# Run the parallel program
#cp /home/e282/e282/filinger/tinker/source/dynamic.x .
#cp source/dynamic.x .


#module add perftools

#export OMP_NUM_THREADS=1
#cd crayPAT_prof/OMP1

time aprun -n 1 ./dynamic_3001.x  bench7 100 1.0 10.0 2 298.0
gprof dynamic_3001.x > prof_3001_O3.dat
#time aprun -n 1 -d 1 ./dynamic.x+pat ../../bench7 100 1.0 10.0 2 298.0
#time aprun -n 1 -d 1 ./dynamic.x+apa ../../bench7 100 1.0 10.0 2 298.0


