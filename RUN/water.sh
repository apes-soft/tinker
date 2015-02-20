#!/bin/bash --login
#
# Parallel script produced by bolt
#        Resource: ARCHER (Cray XC30 (24-core per node))
#    Batch system: PBSPro_select
#
# bolt is written by EPCC (http://www.epcc.ed.ac.uk)
#
#PBS -l select=1
#PBS -N water_timing
#PBS -o water_t.out
#PBS -e water_t.err
#PBS -A e282
#PBS -l walltime=00:05:0


# Switch to current working directory
cd $PBS_O_WORKDIR

export KMP_AFFINITY=disabled

# Run the parallel program
#cp /home/e282/e282/filinger/tinker/source/dynamic.x .
#cp source/dynamic.x .


#module add perftools

export OMP_NUM_THREADS=1
#cd crayPAT_prof/OMP1

#time aprun -n 1 ./dynamic_3001.x 7000_cluster 100 1.0 10.0 2 298.0
#gprof dynamic_3001.x > prof_3001_O3_water.dat

echo "vdW"

time aprun -n 1 -d 1 ./dynamic_vdw.x water 1 1.0 10.0 2 298.0 > vdw_water_t1.log
time aprun -n 1 -d 1 ./dynamic_vdw.x water 1 1.0 10.0 2 298.0 > vdw_water_t2.log
time aprun -n 1 -d 1 ./dynamic_vdw.x water 1 1.0 10.0 2 298.0 > vdw_water_t3.log

echo "induced"

time aprun -n 1 -d 1 ./dynamic_ind.x water 1 1.0 10.0 2 298.0 > ind_water_t1.log
time aprun -n 1 -d 1 ./dynamic_ind.x water 1 1.0 10.0 2 298.0 > ind_water_t2.log
time aprun -n 1 -d 1 ./dynamic_ind.x water 1 1.0 10.0 2 298.0 > ind_water_t3.log

echo "perm "

time aprun -n 1 -d 1 ./dynamic_ereal.x water 1 1.0 10.0 2 298.0 > ereal_water_t1.log
time aprun -n 1 -d 1 ./dynamic_ereal.x water 1 1.0 10.0 2 298.0 > ereal_water_t2.log
time aprun -n 1 -d 1 ./dynamic_ereal.x water 1 1.0 10.0 2 298.0 > ereal_water_t3.log