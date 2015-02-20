#!/bin/bash --login
#
# Parallel script produced by bolt
#        Resource: ARCHER (Cray XC30 (24-core per node))
#    Batch system: PBSPro_select
#
# bolt is written by EPCC (http://www.epcc.ed.ac.uk)
#
#PBS -l select=1
#PBS -N jac_timing
#PBS -o jac_vdw_cray.out
#PBS -e jac_vdw_cray.err
#PBS -A e282
#PBS -l walltime=00:02:0


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

time aprun -n 1 -d 1 ./dyn_vdw_cray.x bench7 1 1.0 10.0 2 298.0 > cray_vdw_t1.log
time aprun -n 1 -d 1 ./dyn_vdw_cray.x bench7 1 1.0 10.0 2 298.0 > cray_vdw_t2.log
time aprun -n 1 -d 1 ./dyn_vdw_cray.x bench7 1 1.0 10.0 2 298.0 > cray_vdw_t3.log

#echo "induced"

#time aprun -n 1 -d 1 ./dynamic_ind.x bench7 1 1.0 10.0 2 298.0 > ind_t1.log
#time aprun -n 1 -d 1 ./dynamic_ind.x bench7 1 1.0 10.0 2 298.0 > ind_t2.log
#time aprun -n 1 -d 1 ./dynamic_ind.x bench7 1 1.0 10.0 2 298.0 > ind_t3.log

#echo "perm "

#time aprun -n 1 -d 1 ./dynamic_ereal.x bench7 1 1.0 10.0 2 298.0 > ereal_t1.log
#time aprun -n 1 -d 1 ./dynamic_ereal.x bench7 1 1.0 10.0 2 298.0 > ereal_t2.log
#time aprun -n 1 -d 1 ./dynamic_ereal.x bench7 1 1.0 10.0 2 298.0 > ereal_t3.log

#mv *log COST/