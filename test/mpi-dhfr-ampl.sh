# Command line arguments to dynamic:
#
# keyFileBaseName NumberOfTimesteps TimeStep(fm) TimeBetweenOuputs(ps) Temp(K) 
# Press(Atms)
#

# Need to take care of slurm
export SLURM_JOBID=

# Number of processes to be used
nprocs=16

# File to output to
outfile="out-ampl.txt"

# Add some empty lines
echo \n\n >> $outfile

(time mpirun -np $nprocs \
            amplxe-cl -r S2_$nprocs -collect hotspots -- \
            ../bin/dynamic dhfr 100 1.0 1.0 2 300.0) 2>&1 | tee -a $outfile
