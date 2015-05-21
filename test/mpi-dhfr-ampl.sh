# Command line arguments to dynamic:
#
# keyFileBaseName NumberOfTimesteps TimeStep(fm) TimeBetweenOuputs(ps) Temp(K) 
# Press(Atms)
#

if [ "$#" -gt 1 ] 
then 

    # Print usage information
    echo
    echo Usage: $0 NumProcs 
    echo 
    echo If only one number is specified that is used for the
    echo number of processes, if no numbers are specified the
    echo value defaults to 1.
    echo

    # ... and exit
    exit

elif [ "$#" -eq 1 ]
then

   nprocs=$1

else

   nprocs=1

fi

# Need to take care of slurm
export SLURM_JOBID=

# File to output to
outfile="out-ampl.txt"

# Add some empty lines
printf "\n\nUsing $nprocs processes\n\n" | tee -a $outfile


(time mpirun -np $nprocs \
            amplxe-cl -r S2_$nprocs -collect hotspots -- \
            ../bin/dynamic dhfr 100 1.0 1.0 2 300.0) 2>&1 | tee -a $outfile
