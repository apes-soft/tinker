#!/bin/bash
#
# Script to run a code using different numbers of threads
# with each run being repeated a number of times. Outpus
# are placed in the variable specified by outdir.
#

# Name the output directory.
outdir="res_cluster_pos"

# time command to use
timecom="/usr/bin/time -v"

# Makesure the output directory exists.
mkdir -p $outdir


# Loop over number of threads.
for numthreads in 1 2 4 6 8 10 12 14 16 24
do

  # Vanity prepend a 0 to numbers less than 10.
  if [ $numthreads -le 9 ]
  then
       num="0"$numthreads
  else
       num=$numthreads
  fi


  # Set the number of threads that are to be used.
  export OMP_NUM_THREADS=$numthreads

  # Repeat the loop a number of times.
  for run in 1 2 
  do  

     # Label for the run.
     if [ $run -le 9 ]
     then
          num2="0"$run
     else
          num2=$run
     fi

     # Output files
     resultfile=$outdir/bench7-$num-$num2".txt"

     # Create an output file specifying how many threads are being used.
     echo "Running on $numthreads thread(s)." > $resultfile

     # Print which run we are doing.
     printf "\n\nRun $run\n"   >> $resultfile
     printf "=====\n\n"        >> $resultfile

     # Print a message specifying what trial we are on.
     echo "Doing run "$run" on "$numthreads" thread(s)."

     # Run the code.

     # This only works on BASH_VERSION 4
     #{ time ../../bin/dynamic bench7 100 1.0 10.0 2 298.0 >> \
     # $resultfile ; } &>> $resultfile

     # This works on BASH_VERSION 3 (and hopefully 4 as well)
     { $timecom ../bin/dynamic 32000_cluster.xyz 100 1.0 100.1 298.0 1.0 \
                -k 32000_cluster.key >> $resultfile ; } >> $resultfile 2>&1
     
  done
done

