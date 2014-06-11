#!/usr/bin/perl
# 
# Run a number of jobs resizing the initial size of the
# array and then compiling. Output goes to a set of files
# that are then hoovered up by another perl script. 
#

use strict;
use warnings;


my $Nrepeats = 1;         # Number of times to run each piece of code.
my $Nruns = 16;           # Number of runs
my $N=10000;              # Starting number of particles.
my $outdir="out6";        # Output directory.
my $code="code4";         # Code basename to use to for experiment.
my $time="/usr/bin/time"; # Timing command to use.

# Create the output directory.
system("mkdir -p $outdir");

# Loop over the number of runs.
for(my $num=1;$num <= $Nruns; $num++){
   # Replace the current number of particles in the code.
   system("perl -pi.bak -e \"s/number=\\d+/number=$N/\" $code.f90");
   # Record the old N for labelling purposes.
   my $oldN=$N;
   # Update the number of particles.
   $N=$N+20000;
   # Recompile the code.
   system("gfortran -o $code $code.f90");
   for(my $run=1; $run <= $Nrepeats; $run++){

      print "Running N=$N ($num/$Nruns), run = $run/$Nrepeats\n";

      # filename to put the output data.
      my $outfile="$outdir/part1-$oldN-$run.txt";

      # Run the code.
      system("$time -v -o $outfile ./$code");
   }

}

# Run the post processing perl script that will generate 
# a CSV file.

#system("./processOutput.pl $outdir");
