#!/usr/bin/perl
# 
# Run a number of jobs resizing the initial size of the
# array and then compiling. Output goes to a set of files
# that are then hoovered up by another perl script. 
#

# Let perl tell us if we are doing anything silly.
use strict;
use warnings;

# Process command line arguments.
use Getopt::Std;
my %opts; # store the options and values used.

# Get the command line arguments
if(! getopt("ho:c:",\%opts)){HELP_MESSAGE();}

# Declare and set varyables to be used.
my($outdir,$code);

# Print out a help message.
if( defined $opts{'h'} ) { HELP_MESSAGE(); }

# Output directory.
if( defined $opts{'o'} ) {$outdir=$opts{'o'};}else{$outdir="out1";}
# Code basename to use to for experiment.
if( defined $opts{'c'} ) {$code=$opts{'c'};}else{$code="code1";}

print "Using output directory $outdir and codebase $code.\n";

my $Nrepeats = 1;          # Number of times to run each piece of code.
my $Nruns = 20;            # Number of runs
my $N=50000;               # Starting number of particles.
my $Nstep=50000;           # Increase in N between runs.
my $time="/usr/bin/time";  # Timing command to use.

# Compiler name with any optimisation flags.
#my $compiler="ifort -O3 -no-ipo -no-prec-div";
#my $compiler="ifort -fast";
#my $compiler="ifort";
# my $compiler="gfortran";
 my $compiler="gfortran -O3";


# Create the output directory.
system("mkdir -p $outdir");

# Loop over the different number of array sizes.
for(my $num=1;$num <= $Nruns; $num++){

   # Replace the current number of particles in the code.
   system("perl -pi.bak -e \"s/number=\\d+/number=$N/\" $code.f90");

   # Recompile the code.
   system("$compiler -o $code $code.f90");

   # Number of repeats for each array size.
   for(my $run=1; $run <= $Nrepeats; $run++){

      print "Running $code with N=$N ($num/$Nruns), run = $run/$Nrepeats to $outdir.\n";

      # filename to put the output data.
      my $outfile="$outdir/part1-$N-$run.txt";

      # Run the code.
      system("$time -v -o $outfile ./$code");
   }

   # Update the array size.
   $N=$N+$Nstep;

}

# Run the post processing perl script that will generate 
# a CSV file.

system("../../bench/processOutput.pl -d $outdir");

# System help message
sub HELP_MESSAGE
{
print <<EOF;

The script run.pl runs a number of experiments for a given piece
of code. Valid arguments are:

  -o Specifies the output directory.
  -c Specifies the code basename to use.

EOF
exit 0;
}
