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
if(! getopt("hd:c:",\%opts)){HELP_MESSAGE();}

# Declare and set varyables to be used.
my($outdir,$code);

# Print out a help message.
if( defined $opts{'h'} ) { HELP_MESSAGE(); }

# Output directory.
if( defined $opts{'d'} ) {$outdir=$opts{'d'};}else{$outdir="out1";}
# Code basename to use to for experiment.
if( defined $opts{'c'} ) {$code=$opts{'c'};}else{$code="code1";}

print "Using output directory $outdir and codebase $code.\n";

my $Nrepeats = 1;          # Number of times to run each piece of code.
my $Nruns = 16;            # Number of runs
my $N=10000;               # Starting number of particles.
my $time="/usr/bin/time";  # Timing command to use.
my $compiler="ifort";      # Compiler name with any optimisation flags.

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
   system("$compiler -o $code $code.f90");
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

system("../../bench/processOutput.pl -d $outdir");

# System help message
sub HELP_MESSAGE
{
print <<EOF;

The script run.pl runs a number of experiments for a given piece
of code. Valid arguments are:

  -d Specifies the output directory.
  -c Specifies the code basename to use.

EOF
exit 0;
}
