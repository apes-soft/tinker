#!/usr/bin/perl
# 
# Perl to process a directory of *.txt file with the output from
# "/usr/bin/time -v" in them and return this output as a csv file.
# The input directory is specified in the outdir directory and 
# this same name is used to create the output csv file.
#
# expected output filename:
#
#  file-N-R.txt
#
# where N can be used for the number of threads or array size and
# R to indicate whether a number of runs has been used.
#
# Author: Mario Antonioletti (mario@epcc.ed.ac.uk)
#

# Make perl warn us if we are doing anything silly.
use strict;
use warnings;

# Process command line arguments.
use Getopt::Std;
$Getopt::Std::STANDARD_HELP_VERSION = 1;
our(%opts);

# Default output directory to collect the files from.
my $outdir = "results2";

# Get the command line arguments
if(! getopt('d:',\%opts)){HELP_MESSAGE();}

if( $opts{'h'} ) { HELP_MESSAGE(); }
if( $opts{'d'} ) { $outdir=$opts{'d'}; }

# Check the directory actually exists.
if(not -d $outdir){
   print "\nThe directory $outdir does not exist. Terminating ...\n\n";
   exit 1;
}

# Store the results in this hash array.
my %results;

# Get a list of the output files with the timings.
my @files = `ls -1 $outdir/*.txt`;

# Name for the first two columns.
my $runlabel    = "0Run";
my $threadlabel = "00N";

# Variable to count the number of runs per thread done.
my $numruns = 0;

####################################################
# Read the data from various files and store in an #
# associative array.                               #
####################################################

# First read the data from the separate files.
# Loop over the files. Open each in turn and take the
# time stuff we want from them.
foreach my $file (@files){

    # Remove the new line character.
    chomp($file);

    # Grab the number of threads and run number
    # from the file name.
    $file  =~ /\w+\-(\d+)\-(\d+)\.txt/;
    my $ln = $1;
    my $lr = $2;

    # Strip a leading 0 if there is one.
    $ln =~ s/^0//;
    $lr =~ s/^0//;

    my $id  = "$ln-$lr";
    $results{$id}{$threadlabel} = $ln;
    $results{$id}{$runlabel}    = $lr;

    # Count the number of runs per thread
    if($results{$id}{$runlabel} > $numruns ){
	$numruns = $results{$id}{$runlabel};
    }

    # Set flag for when we want to start parsing.
    my $start=0;

    open(FILE,"<",$file) or die("Could not open $file: $!\n");

    while(<FILE>){

        if(/Command being timed/) {# Expect this to be the first line of timings
          $start = 1;
        }
        next if not $start;

        # Remove label that makes split harder (i.e. the colons).
        s/\(h:mm:ss or m:ss\)/(seconds)/;

        # Split along the colon
        my($label,$value) = split(":",$_,2);

        # Take off the new line after the value
        chomp($value);

        # Convert the wall clock into seconds.
        if($label =~ /wall clock/){
	    # The split command populates the array from left to right.
	    my @time = split(":",$value);
            my $size = @time;  # Size of the array

            # Convert time to seconds.
 	    if($size == 1){ # Only seconds present
               $value = $time[0];
            }elsif($size == 2){ # We have minutes
               $value = $time[0]*60+$time[1];
	    }elsif($size == 3){ # We have hours
		$value = $time[0]*3600+$time[1]*60+$time[2];
            }
        }    

        # Remove any leading or trailing white spaces
        $label =~ s/^\s+|\s+$//g;
        $value =~ s/^\s+|\s+$//g;

        # Store the results.
        $results{$id}{"$label"}=$value;
	#print "\t\"$id ",$label,"\" --- ",$value,"\n";
    }

    # Close the input file
    close(FILE) or die("Could not close file $file: $!\n");

}

print "Number of runs $numruns.\n";

###############################################
# Now output the data into a csv file format. #
###############################################

# Get the run ids
my @ids = keys(%results);

# Collect the maing changing variables.
my @N;
foreach my $id (@ids){
    my($n,$r) = split("-",$id);
    next if($r != 1);           # Only pick the first run for uniqueness.
    push(@N,$n);
}

# Now get the labels
my @labels = sort keys(%{$results{$ids[0]}});

# Number of items
my $numitems = scalar @labels;

# Output file based on the outdir
my $outfile = $outdir.".csv";

# Open the output file.
open(OUT,">" ,$outfile) or 
                      die("Could not open the output file $outfile: $!.\n");

# Print out the column headings.
print OUT join(",",sort @labels),"\n";

# Want the different run data to be printed in sequence.
# This is a clunky way of doing it but could not come up
# with a better way of doign it.
for(my $r=1; $r <= $numruns; $r++){       # Loop over repeated runs
    
    foreach my $n (sort {$a <=> $b} @N){  # Loop over threads/array sizes
        my $id ="$n-$r";
        print "Printing results for $id\n";
	for(my $i=0; $i < $numitems-1;$i++){
	    print OUT "$results{$id}{$labels[$i]},";
	}
        print OUT "$results{$id}{$labels[$numitems-1]}\n";
    }
}

# Close the output file.
close(OUT) or die("Could not close the output file $outfile: $!.\n");

print "Output has been placed in $outfile.\n";

# System help message
sub HELP_MESSAGE
{
print <<EOF;

The script processOutput.pl concatenates the output produced by the
time command (shell intrinsic and /usr/bin/time) and outputs into a
csv file. It expects the output file to be in a subdirectory and to
have the extension "txt". The output csv file is named the same as the
directory that contains the txt files. You can specify the name of
the directory to be processed using the "-d" flag. Valid options to
the script are:

   -d Directory name - to specify the input directory name.
   -h This help message.

EOF
exit 0;
}

