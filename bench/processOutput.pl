#!/usr/bin/perl
# 
# Perl to process a directory of *.txt file with the output from
# "/usr/bin/time -v" in them and return this output as a csv file.
# The input directory is specified in the outdir directory and 
# this same name is used to create the output csv file.
#
# Author: Mario Antonioletti (mario@epcc.ed.ac.uk)
#

# Make perl warn us if we are doing anything silly.
use strict;
use warnings;

# Output directory to collect the files from.
my $outdir = "results2";

# Store the results in this hash array.
my %results;

# Get a list of the output files with the timings.
my @files = `ls -1 $outdir/*.txt`;

# Some keynames
my $runlabel    = "0Run";
my $threadlabel = "00Thread(s)";

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
    $file  =~ /\w+\-((\d+)\-(\d+))\.txt/;
    my $id                      = $1;
    $results{$id}{$threadlabel} = $2;
    $results{$id}{$runlabel}    = $3;

    # Strip a leading 0 if there is one.
    $results{$id}{$threadlabel} =~ s/^0//;
    $results{$id}{$runlabel}    =~ s/^0//;

    # Set flag for when we want to start parsing.
    my $start=0;

    open(FILE,"<",$file) or die("Could not open $file: $!\n");
    while(<FILE>){

        if(/Command being timed/) {
          $start = 1;
        }
        next if not $start;
        next if(/nn$/);     # Skip temporary glitch in output

        # Remove label that makes split harder.
        s/\(h:mm:ss or m:ss\)/(seconds)/;

        # Split along the colon
        my($label,$value) = split(":",$_,2);

        # Take off the new line after the value
        chomp($value);

        # Convert the wall clock into seconds.
        if($label =~ /wall clock/){
	    # split populates the array from left to right
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
	#print "\t",$label," --- ",$value,"\n";
    }

    # Close the input file
    close(FILE) or die("Could not close file $file: $!\n");

}

###############################################
# Now output the data into a csv file format. #
###############################################

# Get the run ids
my @ids = keys(%results);

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

# Loop round the run ids
foreach my $id (sort @ids){

    for(my $i=0; $i < $numitems-1;$i++){
        print OUT "$results{$id}{$labels[$i]},";
    }
    print OUT "$results{$id}{$labels[$numitems-1]}\n";
}

# Close the output file.
close(OUT) or die("Could not close the output file $outfile: $!.\n");

print "Output has been placed in $outfile.\n";

