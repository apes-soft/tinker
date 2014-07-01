#!/usr/bin/perl
# 
# A short perl script to write out a Fortran module with the
# git hash of the currently compiled version.
#

use strict;
use warnings;

my $version=`git rev-parse --short HEAD`;
my $file   = "gitversion.f";

# Get rid of the new line.
chomp($version);

open(FILE,">",$file) or die("Could not open $file: $!.\n");

print FILE<<EOF;
      !
      ! Module containing the git hash version of the current code.
      !

      module gitversion

       character(9), parameter::version="$version"

      end module gitversion
EOF

close(FILE) or die("Could not close $file: $!.\n");

