#!/bin/sh
#
# Script to remove unwanted files and directories - safer than doing 
# it by hand.
#

rm -f *~ *.bak code? *.csv *.data *.old

# Remove any subdirectories
rm -fR -- */

