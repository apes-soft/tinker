#!/bin/sh
#
# Script to remove unwanted files and directories - safer than doing 
# it by hand.
#

rm -f *~ *.bak code? *.csv *.data

# Remove any subdirectories
rm -R -- */

