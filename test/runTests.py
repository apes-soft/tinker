#!/usr/bin/python
########################################################################
# Test harness to run all the tests supplied in
# tinker. This uses the files supplied as the "correct"
# answer, running tinker and comparing the output using a
# `diff` call to see if they are the same or not.
#
# The canonical name for the "correct" output file will have a .log
# extension which is compared to the current run which produces an
# output file with a .out extensions. These two files are compared
# with a diff which compares floating point numbers to a given 
# tolerance.
# 
# This code was built for the APES project () and is
# distributed under a BSD Licence (see below for the URL).
#
# Copyright (c) 2013, The University of Edinburgh.
#
# The original code for the cmp_files, approxEquality and
# approximateDiff functions, which been fairly
# significantly changed, come from a pyclaw (hyperbolic PDE
# solver) regression test code (regression_test.py)
# available at:
#
# http://tinyurl.com/nvsusqu
#
# Clawpack is distributed under the terms of the 
# Berkeley Software Distribution (BSD) license
#
#  http://www.opensource.org/licenses/bsd-license.php
#
# It is:
#
# Copyright (c) 1994--2010, Randall J. LeVeque and others
# All rights reserved.
#
# The function parseType is based on the ParseStrRE lambda function from 
# the stackoverflow thread in:
#
# http://stackoverflow.com/questions/379906/parse-string-to-float-or-int
#
# and was written by krzym. Stackoverflow code is covered
# by a CC-BY-SA licence.
########################################################################

import unittest     # Unit tests
import subprocess   # Call out to subprocesses
import time         # Timing functions
import os           # Get a number of OS routines


# File to log output to.
logfile = "test.log"

# Tolerance to which floating point numbers are to be compared
floattolerance = 5.0

################################
# RunSystem: run the test script
################################
def RunSystem(name):
    """Run the test script."""
    subprocess.call( "./"+name+".run",stdout=open(name+".out","w"),shell=True)

#########################################################
# CompareFiles: compare a reference output file (with a
# .log extension) with the output generated from a run of
# the shell script. 
#
# There are two possible implementations of this function:
# one relies on the diff unix command utility and the other
# uses built in code that compares floating point values
# to a given tolerance. Rename the one you wish to use
# to CompareFiles and attach a 2 to the other, i.e.
# CompareFiles2 to not use that routine.
#########################################################

# def CompareFiles2(name):
#     """
#      Compare output from program and a canonical .log file
#      using the unix diff utility.
#     """
#     command = "diff -q "+name+".out "+name+".log"
#     fh = open(logfile,"a") # Silence output from diff
#     diffs = subprocess.call(command.split(),stderr=fh,stdout=fh)
#     fh.close()
#     if diffs == 0:
#        return True
#     else:
#        return False

def CompareFiles(name):
    """
     Compare output from program and a canonical .log file.
     Floating point values are compared to within a given
     tolerance.
    """
    return cmp_files(name+".out",name+".log")


###########################################################
# cmp_files: compares two files to see if they are the
# same. Returns True if they are the same, False
# otherwise. 
###########################################################
def cmp_files(file1, file2):

    """
    Compare two files. Report differences to a logfile.
    """

    import filecmp # File comparison
        
    # Are the files the same?
    same = filecmp.cmp(file1, file2)

    # Open file for logging.
    fh = open(logfile,"a")

    # Job done, files are the same, can return.    
    if (same):
        fh.write("%s\nSuccess - no difference in the outputs at all.\n%s\n" %\
                 ("="*46,"="*46))
        fh.close()
        return(True)
       
    tol = float(floattolerance)
    AnyDiff,DiffOut,MaxDiff,MaxLine = approximateDiff(file1,file2,tol)

    # Print to logfile. Comment out if too much information.

    delimiter = "="*15
    if(MaxDiff < tol):
       fh.write("%s\n"%(delimiter*5))
       fh.write("Success:- %s and %s the same to within tolerance (%.2f).\n"\
                % (file1,file2,tol))
       fh.write("Maximum difference = %f on line %d.\n"\
                % (MaxDiff,MaxLine))
       fh.write("%s\n"%(delimiter*5))
    else:
       fh.write("%s Start of %s and %s differences %s\n"\
                % (delimiter,file1,file2,delimiter))
       fh.write("%s and %s : Maximum difference = %f on line %s.\n" \
                % (file1,file2,MaxDiff,MaxLine))
       fh.write('%s' % '\n'.join(map(str, DiffOut)))
       fh.write("\n%s End of %s and %s differences %s\n"\
                % (delimiter,file1,file2,delimiter))
    fh.close()

    # Override differences if MaxDiff is within tolerance
    #if(abs(MaxDff) < tolerance):
    #   AnyDiff = True

    # Return True if the differences are within tolerance.    
    return(not AnyDiff)

##########################################################
# parseType: parse a string and return a typed token.  
###########################################################

import re                          # Regular expression functions
from string import maketrans       # String subroutines (maketrans)

def parseType(x):
    if(x.isalpha()):
       return str(x)
    elif(x.isdigit()):
       return int(x)
    elif(re.match('(?i)^-?(\d+\.?\d*[de]-?\+?\d+|\d+\.\d*|0?\.\d+)$', x)):
       return float(x.translate(maketrans("Dd","Ee")))
    else:
       return(x)

####################################################################  
# approxEquality: Test for approximate equality for two variables.  If
# both tokens are floats then the test is done to within a given
# tolerance. Returns True if the two tokens are the same or within a
# tolerance for floats. 
####################################################################  

def approxEquality(x,y,epsilon):

    """
    Check if two items are equal or not. Floats are treated as special
    as they may be equal within a given tolerance. Returns True if
    elements are equal or floats are equal within epsilon. The two 
    typed variables are also returned.
    """

    # Remove any spurious characters - tokenization is done
    # on spaces so some characters may slip through, e.g. 
    # brackets. 
    m = x.translate(None,")")
    n = y.translate(None,")")

    # Parse the tokens to typed variables.
    a = parseType(m)
    b = parseType(n)

    # Check if the two tokens are the same.
    if(a==b):
       return True

    # If not equal check if floats that are within a given tolerance.
    if(type(a) == float and type(b) == float):
      if(abs(a - b) < epsilon):
         return True
    else:
       # Items are not equal.
       return False


##########################################################
# approximateDiff: calculates the difference between two
# files element by element. Floating point values are 
# treated 
###########################################################
 
def approximateDiff(file1, file2, tol):
    """
    Does an element by element comparison between two files. It
    returns True if there are differences or numerical differences for
    floating point numbers are larger than some prespecified numerical
    tolerance. Returns True for differences, a list of the actual
    differences, the maximum difference and the line number this is
    found at.
    """

    fp1      = open(file1, 'r')   # Input file1
    fp2      = open(file2, 'r')   # Input file2
    lines1   = fp1.readlines()    # Contents of file1
    lines2   = fp2.readlines()    # Contents of file2
    AnyDiff  = False              # Set default (no differences).
    MaxDiff  = 0.0                # Set default max difference
    MaxLine  = 0                  # Line where max difference is
    
    Diff = []                     # Output buffer
  
    #==== Check file lengths first ====
    #--------------------------------------------------------------------
    # A problem here will indicate that something is structurally wrong.
    #--------------------------------------------------------------------
    if not(len(lines1) == len(lines2)):
        Diff.append("Output files are of different length.\n")
        AnyDiff = True

    #==== Check line by line ====
    #----------------------------------------------------------------------
    # This is where numerical differences will be highlighted.  Also, if
    # the files are comparable up to a certain point, this will show where
    # they begin to diverge.
    #----------------------------------------------------------------------
    for i in range(min(len(lines1), len(lines2))):
        split1 = lines1[i].split();
        split2 = lines2[i].split();
  
        if len(split1) == len(split2):
            #-----------------------------------------------------------
            # If lines have the same number of elements, then check for
            # numerical differences.
            #-----------------------------------------------------------
            for j in range(len(split1)):
 
               # Check if we have differences between two elements.
                AreEqual = approxEquality(split1[j],split2[j],tol)
                a = parseType(split1[j].translate(None,")"))
                b = parseType(split2[j].translate(None,")"))
                if(type(a) == float and type(b) == float):
                      diff = abs(a-b)
                      if(diff > MaxDiff):                     
                         MaxDiff = diff
                         MaxLine = i+1

                # For elements that are not the same
                if (not(AreEqual)):
                    AnyDiff = True # Note that we have a difference

                    # Store the MaxDifference and the line it is found in
                    if(type(a) == float and type(b) == float):
                      Diff.append("Line "+str(i+1)+", element "+str(j+1)+\
                      " differs "+file1+": "+str(a)+"  " +file2+": "+\
                      str(b)+" diff="+str(diff)+" (Tol="+str(tol)+")")
                    # Log differences for non-floats
                    else: 
                      if (type(a) != type(b)):
                         # Output types if the types have not been
                         # parsed correctly.
                         Diff.append("Line "+str(i+1)+", element "+str(j+1)+\
                         " differs in "+file1+": "+str(a)+" ("+str(type(a))+\
                         ")  from " +file2+": "+str(b)+" ("+str(type(b))+")")
                      else:
                         Diff.append("Line "+str(i+1)+", element "+str(j+1)+\
                         " differs in "+file1+": "+str(a)+" from " +file2+\
                         ": "+str(b))
        else:
            #-----------------------------------------------------------
            # If lines have a different number of elements, then print
            # their contents.
            #-----------------------------------------------------------
            AntDiff = True
            Diff.append("Line " + str(i+1) + ", number of elements differs:\n")
            Diff.append("  " + file1 + ": " + lines1[i])
            Diff.append("  " + file2 + ": " + lines2[i])

    return AnyDiff, Diff, MaxDiff, MaxLine

############################################################################
# Tinker test cases.                                                       #
#                                                                          #
# Can run individual tests on the command line by using:                   #
#                                                                          #
#                                ./runTest.py Tinker.testAnion             #
#                                                                          #
############################################################################


class Tinker(unittest.TestCase):
    """

    A Python test harness to run TINKER test
    cases as unit tests.

    """

    # Run before every test.
    def setUp(self):
        fh = open(logfile,"a")
        self.startTime = time.time()
        fh.write("Test %s.\n" % (self.id()))
        fh.close()

    # Run after every test.
    def tearDown(self):
        fh = open(logfile,"a")
        t  = time.time() - self.startTime
        fh.write("Time to run test: %.3f seconds.\n" % (t))
        fh.close()

    ###############
    # Begin Tests #
    ###############

    def testAnion(self):
        outStem ="anion"
        RunSystem(outStem)
        self.assertTrue(CompareFiles(outStem))

    def testArgon(self):
        outStem = "argon"
        RunSystem(outStem)
        self.assertTrue(CompareFiles(outStem))

    def testCluster(self):
        outStem = "cluster"
        RunSystem(outStem)
        self.assertTrue(CompareFiles(outStem))

    def testCramblin(self):
        outStem = "crambin"
        RunSystem(outStem)
        self.assertTrue(CompareFiles(outStem))

    def testCyclohex(self):
        outStem = "cyclohex"
        RunSystem(outStem)
        self.assertTrue(CompareFiles(outStem))

    def testDhfr(self):
        outStem = "dhfr"
        RunSystem(outStem)
        self.assertTrue(CompareFiles(outStem))

    def testDialanine(self):
        outStem = "dialanine"
        RunSystem(outStem)
        self.assertTrue(CompareFiles(outStem))

    def testEnkephalin(self):
        outStem = "enkephalin"
        RunSystem(outStem)
        self.assertTrue(CompareFiles(outStem))

    def testEthanol(self):
        outStem = "ethanol"
        RunSystem(outStem)
        self.assertTrue(CompareFiles(outStem))

    def testFormaide(self):
        outStem = "formamide"
        RunSystem(outStem)
        self.assertTrue(CompareFiles(outStem))

    # This one takes a long time.
    def testGpcr(self):
        outStem = "gpcr"
        RunSystem(outStem)
        self.assertTrue(CompareFiles(outStem))

    def testHelix(self):
        outStem = "helix"
        RunSystem(outStem)
        self.assertTrue(CompareFiles(outStem))

    ##########################################
    # Test in the 0README but not in the files.
    #
    # def testIce(self):
    #     outStem = "ice"
    #     RunSystem(outStem)
    #     self.assertTrue(CompareFiles(outStem))

    def testIfabp(self):
        outStem = "ifabp"
        RunSystem(outStem)
        self.assertTrue(CompareFiles(outStem))

    def testSalt(self):
        outStem = "salt"
        RunSystem(outStem)
        self.assertTrue(CompareFiles(outStem))

    def testTetraala(self):
        outStem = "tetraala"
        RunSystem(outStem)
        self.assertTrue(CompareFiles(outStem))

    def testWater(self):
        skipTest("This is giving a segmentation fault.")
        outStem = "water"
        RunSystem(outStem)
        self.assertTrue(CompareFiles(outStem))


# This bit of code allows the tests to be run from the
# command line. The main function uses the
# unittest.TestLoader class to automatically find and load
# test cases within the current module.

if __name__ == '__main__':

     # Check if logfile already exists
     # If it does rename to be appended by the current time.
     if(os.path.exists(logfile)):
        bkupname = logfile+"_"+time.strftime("%H-%M-%S")
        print "Warning: logfile already exists moving existing logfile to \""\
              +bkupname+"\"."
        os.rename(logfile,bkupname)
 
    # Prompt where some of the output has been put
     print "Detailed output being logged to \""+logfile+"\".\n"
     
     # Run the unit tests  
     unittest.main()

 


