c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1995  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ############################################################
c     ##                                                        ##
c     ##  module argue  --  command line arguments at run time  ##
c     ##                                                        ##
c     ############################################################
c
c
c     maxarg    maximum number of command line arguments
c
c     narg      number of command line arguments given to the program
c     listarg   flag to mark available command line arguments
c     arg       strings containing the command line arguments
c
c
      module argue

      implicit none

      integer, parameter:: maxarg=20
      integer:: narg
      logical, dimension(0:maxarg):: listarg
      character (LEN=120), dimension(0:maxarg):: arg

      save
      end
