c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ############################################################
c     ##                                                        ##
c     ##  module iounit  --  Fortran input/output unit numbers  ##
c     ##                                                        ##
c     ############################################################
c
c
      module iounit
      implicit none
      ! Predefined unit numbers for input/output.
      integer, parameter:: input =  5 ! Input (default=5).
      integer, parameter:: iout  =  6 ! Output (default=6).
      integer, parameter:: ixyz  = 10 ! xyz files.
      integer, parameter:: ikey  = 11 ! Key files.
      integer, parameter:: iprm  = 12 ! Parameter files.
      integer, parameter:: iend  = 13 ! Test termination of run.
      integer, parameter:: ivel  = 14 ! Vector components.
      integer, parameter:: idyn  = 15 ! Dynamics file.
      integer, parameter:: ifrc  = 16 ! Force vector components.
      integer, parameter:: iind  = 17 ! Induced dipole moment at 
                                      ! each site.
      save
      end
