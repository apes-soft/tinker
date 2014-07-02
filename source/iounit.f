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
      integer, parameter:: input =  5 ! Fortran I/O unit for main input (default=5)
      integer, parameter:: iout  =  6 ! Fortran I/O unit for main output (default=6)
      integer, parameter:: ixyz  = 10 ! Fortran I/O unit for xyz files.
      integer, parameter:: ikey  = 11 ! Fortran I/O unit for key files.
      integer, parameter:: iprm  = 12 ! Fortran I/O unit for parameter files.
      integer, parameter:: iend  = 13 ! Fortran I/O unit to test for termination of run.
      integer, parameter:: ivel  = 14 ! Fortran I/O unit for velocity vector components.
      integer, parameter:: idyn  = 14 ! Fortran I/O unit for dynamics file.
      save
      end
