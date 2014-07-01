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
      save
      end
