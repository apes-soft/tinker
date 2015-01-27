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
c     input   Fortran I/O unit for main input (default=5)
c     iout    Fortran I/O unit for main output (default=6)
c
c
      module iounit
      implicit none

      integer, parameter:: input =  5 ! Input (default=5).
      integer, parameter:: iout  =  6 ! Output (default=6).

      save
      end
