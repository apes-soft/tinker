!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ###############################################################
!     ##                                                           ##
!     ##  iounit.f90  --  Fortran input/output (I/O) unit numbers  ##
!     ##                                                           ##
!     ###############################################################


module iounit

   implicit none

   ! default unit numbers for input and output
   integer, parameter:: iout=6  ! Fortran I/O unit for main output
   integer, parameter:: input=5 ! Fortran I/O unit for main input 
   save

end module iounit
