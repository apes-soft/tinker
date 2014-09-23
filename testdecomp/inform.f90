!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ###############################################################
!     ##                                                           ##
!     ##  inform.f90  --  control values for I/O and program flow  ##
!     ##                                                           ##
!     ###############################################################


module inform

  implicit none

  integer:: digits  ! decimal places output for energy and coordinates  
  integer:: iprint  ! steps between status printing (0=no printing)     
  integer:: iwrite  ! steps between coordinate dumps (0=no dumps)       
  integer:: isend   ! steps between socket communication (0=no sockets) 
  logical:: silent  ! logical flag to turn off all information printing 
  logical:: verbose ! logical flag to turn on extra information printing
  logical:: debug   ! logical flag to turn on full debug printing       
  logical:: holdup  ! logical flag to wait for carriage return on exit  
  logical:: abort   ! logical flag to stop execution at next chance     
  save

end module inform
