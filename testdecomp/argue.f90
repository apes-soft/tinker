!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1995  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ################################################################
!     ##                                                            ##
!     ##  argue.f90  --  command line arguments at program startup  ##
!     ##                                                            ##
!     ################################################################
!
!
!     maxarg    maximum number of command line arguments
!
!     narg      number of command line arguments to the program
!     listarg   flag to mark available command line arguments
!     arg       strings containing the command line arguments
!
!

module argue

  implicit none

  integer, parameter:: maxarg=20
  integer:: narg
  logical, dimension(0:maxarg):: listarg
  character (len=120), dimension(0:maxarg):: arg
  save

end module argue


