!
!
!     ###################################################
!     ##  COPYRIGHT (C)  2003  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ##############################################################
!     ##                                                          ##
!     ##  params.f90  --  contents of force field parameter file  ##
!     ##                                                          ##
!     ##############################################################
!
!
!     nprm      number of nonblank lines in the parameter file
!     prmline   contents of each individual parameter file line
!
!
 
module params

  use sizes, ONLY: maxprm

  implicit none

  integer:: nprm
  character (len=120), dimension(maxprm):: prmline
  save

end module params
