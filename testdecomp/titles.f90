!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ##############################################################
!     ##                                                          ##
!     ##  titles.f90  --  title for the current molecular system  ##
!     ##                                                          ##
!     ##############################################################
!

module titles

  implicit none

  integer, save:: ltitle ! length in characters of the nonblank title string
  character(len=120),save:: title ! title used to describe the current structure

end module titles
