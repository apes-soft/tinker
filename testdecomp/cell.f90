!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ################################################################
!     ##                                                            ##
!     ##  cell.f90  --  periodic boundaries using replicated cells  ##
!     ##                                                            ##
!     ################################################################
!


module cell

  use sizes, ONLY: maxcell

  implicit none

  integer:: ncell      ! total number of cell replicates for periodic boundaries
  integer, dimension(3,maxcell):: icell  ! offset along axes for each replicate periodic cell
  real (kind=8):: xcell  ! length of the a-axis of the complete replicated cell
  real (kind=8):: ycell  ! length of the b-axis of the complete replicated cell
  real (kind=8):: zcell  ! length of the c-axis of the complete replicated cell
  real (kind=8):: xcell2 ! half the length of the a-axis of the replicated cell
  real (kind=8):: ycell2 ! half the length of the b-axis of the replicated cell
  real (kind=8):: zcell2 ! half the length of the c-axis of the replicated cell

end module cell
