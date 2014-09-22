!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1997  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     #############################################################
!     ##                                                         ##
!     ##  rigid.f90  --  rigid body coordinates for atom groups  ##
!     ##                                                         ##
!     #############################################################
!
!
!     xrb         rigid body reference x-coordinate for each atom
!     yrb         rigid body reference y-coordinate for each atom
!     zrb         rigid body reference z-coordinate for each atom
!     rbc         current rigid body coordinates for each group
!     use_rigid   flag to mark use of rigid body coordinate system
!
!

module rigid

  use sizes, ONLY: maxatm

  implicit none

  real (kind=8), dimension(maxatm):: xrb
  real (kind=8), dimension(maxatm):: yrb
  real (kind=8), dimension(maxatm):: zrb
  real (kind=8), dimension(6,maxatm):: rbc
  logical:: use_rigid
  save

end module rigid
