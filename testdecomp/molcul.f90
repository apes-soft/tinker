!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ##################################################################
!     ##                                                              ##
!     ##  molcul.f90  --  individual molecules within current system  ##
!     ##                                                              ##
!     ##################################################################
!
!
!     molmass   molecular weight for each molecule in the system
!     totmass   total weight of all the molecules in the system
!     nmol      total number of separate molecules in the system
!     kmol      contiguous list of the atoms in each molecule
!     imol      first and last atom of each molecule in the list
!     molcule   number of the molecule to which each atom belongs
!
!

module molcul

  use sizes, ONLY: maxatm

  implicit none

  integer:: nmol
  integer, dimension(maxatm):: kmol
  integer, dimension(2, maxatm):: imol
  integer, dimension(maxatm):: molcule
  real (kind=8), dimension(maxatm)::  molmass
  real (kind=8):: totmass
  save

end module molcul
