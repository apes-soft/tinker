!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ################################################################
!     ##                                                            ##
!     ##  align.i  --  information for superposition of structures  ##
!     ##                                                            ##
!     ################################################################
!
!
!     wfit    weights assigned to atom pairs during superposition
!     nfit    number of atoms to use in superimposing two structures
!     ifit    atom numbers of pairs of atoms to be superimposed
!
!

module align

  use sizes, ONLY: maxatm

  implicit none

  integer:: nfit
  integer, dimension(2,maxatm):: ifit
  real (kind=8), dimension(maxatm):: wfit

  save

end module align
