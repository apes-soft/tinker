!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ###############################################################
!     ##                                                           ##
!     ##  mutant.i  --  hybrid atoms for free energy perturbation  ##
!     ##                                                           ##
!     ###############################################################
!
!
!     lambda     generic weighting between initial and final states
!     vlambda    state weighting value for electrostatic potentials
!     elambda    state weighting value for van der Waals potentials
!     scexp      scale factor for soft core buffered 14-7 potential
!     scalpha    scale factor for soft core buffered 14-7 potential
!     nmut       number of atoms mutated from initial to final state
!     imut       atomic sites differing in initial and final state
!     type0      atom type of each atom in the initial state system
!     class0     atom class of each atom in the initial state system
!     type1      atom type of each atom in the final state system
!     class1     atom class of each atom in the final state system
!     mut        true if an atom is to be mutated, false otherwise
!
!

module mutant

  use sizes, ONLY: maxatm

  implicit none

  integer:: nmut
  integer, dimension(maxatm):: imut
  integer, dimension(maxatm):: type0
  integer, dimension(maxatm):: class0
  integer, dimension(maxatm):: type1
  integer, dimension(maxatm):: class1
  real (kind=8):: lambda
  real (kind=8):: vlambda
  real (kind=8):: elambda
  real (kind=8):: scexp
  real (kind=8):: scalpha
  logical, dimension(maxatm):: mut

  save

end module mutant
