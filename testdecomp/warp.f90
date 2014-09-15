!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ################################################################
!     ##                                                            ##
!     ##  warp.f90  --  parameters for potential surface smoothing  ##
!     ##                                                            ##
!     ################################################################
!
!
!     m2           second moment of the GDA gaussian for each atom
!     deform       value of the smoothing deformation parameter
!     difft        diffusion coefficient for torsional potential
!     diffv        diffusion coefficient for van der Waals potential
!     diffc        diffusion coefficient for charge-charge potential
!     use_smooth   flag to use a potential energy smoothing method
!     use_dem      flag to use diffusion equation method potential
!     use_gda      flag to use gaussian density annealing potential
!     use_tophat   flag to use analytical tophat smoothed potential
!     use_stophat  flag to use shifted tophat smoothed potential
!
!

module warp

  use sizes, ONLY: maxatm

  implicit none

  real (kind=8), dimension(maxatm):: m2
  real (kind=8):: deform
  real (kind=8):: difft,diffv,diffc
  logical:: use_smooth,use_dem,use_gda
  logical:: use_tophat,use_stophat
  save

end module warp
