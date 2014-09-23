!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ##############################################################
!     ##                                                          ##
!     ##  scales.i  --  parameter scale factors for optimization  ##
!     ##                                                          ##
!     ##############################################################
!
!
!     scale      multiplicative factor for each optimization parameter
!     set_scale  logical flag to show if scale factors have been set
!
!

module scales

  use sizes, ONLY: maxvar

  implicit none

  real (kind=8), dimension(maxvar):: scale
  logical:: set_scale
  save

end module scales


