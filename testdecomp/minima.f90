!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ############################################################
!     ##                                                        ##
!     ##  minima.f90  --  general parameters for minimizations  ##
!     ##                                                        ##
!     ############################################################
!
!
!     fctmin    value below which function is deemed optimized
!     hguess    initial value for the H-matrix diagonal elements
!     maxiter   maximum number of iterations during optimization
!     nextiter  iteration number to use for the first iteration
!
!

module minima

  implicit none

  integer:: maxiter,nextiter
  real (kind=8):: fctmin,hguess
  save

end module minima

