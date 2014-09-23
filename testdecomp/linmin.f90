!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ###############################################################
!     ##                                                           ##
!     ##  linmin.f90  --  parameters for line search minimization  ##
!     ##                                                           ##
!     ###############################################################
!
!
!     stpmin   minimum step length in current line search direction
!     stpmax   maximum step length in current line search direction
!     cappa    stringency of line search (0=tight < cappa < 1=loose)
!     slpmax   projected gradient above which stepsize is reduced
!     angmax   maximum angle between search direction and -gradient
!     intmax   maximum number of interpolations during line search
!
!

module linmin

  implicit none

  integer:: intmax
  real (kind=8):: stpmin,stpmax
  real (kind=8):: cappa,slpmax
  real (kind=8):: angmax

  save

end module linmin
