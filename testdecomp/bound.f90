!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ##############################################################
!     ##                                                          ##
!     ##  bound.f90  --  control of periodic boundary conditions  ##
!     ##                                                          ##
!     ##############################################################
!

module bound
   
   implicit none

   real (kind=8):: polycut  ! cutoff distance for infinite polymer nonbonds
   real (kind=8):: polycut2 ! square of infinite polymer nonbond cutoff
   logical use_bounds       ! flag to use periodic boundary conditions
   logical use_replica      ! flag to use replicates for periodic system
   logical use_polymer      ! flag to mark presence of infinite polymer
   save

end module bound
