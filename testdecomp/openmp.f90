!
!
!     ###################################################
!     ##  COPYRIGHT (C)  2010  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ################################################################
!     ##                                                            ##
!     ##  openmp.f90  --  system parameters for OpenMP computation  ##
!     ##                                                            ##
!     ################################################################
!
!
!     nproc     number of processors available to OpenMP
!     nthread   number of threads to be used with OpenMP 
!

module openmp

  implicit none

  integer:: nproc
  integer:: nthread
  save

end module openmp
