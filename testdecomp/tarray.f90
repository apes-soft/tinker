!
!
!     ################################################################
!     ## COPYRIGHT (C) 2013 by Xiao Zhu, Pengyu Ren & Jay W. Ponder ##
!     ##                     All Rights Reserved                    ##
!     ################################################################
!
!     ################################################################
!     ##                                                            ##
!     ##  tarray.f90  --  storage of dipole-dipole matrix elements  ##
!     ##                                                            ##
!     ################################################################
!


module tarray

  implicit none

  integer ntpair  ! number of stored dipole-dipole matrix elements
  integer, pointer :: tindex(:,:) ! index into stored dipole-dipole matrix 
                                  ! values
  real (kind=8), pointer :: tdipdip(:,:) ! stored dipole-dipole matrix element 
                                         ! values
  save

end module tarray
