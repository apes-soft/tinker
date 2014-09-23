!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ################################################################
!     ##                                                            ##
!     ##  keys.f90  --  contents of current keyword parameter file  ##
!     ##                                                            ##
!     ################################################################
!


module keys

  use sizes, ONLY: maxkey

  implicit none

  integer:: nkey ! number of nonblank lines in the keyword file
  character (len=120), dimension(maxkey):: keyline ! contents of each individual
                                                   ! keyword file line
  save

end module keys
