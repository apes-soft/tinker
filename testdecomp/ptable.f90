!
!
!     ###################################################
!     ##  COPYRIGHT (C)  2012  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ################################################################
!     ##                                                            ##
!     ##  ptable.f90  --  atomic symbols for the chemical elements  ##
!     ##                                                            ##
!     ################################################################
!
!
!     elemnt   atomic symbol for each chemical element
!
!

module ptable

  use sizes, ONLY: maxele

  implicit none

  character (len=3), save, dimension(maxele):: elemnt
 
end module ptable
