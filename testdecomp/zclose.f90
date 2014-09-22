!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ###############################################################
!     ##                                                           ##
!     ##  zclose.f90  --  ring openings and closures for Z-matrix  ##
!     ##                                                           ##
!     ###############################################################
!
!
!     nadd   number of added bonds between Z-matrix atoms
!     iadd   numbers of the atom pairs defining added bonds
!     ndel   number of bonds between Z-matrix bonds to delete
!     idel   numbers of the atom pairs defining deleted bonds
!
!

module zclose

  use sizes, ONLY: maxatm

  integer:: nadd
  integer, dimension(2,maxatm):: iadd
  integer:: ndel
  integer, dimension(2,maxatm):: idel
  save

end module zclose
