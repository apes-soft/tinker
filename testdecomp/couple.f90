!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ##############################################################
!     ##                                                          ##
!     ##  couple.f90  --  near-neighbor atom connectivity lists   ##
!     ##                                                          ##
!     ##############################################################
!

module couple

       use sizes, ONLY: maxbonds, maxatm

       implicit none

 integer, parameter:: maxn13=3*maxbonds   ! Maximum number of atoms
                                          ! 1-3 connected to an atom.
 integer, parameter:: maxn14=9*maxbonds   ! Maximum number of ato!ms
                                          ! 1-4 connected to an atom.
 integer, parameter:: maxn15=27*maxbonds  ! Maximum number of atoms
                                          ! 1-5 connected to an atom.
 integer, dimension(maxatm):: n12         ! Number of atoms directly
                                          ! bonded to each atom.
 integer, dimension(maxbonds,maxatm)::i12 ! Atom numbers of atoms 1-2
                                          ! connected to each atom.
 integer, dimension(maxatm):: n13         ! Number of atoms in a 1-3
                                          ! relation to each atom.
 integer, dimension(maxn13,maxatm)::i13   ! Atom numbers of atoms 1-3
                                          ! connected to each atom.
 integer, dimension(maxatm):: n14         ! Number of atoms in a 1-4
                                          ! relation to each atom.
 integer, dimension(maxn14,maxatm):: i14  ! Atom numbers of atoms 1-4
                                          ! connected to each atom.
 integer, dimension(maxatm):: n15         ! Aumber of atoms in a 1-5
                                          ! relation to each atom.
 integer, dimension(maxn15,maxatm):: i15  ! Atom numbers of atoms 1-5
                                          ! connected to each atom.

end module couple
