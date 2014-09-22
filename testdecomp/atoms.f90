!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     #################################################################
!     ##                                                             ##
!     ##  atoms.f90  --  number, position and type of current atoms  ##
!     ##                                                             ##
!     #################################################################
!

module atoms
  use sizes, ONLY: maxatm, maxbonds

  implicit none

  integer:: n                                   ! total number of atoms
  integer, dimension(maxatm)::type              ! atom type number for each atom
  real (kind=8), dimension(maxatm), target:: x  ! x-coordinate for each atom
  real (kind=8), dimension(maxatm), target:: y  ! y-coordinate for each atom 
  real (kind=8), dimension(maxatm), target:: z  ! z-coordinate for each atom

  save ! Make sure values are saved between different calls.

  ! There are too many different data types which will make
  ! distributing more difficult and require more communication
  ! calls. Need to either collapse the number of disparate 
  ! arrays by using x,y,z -> pos(3,maxatm) and other such refactorings
  ! or we could try to use a Fortran 90 derived type as shown below.
  ! Not sure what the impact of these choices are (in terms of cache
  ! misses and the like).

  ! Need to define an atom pointer derived type so we can create an
  ! array of pointers and not a pointer to an array of atoms.  Maybe
  ! should change the static array to a linked list. May be more
  ! conservative in terms of memory usage but will require more
  ! processing to maintain.


  ! NB instead of an atom pointing to another atom it could
  ! point to a bond structure that has information about 
  ! the two atoms involved in the bond.

  ! Used to construct an array of pointers.
  !   type atomptr
  !        type(atom), pointer:: p
  !   end type atomptr
  !
  ! Got rid of this structure as I was not sure how to create
  ! an MPI derived structure to accommodate the pointers so
  ! have gone back to indirect addressing.

  type atomtype
    real (kind=8),dimension(3):: pos      ! Atom position
    real (kind=8):: mass                  ! Atomic weight
    integer:: tag                         ! Atom label
    integer:: type                        ! Atom type
    integer:: class                       ! Atom class number
    integer:: atomic                      ! Atomic number
    integer:: valence                     ! Valence number
    character (len=24):: story            ! Descriptive type
    character (len=3):: name              ! Atom name
    integer, dimension(maxbonds) :: i12   ! Point to bound atoms
    !type (atomptr), dimension(maxbonds) :: i12   ! Point to bound atoms.
  end type atomtype

end module atoms
