c 
c
c     ###################################################
c     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ############################################################
c     ##                                                        ##
c     ##  module atoms  --  number, position and type of atoms  ##
c     ##                                                        ##
c     ############################################################
c
c

      module atoms
      use sizes

      implicit none

      integer:: n ! total number of atoms in the current system

c      integer:: type(maxatm)

      type atomtype
      real(kind=8), dimension(3) :: pos  ! Atom position
      real(kind=8) :: mass               ! Atomic mass
      integer :: tag                     ! Atom label from input coord file
      integer :: type                    ! Atom type number
      integer :: class                   ! Atom class number
      integer :: atomic                  ! Atomic number
      integer :: valence                 ! Valence number for atom
      character(len=24) :: story         ! Descriptive type for atom
      character(len=3) :: name           ! Atom name
      end type atomtype

      save
      type(atomtype), allocatable :: atom(:)

      end
