! As with experiment but now trying to see how using 
! a derived type to store the particle data impacts
! on performance.
!

program code2b

  implicit none

  integer, parameter:: number=250000
  integer, parameter:: maxbonds=8
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
  end type atomtype
  type(atomtype):: a(number)
  real (kind=8):: dr, rmax, rmin
  integer:: i,j,seed

  ! set the random seed and initialise the random number gen.
  seed = 5
  call random_seed(seed)
  ! initialize the data.
  call random_number(a(:)%pos(3))
  ! Initialize max and min distance
  rmax = 0.0
  rmin = 10.0**7
  ! calculate the distance from the origin
  do i=1,number
    do j=i+1,number
       dr = sqrt((a(i)%pos(1)-a(j)%pos(1))*(a(i)%pos(1)-a(j)%pos(1))+&
                 (a(i)%pos(2)-a(j)%pos(2))*(a(i)%pos(2)-a(j)%pos(2))+&
                 (a(i)%pos(3)-a(j)%pos(3))*(a(i)%pos(3)-a(j)%pos(3)))
       if(dr.gt.rmax) rmax=dr
       if(dr.lt.rmin) rmin=dr
    end do
  end do

  print *,"rmax = ",rmax,"rmin = ",rmin

end program code2b
  
