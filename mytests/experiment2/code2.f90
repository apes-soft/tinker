! This is what was code2 in experiment 1. Will compare
! performance to see how introducing a derived data
! type impacts performance. 

program code2

  implicit none

  integer, parameter:: number=390000
  real (kind=8):: pos(3,number)
  real (kind=8):: dr, rmax, rmin
  integer:: i,j,seed

  ! set the random seed and initialise the random number gen.
  seed = 5
  call random_seed(seed)
  ! initialize the data.
  call random_number(pos)
  ! Initialize max and min distance
  rmax = 0.0
  rmin = 10.0**7
  ! calculate the distance from the origin
  do i=1,number
    do j=i+1,number
       dr = sqrt((pos(1,i)-pos(1,j))*(pos(1,i)-pos(1,j))+&
                 (pos(2,i)-pos(2,j))*(pos(2,i)-pos(2,j))+&
                 (pos(3,i)-pos(3,j))*(pos(3,i)-pos(3,j)))
       if(dr.gt.rmax) rmax=dr
       if(dr.lt.rmin) rmin=dr
    end do
  end do

  print *,"rmax = ",rmax,"rmin = ",rmin

end program code2
  
