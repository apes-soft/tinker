! Compare performance of three separate arrays 
! to a single array. Calculate the maximum 
! separation between two points in a random sample.

program code3

  implicit none

  integer, parameter:: number=100000
  real (kind=8):: pos(3*number)
  real (kind=8):: dr, rmax,rmin
  integer:: i,j,seed

  ! set the random seed and initialise the random number gen.
  seed = 5
  call random_seed(seed)

  ! initialize the data.
  ! Assume layout is x1,y1,z1,x2,y2,z2,...
  call random_number(pos)

  ! Initialize max and min distance
  rmax = 0.0
  rmin = 10.0**7

  ! calculate the distance from the origin
  do i=1,3*number,3
    do j=i+3,3*number,3
       dr = sqrt((pos(i)-pos(j))*(pos(i)-pos(j))+         &
                 (pos(i+1)-pos(j+1))*(pos(i+1)-pos(j+1))+ &
                 (pos(i+2)-pos(j+2))*(pos(i+2)-pos(j+2)))
       if(dr.gt.rmax) rmax=dr
       if(dr.lt.rmin) rmin=dr
    end do
  end do

  print *,"rmax = ",rmax,"rmin = ",rmin
end program code3
  
