! Compare performance of three separate arrays 
! to a single array. Calculate the maximum 
! separation between two points in a random sample.

program code4

  implicit none

  integer, parameter:: number=310000
  real (kind=8):: pos(3*number)
  real (kind=8):: dr, rmax,rmin
  integer:: i,j,seed

  ! set the random seed and initialise the random number gen.
  seed = 5
  call random_seed(seed)
  ! initialize the data.
  ! Assume layout is x1,x2,..,y1,y2,...,z1,z2,...
  call random_number(pos)

  ! Initialize max and min distance
  rmax = 0.0
  rmin = 10.0**7
  ! calculate the distance from the origin
  do i=1,number
    do j=i,number
       dr = sqrt((pos(i)-pos(j))*(pos(i)-pos(j))+                             &
                 (pos(i+number)-pos(j+number))*(pos(i+number)-pos(j+number))+ &
                 (pos(i+2*number)-pos(j+2*number))*                           &
                 (pos(i+2*number)-pos(j+2*number)))
       if(dr.gt.rmax) rmax=dr
       if(dr.lt.rmin) rmin=dr
    end do
  end do

  print *,"rmax = ",rmax,"rmin = ",rmin
end program code4
  
