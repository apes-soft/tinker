! Compare performance of three separate arrays 
! to a single array. Calculate the maximum 
! separation between two points in a random sample.

program code1

  implicit none

  integer, parameter:: number=310000
  real (kind=8):: x(number), y(number), z(number)
  real (kind=8):: dr, rmax,rmin
  integer:: i,j,seed

  ! set the random seed and initialise the random number gen.
  seed = 5
  call random_seed(seed)
  ! initialize the data.
  call random_number(x)
  call random_number(y)
  call random_number(z)
  ! Initialize max and min distance
  rmax = 0.0
  rmin = 10.0**7
  ! calculate the distance from the origin
  do i=1,number
    do j=i+1,number
       dr = sqrt((x(i)-x(j))*(x(i)-x(j))+&
                 (y(i)-y(j))*(y(i)-y(j))+&
                 (z(i)-z(j))*(z(i)-z(j)))
       if(dr.gt.rmax) rmax=dr
       if(dr.lt.rmin) rmin=dr
    end do
  end do

  print *,"rmax = ",rmax,"rmin = ",rmin
end program code1
  
