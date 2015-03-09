      ! code to test the splitting of a do loop based on a costs array
      ! for each iteration. For this code it fixes the number of procs
      ! based on the parameter nprocs. It loops over the ranks and 
      ! with a random integer based costs routine of length "length"
      ! and with a max value set by "mult". Looking to make sure 
      ! that the loop is split in a contiguous non-overlapping manner
      ! with the cost for each block being more or less the same. The
      ! algorithm used is simple - single pass, calculating an average
      ! cost you iterate until your block contains the average cost or
      ! less.

      ! Data to be shared. Simulate data that would be passed to the
      ! routine in a bigger code.
      module params

      integer, parameter:: nprocs=8
      integer :: rank

      end module params

      ! This is a basic test harness to see whether 
      ! simulated processes calculate the start and
      ! ends of a do loop in a self consistent manner.
      program testsplit

      use params

      implicit none

      interface
      subroutine dolimits(lstart, lend, cost)

         use params
            integer, intent(out):: lstart              ! loop start
            integer, intent(out):: lend                ! loop end
            integer, dimension(:), intent(in):: cost   ! cost array
         end subroutine

      end interface

      integer, parameter:: length=23558     ! length of the test array
      integer, parameter:: mult=1000      ! max val for simulated data
      integer, dimension(length):: idata  ! integer data for the cost
      real, dimension(length):: rdata     ! auxiliary array to calc idata
      integer:: i,j

      ! generate some sample data
      call random_seed()
      call random_number(rdata)
      idata = int(mult*rdata)

      ! simulate calculation across different processes
      do rank=0, nprocs-1

         ! calculate the loop limits
         call dolimits(i, j, idata)

      end do

      end program testsplit

      ! subroutine to calculate the limits of a do loop
      ! to spread the values in the cost array evenly
      ! to produce an even cost.
      subroutine dolimits(lstart, lend, cost)

      use params

      implicit none

      integer, intent(out):: lstart              ! loop start
      integer, intent(out):: lend                ! loop end
      integer, dimension(:), intent(in):: cost   ! cost array
      integer:: totcost                          ! total cost
      integer:: avgcost                          ! average cost
      integer:: partcost                         ! partial cost
      integer:: tempcost                         ! temporary var

      totcost = sum(cost)        ! total cost
      avgcost = totcost/nprocs   ! average cost

      lstart   = 1               ! loop start
      partcost = 0               ! partial cost

      ! calculate the lower bound of the loop
      do while(partcost.lt.(rank*avgcost)) 
         partcost = partcost + cost(lstart)
         lstart   = lstart + 1
      end do

      ! store the cost to calc the cost for a given rank
      tempcost = partcost 
 
      ! calculate the upper bound of the loop
      lend = lstart
      if(rank.lt.nprocs-1) then
         do while(partcost.lt.((rank+1)*avgcost))
            partcost = partcost + cost(lend)
            lend     = lend + 1
         end do
         ! over counted by one
         lend = lend - 1
       else
         ! last proc gets whatever remains
         lend     = size(cost)
         partcost = sum(cost)
      end if

      ! print a diagnostic message
      print "(I3,A,I6,A,I6,A,I8,A,I8,A,I8,A,I8,A,I6)",rank,",",
     &      lstart,", ",lend,",",
     &      partcost-tempcost,",",avgcost,",",
     &      sum(cost(lstart:lend)),",",totcost,",",size(cost)


      end subroutine dolimits


