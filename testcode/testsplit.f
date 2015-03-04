      ! Data to be shared.
      module params

      integer, parameter:: nprocs=32
      integer :: rank

      end module params

      ! Begin the main program
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

      integer, parameter:: length=32000     ! length of the test array
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

      ! total cost
      totcost = sum(cost)

      ! average cost (integer based)
      avgcost = totcost/nprocs

      ! initialise the loop start and partial cost
      lstart   = 1
      partcost = 0

      ! calculate the lower bound for the loop
      do while(partcost.lt.rank*avgcost) 
         partcost = partcost + cost(lstart)
         lstart   = lstart + 1
      end do

      ! store the cost to calc the cost for a given rank
      tempcost = partcost 
 
      ! calculate the upper bound of the loop
      lend = lstart
      do while(partcost.lt.((rank+1)*avgcost).and.lend.le.size(cost))
         partcost = partcost + cost(lend)
         lend     = lend + 1
      end do
      ! over counted by one
      lend = lend - 1

      ! print a diagnostic message
      print "(I3,A,I6,A,I6,A,I7,A,I7,A,I7,A,I8,A,I7)",rank,": ",
     &      lstart," -> ",lend," of ",size(cost),", cost = ",
     &      partcost-tempcost," Actual: ",
     &      sum(cost(lstart:lend)),"/",totcost,"; avg ",avgcost

      end subroutine dolimits


