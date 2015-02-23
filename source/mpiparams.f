      ! Module to store mpi params.

      module mpiparams

      use mpi

      implicit none

      ! MPI required variables
      integer:: rank   ! Process id
      integer:: nprocs ! Total number of processes involved
      integer:: ierror       ! Error flag required by MPI calls
      integer, dimension(MPI_STATUS_SIZE):: status ! Status for MPI calls

      contains

      ! subroutine to split the limits of a loop based on 
      ! some cost vector
      subroutine splitlimits(lstart, lend, cost)
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
      do while(partcost.lt.(rank*avgcost+1)) 
         partcost = partcost + cost(lstart)
         lstart   = lstart + 1
      end do

      ! overcounted by one
      lstart = lstart - 1

      ! store the cost to calc the cost for a given rank
      tempcost = partcost 
 
      ! calculate the upper bound of the loop
      if(rank.lt.nprocs-1) then 
         lend = lstart
         do while(partcost.lt.((rank+1)*avgcost+1))
            partcost = partcost + cost(lend)
            lend     = lend + 1
         end do
         ! over counted by one
         lend = lend - 1
      else
         lend     = size(cost)
         ! need to add cost at start else it will not be
         ! taken into account in the diagnostic.
         partcost = totcost + cost(lstart)
      end if

      ! print a diagnostic message
      print "(I3,A,I6,A,I6,A,I7,A,I7,A,I7,A,I7,A,I8)",rank," start ",
     &      lstart," -> ",lend," out of ",size(cost)," cost = ",
     &      partcost-tempcost,"/",avgcost," Actual: ",
     &      sum(cost(lstart:lend))," out of tot cost ",totcost

      end subroutine 

      end module mpiparams

