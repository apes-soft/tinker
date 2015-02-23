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

      subroutine dolimits(lstart, lend, cost)
      integer, intent(out):: lstart              ! loop start
      integer, intent(out):: lend                ! loop end
      integer, dimension(:), intent(in):: cost   ! cost array
      integer:: totcost                          ! total cost
      integer:: avgcost                          ! average cost
      integer:: partcost                         ! partial cost

      totcost = sum(cost)
      avgcost = totcost/nprocs

      lstart = 1
      partcost = 0

      if(rank.gt.0) then 
         do while(partcost.lt.(rank*avgcost+1)) 
            partcost = partcost + cost(lstart)
            lstart = lstart + 1
         end do
      end if

      if(rank.eq.nprocs-1) then 
         lend = size(cost)
      else
         lend = lstart+1
         do while(partcost.lt.((rank+1)*avgcost+1))
            partcost = partcost + cost(lend)
            lend   = lend + 1
         end do
      end if

      print *,rank," start ",lstart," end ",lend," out of ",size(cost)

      end subroutine 

      end module mpiparams

