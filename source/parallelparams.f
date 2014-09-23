      module parallelparams

      use mpi
      use atoms

      implicit none

      integer, save:: rank   ! Process id
      integer, save:: nprocs ! Number of processes involved
      integer:: ierror       ! Error flag for MPI calls
      integer, dimension(MPI_STATUS_SIZE):: status ! MPI status for MPI calls

      end module parallelparams

