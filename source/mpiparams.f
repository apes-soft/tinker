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



      end module mpiparams

