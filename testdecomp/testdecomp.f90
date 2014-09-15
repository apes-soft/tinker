! Testing a possible spatially-based decomposition of atoms for
! Tinker. This is only a prototype to see whether things work.
! It SHOULD NOT Become THE code.

program mytest

    use parallelparams
    use iounit

    implicit none

    integer:: provided ! Actual threading model provided by MPI

    ! Allow for the usage of multithreaded applications but MPI
    ! regions will be on a single thread
    call MPI_Init_thread(MPI_THREAD_FUNNELED, provided, ierror)

    ! Find out how many processes we have.
    call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierror)

    ! Find out my own rank.
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

    ! Check that we are using one or a power of 2 number of processes,
    ! nprocs should never be 0
    if((nprocs.ne.1).and.(iand(nprocs,nprocs-1).ne.0)) then
      write(iout,*) "Number of processes must be a power of 2."
      call fatal
    end if

    ! Initialize variables
    call initial

    ! Read in the particle data
    call getxyz

    ! Finish the MPI execution
    call MPI_Finalize(ierror)

end program mytest
