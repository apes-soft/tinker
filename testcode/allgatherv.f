      ! program to test an allgatherv call
      program allgatherv
     
      use mpi

      implicit none

      integer, parameter::n=10
      integer, dimension(n):: val
      integer:: rank, nprocs, provided, ierror
      integer, allocatable, dimension(:)::myarray,incounts,disps
      integer:: i, dn, ls, le, c
      integer (kind=MPI_ADDRESS_KIND):: extent, lb


      ! Initialize MPI
      call MPI_Init_thread(MPI_THREAD_FUNNELED, provided, ierror)

      ! Find out how many processes we have.
      call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierror)

      ! Find out my own rank.
      call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

      ! diagnostic message
      !print *,"Proc ",rank," out of ",nprocs," initialized."

      ! partition the array
      dn = n/nprocs

      ! allocate the array
      allocate(myarray(dn),incounts(nprocs),disps(nprocs))

      ! initialise the arrays
      val      = 0
      incounts = 0
      disps    = 0

      ! assign the local array
      do i=1,dn
         myarray(i) = rank*100 + i
      end do

      print "(A,I3,A,<dn>(1X,I3))", "Before :",rank,":",myarray

      ! assign in counts and displacements
      do i = 1, nprocs
         incounts(i) = dn
      end do 

      ! find the width of the datatype
      call MPI_Type_get_extent(MPI_INTEGER, lb, extent, ierror)

      do i = 2, nprocs
         disps(i)    = disps(i-1) + incounts(i-1) !*extent
      end do 

      ! gather the results
      call MPI_Allgatherv(myarray,dn, MPI_INTEGER,
     &                    val,incounts,disps, MPI_INTEGER,
     &                    MPI_COMM_WORLD, ierror)

      print "(A,I3,A,<n>(1X,I3))","Rank ",rank,":", val

      call MPI_Finalize(ierror)

      
      end program allgatherv
