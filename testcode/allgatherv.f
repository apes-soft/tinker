      ! Program to test the semantics of an MPI allgatherv call.
      ! Code not fully general (i.e. will not work for all cases
      ! but need to ensure that the semantics work for the cases
      ! it will correctly work for, mainly it will only work 
      ! properly for cases where nprocs divides evenly into n).
      program allgatherv
     
      use mpi

      implicit none

      integer, parameter::n=10 
      integer, dimension(2,n):: val             ! array to gather into
      integer:: rank, nprocs, provided, ierror  ! MPI vars
      integer, allocatable, dimension(:,:)::myarray
      integer, allocatable, dimension(:)::incounts,disps
      integer:: i,dn
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
      allocate(myarray(2,dn),incounts(nprocs),disps(nprocs))

      ! initialise the arrays
      val      = 0
      incounts = 0
      disps    = 0

      ! assign the local array
      do i=1,dn
         myarray(1,i) = rank*100 + i
         myarray(2,i) = rank*100 + 50 + i
      end do

      ! The <dn> in the format specifier is a repeater - it may
      ! just be an intel compiler extension.
      print "(A,I3,A,<dn>(1X,I3),A,<dn>(1X,I3))", "Before :",rank,":",
     &        myarray(1,:)," --- ",myarray(2,:)

      ! assign in counts and displacements
      do i = 1, nprocs
         incounts(i) = dn
      end do 


      ! calculate the displacements
      do i = 2, nprocs
         disps(i)    = disps(i-1) + incounts(i-1) 
      end do 

      ! gather the results
      call MPI_Allgatherv(myarray,2*dn, MPI_INTEGER,
     &                    val,2*incounts,2*disps, MPI_INTEGER,
     &                    MPI_COMM_WORLD, ierror)

      print "(A,I3,A,<n>(1X,I3),A,<n>(1X,I3))","Rank ",rank,":", 
     &       val(1,:)," --- ",val(2,:)

      call MPI_Finalize(ierror)

      
      end program allgatherv
