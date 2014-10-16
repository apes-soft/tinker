!
! Subroutine to redistribute atoms between the processes.  Assumes a
! hypercube topology - a power of 2 number of processes must be
! used. Each split across the domain will also split the number of
! processes by half.
!

      subroutine redistributeInit

      use atoms
      use math
      use parallelparams

      implicit none

      integer:: nsplits   ! number of splits
      integer:: ns        ! split loop counter
      integer:: chan      ! communications channel
      real (kind=8), dimension(3):: lminbox ! local min coord
      real (kind=8), dimension(3):: lmaxbox ! local max coord
      real (kind=8), dimension(3):: minbox  ! global min coord
      real (kind=8), dimension(3):: maxbox  ! global max coord
      real (kind=8), dimension(3):: sysbox  ! system extent
      real (kind=8), dimension(3):: lsysbox ! local system extent

      ! Calculate the local extent of the system,
      ! i.e. for atoms hosted by this process.
      lmaxbox(1) = maxval(atom%pos(1))
      lmaxbox(2) = maxval(atom%pos(2))
      lmaxbox(3) = maxval(atom%pos(3))

      lminbox(1) = minval(atom%pos(1))
      lminbox(2) = minval(atom%pos(2))
      lminbox(3) = minval(atom%pos(3))

      ! Find the global system extent
      ! NB this may be dictated by input parameters in the key 
      ! file or in the coordinate file.
      call MPI_Allreduce(lminbox,minbox,3, MPI_DOUBLE_PRECISION, 
     &                   MPI_MIN, MPI_COMM_WORLD, ierror)
  
      call MPI_Allreduce(lmaxbox,maxbox,3, MPI_DOUBLE_PRECISION, 
     &                   MPI_MAX, MPI_COMM_WORLD, ierror)

      ! Extent of the global system box
      sysbox = maxbox - minbox

      ! Number of recursive splits possible
      nsplits = int(ceiling(log2(real(nprocs,kind=8))))

      ! Structure to store information about:
      ! 
      !  split number 
      !  split direction
      !  rank of neighbour in other domain to communicate with 
      !  "above" or "below" in the domain split
      !  communicator for each subdomain
      !  split coord 
      !  min and max coord of the subdomain system box
      !
      if(.not. allocated(splits)) then
         allocate(splits(nsplits))
      end if

      ! Set the the min/max for the top domain.
      splits(1)%minbox = minbox
      splits(1)%maxbox = maxbox

      ! Loop over the recursive bisections
      do ns=1, nsplits

        ! To start set the extent of the current domain 
        ! to be the extent of the previous domain.
        if(ns.gt.1) then 
          splits(ns)%minbox = splits(ns-1)%minbox
          splits(ns)%maxbox = splits(ns-1)%maxbox
        end if

        ! Calculate the local domain extent.
        lsysbox = splits(ns)%maxbox - splits(ns)%minbox

        ! Determine the comms channel - use this construct to determine
        ! neighbouring process and what side of the split this process
        ! belongs to.
        chan = ishft(1,(ns-1))

        ! Deteremine what the neighbouring process rank is - this
        ! is based on a Gray code. Note that applying the inverse
        ! returns the original rank, i.e. ieor(neighbor,split)->rank.
        splits(ns)%neighbor = ieor(rank,chan)

        ! Determine whether we are "above" or "below" the 
        ! split. This will return a 0 or a 1. Assign the 
        ! upper coordinates to the "above" processes. 
        splits(ns)%above = ishft(iand(rank,chan),-(ns-1))

        ! Create a subcommunicator for each of the subdomains
        ! to enable collective communications for the subdomain.
        ! Only want these communicators for collective
        ! operations - point-to-point operations must still 
        ! operate with MPI_COMM_WORLD.
        if(ns.gt.1) then 
           call MPI_Comm_split(splits(ns-1)%comm,
     &                         splits(ns)%above,
     &                         rank,
     &                         splits(ns)%comm, ierror)
        else
           ! For the first split default to the global coordinate
           splits(ns)%comm=MPI_COMM_WORLD
        end if

        ! Determine the splitting direction, i.e. split 
        ! along the longest systems direction
        splits(ns)%splitdir = maxloc(lsysbox, dim=1)

        ! Determine the splitting coordinate
        splits(ns)%splitcoord=findSplit(splits(ns)%splitdir,
     &                                  splits(ns)%minbox,
     &                                  splits(ns)%maxbox,
     &                                  splits(ns)%comm)

      ! sort local atoms along the splitting direction
      ! NB can only do this the first time otherwise all
      ! all the atom relationships will break once these
      ! have been established.
      call sortAtoms(splits(ns)%splitdir, 1, dn)


        !if(lrank.eq.0) then
          print "(A,I3,A,I2,A,F7.3)","Split: ",ns,
     &          " splitdir ",splits(ns)%splitdir," splitcoord: ", 
     &          splits(ns)%splitcoord
        !end if

        ! NB not sure what the halo region should be - could be a 
        ! deal breaker.
        ! swap atoms across the splitting direction
        if (splits(ns)%above.eq.1) then

           ! Above the split so the minbox moves up.
           splits(ns)%minbox(splits(ns)%splitdir) = 
     &           splits(ns)%splitcoord


           ! call MPI_Send(buff,count,datatype,neighbor,100,
    !&                     MPI_COMM_WORLD,ierror)
           ! call MPI_Recv(buff,count,datatype,neighbor,101,
    !&                     MPI_COMM_WORLD,ierror)
        else

           ! Below the split so the maxbox moves down.
           splits(ns)%maxbox(splits(ns)%splitdir) = 
     &           splits(ns)%splitcoord

           ! call MPI_Recv(buff,count,datatype,neighbor,100,
    !&                     MPI_COMM_WORLD,ierror)
           ! call MPI_Send(buff,count,datatype,neighbor,101,
    !&                     MPI_COMM_WORLD,ierror)
        end if
     
      end do ! End loop over splits

      ! NB Deallocate the atom distributions or keep?

      end subroutine redistributeInit
