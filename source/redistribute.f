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
      integer:: end       ! end index for local atoms
      integer:: ninreg    ! number of atoms in the region
      integer:: ninhal    ! number of atoms including halo region
      integer:: nsend     ! number of atoms to send
      integer:: nrecv     ! number of atoms to receive
      integer:: above
      integer:: neighbor
      integer:: splitdir
      real (kind=8):: splitcoord
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

      ! Default communicator to be used for the first split.
      splits(ns)%comm=MPI_COMM_WORLD

      ! Last index for local atoms.
      end = dn

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
        neighbor = splits(ns)%neighbor

        ! Determine whether we are "above" or "below" the 
        ! split. This will return a 0 or a 1. Assign the 
        ! upper coordinates to the "above" processes. 
        splits(ns)%above = ishft(iand(rank,chan),-(ns-1))
        above = splits(ns)%above

        ! Create a subcommunicator for each of the subdomains
        ! to enable collective communications for the subdomain.
        ! Only want these communicators for collective
        ! operations - point-to-point operations must still 
        ! operate with MPI_COMM_WORLD.
        if(ns.gt.1) then 
           call MPI_Comm_split(splits(ns-1)%comm,
     &                         above,
     &                         rank,
     &                         splits(ns)%comm, ierror)
         end if

        ! Determine the splitting direction, i.e. split 
        ! along the longest systems direction
        splits(ns)%splitdir = maxloc(lsysbox, dim=1)
        splitdir = splits(ns)%splitdir

        ! Determine the splitting coordinate
        splits(ns)%splitcoord=findSplit(splitdir,
     &                                  splits(ns)%minbox,
     &                                  splits(ns)%maxbox,
     &                                  splits(ns)%comm)
        splitcoord = splits(ns)%splitcoord

        ! Trick to get an a descening sort in the sort 
        ! routine below.
        if(above.eq.1) then
           atom(:)%pos(splitdir) = -atom(:)%pos(splitdir)
        end if

        ! Sort local atoms along the splitting direction
        ! want local atoms to be in the lower portion of
        ! of the array.
        ! NB can only do this the first time otherwise all
        ! all the atom relationships will break once these
        ! have been established.
        call sortAtoms(splitdir, 1, end)

        ! Return sign back to the original.
        if(splits(ns)%above.eq.1) then
           atom(:)%pos(splitdir) = -atom(:)%pos(splitdir)
        end if

        ! Diagnostic message
        print "(A,I3,A,I2,A,F7.3)","Split: ",ns,
     &        " splitdir ",splitdir," splitcoord: ",splitcoord

        ! NB not sure what the halo region should be - could be a 
        ! deal breaker.
        ! swap atoms across the splitting direction
        if (above.eq.1) then

           ! Update the systems box.
           ! Above the split so the minbox moves up.
           splits(ns)%minbox(splitdir) = splitcoord

           ! Count the number of atoms above the split.
           ninreg = count(atom(:)%pos(splitdir).ge.splitcoord)

           ! Include a halo region.
           ninhal = count(atom(:)%pos(splitdir).ge.
     &                    splitcoord - maxcutoff)

           ! Number of atoms to send across
           nsend = end - ninhal + 1

           ! Let the neighbouring process know how many atoms
           ! it will get and find out how many it has to get.
           call MPI_Send(nsend,1,MPI_INTEGER,neighbor,100,
     &                   MPI_COMM_WORLD,ierror)
           call MPI_Recv(nrecv,1,MPI_INTEGER,neighbor,101,
     &                   MPI_COMM_WORLD,status,ierror)

           ! Now actually send the data
           ! call MPI_Send(buff,count,datatype,neighbor,100,
    !&                     MPI_COMM_WORLD,ierror)
           ! call MPI_Recv(buff,count,datatype,neighbor,101,
    !&                     MPI_COMM_WORLD,ierror)
        else

           ! Update the systems box.
           ! Below the split so the maxbox moves down.
           splits(ns)%maxbox(splitdir) = splitcoord

           ! Count the number of atoms above the split.
           ninreg = count(atom(:)%pos(splitdir).le.splitcoord)

           ! Include a halo region.
           ninhal = count(atom(:)%pos(splitdir).ge.
     &                    splitcoord + maxcutoff)

           ! Number of atoms to send across
           nsend = end - ninhal + 1

           ! Let the neighbouring process know how many atoms
           ! it will get and find out how many it has to get.
           call MPI_Recv(nrecv,1,MPI_INTEGER,neighbor,100,
     &                     MPI_COMM_WORLD,status,ierror)
           call MPI_Send(nsend,1,MPI_INTEGER,neighbor,101,
     &                     MPI_COMM_WORLD,ierror)

           ! call MPI_Recv(buff,count,datatype,neighbor,100,
    !&                     MPI_COMM_WORLD,ierror)
           ! call MPI_Send(buff,count,datatype,neighbor,101,
    !&                     MPI_COMM_WORLD,ierror)
        end if
     
      end do ! End loop over splits

      ! NB Deallocate the atom distributions or keep?

      end subroutine redistributeInit
