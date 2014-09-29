!
! Subroutine to redistribute atoms between the processes.  Assumes a
! hypercube topology - a power of 2 number of processes must be
! used. Each split across the domain will also split the number of
! processes by half.
!

      subroutine redistribute

      use atoms
      use math
      use parallelparams

      implicit none

      integer:: nsplits   ! number of splits
      integer:: ns        ! split loop counter
      integer:: chan      ! communications channel
      real (kind=8), dimension(3):: lminbox,lmaxbox ! local system extent
      real (kind=8), dimension(3):: minbox, maxbox, sysbox
      real (kind=8):: localmedian

 
      ! Calculate the local extent of the system, i.e. for atoms hosted by
      ! this process.
      lmaxbox(1) = maxval(atom%pos(1))
      lmaxbox(2) = maxval(atom%pos(2))
      lmaxbox(3) = maxval(atom%pos(3))

      lminbox(1) = minval(atom%pos(1))
      lminbox(2) = minval(atom%pos(2))
      lminbox(3) = minval(atom%pos(3))


      ! Find the global system extent
      call MPI_Allreduce(lminbox,minbox,3, MPI_DOUBLE_PRECISION, 
     &                   MPI_MIN, MPI_COMM_WORLD, ierror)
  
      call MPI_Allreduce(lmaxbox,maxbox,3, MPI_DOUBLE_PRECISION, 
     &                   MPI_MAX, MPI_COMM_WORLD, ierror)

      ! Extent of the system box
      sysbox = maxbox - minbox

      ! Number of recursive splits
      nsplits = int(ceiling(log2(real(nprocs,kind=8))))

      ! Store: split number, split direction, split coord, neighbouring
      !        process
      allocate(splits(nsplits))

      ! Loop over the number of splits
      do ns=1, nsplits

        ! determine the splitting direction - use the longest direction
        splits(ns)%splitdir = maxloc(sysbox, dim=1)

        ! sort local atoms along the splitting direction
        !call sortAtoms(splits(ns)%splitdir, 1, dn)

        ! local median
c$$$        if(mod(dn,2).eq.0) then ! even dn
c$$$           localmedian = 0.5*(&
c$$$                        atomlist(dn/2)%pos(splits(ns)%splitdir)+&
c$$$                        atomlist(dn/2+1)%pos(splits(ns)%splitdir))
c$$$        else  ! odd dn
c$$$           localmedian = atomlist(int(dn/2+1))%pos(splits(ns)%splitdir)
c$$$        endif
        print *,"Process ",rank," has median ",localmedian,
     &                   " split dir ", splits(ns)%splitdir

        ! determine the splitting point
        splits(ns)%splitcoord=findSplit(splits(ns)%splitdir)

        ! determine the comms channel
        chan = ishft(1,(ns-1))

        ! deteremine what the neighbouring process rank is - this
        ! is based on a Gray code. Note ieor(neighbor,split)->rank.
        splits(ns)%neighbor = ieor(rank,chan)

        ! determine whether we are above or below the split
        splits(ns)%above = ishft(iand(rank,chan),-(ns-1))

        ! NB not sure what the halo region should be - could be a 
        ! deal breaker.
        ! swap atoms across the splitting direction
        if (splits(ns)%above.eq.1) then
           ! call MPI_Send(buff,count,datatype,neighbor,100,
    !&                     MPI_COMM_WORLD,ierror)
           ! call MPI_Recv(buff,count,datatype,neighbor,101,
    !&                     MPI_COMM_WORLD,ierror)
        else
           ! call MPI_Recv(buff,count,datatype,neighbor,100,
    !&                     MPI_COMM_WORLD,ierror)
           ! call MPI_Send(buff,count,datatype,neighbor,101,
    !&                     MPI_COMM_WORLD,ierror)
        end if
     
      end do ! End loop over splits

      ! NB Deallocate the atom distributions or keep?

      end subroutine redistribute
