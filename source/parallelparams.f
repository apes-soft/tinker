!     Variables used for the parallel implementation here including, the
!     process rank, size, etc as well as ancillary functions.

      module parallelparams

      use mpi
      use atoms

      implicit none

      ! MPI required variables
      integer:: rank   ! Process id
      integer:: nprocs ! Total number of processes involved
      integer:: ierror       ! Error flag required by MPI calls
      integer, dimension(MPI_STATUS_SIZE):: status ! Status for MPI calls

      ! dn is the actual number of atoms stored in the local process
      ! while numatoms contains buffer space and halo atoms.
      integer:: dn       ! Local number of atoms
      integer:: numatoms ! Number of atoms allocated locally

      integer:: AtomTypeComm ! MPI derived type for atoms

      ! data type to store information about the split
      type splitinfo
        integer:: splitdir            ! splitting direction 
        integer:: neighbor            ! rank to communicate with
        integer:: above               ! above or below the split
        integer:: comm                ! MPI Communicator for this split
        real (kind=8):: splitcoord    ! bisection coordinate
        real (kind=8), dimension(3):: minbox, maxbox ! systembox
      end type splitinfo

      type(splitinfo), dimension(:),allocatable:: splits

      save

      contains


! +--------------------------------------------------------------------+
! | Create MPI derived data type to communicate atom data across procs |
! +--------------------------------------------------------------------+
      subroutine createMPIAtomType

      implicit none

      type(atomtype):: myatom  ! Derived type to create an MPI derived type

      integer (kind=MPI_ADDRESS_KIND):: baseAddress
      integer, parameter:: count=4         ! Number of types involved
      integer, dimension(count)::types     ! The MPI data types being used
      integer, dimension(count)::blocks    ! How many of each type
      integer (kind=MPI_ADDRESS_KIND), dimension(count)::displs ! Memory layout

      ! Specify the MPI data types present
      types = (/MPI_DOUBLE_PRECISION, 
     &          MPI_INTEGER,          
     &          MPI_CHAR,             
     &          MPI_CHAR             
     &         /)  

      ! Specify how many of each type there are
      blocks = (/4, 5, 24, 3 /)

      ! specify the memory layout
      call MPI_Get_address(myatom%pos(1), baseAddress, ierror)
      call MPI_Get_address(myatom%tag, displs(2), ierror)
      call MPI_Get_address(myatom%story, displs(3), ierror)
      call MPI_Get_address(myatom%name, displs(4), ierror)

      ! Work out the displacements
      displs = displs - baseAddress
      displs(1) = 0

      ! Create the derived MPI data type
      call MPI_Type_create_struct(count,blocks,displs,types, 
     &                            AtomTypeComm,ierror)

      ! Register it with the MPI system
      call MPI_Type_commit(AtomTypeComm, ierror)

      end subroutine createMPIAtomType

! +-----------------------------------+
! | Calculate the splitting direction |
! +-----------------------------------+

      ! Currently using the position of the atoms to determine a
      ! splitting coordinate. This may not give the optimal split
      ! - it would be better to use an atom cost or an interaction
      ! count to weigh the atom cost and determine a splitting 
      ! coordinate that splits the cost and not the atoms.
      function findSplit(dir, minb, maxb,comm)

      implicit none

      real (kind=8):: findSplit    ! return of the function
      integer, intent(in):: dir    ! splitting direction
      real (kind=8), dimension(3), intent(in):: minb, maxb ! origin and max pt
      real (kind=8):: minp, maxp
      integer, intent(in):: comm   ! Communicator for domain
      real (kind=8):: pivot        ! pivot to use to identify midpoint
      integer:: lnatoms            ! local number of atoms
      integer:: gnatoms            ! global number of atoms
      integer:: target             ! target number of atoms
      real (kind=8), allocatable, dimension(:):: coords ! coordinates


      ! Allocate a temporary to hold the coordinate information.
      allocate(coords(dn))
  
      ! Populate a temporary array witht the coords in the 
      ! splitting direction.
      coords = atom(1:dn)%pos(dir)

      ! Calculate the number of atoms in this domain.
      call MPI_Allreduce(dn,gnatoms, 1, MPI_INTEGER, MPI_SUM, 
     &                   comm, ierror)

      ! Global target number of atoms to hit - round up by
      ! one if even.
      target = ceiling(gnatoms/2.0)

      ! Min and max box points.
      minp = minb(dir)
      maxp = maxb(dir)

      ! Assign a pivot.
      pivot = 0.5*(maxp-minp)

      ! Count the local number of atoms below this pivot.
      lnatoms = count(coords < pivot)

      ! Now get the number of atoms below the pivot in this domain.
      call MPI_Allreduce(lnatoms,gnatoms, 1, MPI_INTEGER, MPI_SUM, 
     &                   comm, ierror)

      ! Repeat until we find a splitting point.
      do while(gnatoms /= target)

        ! Reset the pivot
        if(gnatoms.lt.target) then 

          minp  = pivot
          pivot = 0.5*(pivot+maxp)

        else 
       
          maxp  = pivot
          pivot = 0.5*(minp+pivot)

        end if

        ! Count the number of atoms below the new pivot.
        lnatoms = count(coords < pivot)

        ! Now calculate the global number of atoms in this domain.
        call MPI_Allreduce(lnatoms,gnatoms, 1, MPI_INTEGER, MPI_SUM, 
     &                     comm, ierror)

      end do ! end while

      ! Free the temporary arrays
      deallocate(coords)

      ! Assign the value for the splitting coordinate
      findSplit = pivot

      end function findSplit

      end module parallelparams

