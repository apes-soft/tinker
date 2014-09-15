!
! Variables and routines pertinent to the use of MPI
!

module parallelparams

  use mpi
  use atoms

  implicit none

  integer, save:: rank   ! Process id
  integer, save:: nprocs ! Number of processes involved
  integer:: ierror       ! Error flag for MPI calls
  integer, dimension(MPI_STATUS_SIZE):: status ! MPI status for MPI calls

  integer, save:: AtomTypeComm ! MPI derived type for atoms

  type(atomtype), allocatable, dimension(:), save:: atomlist ! List of atoms
  integer, save:: dn       ! Local number of atoms
  integer, save:: numatoms ! Number of atoms allocated locally

  ! data type to store information about the split
  type splitinfo
    integer:: splitdir            ! splitting direction 
    integer:: neighbor            ! rank to communicate with
    integer:: above               ! above or below the split
    real (kind=8):: splitcoord    ! bisection coordinate
  end type splitinfo

  type(splitinfo), dimension(:),allocatable, save:: splits

contains

! +--------------------------------------------------------------------+
! | Create MPI derived data type to communicate atom data across procs |
! +--------------------------------------------------------------------+
subroutine createMPIAtomType

  implicit none

 type(atomtype):: myatom  ! Derived type to create an MPI derived type

 integer (kind=MPI_ADDRESS_KIND):: baseAddress
 integer, parameter:: count=5         ! Number of types involved
 integer, dimension(count)::types     ! The MPI data types being used
 integer, dimension(count)::blocks    ! How many of each type
 integer (kind=MPI_ADDRESS_KIND), dimension(count)::displs ! Memory layout

 ! Specify the MPI data types present
 types = (/MPI_DOUBLE_PRECISION, &
           MPI_INTEGER,          &
           MPI_CHAR,             &
           MPI_CHAR,             &
           MPI_INTEGER/)  

 ! Specify how many of each type there are
 blocks = (/4, 5, 24, 3, maxbonds/)

 ! specify the memory layout
 call MPI_Get_address(myatom%pos(1), baseAddress, ierror)
 call MPI_Get_address(myatom%tag, displs(2), ierror)
 call MPI_Get_address(myatom%story, displs(3), ierror)
 call MPI_Get_address(myatom%name, displs(4), ierror)
 call MPI_Get_address(myatom%i12(1),displs(5),ierror)

 ! Work out the displacements
 displs = displs - baseAddress
 displs(1) = 0

 ! Create the derived MPI data type
 call MPI_Type_create_struct(count,blocks,displs,types, &
                             AtomTypeComm,ierror)

 ! Register it with the MPI system
 call MPI_Type_commit(AtomTypeComm, ierror)

end subroutine createMPIAtomType


! +-----------------------------------+
! | Calculate the splitting direction |
! +-----------------------------------+

function findSplit(dir)

  implicit none

  real (kind=8):: findSplit
  integer, intent(in):: dir


  
end function findSplit

end module parallelparams
