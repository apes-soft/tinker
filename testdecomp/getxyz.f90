!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     #################################################################
!     ##                                                             ##
!     ##  subroutine getxyz  --  get Cartesian coordinate structure  ##
!     ##                                                             ##
!     #################################################################
!
!
!     "getxyz" asks for a Cartesian coordinate file name,
!     then reads in the coordinates file
!
!
      
subroutine getxyz

  use inform
  use iounit
  use output
  use parallelparams

  implicit none

  logical:: exist
  character (len=120):: xyzfile

  ! try to get a filename from the command line arguments
  call nextarg (xyzfile,exist)
  if (exist) then
     call basefile (xyzfile)
     call suffix (xyzfile,'xyz','old')
  else
     write (iout,*) ' GETXYZ  --  No cartesian coordinates file specified.'
     call fatal
  end if


  ! read the Cartesian coordinates file
  coordtype = 'CARTESIAN'
  call readxyz 

  ! quit if the Cartesian coordinates file contains no atoms
  if (abort) then
     write (iout,*) ' GETXYZ  --  Cartesian Coordinates File'// &
                    ' does not Contain Any Atoms'
     call fatal
  end if
  return
      
end subroutine getxyz
