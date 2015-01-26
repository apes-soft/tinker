c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine getxyz  --  get Cartesian coordinate structure  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "getxyz" asks for a Cartesian coordinate file name,
c     then reads in the coordinates file
c
c
      subroutine getxyz
      use inform
      use iounit
      use output
      use mpiparams
      implicit none
      integer ixyz
      integer freeunit
      logical exist
      character*120 xyzfile
c
c
c     try to get a filename from the command line arguments
c
      call nextarg (xyzfile,exist)
      if (exist) then

         ! get the basefile name for the xyz file
         call basefile (xyzfile)

         ! check file for the 'xyz' extension
         call suffix (xyzfile,'xyz','old')

         ! check if the file exists.
         inquire (file=xyzfile,exist=exist)
      end if
c
      ! fail if no xyz file has been supplied
      if (.not. exist) then
         write (iout,*) ' GETXYZ -- the file ',xyzfile,
     &                  ' does not exist.'
         call fatal
      end if
c
c     first open and then read the Cartesian coordinates file
c
      coordtype = 'CARTESIAN'
      ixyz = freeunit ()
      open (unit=ixyz,file=xyzfile,status='old')
      rewind (unit=ixyz)
      call readxyz (ixyz)
      close (unit=ixyz)
c
c     quit if the Cartesian coordinates file contains no atoms
c
      if (abort) then
         write (iout,30)
   30    format (/,' GETXYZ  --  Cartesian Coordinates File',
     &              ' does not Contain Any Atoms')
         call fatal
      end if
      return
      end
