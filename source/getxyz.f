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
      implicit none
      logical exist
      character*120 xyzfile
c
c
c     try to get a filename from the command line arguments
c
      call nextarg (xyzfile,exist)
      if (exist) then
         call basefile (xyzfile)
         call suffix (xyzfile,'xyz','old')
         inquire (file=xyzfile,exist=exist)
      end if
c
c     terminate execution if Cartesian coordinate file is not provided
c
      do while (.not. exist)
         write (iout,*) "The Cartesian Coordinate file base name must ",
     &                  "be specified at the command line."
         call usage
         call fatal
      end do
c
c     first open and then read the Cartesian coordinates file
c
      coordtype = 'CARTESIAN'
      open (unit=ixyz,file=xyzfile,status='old')
      rewind (unit=ixyz)
      call readxyz ()
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
