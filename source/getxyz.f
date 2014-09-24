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
      use parallelparams

      implicit none

      logical exist
      character*120 xyzfile

      ! only get process 0 to get file information
      if(rank.eq.0) then 
         ! try to get a filename from the command line arguments
         call nextarg (xyzfile,exist)

         if (exist) then
            call basefile (xyzfile)
            call suffix (xyzfile,'xyz','old')
            inquire (file=xyzfile,exist=exist)
         else
            write (iout,*) ' GETXYZ  --  ',
     &                     'No cartesian coordinates file specified.'
            call fatal
         end if

         ! check if file does not exist
         if (.not. exist) then
            write (iout,*) ' GETXYZ -- the file ',xyzfile,
     &                     ' does not exist.'
            call fatal
         end if

      end if

      ! read the coordinate file
      call readxyz

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
