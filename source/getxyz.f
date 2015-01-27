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


      ! try to get a filename from the command line arguments
      call nextarg (xyzfile,exist)
      if (exist) then

         ! get the basefile name for the xyz file
         call basefile (xyzfile)

         ! read and store the keywords from the keyfile
         call getkey

         ! get the information level and output style
         call control

         ! check file for the 'xyz' extension
         call suffix (xyzfile,'xyz','old')

         ! check if the file exists.
         inquire (file=xyzfile,exist=exist)

      end if ! end if exist

      ! fail if no xyz file has been supplied
      if (.not. exist) then
         if(rank.eq.0) then
            write (iout,*) ' GETXYZ -- the file ',xyzfile,
     &                     ' does not exist.'
         end if
         call fatal
      end if

      ! first open and then read the Cartesian coordinates file
      coordtype = 'CARTESIAN'

      ! Only want process 0 to open/close files.
      if(rank.eq.0) then
        ixyz = freeunit ()
        open (unit=ixyz,file=xyzfile,status='old')
        rewind (unit=ixyz)
      end if

      ! Now read the coordinates
      call readxyz (ixyz)

      ! Only proc 0 opened the file
      if(rank.eq.0) then
        close (unit=ixyz)
      end if

      ! quit if the Cartesian coordinates file contains no atoms
      if (abort) then
         if(rank.eq.0) then 
           write (iout,30)
   30      format (/,' GETXYZ  --  Cartesian Coordinates File',
     &               ' does not Contain Any Atoms')
         end if
         call fatal
      end if
      return
      end

