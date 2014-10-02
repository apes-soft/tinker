c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ##############################################################
c     ##                                                          ##
c     ##  subroutine readxyz  --  input of Cartesian coordinates  ##
c     ##                                                          ##
c     ##############################################################
c
c
c     "readxyz" gets a set of Cartesian coordinates from
c     an external disk file
c
c
      subroutine readxyz ()
      use sizes
      use atoms
      use boxes
      use couple
      use files
      use inform
      use iounit
      use titles
      implicit none
      integer i,j,k,m
      integer nmax
      integer next,size
      integer first,last
      integer nexttext
      integer trimtext
      integer, allocatable :: list(:)
      real*8 xlen,ylen,zlen
      real*8 aang,bang,gang
      logical exist,opened
      logical quit,reorder
      logical clash
      character*120 xyzfile
      character*120 record
      character*120 string
c
c
c     initialize the total number of atoms in the system
c
      n = 0
c
c     open the input file if it has not already been done
c
      inquire (unit=ixyz,opened=opened)
      if (.not. opened) then
         xyzfile = filename(1:leng)//'.xyz'
         call version (xyzfile,'old')
         inquire (file=xyzfile,exist=exist)
         if (exist) then
            open (unit=ixyz,file=xyzfile,status='old')
            rewind (unit=ixyz)
         else
            write (iout,10)
   10       format (/,' READXYZ  --  Unable to Find the Cartesian',
     &                 ' Coordinates File')
            call fatal
         end if
      end if
c
c     read first line and return if already at end of file
c
      quit = .false.
      abort = .true.
      size = 0
      do while (size .eq. 0)
         read (ixyz,20,err=80,end=80)  record
   20    format (a120)
         size = trimtext (record)
      end do
      abort = .false.
      quit = .true.
c
c     parse the title line to get the number of atoms
c
      i = 0
      next = 1
      call gettext (record,string,next)
      read (string,*,err=80,end=80)  n
c
c     extract the title and determine its length
c
      string = record(next:120)
      first = nexttext (string)
      last = trimtext (string)
      if (last .eq. 0) then
         title = ' '
         ltitle = 0
      else
         title = string(first:last)
         ltitle = trimtext (title)
      end if
c
c     check for too few or too many total atoms in the file
c
      if (n .le. 0) then
         write (iout,30)
   30    format (/,' READXYZ  --  The Coordinate File Does Not',
     &              ' Contain Any Atoms')
         call fatal
      else if (n .gt. maxatm) then
         write (iout,40)  maxatm
   40    format (/,' READXYZ  --  The Maximum of',i8,' Atoms',
     &              ' has been Exceeded')
         call fatal
      end if

      allocate (atom(n))


c
c     creating atom data type for each atom in the system
c

      allocate (atom(n))

c
c     initialize coordinates and connectivities for each atom
c

      do i = 1, n
         atom(i)%tag = 0
         atom(i)%name = '   '
         atom(i)%pos(1) = 0.0d0
         atom(i)%pos(2) = 0.0d0
         atom(i)%pos(3) = 0.0d0
         atom(i)%type = 0
         do j = 1, maxval
            atom(i)%i12(j) = 0
         end do
      end do
c
c     read the coordinates and connectivities for each atom
c
      do i = 1, n
         size = 0
         do while (size .eq. 0)
            read (ixyz,50,err=80,end=80)  record
   50       format (a120)
            size = trimtext (record)
            if (i .eq. 1) then
               next = 1
               call getword (record,atom(i)%name,next)
               if (atom(i)%name .ne. '   ')  goto 60
               read (record,*,err=60,end=60)  xlen,ylen,zlen,
     &                                        aang,bang,gang
               size = 0
               xbox = xlen
               ybox = ylen
               zbox = zlen
               alpha = aang
               beta = bang
               gamma = gang
c              use_bounds = .true.
               call lattice
            end if
   60       continue
         end do
         read (record,*,err=80,end=80)  atom(i)%tag
         next = 1
         call getword (record,atom(i)%name,next)
         string = record(next:120)
         read (string,*,err=70,end=70)  atom(i)%pos(1),atom(i)%pos(2),
     &        atom(i)%pos(3),atom(i)%type,(atom(i)%i12(j),j=1,maxval)
   70    continue
      end do
      quit = .false.
   80 continue
      if (.not. opened)  close (unit=ixyz)
c
c     an error occurred in reading the coordinate file
c
      if (quit) then
         write (iout,90)  i
   90    format (/,' READXYZ  --  Error in Coordinate File at Atom',i6)
         call fatal
      end if
c
c     for each atom, count and sort its attached atoms
c
      do i = 1, n
         atom(i)%n12 = 0
         do j = maxval, 1, -1
            if (atom(i)%i12(j) .ne. 0) then
               atom(i)%n12 = j
               goto 100
            end if
         end do
  100    continue
         call sort (atom(i)%n12,atom(i)%i12(1))
      end do
c
c     perform dynamic allocation of some local arrays
c
      nmax = 0
      do i = 1, n
         nmax = max(atom(i)%tag,nmax)
         do j = 1, atom(i)%n12
            nmax = max(atom(i)%i12(j),nmax)
         end do
      end do
      allocate (list(nmax))
c
c     check for scrambled atom order and attempt to renumber
c
      reorder = .false.
      do i = 1, n
         list(atom(i)%tag) = i
         if (atom(i)%tag .ne. i)  reorder = .true.
      end do
      if (reorder) then
         write (iout,110)
  110    format (/,' READXYZ  --  Atom Labels not Sequential,',
     &              ' Attempting to Renumber')
         do i = 1, n
            atom(i)%tag = i
            do j = 1, atom(i)%n12
               atom(i)%i12(j) = list(atom(i)%i12(j))
            end do
            call sort (atom(i)%n12,atom(i)%i12(1))
         end do
      end if
c
c     perform deallocation of some local arrays
c
      deallocate (list)
c
c     check for atom pairs with identical coordinates
c
      clash = .false.
      if (n .le. 10000)  call chkxyz (clash)
c
c     make sure that all connectivities are bidirectional
c
      do i = 1, n
         do j = 1, atom(i)%n12
            k = atom(i)%i12(j)
            do m = 1, atom(k)%n12
               if (atom(k)%i12(m) .eq. i)  goto 130
            end do
            write (iout,120)  k,i
  120       format (/,' READXYZ  --  Check Connection of Atom',
     &                 i6,' to Atom',i6)
            call fatal
  130       continue
         end do
      end do
      return
      end
