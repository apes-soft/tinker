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
      subroutine readxyz (ixyz)
      use sizes
      use atomid
      use atoms
      use bound
      use boxes
      use couple
      use files
      use inform
      use iounit
      use titles
      use mpiparams
      implicit none
      integer i,j,k,m
      integer ixyz,nmax
      integer next,size
      integer first,last
      integer nexttext
      integer trimtext
      integer, allocatable :: list(:)
      real*8,dimension(6):: bbox ! Boundbox buffer
      logical exist,opened
      logical quit,reorder
      logical clash
      character*120 xyzfile
      character*120 record
      character*120 string

      ! initialize the total number of atoms in the system
      n = 0 

      ! initialize coordinates and connectivities for all atom
      ! using fortran 90 syntax
      tag  = 0
      name = '   '
      x    = 0.0d0
      y    = 0.0d0
      z    = 0.0d0
      type = 0
      n12  = 0
      i12  = 0

      ! open the input file if it has not already been done
      ! only process 0 reads the input in and then broadcasts
      ! it to the other participating processes.
      if(rank.eq.0) then
        inquire (unit=ixyz,opened=opened)
        if (.not. opened) then

           xyzfile = filename(1:leng)//'.xyz'

           ! check the name of the file about to be opened
           call version (xyzfile,'old')

           inquire (file=xyzfile,exist=exist)

           if (exist) then
              open (unit=ixyz,file=xyzfile,status='old')
              rewind (unit=ixyz)
           else
              write (iout,10)
   10         format (/,' READXYZ  --  Unable to Find the Cartesian',
     &                  ' Coordinates File')
              call fatal
           end if ! end of if exist
        end if ! end of if not opened

        ! read first line and return if already at end of file
        quit  = .false.
        abort = .true.
        size  = 0
        do while (size .eq. 0)
           read (ixyz,20,err=80,end=80)  record
   20      format (a120)
           size = trimtext (record)
        end do
        abort = .false.
        quit  = .true.

        ! parse the title line to get the number of atoms
        i    = 0
        next = 1
        call gettext (record,string,next)
        read (string,*,err=80,end=80)  n

        ! extract the title and determine its length
        string = record(next:120)
        first  = nexttext (string)
        last   = trimtext (string)
        if (last .eq. 0) then
           title  = ' '
           ltitle = 0
        else
           title  = string(first:last) 
           ltitle = trimtext (title)   
        end if

        ! check for too few or too many total atoms in the file
        if (n .le. 0) then
           write (iout,30)
   30      format (/,' READXYZ  --  The Coordinate File Does Not',
     &               ' Contain Any Atoms')
           call fatal
        else if (n .gt. maxatm) then
           write (iout,40)  maxatm
   40      format (/,' READXYZ  --  The Maximum of',i8,' Atoms',
     &               ' has been Exceeded')
           call fatal
        end if

        ! read the coordinates and connectivities for each atom
        do i = 1, n
           size = 0
           do while (size .eq. 0)
              read (ixyz,50,err=80,end=80)  record
   50         format (a120)
              size = trimtext (record)
              if (i .eq. 1) then
                 next = 1
                 call getword (record,name(i),next)
                 if (name(1) .ne. '   ')  goto 60
                 read (record,*,err=60,end=60)  (bbox(j),j=1,6)
                 size        = 0
                 use_bounds  = .true.
              end if
   60         continue
           end do
           read (record,*,err=80,end=80)  tag(i)
           next = 1
           call getword (record,name(i),next)
           string = record(next:120)
           read (string,*,err=70,end=70)  x(i),y(i),z(i),type(i),
     &                                    (i12(j,i),j=1,maxcons)
   70      continue
        end do
        quit = .false.
   80   continue
        if (.not. opened)  close (unit=ixyz) ! CLOSED HERE AND FROM getxyz.f

        ! an error occurred in reading the coordinate file
        if (quit) then
           write (iout,90)  i
   90      format (/,' READXYZ  --  Error in Coordinate File at Atom',
     &            i6)
           call fatal
        end if
      end if ! end of if rank 0

      ! Send out the data to the processes involved
      ! Broadcast the total number of atoms to other procs
      call MPI_Bcast(n, 1, MPI_INTEGER, 0, MPI_COMM_WORLD,ierror)

      ! Bradcast the length of the title
      call MPI_Bcast(ltitle, 1, MPI_INTEGER, 0, MPI_COMM_WORLD,ierror)

      ! Broadcast the title to the other procs
      call MPI_Bcast(title,ltitle,MPI_CHAR,0,MPI_COMM_WORLD,ierror)

      ! Broadcast whether we have periodic bounds
      call MPI_Bcast(use_bounds,1,MPI_LOGICAL,0,MPI_COMM_WORLD,ierror)

      ! If we are using periodic bounds we need the bounding box
      if(use_bounds.eqv..true.) then
         call MPI_Bcast(bbox,6,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,
     &                 ierror)

         xbox  = bbox(1)
         ybox  = bbox(2)
         zbox  = bbox(3)
         alpha = bbox(4)
         beta  = bbox(5)
         gamma = bbox(6)

         ! store periodic box dimension and set angles for 
         ! calculating fractional coordinates
         call lattice 
     
      end if

      ! transfer the atom data
      call MPI_Bcast(x,n,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierror)
      call MPI_Bcast(y,n,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierror)
      call MPI_Bcast(z,n,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierror)
      call MPI_Bcast(type,n,MPI_INTEGER,0,MPI_COMM_WORLD,ierror)

      ! for each atom, count and sort its attached atoms
      do i = 1, n
         do j = maxcons, 1, -1
            if (i12(j,i) .ne. 0) then
                n12(i) = j            ! count of direct neighbours
                goto 100
            end if
         end do
  100    continue
         call sort (n12(i),i12(1,i))
      end do

      ! find the size of the maxtag
      nmax = 0
      do i = 1, n
         nmax = max(tag(i),nmax)
         do j = 1, n12(i)   ! MARIO - why is going through the neighbours necessary?
            nmax = max(i12(j,i),nmax)
         end do
      end do
      ! allocate a list that has the max tag.
      allocate (list(nmax))

      ! check for scrambled atom order and attempt to renumber
      reorder = .false.
      do i = 1, n
         list(tag(i)) = i
         if (tag(i) .ne. i)  reorder = .true.
      end do

      ! reorder the atoms if required
      if (reorder) then

         ! all processes do this but only 0 prints out
         if(rank.eq.0) then 
            write (iout,110)
  110       format (/,' READXYZ  --  Atom Labels not Sequential,',
     &                ' Attempting to Renumber')
         end if 

         ! relable the atoms
         do i = 1, n
            tag(i) = i
            do j = 1, n12(i)
               i12(j,i) = list(i12(j,i))
            end do
            call sort (n12(i),i12(1,i))
         end do
      end if

      ! deallocation the list array
      deallocate (list)

      ! check for atom pairs with identical coordinates
      clash = .false. ! MARIO this is initialised in chkxyz
      if (n .le. 10000)  call chkxyz (clash)

      ! make sure that all connectivities are bidirectional
      do i = 1, n

         do j = 1, n12(i)
            ! my neighbour
            k = i12(j,i)
            ! check my neighbour points back to me
            do m = 1, n12(k)
               if (i12(m,k) .eq. i)  goto 130
            end do

            ! only process 0 prints error out, all procs doing this
            if(rank.eq.0) then 
               write (iout,120)  k,i
  120          format (/,' READXYZ  --  Check Connection of Atom',
     &                 i6,' to Atom',i6)
            end if
            call fatal
  130       continue
         end do
      end do

      return
      end
