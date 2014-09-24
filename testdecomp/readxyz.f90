!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ##############################################################
!     ##                                                          ##
!     ##  subroutine readxyz  --  input of Cartesian coordinates  ##
!     ##                                                          ##
!     ##############################################################
!
!
!     "readxyz" gets a set of Cartesian coordinates from
!     an external disk file
!
!

subroutine readxyz 

   !use sizes
   use parallelparams
   use atmtyp
   use atoms
   use bound, ONLY: use_bounds
   use boxes
   use couple
   use files
   use inform
   use iounit, ONLY: iout
   use titles

   implicit none

   integer:: i,j,k
   !integer:: m
   integer:: proc
   integer, parameter:: ixyz=10
   !integer:: nmax
   integer:: next,size
   integer:: first,last
   integer:: nexttext
   integer:: trimtext
   integer:: start, end, nlocal
   !integer, allocatable :: list(:)
   real (kind=8), dimension(6):: boxparams
   logical:: exist
   logical:: quit
   !logical:: reorder
   !logical:: clash
   character(len=120):: xyzfile
   character(len=120):: record
   character(len=120):: string


   ! initialize the total number of atoms in the system
   n = 0

   ! create an MPI derived type to communicate atoms
   call createMPIAtomType

   ! only process 0 opens the input file and redistributes to 
   ! the other processes. Read the number of atoms and 
   ! simulation title first
   if(rank.eq.0) then

         xyzfile = filename(1:leng)//'.xyz'

         call version (xyzfile,'old')  
         inquire (file=xyzfile,exist=exist)
         if (exist) then
            open (unit=ixyz,file=xyzfile,status='old')
            rewind (unit=ixyz) ! NB Why? Expecting to have been opened before?
         else
            write (iout,*) ' READXYZ  --  Unable to Find the Cartesian' &
                         //' Coordinates File'
            call fatal
         end if

         ! read first line with number of atoms and title
         ! and return if already at end of file  NB - why?
         quit  = .true.
         abort = .false.
         size  = 0
         do while (size .eq. 0)
            read (ixyz,"(a120)", err=10, end=10)  record
            size = trimtext (record)   
         end do
         quit  = .false.
         abort = .true.
10        continue
         ! check if error occurred in reading the coordinate file
         if (quit) then
            write (iout,"(A)") ' READXYZ  --  Error in Coordinate File ' &
                             //'in the number of atoms and title line'
            call fatal
         end if
         quit  = .true.
         abort = .false.
 
         ! parse the title line to get the number of atoms
         i    = 0
         next = 1
         call gettext (record,string,next)
         read (string,*)  n
         print "(A,I6)","Number of atoms =",n !NB diagnostic print

         ! extract the title and determine its length
         string = record(next:120)
         first  = nexttext (string)
         last   = trimtext (string)
         if (last .eq. 0) then
            title  = ' '
            ltitle = 0
         else
            title  = string(first:last) ! title
            ltitle = trimtext (title)   ! length of the title
         end if

         ! check we have more than 0 atoms 
         ! and the total atoms less than maxatms, NB necessary still?
         if (n .le. 0) then
            write (iout,*) ' READXYZ  --  The Coordinate File Does Not' &
                         //' Contain Any Atoms'
            call fatal
         else if (n .gt. maxatm) then
            ! NB this check no longer makes sense - no maximum.
            write (iout,"(A,i8,A)") ' READXYZ  --  The Maximum of',maxatm, &
                                    ' Atoms has been Exceeded'
            call fatal
         end if

    end if ! rank 0

    ! Broadcast the total number of atoms to other procs
    call MPI_Bcast(n, 1, MPI_INTEGER, 0, MPI_COMM_WORLD,ierror)

    ! Bradcast the length of the title
    call MPI_Bcast(ltitle, 1, MPI_INTEGER, 0, MPI_COMM_WORLD,ierror)

    ! Broadcast the title to the other procs
    call MPI_Bcast(title,ltitle,MPI_CHAR,0,MPI_COMM_WORLD,ierror)

    ! Determine local number of atoms
    dn = n/nprocs

    ! Assign atom remainder to lower rank processes if any
    if(rank.lt.MOD(n,nprocs)) then
      dn = dn +1
    endif

    ! Number of atoms to be allocated locally
    ! NB this is currently an assumption
    numatoms = 2*(dn+1)

    ! Allocate enough space to hold the atom list
    allocate(atomlist(numatoms))

    ! Print out the number of particles - diagnostic statement
    print "(A,I3,A,I6,A,I6,A)","Process ",rank," has ",dn, &
                               " atoms out of ",n," atoms."

    ! Initialize local atoms
    atomlist%tag    = 0
    atomlist%name   = '   '
    atomlist%type   = 0
    do i=1,numatoms
       atomlist(i)%pos = 0.0d0
       atomlist(i)%i12 = 0
    end do

    ! only process 0 reads the atoms from file and then redistributes
    if(rank.eq.0) then 

         ! Read atoms first
         do i = 1, dn
            size = 0
            do while (size .eq. 0)
               read (ixyz,"(a120)",err=80,end=80)  record
               !print "(A,I5,A,A)","Record ",i," =",record
               size = trimtext (record)
                              if (i .eq. 1) then
                  next = 1
                  call getword (record,atomlist(i)%name,next)
                  if (atomlist(i)%name .ne. '   ')  goto 20
                  print *,"Reading box parameters."; call flush
                  read (record,*,err=20,end=20) (boxparams(j),j=1,6)
                  size       = 0
                  xbox       = boxparams(1)
                  ybox       = boxparams(2)
                  zbox       = boxparams(3)
                  alpha      = boxparams(4)
                  beta       = boxparams(5)
                  gamma      = boxparams(6)
                  use_bounds = .true.
               end if
20             continue
            end do
            read (record,*,err=80,end=80) atomlist(i)%tag
            next = 1
            call getword (record,atomlist(i)%name,next)
            string = record(next:120)
            read (string,*,err=30,end=30) atomlist(i)%pos(1), &
                                          atomlist(i)%pos(2), &
                                          atomlist(i)%pos(3), &
                                          atomlist(i)%type,   &
                                          (atomlist(i)%i12(j),j=1,maxbonds)
30          continue
         end do

         ! Read atom information if we have more than one process
         if(nprocs.gt.1) then
            ! Position after last sorted atom
            end = dn+1

            ! Read atoms for the other processes
            do proc=1,nprocs-1
               ! Number of atoms process proc has
               nlocal = n/nprocs
               ! Add additional atoms if there is a remainder
               if(proc.lt.MOD(n,nprocs)) then
                  nlocal = nlocal + 1
               endif
               ! start and end point 
               start = end
               end   = start + nlocal - 1
               j = dn+1 ! Read new atoms just after local atoms and reuse
               !print *, proc,start,end 
               do i= start, end
                  size=0
                  do while (size .eq. 0)
                     read (ixyz,"(a120)",err=80,end=80)  record
                     size = trimtext (record)
                     read (record,*,err=80,end=80) atomlist(j)%tag
                     next = 1
                     call getword (record,atomlist(j)%name,next)
                     string = record(next:120)
                     read (string,*,err=40,end=40) atomlist(j)%pos(1), &
                                                   atomlist(j)%pos(2), &
                                                   atomlist(j)%pos(3), &
                                                   atomlist(j)%type,   &
                                               (atomlist(j)%i12(k),k=1,maxbonds)
40                   continue
                  end do ! end while
                  j = j+1 
               end do ! loop over atoms

               ! Send the atoms to process proc
               call MPI_Send(atomlist(dn+1),nlocal,AtomTypeComm, proc,  &
                             100,MPI_COMM_WORLD,ierror)

            end do ! loop over processes
         endif ! more than one process

         ! Read succeeded
         quit = .false.

80       continue ! if we come directly here something went wrong

         ! finished reading so close the input file
         close (unit=ixyz)

         ! an error occurred in reading the coordinate file
         if (quit) then
            write (iout,"(A,i6)") ' READXYZ  --  Error in Coordinate File ' &
                                //'at Atom', i
            call fatal
         end if
      else if(nprocs.gt.1) then

         ! Receive data from proc 0
         call MPI_Recv(atomlist(1),dn,AtomTypeComm,0,100,MPI_COMM_WORLD, &
                       status, ierror)

      end if ! Proc 0

      print "(A,I3,A,I6,A,I6,A,I6)","Process ",rank," has first atom ", &
             atomlist(1)%tag," and last ", atomlist(dn)%tag," dn =",dn

      ! Notify the other procs whether we are using bounds.
      call MPI_Bcast(use_bounds,1,MPI_LOGICAL,MPI_COMM_WORLD,0,ierror)

      ! Broadcast the box parameters if bounds are being used
      if(use_bounds.eqv..true.) then
        call MPI_Bcast(boxparams,6,MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierror)
        xbox       = boxparams(1)
        ybox       = boxparams(2)
        zbox       = boxparams(3)
        alpha      = boxparams(4)
        beta       = boxparams(5)
        gamma      = boxparams(6)
        call lattice 
      endif

      ! redistribute the atoms
      call redistribute

      ! No point in going on because the stuff below has not
      ! been converted.


! !     for each atom, count and sort its attached atoms
! !
!       do i = 1, n
!          n12(i) = 0
!          do j = maxbonds, 1, -1
!             if (i12(j,i) .ne. 0) then
!                n12(i) = j
!                goto 100
!             end if
!          end do
!   100    continue
!          call sort (n12(i),i12(1,i))
!       end do
! !
! !     perform dynamic allocation of some local arrays
! !
!       nmax = 0
!       do i = 1, n
!          nmax = max(tag(i),nmax)
!          do j = 1, n12(i)
!             nmax = max(i12(j,i),nmax)
!          end do
!       end do
!       allocate (list(nmax))
! !
! !     check for scrambled atom order and attempt to renumber
! !
!       reorder = .false.
!       do i = 1, n
!          list(tag(i)) = i
!          if (tag(i) .ne. i)  reorder = .true.
!       end do
!       if (reorder) then
!          write (iout,110)
!   110    format (/,' READXYZ  --  Atom Labels not Sequential,', &
!                     ' Attempting to Renumber')
!          do i = 1, n
!             tag(i) = i
!             do j = 1, n12(i)
!                i12(j,i) = list(i12(j,i))
!             end do
!             call sort (n12(i),i12(1,i))
!          end do
!       end if
! !
! !     perform deallocation of some local arrays
! !
!       deallocate (list)
! !
! !     check for atom pairs with identical coordinates
! !
!       clash = .false.
!       if (n .le. 10000)  call chkxyz (clash)
! !
! !     make sure that all connectivities are bidirectional
! !
!       do i = 1, n
!          do j = 1, n12(i)
!             k = i12(j,i)
!             do m = 1, n12(k)
!                if (i12(m,k) .eq. i)  goto 130
!             end do
!             write (iout,120)  k,i
!   120       format (/,' READXYZ  --  Check Connection of Atom', &
!                        i6,' to Atom',i6)
!             call fatal
!   130       continue
!          end do
!       end do
!       return

end subroutine readxyz
