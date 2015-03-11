c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1996  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine getkey  --  find and store contents of keyfile  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "getkey" finds a valid keyfile and stores its contents as
c     line images for subsequent keyword parameter searching
c
c
      subroutine getkey
      use argue
      use files
      use iounit
      use keys
      use openmp
      use mpiparams
      implicit none
      integer i,ikey
      integer next,length
      integer freeunit
      integer trimtext
      logical exist,header
      character*20 keyword
      character*120 keyfile
      character*120 comment
      character*120 record
      character*120 string

      ! only proc 0 reads the file
      if(rank.eq.0) then

        ! check for a keyfile specified on the command line
        exist = .false.
        do i = 1, narg-1
           string = arg(i)
           call upcase (string)
           if (string(1:2) .eq. '-K') then
              keyfile = arg(i+1)
              call suffix (keyfile,'key','old')
              inquire (file=keyfile,exist=exist)
              if (.not. exist) then
                 write (iout,10)
   10            format (/,' GETKEY  --  Keyfile Specified',
     &                    ' on Command Line was not Found')
                 call fatal
              end if
           end if
        end do

        ! try to get keyfile from base name of current system
        if (.not. exist) then
           keyfile = filename(1:leng)//'.key'
           call version (keyfile,'old')
           inquire (file=keyfile,exist=exist)
        end if

        ! check for the existence of a generic keyfile
        if (.not. exist) then
           if (ldir .eq. 0) then
              keyfile = 'tinker.key'
           else
              keyfile = filename(1:ldir)//'tinker.key'
           end if
           call version (keyfile,'old')
           inquire (file=keyfile,exist=exist)
        end if

        ! read the keyfile and store it for latter use
        nkey = 0
        if (exist) then
           ikey = freeunit ()
           open (unit=ikey,file=keyfile,status='old')
           rewind (unit=ikey)
           do while (.true.)
              read (ikey,20,err=40,end=40)  record
   20         format (a120)
              nkey = nkey + 1
              keyline(nkey) = record
              if (nkey .ge. maxkey) then
                 write (iout,30)
   30            format (/,' GETKEY  --  Keyfile Too Large;',
     &                     ' Increase MAXKEY')
                 call fatal
              end if
           end do
   40      continue
           close (unit=ikey)
        end if

        ! check for comment lines to be echoed to the output
        header = .true.
        do i = 1, nkey
           next = 1
           record = keyline(i)
           call gettext (record,keyword,next)
           call upcase (keyword)
           if (keyword(1:5) .eq. 'ECHO ') then
              comment = record(next:120)
              length = trimtext (comment)
              if (header) then
                 header = .false.
                 write (iout,50)
   50            format ()
              end if
              if (length .eq. 0) then
                 write (iout,60)
   60            format ()
              else
                 write (iout,70)  comment(1:length)
   70            format (a)
              end if
           end if
        end do

      end if ! End if rank 0

      ! broadcast the number of lines in the key file to other procs
      call MPI_Bcast(nkey, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)

      ! broadcast the contents of the keyline array to other procs
      call MPI_Bcast(keyline, maxkey*120, MPI_CHARACTER, 0,
     &               MPI_COMM_WORLD, ierror)

      ! set number of threads for OpenMP parallelization
      do i = 1, nkey
         next = 1
         record = keyline(i)
         call upcase (record)
         call gettext (record,keyword,next)
         string = record(next:120)
         if (keyword(1:15) .eq. 'OPENMP-THREADS ') then
            read (string,*,err=80,end=80)  nthread
!$          call omp_set_num_threads (nthread)
         end if
   80    continue
      end do
      return
      end
