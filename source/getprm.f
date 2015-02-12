c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     #############################################################
c     ##                                                         ##
c     ##  subroutine getprm  --  get force field parameter file  ##
c     ##                                                         ##
c     #############################################################
c
c
c     "getprm" finds the potential energy parameter file
c     and then opens and reads the parameters
c
c
      subroutine getprm
      use files
      use iounit
      use keys
      use params
      use mpiparams
      implicit none
      integer i,iprm,next
      integer freeunit
      logical exist,useprm
      character*4 none
      character*20 keyword
      character*120 prmfile
      character*120 record
      character*120 string

      ! Only get process 0 to read the parameter file and
      ! then broadcast the result to the other processes
      if(rank.eq.0) then 

        ! set the default name for the parameter file
         useprm  = .true.
         prmfile = filename(1:leng)//'.prm'

        ! search the keyword list for the parameter filename
        do i = 1, nkey
           next = 1
           record = keyline(i)
           call gettext (record,keyword,next)
           call upcase (keyword)
           if (keyword(1:11) .eq. 'PARAMETERS ') then
              string = record(next:120)
              next = 1
              call getstring (string,prmfile,next)
              if (next .eq. 1)  call gettext (string,prmfile,next)
           end if
        end do

        ! check existence of default or specified parameter file
        call suffix (prmfile,'prm','old')
        inquire (file=prmfile,exist=exist)

        ! test for user specified absence of a parameter file
        if (.not. exist) then
           none = prmfile(1:4)
           call upcase (none)
           if (none .eq. 'NONE') then
              exist  = .true.
              useprm = .false.
           end if
        end if

        ! try to get a parameter filename from the command line
        if (.not. exist) then
           call nextarg (prmfile,exist)
           if (exist) then
              call suffix (prmfile,'prm','old')
              inquire (file=prmfile,exist=exist)
           end if
        end if

        ! if the potential paremter file does not exist fail
        if (.not. exist) then
            write (iout,*) "Potential parameter file not found."
            call fatal(55)
        end if

        ! initialize force field control and parameter values
        call initprm

        ! read the parameter file and store it for latter use
        nprm = 0
        if (useprm) then
           iprm = freeunit ()
           open (unit=iprm,file=prmfile,status='old')
           rewind (unit=iprm)
           do while (.true.)
              read (iprm,30,err=50,end=50)  record
   30         format (a120)
              nprm = nprm + 1
              prmline(nprm) = record
              if (nprm .ge. maxprm) then
                 write (iout,40)
   40            format (/,' GETPRM  --  Parameter File Too Large;',
     &                     ' Increase MAXPRM')
                 call fatal
              end if
           end do
   50      continue
           close (unit=iprm)
        end if

        ! broadcast the number of lines in the prm file
        call MPI_Bcast(nprm, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)

        ! broadcast the entire contents of the prmline array to other procs
        call MPI_Bcast(prmline, maxprm*120, MPI_CHARACTER, 0,
     &                 MPI_COMM_WORLD, ierror)

      else ! prcs > 0

        ! initialize force field control and parameter values
        call initprm

        ! receive the number of lines in the prm file
        call MPI_Bcast(nprm, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
 
        ! broadcast the entire contents of the prmline array to other procs
        call MPI_Bcast(prmline, maxprm*120, MPI_CHARACTER, 0,
     &                 MPI_COMM_WORLD, ierror)

      end if ! Conditional on processes

      ! get control and parameter values from the parameter file
      if (useprm)  call readprm

      return
      end
