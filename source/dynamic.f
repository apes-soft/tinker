c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     #################################################################
c     ##                                                             ##
c     ##  program dynamic  --  run molecular or stochastic dynamics  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "dynamic" computes a molecular or stochastic dynamics trajectory
c     in one of the standard statistical mechanical ensembles and using
c     any of several possible integration methods
c
c
      program dynamic
      use sizes
      use atoms
      use bath
      use bndstr
      use inform
      use iounit
      use keys
      use mdstuf
      use parallelparams
      implicit none
      integer:: provided ! Actual threading model provided by MPI
      integer istep,nstep
      integer mode
      real*8 dt,dtdump
      logical exist,query
      character*120 string

      ! Allow for the usage of multithreaded applications but MPI
      ! regions will be on a single thread
      call MPI_Init_thread(MPI_THREAD_FUNNELED, provided, ierror)

      ! Find out how many processes we have.
      call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierror)

      ! Find out my own rank.
      call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

      ! Check that we are using one or a power of 2 number of processes,
      ! nprocs should never be 0
      if((nprocs.ne.1).and.(iand(nprocs,nprocs-1).ne.0)) then
         write(iout,*) "Number of processes must be a power of 2."
         call fatal
      end if

c
c
c     set up the structure and molecular mechanics calculation
c
      call initial
      call getxyz
      call mechanic
c
c     initialize the temperature, pressure and coupling baths
c
      kelvin     = 0.0d0
      atmsph     = 0.0d0
      isothermal = .false.
      isobaric   = .false.
c
c     check for keywords containing any altered parameters
c
      integrate = 'VERLET'
c
c     initialize the simulation length as number of time steps
c
      query = .true.
      call nextarg (string,exist)
      if (exist) then
         read (string,*,err=10,end=10)  nstep
         query = .false.
      else
         write(iout,*) "Need to specify the number of dynamic steps ", 
     &                 "to be taken at the command line."
         call usage
         call fatal
      end if
   10 continue

c
c     get the length of the dynamics time step in picoseconds
c
      dt = -1.0d0
      call nextarg (string,exist)
      if (exist) read (string,*,err=40,end=40)  dt
   40 continue
      if (dt .lt. 0.0d0) then
         write (iout,*) "The time step length in Femtoseconds ",
     &                  "must be specified at the command line."
         call usage
         call fatal
      end if
      dt = 0.001d0 * dt

c
c     enforce bounds on thermostat and barostat coupling times
c
      tautemp = max(tautemp,dt)
      taupres = max(taupres,dt)
c
c     set the time between trajectory snapshot coordinate dumps
c
      dtdump = -1.0d0
      call nextarg (string,exist)
      if (exist)  read (string,*,err=80,end=80)  dtdump
   80 continue
      if (dtdump .lt. 0.0d0) then
         write (iout,*) "The time between dumps in Picoseconds ",
     &                  "must be specified at the command line."
         call usage
         call fatal
      end if
      iwrite = nint(dtdump/dt)
c 
c
c     get choice of statistical ensemble for periodic system
c
      mode       = 4
      isothermal = .true.
      kelvin     = -1.0d0
      call nextarg (string,exist)
      if (exist)  read (string,*,err=170,end=170)  kelvin
  170 continue
      if  (kelvin .lt. 0.0d0) then
         write (iout,*) "The Desired Temperature in Degrees K ",
     &                  "must be specified at the command line."
         call usage
         call fatal
      end if 

      isobaric = .true.
      atmsph   = -1.0d0

      call nextarg (string,exist)
      if (exist)  read (string,*,err=210,end=210)  atmsph
  210 continue
      if (atmsph .lt. 0.0d0) then
         write(iout,*) "The Desired Pressure in Atm ",
     &                 "must be specified at the command line."
         call usage
         call fatal 
      end if

c
c     initialize any holonomic constraints and setup dynamics
c
      call mdinit
c
c     print out a header line for the dynamics computation
c
      write (iout,330)
  330 format (/,' Molecular Dynamics Trajectory via',
     &              ' Velocity Verlet Algorithm')
c
c     integrate equations of motion to take a time step
c
      do istep = 1, nstep
         call verlet (istep,dt)
      end do
c
c     perform any final tasks before program exit
c
      call final

      ! Finish the MPI execution
      call MPI_Finalize(ierror)

      end
