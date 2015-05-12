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
      use bound
      use inform
      use iounit
      use keys
      use mdstuf
      use potent
      use solute
      use stodyn
      use usage
      use mpiparams
      implicit none
      integer:: provided ! Actual threading model provided by MPI
      integer i,istep,nstep
      integer mode,next
      real*8 dt,dtdump
      logical exist
      character*20 keyword
      character*120 record
      character*120 string

      ! Allow for the usage of multithreaded applications but MPI
      ! regions will be on a single thread
      call MPI_Init_thread(MPI_THREAD_FUNNELED, provided, ierror)

      ! Find out how many processes we have.
      call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierror)

      ! Find out my own rank.
      call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

      ! set up the structure and molecular mechanics calculation
      call initial
      call getxyz
      call mechanic

      ! initialize the temperature, pressure and coupling baths
      kelvin     = 0.0d0
      atmsph     = 0.0d0
      isothermal = .false.
      isobaric   = .false.

      ! check for keywords containing any altered parameters
      integrate = 'BEEMAN'
      do i = 1, nkey
         next = 1
         record = keyline(i)
         call gettext (record,keyword,next)
         call upcase (keyword)
         string = record(next:120)
         if (keyword(1:11) .eq. 'INTEGRATOR ') then
            call getword (record,integrate,next)
            call upcase (integrate)
         end if
      end do


      ! initialize the simulation length as number of time steps
      nstep = -1
      call nextarg (string,exist)
      if (exist)  read (string,*,err=10,end=10)  nstep
   10 continue

      if (nstep.lt.0) then
         if(rank.eq.0) then
           write(iout,*) "Need to specify the number of dynamic steps ",
     &                   "to be taken at the command line."
         end if
         call help
         call fatal(2)
      end if

      ! get the length of the dynamics time step in picoseconds
      dt = -1.0d0
      call nextarg (string,exist)
      if (exist)  read (string,*,err=40,end=40)  dt
   40 continue
      if (dt .lt. 0.0d0) then
         if(rank.eq.0) then
           write (iout,*) "The time step length in Femtoseconds ",
     &                    "must be specified at the command line."
         end if
         call help
         call fatal(3)
      end if

      dt = 0.001d0 * dt

      ! enforce bounds on thermostat and barostat coupling times
      tautemp = max(tautemp,dt)
      taupres = max(taupres,dt)

      ! set the time between trajectory snapshot coordinate dumps
      dtdump = -1.0d0
      call nextarg (string,exist)

      if (exist)  read (string,*,err=80,end=80)  dtdump
   80 continue
      if (dtdump .lt. 0.0d0) then
         if(rank.eq.0) then
           write (iout,*) "The time between dumps in Picoseconds ",
     &                    "must be specified at the command line."
         end if
         call help
         call fatal(4)
      end if

      iwrite = nint(dtdump/dt)

      ! get choice of statistical ensemble for periodic system
      if (use_bounds) then
         mode = -1
         call nextarg (string,exist)

         if (exist)  read (string,*,err=120,end=120)  mode
  120    continue
         if (mode.lt.1 .or. mode.gt.4) then
            if(rank.eq.0) then
               write (iout,*) "You need to specify a statistical ",
     &                        "mechanical ensembles (1-4):",
     &                        "(1) Microcanonical (NVE), ",
     &                        "(2) Canonical (NVT). ",
     &                        "(3) Isoenthalpic-Isobaric (NPH), ",
     &                        "(4) Isothermal-Isobaric (NPT)."
            end if
            call help
            call fatal(5)
         end if


         if (integrate.eq.'BUSSI' .or. integrate.eq.'NOSE-HOOVER'
     &                .or. integrate.eq.'GHMC') then
            if (mode .ne. 4) then
               mode = 4

               if(rank.eq.0) then 
                  write (iout,160)
  160             format (/,' Switching to NPT Ensemble as Required',
     &                      ' by Chosen Integrator')
               end if

            end if
         end if
         if (mode.eq.2 .or. mode.eq.4) then
            isothermal = .true.
            kelvin = -1.0d0
            call nextarg (string,exist)
            if (exist)  read (string,*,err=170,end=170)  kelvin
  170       continue
            if (kelvin .le. 0.0d0)  kelvin = 298.0d0
         end if
         if (mode.eq.3 .or. mode.eq.4) then
            isobaric = .true.
            atmsph = -1.0d0
            call nextarg (string,exist)
            if (exist)  read (string,*,err=210,end=210)  atmsph
  210       continue
            if (atmsph .le. 0.0d0)  atmsph = 1.0d0
         end if
      end if

      ! use constant energy or temperature for nonperiodic system
      ! 
      ! choices:
      ! 
      ! (1) Constant Total Energy Value (E)
      ! (2) Constant Temperature via Thermostat (T)
 
      if (.not. use_bounds) then
         mode = -1
         call nextarg (string,exist)
         if (exist)  read (string,*,err=250,end=250)  mode
  250    continue
         if (mode .le. 0)  mode = 1
 
         if (mode .eq. 2) then
            isothermal = .true.
            kelvin = -1.0d0
            call nextarg (string,exist)
            if (exist)  read (string,*,err=290,end=290)  kelvin
  290       continue
            if (kelvin .le. 0.0d0)  kelvin = 298.0d0
           end if
      end if

      ! initialize any holonomic constraints and setup dynamics
      call shakeup
      call mdinit
 
      ! print out a header line for the dynamics computation
      ! Only process 0 prints out.
      if(rank.eq.0) then
        if (integrate .eq. 'VERLET') then
           write (iout,330)
  330      format (/,' Molecular Dynamics Trajectory via',
     &               ' Velocity Verlet Algorithm')
        else if (integrate .eq. 'STOCHASTIC') then
           write (iout,340)
  340      format (/,' Stochastic Dynamics Trajectory via',
     &               ' Velocity Verlet Algorithm')
        else if (integrate .eq. 'BUSSI') then
           write (iout,350)
  350      format (/,' Molecular Dynamics Trajectory via',
     &               ' Bussi-Parrinello NPT Algorithm')
        else if (integrate .eq. 'NOSE-HOOVER') then
           write (iout,360)
  360      format (/,' Molecular Dynamics Trajectory via',
     &               ' Nose-Hoover NPT Algorithm')
        else if (integrate .eq. 'GHMC') then
           write (iout,370)
  370      format (/,' Stochastic Dynamics Trajectory via',
     &               ' Generalized Hybrid Monte Carlo')
        else if (integrate .eq. 'RIGIDBODY') then
           write (iout,380)
  380      format (/,' Molecular Dynamics Trajectory via',
     &               ' Rigid Body Algorithm')
        else if (integrate .eq. 'RESPA') then
           write (iout,390)
  390      format (/,' Molecular Dynamics Trajectory via',
     &               ' r-RESPA MTS Algorithm')
        else
           write (iout,400)
  400      format (/,' Molecular Dynamics Trajectory via',
     &               ' Modified Beeman Algorithm')
        end if
      end if ! conditional over rank 0

!      call system_clock(t2)

      ! integrate equations of motion to take a time step

      do istep = 1, nstep
         if (integrate .eq. 'VERLET') then
            call verlet (istep,dt)
         else if (integrate .eq. 'STOCHASTIC') then
            call sdstep (istep,dt)
         else if (integrate .eq. 'BUSSI') then
            call bussi (istep,dt)
         else if (integrate .eq. 'NOSE-HOOVER') then
            call nose (istep,dt)
         else if (integrate .eq. 'GHMC') then
            call ghmcstep (istep,dt)
         else if (integrate .eq. 'RIGIDBODY') then
            call rgdstep (istep,dt)
         else if (integrate .eq. 'RESPA') then
            call respa (istep,dt)
         else
            call beeman (istep,dt)
         end if
      end do

!      call system_clock(t3)

      ! perform any final tasks before program exit
      call final

      !call system_clock(t4)

!      print "(I2,A,F7.3,A,F7.3,A,F7.3)", rank, ",",dble(t2-t1)/tick,",",
!     &       dble(t3-t2)/tick,",",dble(t4-t3)/tick
      ! Finish the MPI execution
      call MPI_Finalize(ierror)

      end

