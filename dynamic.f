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
      implicit none
      integer i,istep,nstep
      integer mode,next
      real*8 dt,dtdump
      logical exist,query
      character*20 keyword
      character*120 record
      character*120 string
      real*8 start,finish
      real*8 omp_get_wtime
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
      kelvin = 0.0d0
      atmsph = 0.0d0
      isothermal = .false.
      isobaric = .false.
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
      end if
   10 continue
      if (query) then
         write (iout,20)
   20    format (/,' Enter the Number of Dynamics Steps to be',
     &              ' Taken :  ',$)
         read (input,30)  nstep
   30    format (i10)
      end if
c
c     get the length of the dynamics time step in picoseconds
c
      dt = -1.0d0
      call nextarg (string,exist)
      if (exist)  read (string,*,err=40,end=40)  dt
   40 continue
      do while (dt .lt. 0.0d0)
         write (iout,50)
   50    format (/,' Enter the Time Step Length in Femtoseconds',
     &              ' [1.0] :  ',$)
         read (input,60,err=70)  dt
   60    format (f20.0)
         if (dt .le. 0.0d0)  dt = 1.0d0
   70    continue
      end do
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
      do while (dtdump .lt. 0.0d0)
         write (iout,90)
   90    format (/,' Enter Time between Dumps in Picoseconds',
     &              ' [0.1] :  ',$)
         read (input,100,err=110)  dtdump
  100    format (f20.0)
         if (dtdump .le. 0.0d0)  dtdump = 0.1d0
  110    continue
      end do
      iwrite = nint(dtdump/dt)
c
c     get choice of statistical ensemble for periodic system
c
      mode = 4
      isothermal = .true.
      kelvin = -1.0d0
      call nextarg (string,exist)
      if (exist)  read (string,*,err=170,end=170)  kelvin
  170 continue
      do while (kelvin .lt. 0.0d0)
         write (iout,180)
  180    format (/,' Enter the Desired Temperature in Degrees',
     &                    ' K [298] :  ',$)
         read (input,190,err=200)  kelvin
  190    format (f20.0)
         if (kelvin .le. 0.0d0)  kelvin = 298.0d0
  200    continue
      end do
      isobaric = .true.
      atmsph = -1.0d0
      call nextarg (string,exist)
      if (exist)  read (string,*,err=210,end=210)  atmsph
  210 continue
      do while (atmsph .lt. 0.0d0)
         write (iout,220)
  220    format (/,' Enter the Desired Pressure in Atm',
     &                    ' [1.0] :  ',$)
         read (input,230,err=240)  atmsph
  230    format (f20.0)
         if (atmsph .le. 0.0d0)  atmsph = 1.0d0
  240    continue
      end do
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
      !print*, "totmass", totmass, volbox
      !print*, x(1),a(1,1)
      !print*, y(1),a(2,1)
      !print*, z(1),a(3,1)
      !print*, x(2),a(1,2)
      !print*, y(2),a(2,2)
      !print*, z(2),a(3,2)
      !stop
      start = omp_get_wtime()
      do istep = 1, nstep
         call verlet (istep,dt)
      end do
      finish = omp_get_wtime()
      print*, "Dynamics complete in ", finish-start, " seconds."
c
c     perform any final tasks before program exit
c
      call final
      end
