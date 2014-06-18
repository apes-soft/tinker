c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1993  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine unitcell  --  get periodic boundary conditions  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "unitcell" gets the periodic boundary box size and related
c     values from an external keyword file
c
c
      subroutine unitcell
      use sizes
      use boxes
      use iounit
      use keys
      implicit none
      integer i,next
      real*8 boxmax
      logical nosymm
      character*20 keyword
      character*120 record
      character*120 string
c
c     set the default values for the unitcell variables
c
      orthogonal = .true.
      monoclinic = .false.
      triclinic = .false.
      octahedron = .false.
      spacegrp = '          '
      nosymm = .false.
c
c     get keywords containing crystal lattice dimensions
c
      do i = 1, nkey
         next = 1
         record = keyline(i)
         call gettext (record,keyword,next)
         call upcase (keyword)
         string = record(next:120)
         if (keyword(1:7) .eq. 'X-AXIS ') then
            if (xbox .eq. 0.0d0)  read (string,*,err=10,end=10)  xbox
         else if (keyword(1:7) .eq. 'Y-AXIS ') then
            if (ybox .eq. 0.0d0)  read (string,*,err=10,end=10)  ybox
         else if (keyword(1:7) .eq. 'Z-AXIS ') then
            if (zbox .eq. 0.0d0)  read (string,*,err=10,end=10)  zbox
         else if (keyword(1:7) .eq. 'A-AXIS ') then
            if (xbox .eq. 0.0d0)  read (string,*,err=10,end=10)  xbox
         else if (keyword(1:7) .eq. 'B-AXIS ') then
            if (ybox .eq. 0.0d0)  read (string,*,err=10,end=10)  ybox
         else if (keyword(1:7) .eq. 'C-AXIS ') then
            if (zbox .eq. 0.0d0)  read (string,*,err=10,end=10)  zbox
         end if
   10    continue
      end do
c
c     use periodic boundary conditions if a cell was defined
c
      boxmax = max(xbox,ybox,zbox)
c
c     set unspecified periodic boundary box lengths and angles
c
      if (xbox .eq. 0.0d0)  xbox = boxmax
      if (ybox .eq. 0.0d0)  ybox = xbox
      if (zbox .eq. 0.0d0)  zbox = xbox
      alpha = 90.0d0
      beta = 90.0d0
      gamma = 90.0d0
c
c     check for proper use of truncated octahedron boundary
c
c     if (octahedron) then
c        if (xbox.eq.ybox .and. xbox.eq.zbox .and. orthogonal) then
c           orthogonal = .false.
c           monoclinic = .false.
c           triclinic = .false.
c        else
c           write (iout,20)
c  20       format (/,' UNITCELL  --  Truncated Octahedron',
c    &                 ' Incompatible with Defined Cell')
c           call fatal
c        end if
c     end if
      return
      end
