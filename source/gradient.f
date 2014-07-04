c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ##################################################################
c     ##                                                              ##
c     ##  subroutine gradient  --  find energy & gradient components  ##
c     ##                                                              ##
c     ##################################################################
c
c
c     "gradient" calls subroutines to calculate the potential energy
c     and first derivatives with respect to Cartesian coordinates
c
c
      subroutine gradient (energy,derivs)
      use sizes
      use atoms
      use deriv
      use energi
      use iounit
      use limits
      use vdwpot
      use virial
      implicit none
      integer i,j
      real*8 energy
      real*8 derivs(3,*)
      logical first
      save first
      data first  / .true. /
c
c
c     zero out each of the potential energy components
c
      eb = 0.0d0
      ea = 0.0d0
      eba = 0.0d0
      eub = 0.0d0
      eopb = 0.0d0
      et = 0.0d0
      ept = 0.0d0
      ett = 0.0d0
      ev = 0.0d0
      em = 0.0d0
      ep = 0.0d0
c
c     perform dynamic allocation of some global arrays
c
      if (first) then
         first = .false.
         if (.not. allocated(desum))  allocate (desum(3,n))
         if (.not. allocated(deb))  allocate (deb(3,n))
         if (.not. allocated(dea))  allocate (dea(3,n))
         if (.not. allocated(deba))  allocate (deba(3,n))
         if (.not. allocated(deub))  allocate (deub(3,n))
         if (.not. allocated(deopb))  allocate (deopb(3,n))
         if (.not. allocated(det))  allocate (det(3,n))
         if (.not. allocated(dept))  allocate (dept(3,n))
         if (.not. allocated(dett))  allocate (dett(3,n))
         if (.not. allocated(dev))  allocate (dev(3,n))
         if (.not. allocated(dem))  allocate (dem(3,n))
         if (.not. allocated(dep))  allocate (dep(3,n))
      end if
c
c     zero out each of the first derivative components
c
      do i = 1, n
         do j = 1, 3
            deb(j,i) = 0.0d0
            dea(j,i) = 0.0d0
            deba(j,i) = 0.0d0
            deub(j,i) = 0.0d0
            deopb(j,i) = 0.0d0
            det(j,i) = 0.0d0
            dept(j,i) = 0.0d0
            dett(j,i) = 0.0d0
            dev(j,i) = 0.0d0
            dem(j,i) = 0.0d0
            dep(j,i) = 0.0d0
         end do
      end do
c
c     zero out the virial and the intermolecular energy
c
      do i = 1, 3
         do j = 1, 3
            vir(j,i) = 0.0d0
         end do
      end do
c
c     maintain any periodic boundary conditions
c
      call bounds
c
c     update the pairwise interaction neighbor lists
c
      call nblist
c
c     call the local geometry energy and gradient routines
c
      call ebond1
      call eangle1
      call estrbnd1
      call eurey1
      call eopbend1
      call etors1
      call epitors1
      call etortor1
c
c     call the van der Waals energy and gradient routines
c
      call ehal1
c
c     call the electrostatic energy and gradient routines
c
      call empole1
c
c     sum up to get the total energy and first derivatives
c
      esum = eb + ea + eba + eub + eopb + et + ept  + ett + ev + em + ep
      energy = esum
      do i = 1, n
         do j = 1, 3
            desum(j,i) = deb(j,i) + dea(j,i) + deba(j,i) + deub(j,i)
     &                 + deopb(j,i) + det(j,i) + dept(j,i) + dett(j,i)
     &                 + dev(j,i) + dem(j,i) + dep(j,i)
            derivs(j,i) = desum(j,i)
         end do
      end do
c
c     check for an illegal value for the total energy
c
      if (isnan(esum)) then
         write (iout,10)
   10    format (/,' GRADIENT  --  Illegal Value for the Total',
     &              ' Potential Energy')
         call fatal
      end if
      return
      end
