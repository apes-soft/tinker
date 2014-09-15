c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     #############################################################
c     ##                                                         ##
c     ##  subroutine verlet  --  Verlet molecular dynamics step  ##
c     ##                                                         ##
c     #############################################################
c
c
c     "verlet" performs a single molecular dynamics time step
c     via the velocity Verlet multistep recursion formula
c
c
      subroutine verlet (istep,dt)
      use sizes
      use atomid
      use atoms
      use moldyn
      use units
      implicit none
      integer i,j,istep
      real*8 dt,dt_2
      real*8 etot,epot
      real*8 eksum
      real*8 temp,pres
      real*8 ekin(3,3)
      real*8 stress(3,3)
      real*8, allocatable :: derivs(:,:)
c
c
c     set some time values for the dynamics integration
c
      dt_2 = 0.5d0 * dt
c
c     make half-step temperature and pressure corrections
c
      call temper (dt)
c
c     perform dynamic allocation of some local arrays
c
      allocate (derivs(3,n))
c
c     store the current atom positions, then find half-step
c     velocities and full-step positions via Verlet recursion
c
      do i = 1, n
         do j = 1, 3
            v(j,i) = v(j,i) + a(j,i)*dt_2
         end do
         atom(i)%pos(1) = atom(i)%pos(1) + v(1,i)*dt
         atom(i)%pos(2) = atom(i)%pos(2) + v(2,i)*dt
         atom(i)%pos(3) = atom(i)%pos(3) + v(3,i)*dt
      end do
c
c     get the potential energy and atomic forces
c
      call gradient (epot,derivs)
c
c     use Newton's second law to get the next accelerations;
c     find the full-step velocities using the Verlet recursion
c
      do i = 1, n
         do j = 1, 3
            a(j,i) = -convert * derivs(j,i) / mass(i)
            v(j,i) = v(j,i) + a(j,i)*dt_2
         end do
      end do
c
c     perform deallocation of some local arrays
c
      deallocate (derivs)
c
c     make full-step temperature and pressure corrections
c
      call temper2 (dt,eksum,ekin,temp)
      call pressure (dt,epot,ekin,temp,pres,stress)
c
c     total energy is sum of kinetic and potential energies
c
      etot = eksum + epot
c
c     compute statistics and save trajectory for this step
c
      call mdstat (istep,dt,etot,epot,eksum,temp,pres)
      call mdsave (istep,dt,epot,eksum)
      call mdrest (istep)
      return
      end
