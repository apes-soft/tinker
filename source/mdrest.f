c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine mdrest  --  stop system translation & rotation  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "mdrest" finds and removes any translational or rotational
c     kinetic energy of the overall system center of mass
c
c
      subroutine mdrest (istep)
      use sizes
      use atoms
      use inform
      use iounit
      use mdstuf
      use moldyn
      use units
      implicit none
      integer i,j,istep
      real*8 etrans
      real*8 weigh,totmass
      real*8 vtot(3)
c
c
c     check steps between center of mass motion removal
c
      if (mod(istep,irest) .ne. 0)  return
c
c     zero out the total mass and overall linear velocity
c
      totmass = 0.0d0
      do j = 1, 3
         vtot(j) = 0.0d0
      end do
c
c     compute linear velocity of the system center of mass
c
      do i = 1, n
         weigh = atom(i)%mass
         totmass = totmass + weigh
         do j = 1, 3
            vtot(j) = vtot(j) + v(j,i)*weigh
         end do
      end do
c
c     compute translational kinetic energy of overall system
c
      etrans = 0.0d0
      do j = 1, 3
         vtot(j) = vtot(j) / totmass
         etrans = etrans + vtot(j)**2
      end do
      etrans = 0.5d0 * etrans * totmass / convert
c
c     eliminate any translation of the overall system
c
      do i = 1, n
         do j = 1, 3
            v(j,i) = v(j,i) - vtot(j)
         end do
      end do
      return
      end
