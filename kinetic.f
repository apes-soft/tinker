c
c
c     ###################################################
c     ##  COPYRIGHT (C)  2001  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine kinetic  --  compute kinetic energy components  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "kinetic" computes the total kinetic energy and kinetic energy
c     contributions to the pressure tensor by summing over velocities
c
c
      subroutine kinetic (eksum,ekin)
      use sizes
      use atomid
      use atoms
      use bath
      use mdstuf
      use moldyn
      use units
      implicit none
      integer i,j,k
      integer start,stop
      real*8 eksum
      real*8 term,value
      real*8 ekin(3,3)
      real*8 inert(3,3)
c
c
c     zero out the total kinetic energy and its outer product
c
      eksum = 0.0d0
      do i = 1, 3
         do j = 1, 3
            ekin(j,i) = 0.0d0
         end do
      end do
c
c     get the total kinetic energy and tensor for atomic sites
c
      do i = 1, n
         term = 0.5d0 * mass(i) / convert
         do j = 1, 3
            do k = 1, 3
               value = term * v(j,i) * v(k,i)
               ekin(k,j) = ekin(k,j) + value
            end do
         end do
      end do
      eksum = ekin(1,1) + ekin(2,2) + ekin(3,3)
      return
      end
