c
c
c     ###################################################
c     ##  COPYRIGHT (C)  2000  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ################################################################
c     ##                                                            ##
c     ##  subroutine replica  --  periodicity via cell replication  ##
c     ##                                                            ##
c     ################################################################
c
c
c     "replica" decides between images and replicates for generation
c     of periodic boundary conditions, and sets the cell replicate
c     list if the replicates method is to be used
c
c
      subroutine replica (cutoff)
      use sizes
      use boxes
      use cell
      use inform
      use iounit
      implicit none
      integer i,j,k
      integer nx,ny,nz
      real*8 cutoff,maximage
      real*8 xlimit,ylimit,zlimit
c
c     find the maximum sphere radius inscribed in periodic box
c
      xlimit = xbox2
      ylimit = ybox2
      zlimit = zbox2
      !else if (octahedron) then
      !   xlimit = (sqrt(3.0d0)/4.0d0) * xbox
      !   ylimit = xlimit
      !   zlimit = xlimit
      !end if
      maximage = min(xlimit,ylimit,zlimit)
c
c     use replicate method to handle cutoffs too large for images
c
      if (cutoff .gt. maximage) then
         write (iout,10)
   10    format (/,' Pairwise Neighbor List cannot',
     &              ' be used with Replicas')
         call fatal
      end if
c
c     truncated octahedron cannot use the replicates method
c
!      if (octahedron .and. use_replica) then
!         write (iout,10)
!   10    format (/,' REPLICA  --  Truncated Octahedron',
!     &              ' cannot be Replicated')
!         call fatal
!      end if
c
c     find the number of replicates needed based on cutoff
c
      nx = int(cutoff/xlimit)
      ny = int(cutoff/ylimit)
      nz = int(cutoff/zlimit)
      if (cutoff .gt. dble(nx)*xlimit)  nx = nx + 1
      if (cutoff .gt. dble(ny)*ylimit)  ny = ny + 1
      if (cutoff .gt. dble(nz)*zlimit)  nz = nz + 1
      if (nx .lt. 1)  nx = 1
      if (ny .lt. 1)  ny = 1
      if (nz .lt. 1)  nz = 1
c
c     set the replicated cell length and the half width
c
      xcell = dble(nx) * xbox
      ycell = dble(ny) * ybox
      zcell = dble(nz) * zbox
      xcell2 = 0.5d0 * xcell
      ycell2 = 0.5d0 * ycell
      zcell2 = 0.5d0 * zcell
c
c     check the total number of replicated unit cells
c
      ncell = nx*ny*nz - 1
      if (ncell .gt. maxcell) then
         write (iout,20)
   20    format (/,' REPLICA  --  Increase MAXCELL or Decrease',
     &              ' the Interaction Cutoffs')
         call fatal
      end if
c
c     assign indices to the required cell replicates
c
      ncell = 0
      do k = 0, nz-1
         do j = 0, ny-1
            do i = 0, nx-1
               if (k.ne.0 .or. j.ne.0 .or. i.ne.0) then
                  ncell = ncell + 1
                  icell(1,ncell) = i
                  icell(2,ncell) = j
                  icell(3,ncell) = k
               end if
            end do
         end do
      end do
      return
      end
