c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ################################################################
c     ##                                                            ##
c     ##  subroutine image  --  compute the minimum image distance  ##
c     ##                                                            ##
c     ################################################################
c
c
c     "image" takes the components of pairwise distance between
c     two points in a periodic box and converts to the components
c     of the minimum image distance
c
c
      subroutine image (xr,yr,zr)
      use sizes
      use boxes
      use cell
      implicit none
      real*8 xr,yr,zr
c
c
c     for orthogonal lattice, find the desired image directly
c
      do while (abs(xr) .gt. xcell2)
         xr = xr - sign(xcell,xr)
      end do
      do while (abs(yr) .gt. ycell2)
         yr = yr - sign(ycell,yr)
      end do
      do while (abs(zr) .gt. zcell2)
         zr = zr - sign(zcell,zr)
      end do

c
c     for truncated octahedron, use orthogonal box equations,
c     then perform extra tests to remove corner pieces
c
      !else if (octahedron) then
      !   do while (abs(xr) .gt. xbox2)
      !      xr = xr - sign(xbox,xr)
      !   end do
      !   do while (abs(yr) .gt. ybox2)
      !      yr = yr - sign(ybox,yr)
      !   end do
      !   do while (abs(zr) .gt. zbox2)
      !      zr = zr - sign(zbox,zr)
      !   end do
      !   if (abs(xr)+abs(yr)+abs(zr) .gt. box34) then
      !      xr = xr - sign(xbox2,xr)
      !      yr = yr - sign(ybox2,yr)
      !      zr = zr - sign(zbox2,zr)
      !   end if
      !end if
      return
      end
