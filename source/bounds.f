c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine bounds  --  check periodic boundary conditions  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "bounds" finds the center of mass of each molecule and
c     translates any stray molecules back into the periodic box
c
c
      subroutine bounds
      use sizes
      use atomid
      use atoms
      use boxes
      use molcul
      implicit none
      integer i,j,k
      integer init,stop
      real*8 weigh
      real*8 xmid,ymid,zmid
      real*8 xfrac,yfrac,zfrac
      real*8 xcom,ycom,zcom
c
c
c     locate the center of mass of each molecule
c
      do i = 1, nmol
         init = imol(1,i)
         stop = imol(2,i)
         xmid = 0.0d0
         ymid = 0.0d0
         zmid = 0.0d0
         do j = init, stop
            k = kmol(j)
            weigh = mass(k)
            xmid = xmid + x(k)*weigh
            ymid = ymid + y(k)*weigh
            zmid = zmid + z(k)*weigh
         end do
         weigh = molmass(i)
         xmid = xmid / weigh
         ymid = ymid / weigh
         zmid = zmid / weigh
c
c     get fractional coordinates of center of mass
c
c        if (orthogonal .or. octahedron) then
            zfrac = zmid
            yfrac = ymid
            xfrac = xmid
c        else if (monoclinic) then
c           zfrac = zmid / beta_sin
c           yfrac = ymid
c           xfrac = xmid - zfrac*beta_cos
c        else if (triclinic) then
c           zfrac = zmid / gamma_term
c           yfrac = (ymid - zfrac*beta_term) / gamma_sin
c           xfrac = xmid - yfrac*gamma_cos - zfrac*beta_cos
c        end if
c
c     translate center of mass into the periodic box
c
         do while (xfrac .gt. xbox2)
            xfrac = xfrac - xbox
         end do
         do while (xfrac .lt. -xbox2)
            xfrac = xfrac + xbox
         end do
         do while (yfrac .gt. ybox2)
            yfrac = yfrac - ybox
         end do
         do while (yfrac .lt. -ybox2)
            yfrac = yfrac + ybox
         end do
         do while (zfrac .gt. zbox2)
            zfrac = zfrac - zbox
         end do
         do while (zfrac .lt. -zbox2)
            zfrac = zfrac + zbox
         end do
c
c     truncated octahedron needs to have corners removed
c
c        if (octahedron) then
c           if (abs(xfrac)+abs(yfrac)+abs(zfrac) .gt. box34) then
c              xfrac = xfrac - sign(xbox2,xfrac)
c              yfrac = yfrac - sign(ybox2,yfrac)
c              zfrac = zfrac - sign(zbox2,zfrac)
c           end if
c        end if
c
c     convert translated fraction center of mass to Cartesian
c
c        if (orthogonal .or. octahedron) then
            xcom = xfrac
            ycom = yfrac
            zcom = zfrac
c        else if (monoclinic) then
c           xcom = xfrac + zfrac*beta_cos
c           ycom = yfrac
c           zcom = zfrac * beta_sin
c        else if (triclinic) then
c           xcom = xfrac + yfrac*gamma_cos + zfrac*beta_cos
c           ycom = yfrac*gamma_sin + zfrac*beta_term
c           zcom = zfrac * gamma_term
c        end if
c
c     translate coordinates via offset from center of mass
c
         do j = init, stop
            k = kmol(j)
            x(k) = x(k) - xmid + xcom
            y(k) = y(k) - ymid + ycom
            z(k) = z(k) - zmid + zcom
         end do
      end do
      return
      end
