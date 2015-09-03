c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1993  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ################################################################
c     ##                                                            ##
c     ##  subroutine eurey1  --  bond stretch energy & derivatives  ##
c     ##                                                            ##
c     ################################################################
c
c
c     "eurey1" calculates the Urey-Bradley interaction energy and
c     its first derivatives with respect to Cartesian coordinates
c
c
      subroutine eurey1
      use sizes
      use atoms
      use bound
      use deriv
      use energi
      use group
      use urey
      use urypot
      use usage
      use virial
      use openmp
      implicit none
      integer i,ia,ic
      real*8 e
      real*8 de,ideal,force
      real*8 dt,dt2,deddt,fgrp
      real*8 dedx,dedy,dedz
      real*8 vxx,vyy,vzz
      real*8 vyx,vzx,vzy
      real*8 xac,yac,zac,rac
      logical proceed

c
c     set OpenMP directives for the major loop structure
c

!$OMP DO private(ia,ic,ideal,force,proceed,fgrp,xac,yac,zac,rac,
!$OMP& dt,dt2,e,deddt,de,dedx,dedy,dedz,vxx,vyx,vzx,vyy,vzy,vzz)
!$OMP&  schedule(guided)
c
c     calculate the Urey-Bradley 1-3 energy and first derivatives
c
      do i = 1, nurey
         ia = iury(1,i)
         ic = iury(3,i)
         ideal = ul(i)
         force = uk(i)
c
c     decide whether to compute the current interaction
c
         proceed = .true.
         if (use_group)  call groups (proceed,fgrp,ia,ic,0,0,0,0)
         if (proceed)  proceed = (use(ia) .or. use(ic))
c
c     compute the value of the 1-3 distance deviation
c
         if (proceed) then
            xac = x(ia) - x(ic)
            yac = y(ia) - y(ic)
            zac = z(ia) - z(ic)
            if (use_polymer)  call image (xac,yac,zac)
            rac = sqrt(xac*xac + yac*yac + zac*zac)
            dt = rac - ideal
            dt2 = dt * dt
            e = ureyunit * force * dt2 * (1.0d0+cury*dt+qury*dt2)
            deddt = 2.0d0 * ureyunit * force * dt
     &                 * (1.0d0+1.5d0*cury*dt+2.0d0*qury*dt2)
c
c     scale the interaction based on its group membership
c
            if (use_group) then
               e = e * fgrp
               deddt = deddt * fgrp
            end if
c
c     compute chain rule terms needed for derivatives
c
            de = deddt / rac
            dedx = de * xac
            dedy = de * yac
            dedz = de * zac
c
c     increment the total Urey-Bradley energy and first derivatives
c
            en_th(th_id) = en_th(th_id) + e
            drv_th(th_id,1,ia) = drv_th(th_id,1,ia) + dedx
            drv_th(th_id,2,ia) = drv_th(th_id,2,ia) + dedy
            drv_th(th_id,3,ia) = drv_th(th_id,3,ia) + dedz
            drv_th(th_id,1,ic) = drv_th(th_id,1,ic) - dedx
            drv_th(th_id,2,ic) = drv_th(th_id,2,ic) - dedy
            drv_th(th_id,3,ic) = drv_th(th_id,3,ic) - dedz
c
c     increment the internal virial tensor components
c
            vxx = xac * dedx
            vyx = yac * dedx
            vzx = zac * dedx
            vyy = yac * dedy
            vzy = zac * dedy
            vzz = zac * dedz
            vir_th(th_id,1,1) = vir_th(th_id,1,1) + vxx
            vir_th(th_id,2,1) = vir_th(th_id,2,1) + vyx
            vir_th(th_id,3,1) = vir_th(th_id,3,1) + vzx
            vir_th(th_id,1,2) = vir_th(th_id,1,2) + vyx
            vir_th(th_id,2,2) = vir_th(th_id,2,2) + vyy
            vir_th(th_id,3,2) = vir_th(th_id,3,2) + vzy
            vir_th(th_id,1,3) = vir_th(th_id,1,3) + vzx
            vir_th(th_id,2,3) = vir_th(th_id,2,3) + vzy
            vir_th(th_id,3,3) = vir_th(th_id,3,3) + vzz
         end if
      end do
c
c     end OpenMP directives for the major loop structure
c
!$OMP END DO no wait

      return
      end
