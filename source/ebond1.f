c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ################################################################
c     ##                                                            ##
c     ##  subroutine ebond1  --  bond stretch energy & derivatives  ##
c     ##                                                            ##
c     ################################################################
c
c
c     "ebond1" calculates the bond stretching energy and
c     first derivatives with respect to Cartesian coordinates
c
c
      subroutine ebond1
      use sizes
      use atoms
      use bndpot
      use bndstr
      use bound
      use deriv
      use energi
      use group
      use usage
      use virial
      use openmp
      implicit none
      integer i,ia,ib,j
      real*8 e
      real*8 ideal,force
      real*8 expterm,bde,fgrp
      real*8 dt,dt2,deddt
      real*8 de,dedx,dedy,dedz
      real*8 xab,yab,zab,rab
      real*8 vxx,vyy,vzz
      real*8 vyx,vzx,vzy
      logical proceed
 

      vir_tmp = 0.0d0

c
c     set OpenMP directives for the major loop structure
c

!$OMP DO private(ia, ib, ideal, force, proceed, fgrp,xab,yab,
!$OMP& zab,rab,dt,dt2,e,deddt,expterm,bde,de,dedx,dedy,dedz,vxx,
!$OMP& vyx,vzx,vyy,vzy,vzz)  schedule(guided)
c
c     calculate the bond stretch energy and first derivatives
c
      do i = 1, nbond
         ia = ibnd(1,i)
         ib = ibnd(2,i)
         ideal = bl(i)
         force = bk(i)
c
c     decide whether to compute the current interaction
c
         proceed = .true.
         if (use_group)  call groups (proceed,fgrp,ia,ib,0,0,0,0)
         if (proceed)  proceed = (use(ia) .or. use(ib))
c
c     compute the value of the bond length deviation
c
         if (proceed) then
            xab = x(ia) - x(ib)
            yab = y(ia) - y(ib)
            zab = z(ia) - z(ib)
            if (use_polymer)  call image (xab,yab,zab)
            rab = sqrt(xab*xab + yab*yab + zab*zab)
            dt = rab - ideal
c
c     harmonic potential uses Taylor expansion of Morse potential
c     through the fourth power of the bond length deviation
c
            if (bndtyp .eq. 'HARMONIC') then
               dt2 = dt * dt
               e = bndunit * force * dt2 * (1.0d0+cbnd*dt+qbnd*dt2)
               deddt = 2.0d0 * bndunit * force * dt
     &                    * (1.0d0+1.5d0*cbnd*dt+2.0d0*qbnd*dt2)
c
c     Morse potential uses energy = BDE * (1 - e**(-alpha*dt))**2)
c     with the approximations alpha = sqrt(ForceConst/BDE) = -2
c     and BDE = Bond Dissociation Energy = ForceConst/alpha**2
c
            else if (bndtyp .eq. 'MORSE') then
               expterm = exp(-2.0d0*dt)
               bde = 0.25d0 * bndunit * force
               e = bde * (1.0d0-expterm)**2
               deddt = 4.0d0 * bde * (1.0d0-expterm) * expterm
            end if
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
            if (rab .eq. 0.0d0) then
               de = 0.0d0
            else
               de = deddt / rab
            end if
            dedx = de * xab
            dedy = de * yab
            dedz = de * zab
c
c     increment the total bond energy and first derivatives
c
            en_th(th_id) = en_th(th_id) + e

            drv_th(th_id,1,ia) = drv_th(th_id,1,ia) + dedx
            drv_th(th_id,2,ia) = drv_th(th_id,2,ia) + dedy
            drv_th(th_id,3,ia) = drv_th(th_id,3,ia) + dedz
            drv_th(th_id,1,ib) = drv_th(th_id,1,ib) - dedx
            drv_th(th_id,2,ib) = drv_th(th_id,2,ib) - dedy
            drv_th(th_id,3,ib) = drv_th(th_id,3,ib) - dedz
c
c     increment the internal virial tensor components
c
            vxx = xab * dedx
            vyx = yab * dedx
            vzx = zab * dedx
            vyy = yab * dedy
            vzy = zab * dedy
            vzz = zab * dedz
           
            vir_tmp(1,1) = vir_tmp(1,1) + vxx
            vir_tmp(2,1) = vir_tmp(2,1) + vyx          
            vir_tmp(3,1) = vir_tmp(3,1) + vzx

            vir_tmp(1,2) = vir_tmp(1,2) + vyx
            vir_tmp(2,2) = vir_tmp(2,2) + vyy
            vir_tmp(3,2) = vir_tmp(3,2) + vzy

            vir_tmp(1,3) = vir_tmp(1,3) + vzx
            vir_tmp(2,3) = vir_tmp(2,3) + vzy
            vir_tmp(3,3) = vir_tmp(3,3) + vzz   

         end if
      end do
c
c     end OpenMP directives for the major loop structure
c
!$OMP END DO 
!no wait

      do i=1,3
         do j=1,3
            vir_th(th_id,j,i) = vir_th(th_id,j,i) +  vir_tmp(j,i)
         end do
      end do
c      vir_tmp = 0.0d0

     
      return
      end
