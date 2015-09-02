c
c
c     ###################################################
c     ##  COPYRIGHT (C)  2003  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine epitors1  --  pi-orbit torsion energy & derivs  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "epitors1" calculates the pi-orbital torsion potential energy
c     and first derivatives with respect to Cartesian coordinates
c
c
      subroutine epitors1
      use sizes
      use atoms
      use bound
      use deriv
      use energi
      use group
      use pitors
      use torpot
      use usage
      use virial
      use openmp
      implicit none
      integer i,ia,ib,ic
      integer id,ie,ig
      real*8 e,dedphi,fgrp
      real*8 xt,yt,zt,rt2
      real*8 xu,yu,zu,ru2
      real*8 xtu,ytu,ztu
      real*8 rdc,rtru
      real*8 v2,c2,s2
      real*8 phi2,dphi2
      real*8 sine,cosine
      real*8 sine2,cosine2
      real*8 xia,yia,zia
      real*8 xib,yib,zib
      real*8 xic,yic,zic
      real*8 xid,yid,zid
      real*8 xie,yie,zie
      real*8 xig,yig,zig
      real*8 xip,yip,zip
      real*8 xiq,yiq,ziq
      real*8 xad,yad,zad
      real*8 xbd,ybd,zbd
      real*8 xec,yec,zec
      real*8 xgc,ygc,zgc
      real*8 xcp,ycp,zcp
      real*8 xdc,ydc,zdc
      real*8 xqd,yqd,zqd
      real*8 xdp,ydp,zdp
      real*8 xqc,yqc,zqc
      real*8 dedxt,dedyt,dedzt
      real*8 dedxu,dedyu,dedzu
      real*8 dedxia,dedyia,dedzia
      real*8 dedxib,dedyib,dedzib
      real*8 dedxic,dedyic,dedzic
      real*8 dedxid,dedyid,dedzid
      real*8 dedxie,dedyie,dedzie
      real*8 dedxig,dedyig,dedzig
      real*8 dedxip,dedyip,dedzip
      real*8 dedxiq,dedyiq,dedziq
      real*8 vxterm,vyterm,vzterm
      real*8 vxx,vyy,vzz
      real*8 vyx,vzx,vzy
      logical proceed

c
c     calculate the pi-orbital torsion angle energy term
c

!$OMP DO  private(ia,ib,ic,id,ie,ig,proceed,
!$OMP& fgrp,xia,yia,zia,xib,yib,zib,xic,yic,zic,xid,yid,zid,xie,
!$OMP& yie,zie,xig,yig,zig,xad,yad,zad,xbd,ybd,zbd,xec,yec,zec,
!$OMP& xgc,ygc,zgc,xip,yip,zip,xiq,ziq,yiq,xcp,ycp,zcp,xdc,ydc,
!$OMP& zdc,xqd,yqd,zqd,xt,yt,zt,xu,yu,zu,xtu,ytu,ztu,rt2,ru2,rtru,
!$OMP& rdc,cosine,sine,v2,c2,s2,cosine2,sine2,phi2,dphi2,e,dedphi,
!$OMP& xdp,ydp,zdp,xqc,yqc,zqc,dedxt,dedyt,dedzt,dedxu,dedyu,dedzu,
!$OMP& dedxip,dedyip,dedzip,dedxic,dedyic,dedzic,dedxid,dedyid,
!$OMP& dedzid,dedxiq,dedyiq,dedziq,dedzia,dedxib,dedyib,dedxia,
!$OMP& dedzib,dedxie,dedyie,dedzie,dedxig,dedyig,dedzig,vxterm,vyterm,
!$OMP& vzterm,vxx,vyx,vzx,vyy,vzy,dedyia,vzz)
!$OMP& firstprivate(th_id)  schedule(guided)


      do i = 1, npitors
         ia = ipit(1,i)
         ib = ipit(2,i)
         ic = ipit(3,i)
         id = ipit(4,i)
         ie = ipit(5,i)
         ig = ipit(6,i)
c
c     decide whether to compute the current interaction
c
         proceed = .true.
         if (use_group)  call groups (proceed,fgrp,ia,ib,ic,id,ie,ig)
         if (proceed)  proceed = (use(ia) .or. use(ib) .or. use(ic) .or.
     &                              use(id) .or. use(ie) .or. use(ig))
c
c     compute the value of the pi-orbital torsion angle
c
         if (proceed) then
            xia = x(ia)
            yia = y(ia)
            zia = z(ia)
            xib = x(ib)
            yib = y(ib)
            zib = z(ib)
            xic = x(ic)
            yic = y(ic)
            zic = z(ic)
            xid = x(id)
            yid = y(id)
            zid = z(id)
            xie = x(ie)
            yie = y(ie)
            zie = z(ie)
            xig = x(ig)
            yig = y(ig)
            zig = z(ig)
            xad = xia - xid
            yad = yia - yid
            zad = zia - zid
            xbd = xib - xid
            ybd = yib - yid
            zbd = zib - zid
            xec = xie - xic
            yec = yie - yic
            zec = zie - zic
            xgc = xig - xic
            ygc = yig - yic
            zgc = zig - zic
            if (use_polymer) then
               call image (xad,yad,zad)
               call image (xbd,ybd,zbd)
               call image (xec,yec,zec)
               call image (xgc,ygc,zgc)
            end if
            xip = yad*zbd - ybd*zad + xic
            yip = zad*xbd - zbd*xad + yic
            zip = xad*ybd - xbd*yad + zic
            xiq = yec*zgc - ygc*zec + xid
            yiq = zec*xgc - zgc*xec + yid
            ziq = xec*ygc - xgc*yec + zid
            xcp = xic - xip
            ycp = yic - yip
            zcp = zic - zip
            xdc = xid - xic
            ydc = yid - yic
            zdc = zid - zic
            xqd = xiq - xid
            yqd = yiq - yid
            zqd = ziq - zid
            if (use_polymer) then
               call image (xcp,ycp,zcp)
               call image (xdc,ydc,zdc)
               call image (xqd,yqd,zqd)
            end if
            xt = ycp*zdc - ydc*zcp
            yt = zcp*xdc - zdc*xcp
            zt = xcp*ydc - xdc*ycp
            xu = ydc*zqd - yqd*zdc
            yu = zdc*xqd - zqd*xdc
            zu = xdc*yqd - xqd*ydc
            xtu = yt*zu - yu*zt
            ytu = zt*xu - zu*xt
            ztu = xt*yu - xu*yt
            rt2 = xt*xt + yt*yt + zt*zt
            ru2 = xu*xu + yu*yu + zu*zu
            rtru = sqrt(rt2 * ru2)
            if (rtru .ne. 0.0d0) then
               rdc = sqrt(xdc*xdc + ydc*ydc + zdc*zdc)
               cosine = (xt*xu + yt*yu + zt*zu) / rtru
               sine = (xdc*xtu + ydc*ytu + zdc*ztu) / (rdc*rtru)
c
c     set the pi-orbital torsion parameters for this angle
c
               v2 = kpit(i)
               c2 = -1.0d0
               s2 = 0.0d0
c
c     compute the multiple angle trigonometry and the phase terms
c
               cosine2 = cosine*cosine - sine*sine
               sine2 = 2.0d0 * cosine * sine
               phi2 = 1.0d0 + (cosine2*c2 + sine2*s2)
               dphi2 = 2.0d0 * (cosine2*s2 - sine2*c2)
c
c     calculate pi-orbital torsion energy and master chain rule term
c
               e = ptorunit * v2 * phi2
               dedphi = ptorunit * v2 * dphi2
c
c     scale the interaction based on its group membership
c
               if (use_group) then
                  e = e * fgrp
                  dedphi = dedphi * fgrp
               end if
c
c     chain rule terms for first derivative components
c
               xdp = xid - xip
               ydp = yid - yip
               zdp = zid - zip
               xqc = xiq - xic
               yqc = yiq - yic
               zqc = ziq - zic
               dedxt = dedphi * (yt*zdc - ydc*zt) / (rt2*rdc)
               dedyt = dedphi * (zt*xdc - zdc*xt) / (rt2*rdc)
               dedzt = dedphi * (xt*ydc - xdc*yt) / (rt2*rdc)
               dedxu = -dedphi * (yu*zdc - ydc*zu) / (ru2*rdc)
               dedyu = -dedphi * (zu*xdc - zdc*xu) / (ru2*rdc)
               dedzu = -dedphi * (xu*ydc - xdc*yu) / (ru2*rdc)
c
c     compute first derivative components for pi-orbital angle
c
               dedxip = zdc*dedyt - ydc*dedzt
               dedyip = xdc*dedzt - zdc*dedxt
               dedzip = ydc*dedxt - xdc*dedyt
               dedxic = ydp*dedzt - zdp*dedyt + zqd*dedyu - yqd*dedzu
               dedyic = zdp*dedxt - xdp*dedzt + xqd*dedzu - zqd*dedxu
               dedzic = xdp*dedyt - ydp*dedxt + yqd*dedxu - xqd*dedyu
               dedxid = zcp*dedyt - ycp*dedzt + yqc*dedzu - zqc*dedyu
               dedyid = xcp*dedzt - zcp*dedxt + zqc*dedxu - xqc*dedzu
               dedzid = ycp*dedxt - xcp*dedyt + xqc*dedyu - yqc*dedxu
               dedxiq = zdc*dedyu - ydc*dedzu
               dedyiq = xdc*dedzu - zdc*dedxu
               dedziq = ydc*dedxu - xdc*dedyu
c
c     compute first derivative components for individual atoms
c
               dedxia = ybd*dedzip - zbd*dedyip
               dedyia = zbd*dedxip - xbd*dedzip
               dedzia = xbd*dedyip - ybd*dedxip
               dedxib = zad*dedyip - yad*dedzip
               dedyib = xad*dedzip - zad*dedxip
               dedzib = yad*dedxip - xad*dedyip
               dedxie = ygc*dedziq - zgc*dedyiq
               dedyie = zgc*dedxiq - xgc*dedziq
               dedzie = xgc*dedyiq - ygc*dedxiq
               dedxig = zec*dedyiq - yec*dedziq
               dedyig = xec*dedziq - zec*dedxiq
               dedzig = yec*dedxiq - xec*dedyiq
               dedxic = dedxic + dedxip - dedxie - dedxig
               dedyic = dedyic + dedyip - dedyie - dedyig
               dedzic = dedzic + dedzip - dedzie - dedzig
               dedxid = dedxid + dedxiq - dedxia - dedxib
               dedyid = dedyid + dedyiq - dedyia - dedyib
               dedzid = dedzid + dedziq - dedzia - dedzib
c
c     increment the total pi-orbital torsion energy and gradient
c
!$OMP atomic
               ept = ept + e

               call OMP_set_lock(lck_drv(ia))
               dept(1,ia) = dept(1,ia) + dedxia
               dept(2,ia) = dept(2,ia) + dedyia
               dept(3,ia) = dept(3,ia) + dedzia
               call OMP_unset_lock(lck_drv(ia))

               call OMP_set_lock(lck_drv(ib))
               dept(1,ib) = dept(1,ib) + dedxib
               dept(2,ib) = dept(2,ib) + dedyib
               dept(3,ib) = dept(3,ib) + dedzib
               call OMP_unset_lock(lck_drv(ib))

               call OMP_set_lock(lck_drv(ic))
               dept(1,ic) = dept(1,ic) + dedxic
               dept(2,ic) = dept(2,ic) + dedyic
               dept(3,ic) = dept(3,ic) + dedzic
               call OMP_unset_lock(lck_drv(ic))

               call OMP_set_lock(lck_drv(id))
               dept(1,id) = dept(1,id) + dedxid
               dept(2,id) = dept(2,id) + dedyid
               dept(3,id) = dept(3,id) + dedzid
               call OMP_unset_lock(lck_drv(id))

               call OMP_set_lock(lck_drv(ie))
               dept(1,ie) = dept(1,ie) + dedxie
               dept(2,ie) = dept(2,ie) + dedyie
               dept(3,ie) = dept(3,ie) + dedzie
               call OMP_unset_lock(lck_drv(ie))

               call OMP_set_lock(lck_drv(ig))
               dept(1,ig) = dept(1,ig) + dedxig
               dept(2,ig) = dept(2,ig) + dedyig
               dept(3,ig) = dept(3,ig) + dedzig
               call OMP_unset_lock(lck_drv(ig))
c
c     increment the internal virial tensor components
c
               vxterm = dedxid + dedxia + dedxib
               vyterm = dedyid + dedyia + dedyib
               vzterm = dedzid + dedzia + dedzib
               vxx = xdc*vxterm + xcp*dedxip - xqd*dedxiq
               vyx = ydc*vxterm + ycp*dedxip - yqd*dedxiq
               vzx = zdc*vxterm + zcp*dedxip - zqd*dedxiq
               vyy = ydc*vyterm + ycp*dedyip - yqd*dedyiq
               vzy = zdc*vyterm + zcp*dedyip - zqd*dedyiq
               vzz = zdc*vzterm + zcp*dedzip - zqd*dedziq
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
         end if
      end do
!$OMP end do NOWAIT

      return
      end
