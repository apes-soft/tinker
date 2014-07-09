c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine etors1  --  standard torsional energy & derivs  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "etors1" calculates the torsional potential energy and first
c     derivatives with respect to Cartesian coordinates using a
c     standard sum of Fourier terms
c
c
      subroutine etors1
      use sizes
      use atoms
      use deriv
      use energi
      use torpot
      use tors
      use virial
      implicit none
      integer i,ia,ib,ic,id
      real*8 e,dedphi,rcb
      real*8 v1,v2,v3,v4,v5,v6
      real*8 c1,c2,c3,c4,c5,c6
      real*8 s1,s2,s3,s4,s5,s6
      real*8 cosine,cosine2
      real*8 cosine3,cosine4
      real*8 cosine5,cosine6
      real*8 sine,sine2,sine3
      real*8 sine4,sine5,sine6
      real*8 phi1,phi2,phi3
      real*8 phi4,phi5,phi6
      real*8 xia,yia,zia
      real*8 xib,yib,zib
      real*8 xic,yic,zic
      real*8 xid,yid,zid
      real*8 xba,yba,zba
      real*8 xdc,ydc,zdc
      real*8 xcb,ycb,zcb
      real*8 xca,yca,zca
      real*8 xdb,ydb,zdb
      real*8 xt,yt,zt,rt2
      real*8 xu,yu,zu,ru2
      real*8 xtu,ytu,ztu,rtru
      real*8 dphi1,dphi2,dphi3
      real*8 dphi4,dphi5,dphi6
      real*8 dedxt,dedyt,dedzt
      real*8 dedxu,dedyu,dedzu
      real*8 dedxia,dedyia,dedzia
      real*8 dedxib,dedyib,dedzib
      real*8 dedxic,dedyic,dedzic
      real*8 dedxid,dedyid,dedzid
      real*8 vxx,vyy,vzz
      real*8 vyx,vzx,vzy
c
c
c     zero out the torsional energy and first derivatives
c
      et = 0.0d0
      do i = 1, n
         det(1,i) = 0.0d0
         det(2,i) = 0.0d0
         det(3,i) = 0.0d0
      end do
c
c     calculate the torsional angle energy and first derivatives
c
      do i = 1, ntors
         ia = itors(1,i)
         ib = itors(2,i)
         ic = itors(3,i)
         id = itors(4,i)
c
c     compute the value of the torsional angle
c
         xia = pos(1,ia)
         yia = pos(2,ia)
         zia = pos(3,ia)
         xib = pos(1,ib)
         yib = pos(2,ib)
         zib = pos(3,ib)
         xic = pos(1,ic)
         yic = pos(2,ic)
         zic = pos(3,ic)
         xid = pos(1,id)
         yid = pos(2,id)
         zid = pos(3,id)
         xba = xib - xia
         yba = yib - yia
         zba = zib - zia
         xcb = xic - xib
         ycb = yic - yib
         zcb = zic - zib
         xdc = xid - xic
         ydc = yid - yic
         zdc = zid - zic
         xt = yba*zcb - ycb*zba
         yt = zba*xcb - zcb*xba
         zt = xba*ycb - xcb*yba
         xu = ycb*zdc - ydc*zcb
         yu = zcb*xdc - zdc*xcb
         zu = xcb*ydc - xdc*ycb
         xtu = yt*zu - yu*zt
         ytu = zt*xu - zu*xt
         ztu = xt*yu - xu*yt
         rt2 = xt*xt + yt*yt + zt*zt
         ru2 = xu*xu + yu*yu + zu*zu
         rtru = sqrt(rt2 * ru2)
         if (rtru .ne. 0.0d0) then
            rcb = sqrt(xcb*xcb + ycb*ycb + zcb*zcb)
            cosine = (xt*xu + yt*yu + zt*zu) / rtru
            sine = (xcb*xtu + ycb*ytu + zcb*ztu) / (rcb*rtru)
c
c     set the torsional parameters for this angle
c
            v1 = tors1(1,i)
            c1 = tors1(3,i)
            s1 = tors1(4,i)
            v2 = tors2(1,i)
            c2 = tors2(3,i)
            s2 = tors2(4,i)
            v3 = tors3(1,i)
            c3 = tors3(3,i)
            s3 = tors3(4,i)
            v4 = tors4(1,i)
            c4 = tors4(3,i)
            s4 = tors4(4,i)
            v5 = tors5(1,i)
            c5 = tors5(3,i)
            s5 = tors5(4,i)
            v6 = tors6(1,i)
            c6 = tors6(3,i)
            s6 = tors6(4,i)
c
c     compute the multiple angle trigonometry and the phase terms
c
            cosine2 = cosine*cosine - sine*sine
            sine2 = 2.0d0 * cosine * sine
            cosine3 = cosine*cosine2 - sine*sine2
            sine3 = cosine*sine2 + sine*cosine2
            cosine4 = cosine*cosine3 - sine*sine3
            sine4 = cosine*sine3 + sine*cosine3
            cosine5 = cosine*cosine4 - sine*sine4
            sine5 = cosine*sine4 + sine*cosine4
            cosine6 = cosine*cosine5 - sine*sine5
            sine6 = cosine*sine5 + sine*cosine5
            phi1 = 1.0d0 + (cosine*c1 + sine*s1)
            phi2 = 1.0d0 + (cosine2*c2 + sine2*s2)
            phi3 = 1.0d0 + (cosine3*c3 + sine3*s3)
            phi4 = 1.0d0 + (cosine4*c4 + sine4*s4)
            phi5 = 1.0d0 + (cosine5*c5 + sine5*s5)
            phi6 = 1.0d0 + (cosine6*c6 + sine6*s6)
            dphi1 = (cosine*s1 - sine*c1)
            dphi2 = 2.0d0 * (cosine2*s2 - sine2*c2)
            dphi3 = 3.0d0 * (cosine3*s3 - sine3*c3)
            dphi4 = 4.0d0 * (cosine4*s4 - sine4*c4)
            dphi5 = 5.0d0 * (cosine5*s5 - sine5*c5)
            dphi6 = 6.0d0 * (cosine6*s6 - sine6*c6)
c
c     calculate torsional energy and master chain rule term
c
            e = torsunit * (v1*phi1 + v2*phi2 + v3*phi3
     &                            + v4*phi4 + v5*phi5 + v6*phi6)
            dedphi = torsunit * (v1*dphi1 + v2*dphi2 + v3*dphi3
     &                                 + v4*dphi4 + v5*dphi5 + v6*dphi6)
c
c     chain rule terms for first derivative components
c
            xca = xic - xia
            yca = yic - yia
            zca = zic - zia
            xdb = xid - xib
            ydb = yid - yib
            zdb = zid - zib
            dedxt = dedphi * (yt*zcb - ycb*zt) / (rt2*rcb)
            dedyt = dedphi * (zt*xcb - zcb*xt) / (rt2*rcb)
            dedzt = dedphi * (xt*ycb - xcb*yt) / (rt2*rcb)
            dedxu = -dedphi * (yu*zcb - ycb*zu) / (ru2*rcb)
            dedyu = -dedphi * (zu*xcb - zcb*xu) / (ru2*rcb)
            dedzu = -dedphi * (xu*ycb - xcb*yu) / (ru2*rcb)
c
c     compute first derivative components for this angle
c
            dedxia = zcb*dedyt - ycb*dedzt
            dedyia = xcb*dedzt - zcb*dedxt
            dedzia = ycb*dedxt - xcb*dedyt
            dedxib = yca*dedzt - zca*dedyt + zdc*dedyu - ydc*dedzu
            dedyib = zca*dedxt - xca*dedzt + xdc*dedzu - zdc*dedxu
            dedzib = xca*dedyt - yca*dedxt + ydc*dedxu - xdc*dedyu
            dedxic = zba*dedyt - yba*dedzt + ydb*dedzu - zdb*dedyu
            dedyic = xba*dedzt - zba*dedxt + zdb*dedxu - xdb*dedzu
            dedzic = yba*dedxt - xba*dedyt + xdb*dedyu - ydb*dedxu
            dedxid = zcb*dedyu - ycb*dedzu
            dedyid = xcb*dedzu - zcb*dedxu
            dedzid = ycb*dedxu - xcb*dedyu
c
c     increment the total torsional angle energy and gradient
c
            et = et + e
            det(1,ia) = det(1,ia) + dedxia
            det(2,ia) = det(2,ia) + dedyia
            det(3,ia) = det(3,ia) + dedzia
            det(1,ib) = det(1,ib) + dedxib
            det(2,ib) = det(2,ib) + dedyib
            det(3,ib) = det(3,ib) + dedzib
            det(1,ic) = det(1,ic) + dedxic
            det(2,ic) = det(2,ic) + dedyic
            det(3,ic) = det(3,ic) + dedzic
            det(1,id) = det(1,id) + dedxid
            det(2,id) = det(2,id) + dedyid
            det(3,id) = det(3,id) + dedzid
c
c     increment the internal virial tensor components
c
            vxx = xcb*(dedxic+dedxid) - xba*dedxia + xdc*dedxid
            vyx = ycb*(dedxic+dedxid) - yba*dedxia + ydc*dedxid
            vzx = zcb*(dedxic+dedxid) - zba*dedxia + zdc*dedxid
            vyy = ycb*(dedyic+dedyid) - yba*dedyia + ydc*dedyid
            vzy = zcb*(dedyic+dedyid) - zba*dedyia + zdc*dedyid
            vzz = zcb*(dedzic+dedzid) - zba*dedzia + zdc*dedzid
            vir(1,1) = vir(1,1) + vxx
            vir(2,1) = vir(2,1) + vyx
            vir(3,1) = vir(3,1) + vzx
            vir(1,2) = vir(1,2) + vyx
            vir(2,2) = vir(2,2) + vyy
            vir(3,2) = vir(3,2) + vzy
            vir(1,3) = vir(1,3) + vzx
            vir(2,3) = vir(2,3) + vzy
            vir(3,3) = vir(3,3) + vzz
         end if
      end do
      return
      end
