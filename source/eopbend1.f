c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1995  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ###############################################################
c     ##                                                           ##
c     ##  subroutine eopbend1  --  out-of-plane energy and derivs  ##
c     ##                                                           ##
c     ###############################################################
c
c
c     "eopbend1" computes the out-of-plane bend potential energy and
c     first derivatives at trigonal centers via a Wilson-Decius-Cross
c     or Allinger angle
c
c
      subroutine eopbend1
      use sizes
      use angbnd
      use angpot
      use atoms
      use deriv
      use energi
      use math
      use opbend
      use virial
      implicit none
      integer i,iopbend
      integer ia,ib,ic,id
      real*8 e,angle,force
      real*8 dot,cosine
      real*8 cc,ee,bkk2,term
      real*8 deddt,dedcos
      real*8 dt,dt2,dt3,dt4
      real*8 xia,yia,zia
      real*8 xib,yib,zib
      real*8 xic,yic,zic
      real*8 xid,yid,zid
      real*8 xab,yab,zab
      real*8 xcb,ycb,zcb
      real*8 xdb,ydb,zdb
      real*8 xad,yad,zad
      real*8 xcd,ycd,zcd
      real*8 rdb2,rad2,rcd2
      real*8 dccdxia,dccdyia,dccdzia
      real*8 dccdxic,dccdyic,dccdzic
      real*8 dccdxid,dccdyid,dccdzid
      real*8 deedxia,deedyia,deedzia
      real*8 deedxic,deedyic,deedzic
      real*8 deedxid,deedyid,deedzid
      real*8 dedxia,dedyia,dedzia
      real*8 dedxib,dedyib,dedzib
      real*8 dedxic,dedyic,dedzic
      real*8 dedxid,dedyid,dedzid
      real*8 vxx,vyy,vzz
      real*8 vyx,vzx,vzy
c
c
c     zero out out-of-plane energy and first derivatives
c
      eopb = 0.0d0
      do i = 1, n
         deopb(1,i) = 0.0d0
         deopb(2,i) = 0.0d0
         deopb(3,i) = 0.0d0
      end do
c
c     calculate the out-of-plane bending energy and derivatives
c
      do iopbend = 1, nopbend
         i = iopb(iopbend)
         ia = iang(1,i)
         ib = iang(2,i)
         ic = iang(3,i)
         id = iang(4,i)
         force = opbk(iopbend)
c
c     get the coordinates of the atoms at trigonal center
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
c
c     compute the out-of-plane bending angle
c
         xab = xia - xib
         yab = yia - yib
         zab = zia - zib
         xcb = xic - xib
         ycb = yic - yib
         zcb = zic - zib
         xdb = xid - xib
         ydb = yid - yib
         zdb = zid - zib
         xad = xia - xid
         yad = yia - yid
         zad = zia - zid
         xcd = xic - xid
         ycd = yic - yid
         zcd = zic - zid
c
c     Allinger angle between A-C-D plane and D-B vector for D-B<AC
c
         rad2 = xad*xad + yad*yad + zad*zad
         rcd2 = xcd*xcd + ycd*ycd + zcd*zcd
         dot = xad*xcd + yad*ycd + zad*zcd
         cc = rad2*rcd2 - dot*dot
c
c     find the out-of-plane angle bending energy
c
         ee = xdb*(yab*zcb-zab*ycb) + ydb*(zab*xcb-xab*zcb)
     &              + zdb*(xab*ycb-yab*xcb)
         rdb2 = xdb*xdb + ydb*ydb + zdb*zdb
         if (rdb2.ne.0.0d0 .and. cc.ne.0.0d0) then
            bkk2 = rdb2 - ee*ee/cc
            cosine = sqrt(bkk2/rdb2)
            cosine = min(1.0d0,max(-1.0d0,cosine))
            angle = radian * acos(cosine)
            dt = angle
            dt2 = dt * dt
            dt3 = dt2 * dt
            dt4 = dt2 * dt2
            e = opbunit * force * dt2
     &                * (1.0d0+copb*dt+qopb*dt2+popb*dt3+sopb*dt4)
            deddt = opbunit * force * dt * radian
     &                    * (2.0d0 + 3.0d0*copb*dt + 4.0d0*qopb*dt2
     &                        + 5.0d0*popb*dt3 + 6.0d0*sopb*dt4)
            dedcos = -deddt * sign(1.0d0,ee) / sqrt(cc*bkk2)
c
c     chain rule terms for first derivative components
c
            term = ee / cc
            dccdxia = (xad*rcd2-xcd*dot) * term
            dccdyia = (yad*rcd2-ycd*dot) * term
            dccdzia = (zad*rcd2-zcd*dot) * term
            dccdxic = (xcd*rad2-xad*dot) * term
            dccdyic = (ycd*rad2-yad*dot) * term
            dccdzic = (zcd*rad2-zad*dot) * term
            dccdxid = -dccdxia - dccdxic
            dccdyid = -dccdyia - dccdyic
            dccdzid = -dccdzia - dccdzic
            term = ee / rdb2
            deedxia = ydb*zcb - zdb*ycb
            deedyia = zdb*xcb - xdb*zcb
            deedzia = xdb*ycb - ydb*xcb
            deedxic = yab*zdb - zab*ydb
            deedyic = zab*xdb - xab*zdb
            deedzic = xab*ydb - yab*xdb
            deedxid = ycb*zab - zcb*yab + xdb*term
            deedyid = zcb*xab - xcb*zab + ydb*term
            deedzid = xcb*yab - ycb*xab + zdb*term
c
c     compute first derivative components for this angle
c
            dedxia = dedcos * (dccdxia+deedxia)
            dedyia = dedcos * (dccdyia+deedyia)
            dedzia = dedcos * (dccdzia+deedzia)
            dedxic = dedcos * (dccdxic+deedxic)
            dedyic = dedcos * (dccdyic+deedyic)
            dedzic = dedcos * (dccdzic+deedzic)
            dedxid = dedcos * (dccdxid+deedxid)
            dedyid = dedcos * (dccdyid+deedyid)
            dedzid = dedcos * (dccdzid+deedzid)
            dedxib = -dedxia - dedxic - dedxid
            dedyib = -dedyia - dedyic - dedyid
            dedzib = -dedzia - dedzic - dedzid
c
c     increment the out-of-plane bending energy and gradient
c
            eopb = eopb + e
            deopb(1,ia) = deopb(1,ia) + dedxia
            deopb(2,ia) = deopb(2,ia) + dedyia
            deopb(3,ia) = deopb(3,ia) + dedzia
            deopb(1,ib) = deopb(1,ib) + dedxib
            deopb(2,ib) = deopb(2,ib) + dedyib
            deopb(3,ib) = deopb(3,ib) + dedzib
            deopb(1,ic) = deopb(1,ic) + dedxic
            deopb(2,ic) = deopb(2,ic) + dedyic
            deopb(3,ic) = deopb(3,ic) + dedzic
            deopb(1,id) = deopb(1,id) + dedxid
            deopb(2,id) = deopb(2,id) + dedyid
            deopb(3,id) = deopb(3,id) + dedzid
c
c     increment the internal virial tensor components
c
            vxx = xab*dedxia + xcb*dedxic + xdb*dedxid
            vyx = yab*dedxia + ycb*dedxic + ydb*dedxid
            vzx = zab*dedxia + zcb*dedxic + zdb*dedxid
            vyy = yab*dedyia + ycb*dedyic + ydb*dedyid
            vzy = zab*dedyia + zcb*dedyic + zdb*dedyid
            vzz = zab*dedzia + zcb*dedzic + zdb*dedzid
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
