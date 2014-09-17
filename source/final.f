c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1997  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ###############################################################
c     ##                                                           ##
c     ##  subroutine final  --  final actions before program exit  ##
c     ##                                                           ##
c     ###############################################################
c
c
c     "final" performs any final program actions such as deallocation
c     of global memory, prints a status message, and then pauses if
c     necessary to avoid closing the execution window
c
c
      subroutine final
      use atoms
      use sizes
      use angbnd
      use atmlst
      use bitor
      use bndstr
      use chunks
      use couple
      use deriv
      use inform
      use iounit
      use light
      use molcul
      use moldyn
      use mpole
      use neigh
      use opbend
      use pitors
      use pme
      use polar
      use polgrp
      use ring
      use strbnd
      use tarray
      use tors
      use tortor
      use uprior
      use urey
      use vdw
      implicit none
c
c
c     deallocation of global arrays from module angbnd
c
      if (allocated(iang))  deallocate (iang)
      if (allocated(ak))  deallocate (ak)
      if (allocated(anat))  deallocate (anat)
      if (allocated(afld))  deallocate (afld)
c
c     deallocation of global arrays from module atmlst
c
      if (allocated(bndlist))  deallocate (bndlist)
      if (allocated(anglist))  deallocate (anglist)
c
c     deallocation of global arrays from module bitor
c
      if (allocated(ibitor))  deallocate (ibitor)
c
c     deallocation of global arrays from module bndstr
c
      if (allocated(ibnd))  deallocate (ibnd)
      if (allocated(bk))  deallocate (bk)
      if (allocated(bl))  deallocate (bl)
c
c     deallocation of global arrays from module chunks
c
      if (allocated(pmetable))  deallocate (pmetable)
c
c     deallocation of global arrays from module couple
c
c      if (allocated(n13))  deallocate (n13)
c      if (allocated(n14))  deallocate (n14)
c      if (allocated(n15))  deallocate (n15)
c      if (allocated(i13))  deallocate (i13)
c      if (allocated(i14))  deallocate (i14)
c      if (allocated(i15))  deallocate (i15)

c
c     deallocation of global arrays from module deriv
c
      if (allocated(desum))  deallocate (desum)
      if (allocated(deb))  deallocate (deb)
      if (allocated(dea))  deallocate (dea)
      if (allocated(deba))  deallocate (deba)
      if (allocated(deub))  deallocate (deub)
      if (allocated(deopb))  deallocate (deopb)
      if (allocated(det))  deallocate (det)
      if (allocated(dept))  deallocate (dept)
      if (allocated(dett))  deallocate (dett)
      if (allocated(dev))  deallocate (dev)
      if (allocated(dem))  deallocate (dem)
      if (allocated(dep))  deallocate (dep)
c
c     deallocation of global arrays from module light
c
      if (allocated(kbx))  deallocate (kbx)
      if (allocated(kby))  deallocate (kby)
      if (allocated(kbz))  deallocate (kbz)
      if (allocated(kex))  deallocate (kex)
      if (allocated(key))  deallocate (key)
      if (allocated(kez))  deallocate (kez)
      if (allocated(locx))  deallocate (locx)
      if (allocated(locy))  deallocate (locy)
      if (allocated(locz))  deallocate (locz)
      if (allocated(rgx))  deallocate (rgx)
      if (allocated(rgy))  deallocate (rgy)
      if (allocated(rgz))  deallocate (rgz)
c
c     deallocation of global arrays from module molcul
c
      if (allocated(imol))  deallocate (imol)
      if (allocated(kmol))  deallocate (kmol)
      if (allocated(molcule))  deallocate (molcule)
      if (allocated(molmass))  deallocate (molmass)
c
c     deallocation of global arrays from module moldyn
c
      if (allocated(v))  deallocate (v)
      if (allocated(a))  deallocate (a)
      if (allocated(aalt))  deallocate (aalt)
c
c     deallocation of global arrays from module mpole
c
      if (allocated(ipole))  deallocate (ipole)
      if (allocated(polsiz))  deallocate (polsiz)
      if (allocated(pollist))  deallocate (pollist)
      if (allocated(zaxis))  deallocate (zaxis)
      if (allocated(xaxis))  deallocate (xaxis)
      if (allocated(yaxis))  deallocate (yaxis)
      if (allocated(pole))  deallocate (pole)
      if (allocated(rpole))  deallocate (rpole)
      if (allocated(polaxe))  deallocate (polaxe)
c
c     deallocation of global arrays from module neigh
c
      if (allocated(xvold))  deallocate (xvold)
      if (allocated(yvold))  deallocate (yvold)
      if (allocated(zvold))  deallocate (zvold)
      if (allocated(xcold))  deallocate (xcold)
      if (allocated(ycold))  deallocate (ycold)
      if (allocated(zcold))  deallocate (zcold)
      if (allocated(xmold))  deallocate (xmold)
      if (allocated(ymold))  deallocate (ymold)
      if (allocated(zmold))  deallocate (zmold)
      if (allocated(nvlst))  deallocate (nvlst)
      if (allocated(vlst))  deallocate (vlst)
      if (allocated(nelst))  deallocate (nelst)
      if (allocated(elst))  deallocate (elst)
      if (allocated(nulst))  deallocate (nulst)
      if (allocated(ulst))  deallocate (ulst)
c
c     deallocation of global arrays from module opbend
c
      if (allocated(iopb))  deallocate (iopb)
      if (allocated(opbk))  deallocate (opbk)
c
c     deallocation of global arrays from module pitors
c
      if (allocated(ipit))  deallocate (ipit)
      if (allocated(kpit))  deallocate (kpit)
c
c     deallocation of global arrays from module pme
c
      if (allocated(igrid))  deallocate (igrid)
      if (allocated(thetai1))  deallocate (thetai1)
      if (allocated(thetai2))  deallocate (thetai2)
      if (allocated(thetai3))  deallocate (thetai3)
      if (allocated(qgrid))  deallocate (qgrid)
c     if (allocated(qfac))  deallocate (qfac)
c
c     deallocation of global arrays from module polar
c
      if (allocated(polarity))  deallocate (polarity)
      if (allocated(thole))  deallocate (thole)
      if (allocated(pdamp))  deallocate (pdamp)
      if (allocated(uind))  deallocate (uind)
      if (allocated(uinp))  deallocate (uinp)
      if (allocated(uinds))  deallocate (uinds)
      if (allocated(uinps))  deallocate (uinps)
c
c     deallocation of global arrays from module polgrp
c
      if (allocated(ip11))  deallocate (ip11)
      if (allocated(ip12))  deallocate (ip12)
      if (allocated(ip13))  deallocate (ip13)
      if (allocated(ip14))  deallocate (ip14)
c
c     deallocation of global arrays from module ring
c
      if (allocated(iring3))  deallocate (iring3)
      if (allocated(iring4))  deallocate (iring4)
      if (allocated(iring5))  deallocate (iring5)
      if (allocated(iring6))  deallocate (iring6)
c
c     deallocation of global arrays from module strbnd
c
      if (allocated(isb))  deallocate (isb)
      if (allocated(sbk))  deallocate (sbk)
c
c     deallocation of global arrays from module tarray
c
      if (allocated(tindex))  deallocate (tindex)
      if (allocated(tdipdip))  deallocate (tdipdip)
c
c     deallocation of global arrays from module tors
c
      if (allocated(itors))  deallocate (itors)
      if (allocated(tors1))  deallocate (tors1)
      if (allocated(tors2))  deallocate (tors2)
      if (allocated(tors3))  deallocate (tors3)
      if (allocated(tors4))  deallocate (tors4)
      if (allocated(tors5))  deallocate (tors5)
      if (allocated(tors6))  deallocate (tors6)
c
c     deallocation of global arrays from module tortor
c
      if (allocated(itt))  deallocate (itt)
c
c     deallocation of global arrays from module uprior
c
      if (allocated(udalt))  deallocate (udalt)
      if (allocated(upalt))  deallocate (upalt)
c
c     deallocation of global arrays from module urey
c
      if (allocated(iury))  deallocate (iury)
      if (allocated(uk))  deallocate (uk)
      if (allocated(ul))  deallocate (ul)
c
c     deallocation of global arrays from module vdw
c
      if (allocated(ivdw))  deallocate (ivdw)
      if (allocated(jvdw))  deallocate (jvdw)
      if (allocated(ired))  deallocate (ired)
      if (allocated(kred))  deallocate (kred)
      if (allocated(radmin))  deallocate (radmin)
      if (allocated(epsilon))  deallocate (epsilon)
      if (allocated(radmin4))  deallocate (radmin4)
      if (allocated(epsilon4))  deallocate (epsilon4)
      if (allocated(radhbnd))  deallocate (radhbnd)
      if (allocated(epshbnd))  deallocate (epshbnd)

c
c     deallocation of global data type atom
c     

      deallocate (atom)

c
c     print a final status message before exiting TINKER
c
      if (debug) then
         write (iout,10)
   10    format (/,' TINKER is Exiting following Normal Termination',
     &              ' of the Program',/)
      end if
c
c     may need a pause to avoid closing the execution window
c
      if (holdup) then
         read (input,20)
   20    format ()
      end if
      return
      end
