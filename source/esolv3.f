c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1993  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ############################################################
c     ##                                                        ##
c     ##  subroutine esolv3  --  solvation energy and analysis  ##
c     ##                                                        ##
c     ############################################################
c
c
c     "esolv3" calculates the implicit solvation energy for
c     surface area, generalized Born, generalized Kirkwood
c     and Poisson-Boltzmann solvation models; also partitions
c     the energy among the atoms
c
c
      subroutine esolv3
      implicit none
      include 'sizes.i'
      include 'action.i'
      include 'analyz.i'
      include 'atmtyp.i'
      include 'atoms.i'
      include 'energi.i'
      include 'inform.i'
      include 'iounit.i'
      include 'math.i'
      include 'potent.i'
      include 'solute.i'
      include 'warp.i'
      integer i,nehp
      real*8 e,ai,ri,rb
      real*8 term,probe
      real*8 esurf,ehp,eace
      real*8 ecav,edisp
      real*8 aecav(maxatm)
      real*8 aedisp(maxatm)
      real*8 aehp(maxatm)
      real*8 aenp(maxatm)
      logical header,huge
c
c
c     zero out the implicit solvation energy and partitioning
c
      nes = 0
      do i = 1, n
         aes(i) = 0.0d0
      end do
      es = 0.0d0
      esurf = 0.0d0
      ecav = 0.0d0
      edisp = 0.0d0
      ehp = 0.0d0
      eace = 0.0d0
c
c     set a value for the solvent molecule probe radius
c
      probe = 1.4d0
c
c     total solvation energy for surface area only models
c
      if (solvtyp.eq.'ASP' .or. solvtyp.eq.'SASA') then
         call surface (es,aes,rsolv,asolv,probe)
         nes = nes + n
c
c     nonpolar energy for Onion GB method via exact area
c
      else if (solvtyp .eq. 'ONION') then
         call surface (esurf,aes,rsolv,asolv,probe)
         es = esurf
         nes = nes + n
c
c     nonpolar energy as cavity formation plus dispersion
c
      else if (solvtyp.eq.'GK' .or. solvtyp.eq.'PB') then
         call enp3 (ecav,aecav,edisp,aedisp)
         es = ecav + edisp
         nes = nes + 2*n
         do i = 1, n
            aes(i) = aecav(i) + aedisp(i)
         end do
c
c     nonpolar energy as hydrophobic potential of mean force
c
      else if (solvtyp.eq.'GB-HPMF' .or. solvtyp.eq.'GK-HPMF'
     &            .or. solvtyp.eq.'PB-HPMF') then
         call ehpmf3 (ehp,nehp,aehp)
         es = ehp
         nes = nes + nehp
         do i = 1, n
            aes(i) = aes(i) + aehp(i)
         end do
c
c     nonpolar energy via ACE surface area approximation
c
      else
         term = 4.0d0 * pi
         do i = 1, n
            ai = asolv(i)
            ri = rsolv(i)
            rb = rborn(i)
            if (rb .ne. 0.0d0) then
               e = ai * term * (ri+probe)**2 * (ri/rb)**6
               eace = eace + e
               nes = nes + 1
               aes(i) = aes(i) + e
            end if
         end do
         es = eace
      end if
c
c     store the nonpolar or surface area energy for each atom
c
      do i = 1, n
         aenp(i) = aes(i)
      end do
c
c     get polarization energy term for the solvation methods
c
      if (solvtyp .eq. 'GK') then
         call egk3
      else if (solvtyp .eq. 'PB') then
         call epb3
      else if (use_born) then
         if (use_smooth) then
            call egb3b
         else
            call egb3a
         end if
      end if
c
c     print a message if the energy of any interaction is large
c
      header = .true.
      do i = 1, n
         huge = (abs(aes(i)) .gt. 25.0d0)
         if (debug .or. (verbose.and.huge)) then
            if (header) then
               header = .false.
               write (iout,10)
   10          format (/,' Individual Atomic Solvation Energy',
     &                    ' Terms :',
     &                 //,' Type',11x,'Atom Name',18x,'Nonpolar',
     &                    7x,'Polar',6x,'Energy',/)
            end if
            write (iout,20)  i,name(i),aenp(i),aes(i)-aenp(i),aes(i)
   20       format (' Solvate',7x,i5,'-',a3,15x,3f12.4)
         end if
      end do
c
c     print nonpolar and electrostatic components for selected models
c
      if (verbose) then
         if (esurf .ne. 0.0d0) then
            write (iout,30)  esurf,es-esurf
   30       format (/,' Implicit Solvation Energy Components :'
     &              //,' SA Nonpolar',25x,f12.4,
     &              /,' Electrostatic',23x,f12.4)
         else if (ecav.ne.0.0d0 .or. edisp.ne.0.0d0) then
            write (iout,40)  ecav,edisp,es-ecav-edisp
   40       format (/,' Implicit Solvation Energy Components :'
     &              //,' Cavitation',26x,f12.4,
     &              /,' Dispersion',26x,f12.4,
     &              /,' Electrostatic',23x,f12.4)
         else if (ehp .ne. 0.0d0) then
            write (iout,50)  ehp,es-ehp
   50       format (/,' Implicit Solvation Energy Components :'
     &              //,' Hydrophobic PMF',21x,f12.4,
     &              /,' Electrostatic',23x,f12.4)
         else if (eace .ne. 0.0d0) then
            write (iout,60)  eace,es-eace
   60       format (/,' Implicit Solvation Energy Components :'
     &              //,' ACE Nonpolar',24x,f12.4,
     &              /,' Electrostatic',23x,f12.4)
         end if
      end if
      return
      end
c
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine egb3a  --  GB polarization energy and analysis  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "egb3a" calculates the generalized Born electrostatic energy
c     for GB/SA solvation models; also partitions the energy among
c     the atoms
c
c
      subroutine egb3a
      implicit none
      include 'sizes.i'
      include 'action.i'
      include 'analyz.i'
      include 'atoms.i'
      include 'charge.i'
      include 'chgpot.i'
      include 'energi.i'
      include 'group.i'
      include 'inter.i'
      include 'molcul.i'
      include 'shunt.i'
      include 'solute.i'
      include 'usage.i'
      integer i,k,ii,kk
      real*8 e,f,fi,fik
      real*8 dwater,fgrp
      real*8 rb2,rm2,fgb,fgm
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 r,r2,r3,r4
      real*8 r5,r6,r7
      real*8 shift,taper,trans
      logical proceed,usei
c
c
c     set the solvent dielectric and energy conversion factor
c
      if (nion .eq. 0)  return
      dwater = 78.3d0
      f = -electric * (1.0d0 - 1.0d0/dwater)
c
c     set cutoff distances and switching function coefficients
c
      call switch ('CHARGE')
c
c     calculate GB electrostatic polarization energy term
c
      do ii = 1, nion
         i = iion(ii)
         usei = use(i)
         xi = x(i)
         yi = y(i)
         zi = z(i)
         fi = f * pchg(ii)
c
c     decide whether to compute the current interaction
c
         do kk = ii, nion
            k = iion(kk)
            proceed = .true.
            if (use_group)  call groups (proceed,fgrp,i,k,0,0,0,0)
            if (proceed)  proceed = (usei .or. use(k))
c
c     compute the energy contribution for this interaction
c
            if (proceed) then
               xr = xi - x(k)
               yr = yi - y(k)
               zr = zi - z(k)
               r2 = xr*xr + yr*yr + zr*zr
               if (r2 .le. off2) then
                  fik = fi * pchg(kk)
                  rb2 = rborn(i) * rborn(k)
                  fgb = sqrt(r2 + rb2*exp(-0.25d0*r2/rb2))
                  e = fik / fgb
c
c     use shifted energy switching if near the cutoff distance
c
                  rm2 = (0.5d0 * (off+cut))**2
                  fgm = sqrt(rm2 + rb2*exp(-0.25d0*rm2/rb2))
                  shift = fik / fgm
                  e = e - shift
                  if (r2 .gt. cut2) then
                     r = sqrt(r2)
                     r3 = r2 * r
                     r4 = r2 * r2
                     r5 = r2 * r3
                     r6 = r3 * r3
                     r7 = r3 * r4
                     taper = c5*r5 + c4*r4 + c3*r3
     &                          + c2*r2 + c1*r + c0
                     trans = fik * (f7*r7 + f6*r6 + f5*r5 + f4*r4
     &                               + f3*r3 + f2*r2 + f1*r + f0)
                     e = e*taper + trans
                  end if
c
c     scale the interaction based on its group membership
c
                  if (use_group)  e = e * fgrp
c
c     increment the overall GB polarization energy component
c
                  nes = nes + 1
                  if (i .eq. k) then
                     es = es + 0.5d0*e
                     aes(i) = aes(i) + 0.5d0*e
                  else
                     es = es + e
                     aes(i) = aes(i) + 0.5d0*e
                     aes(k) = aes(k) + 0.5d0*e
                  end if
c
c     increment the total intermolecular energy
c
                  if (molcule(i) .ne. molcule(k)) then
                     einter = einter + e
                  end if
               end if
            end if
         end do
      end do
      return
      end
c
c
c     ##################################################################
c     ##                                                              ##
c     ##  subroutine egb3b  --  GB energy and analysis for smoothing  ##
c     ##                                                              ##
c     ##################################################################
c
c
c     "egb3b" calculates the generalized Born electrostatuc energy
c     for GB/SA solvation models for use with potential smoothing
c     methods via analogy to the smoothing of Coulomb's law; also
c     partitions the energy among the atoms
c
c
      subroutine egb3b
      implicit none
      include 'sizes.i'
      include 'action.i'
      include 'analyz.i'
      include 'atoms.i'
      include 'charge.i'
      include 'chgpot.i'
      include 'energi.i'
      include 'group.i'
      include 'inter.i'
      include 'molcul.i'
      include 'solute.i'
      include 'usage.i'
      include 'warp.i'
      integer i,k,ii,kk
      real*8 e,fgrp
      real*8 f,fi,fik
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 dwater,width
      real*8 erf,sterm
      real*8 r2,fgb,rb2
      logical proceed,usei
      external erf
c
c
c     set the solvent dielectric and energy conversion factor
c
      if (nion .eq. 0)  return
      dwater = 78.3d0
      f = -electric * (1.0d0 - 1.0d0/dwater)
c
c     set the extent of smoothing to be performed
c
      sterm = 0.5d0 / sqrt(diffc)
c
c     calculate GB electrostatic polarization energy term
c
      do ii = 1, nion
         i = iion(ii)
         usei = use(i)
         xi = x(i)
         yi = y(i)
         zi = z(i)
         fi = f * pchg(ii)
c
c     decide whether to compute the current interaction
c
         do kk = ii, nion
            k = iion(kk)
            proceed = .true.
            if (use_group)  call groups (proceed,fgrp,i,k,0,0,0,0)
            if (proceed)  proceed = (usei .or. use(k))
c
c     compute the energy contribution for this interaction
c
            if (proceed) then
               xr = xi - x(k)
               yr = yi - y(k)
               zr = zi - z(k)
               r2 = xr*xr + yr*yr + zr*zr
               fik = fi * pchg(kk)
               rb2 = rborn(i) * rborn(k)
               fgb = sqrt(r2 + rb2*exp(-0.25d0*r2/rb2))
               e = fik / fgb
c
c     use a smoothable GB analogous to the Coulomb solution
c
               if (deform .gt. 0.0d0) then
                  width = deform + 0.15d0*rb2*exp(-0.006d0*rb2/deform)
                  width = sterm / sqrt(width)
                  e = e * erf(width*fgb)
               end if
c
c     scale the interaction based on its group membership
c
               if (use_group)  e = e * fgrp
c
c     increment the overall GB solvation energy component
c
               nes = nes + 1
               if (i .eq. k) then
                  es = es + 0.5d0*e
                  aes(i) = aes(i) + 0.5d0*e
               else
                  es = es + e
                  aes(i) = aes(i) + 0.5d0*e
                  aes(k) = aes(k) + 0.5d0*e
               end if
c
c     increment the total intermolecular energy
c
               if (molcule(i) .ne. molcule(k)) then
                  einter = einter + e
               end if
            end if
         end do
      end do
      return
      end
c
c
c     ##############################################################
c     ##                                                          ##
c     ##  subroutine egk3  --  GK polarization energy & analysis  ##
c     ##                                                          ##
c     ##############################################################
c
c
c     "egk3" calculates the generalized Kirkwood electrostatic
c     energy for GK/NP solvation models; also partitions the
c     energy among the atoms
c
c
      subroutine egk3
      implicit none
      include 'energi.i'
c
c
c     compute the generalized Kirkwood energy and analysis
c
      call egk3a
c
c     correct the solvation energy for vacuum to polarized state
c
      call ediff3
      return
      end
c
c
c     ##############################################################
c     ##                                                          ##
c     ##  subroutine egk3a  --  find generalized Kirkwood energy  ##
c     ##                                                          ##
c     ##############################################################
c
c
c     "egk3a" calculates the electrostatic portion of the implicit
c     solvation energy via the generalized Kirkwood model; also
c     partitions the energy among the atoms
c
c
      subroutine egk3a
      implicit none
      include 'sizes.i'
      include 'action.i'
      include 'analyz.i'
      include 'atoms.i'
      include 'atmtyp.i'
      include 'chgpot.i'
      include 'energi.i'
      include 'gkstuf.i'
      include 'group.i'
      include 'inform.i'
      include 'inter.i'
      include 'iounit.i'
      include 'molcul.i'
      include 'mpole.i'
      include 'polar.i'
      include 'potent.i'
      include 'shunt.i'
      include 'solute.i'
      include 'usage.i'
      integer i,k,ii,kk
      real*8 e,ei
      real*8 fc,fd,fq
      real*8 dwater,fgrp
      real*8 r2,rb2
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 xr2,yr2,zr2
      real*8 ci,ck
      real*8 uxi,uyi,uzi
      real*8 uxk,uyk,uzk
      real*8 dxi,dyi,dzi
      real*8 dxk,dyk,dzk
      real*8 qxxi,qxyi,qxzi
      real*8 qyyi,qyzi,qzzi
      real*8 qxxk,qxyk,qxzk
      real*8 qyyk,qyzk,qzzk
      real*8 rbi,rbk
      real*8 expterm
      real*8 gf,gf2,gf3
      real*8 gf5,gf7,gf9
      real*8 expc,dexpc
      real*8 expc1,expcdexpc
      real*8 a(0:4,0:2)
      real*8 gc(10),gux(10)
      real*8 guy(10),guz(10)
      real*8 gqxx(10),gqxy(10)
      real*8 gqxz(10),gqyy(10)
      real*8 gqyz(10),gqzz(10)
      real*8 esym,ewi,ewk
      real*8 esymi,ewii,ewki
      real*8 eself(maxatm)
      real*8 ecross(maxatm)
      logical proceed,usei
c
c
c     zero out self energy and cross terms
c
      do i = 1, n
         eself(i) = 0.0d0
         ecross(i) = 0.0d0
      end do
c
c     set the bulk dielectric constant to the water value
c
      if (npole .le. 0)  return
      dwater = 78.3d0
      fc = electric * 1.0d0 * (1.0d0-dwater)/(0.0d0+1.0d0*dwater)
      fd = electric * 2.0d0 * (1.0d0-dwater)/(1.0d0+2.0d0*dwater)
      fq = electric * 3.0d0 * (1.0d0-dwater)/(2.0d0+3.0d0*dwater)
c
c     set cutoff distances and switching function coefficients
c
      call switch ('MPOLE')
c
c     setup the multipoles for solvation only calculations
c
      if (.not.use_mpole .and. .not.use_polar) then
         call chkpole
         call rotpole
      end if
c
c     calculate GK electrostatic solvation free energy
c
      do ii = 1, npole
         i = ipole(ii)
         usei = use(i)
         xi = x(i)
         yi = y(i)
         zi = z(i)
         rbi = rborn(i)
         ci = rpole(1,ii)
         uxi = rpole(2,ii)
         uyi = rpole(3,ii)
         uzi = rpole(4,ii)
         qxxi = rpole(5,ii)
         qxyi = rpole(6,ii)
         qxzi = rpole(7,ii)
         qyyi = rpole(9,ii)
         qyzi = rpole(10,ii)
         qzzi = rpole(13,ii)
         dxi = uinds(1,ii)
         dyi = uinds(2,ii)
         dzi = uinds(3,ii)
c
c     decide whether to compute the current interaction
c
         do kk = ii, npole
            k = ipole(kk)
            proceed = .true.
            if (use_group)  call groups (proceed,fgrp,i,k,0,0,0,0)
            if (proceed)  proceed = (usei .or. use(k))
c
c     compute the energy contribution for this interaction
c
            if (proceed) then
               xr = x(k) - xi
               yr = y(k) - yi
               zr = z(k) - zi
               xr2 = xr * xr
               yr2 = yr * yr
               zr2 = zr * zr
               r2 = xr2 + yr2 + zr2
               if (r2 .le. off2) then
                  rbk = rborn(k)
                  ck = rpole(1,kk)
                  uxk = rpole(2,kk)
                  uyk = rpole(3,kk)
                  uzk = rpole(4,kk)
                  qxxk = rpole(5,kk)
                  qxyk = rpole(6,kk)
                  qxzk = rpole(7,kk)
                  qyyk = rpole(9,kk)
                  qyzk = rpole(10,kk)
                  qzzk = rpole(13,kk)
                  dxk = uinds(1,kk)
                  dyk = uinds(2,kk)
                  dzk = uinds(3,kk)
                  rb2 = rbi * rbk
                  expterm = exp(-r2 / (gkc * rb2))
                  expc = expterm / gkc
                  dexpc = -2.0d0/(gkc*rbi*rbk)
                  gf2 = 1.0d0 / (r2 + rb2*expterm)
                  gf = sqrt(gf2)
                  gf3 = gf2 * gf
                  gf5 = gf3 * gf2
                  gf7 = gf5 * gf2
                  gf9 = gf7 * gf2
c
c     reaction potential auxiliary terms
c
                  a(0,0) = gf
                  a(1,0) = -gf3
                  a(2,0) = 3.0d0 * gf5
                  a(3,0) = -15.0d0 * gf7
                  a(4,0) = 105.0d0 * gf9
c
c     reaction potential gradient auxiliary terms
c
                  expc1 = 1.0d0 - expc
                  a(0,1) = expc1 * a(1,0)
                  a(1,1) = expc1 * a(2,0)
                  a(2,1) = expc1 * a(3,0)
                  a(3,1) = expc1 * a(4,0)
c
c     second reaction potential gradient auxiliary terms
c
                  expcdexpc = -expc * dexpc
                  a(0,2) = expc1*a(1,1) + expcdexpc*a(1,0)
                  a(1,2) = expc1*a(2,1) + expcdexpc*a(2,0)
                  a(2,2) = expc1*a(3,1) + expcdexpc*a(3,0)
c
c     multiply the auxillary terms by their dieletric functions
c
                  a(0,0) = fc * a(0,0)
                  a(0,1) = fc * a(0,1)
                  a(0,2) = fc * a(0,2)
                  a(1,0) = fd * a(1,0)
                  a(1,1) = fd * a(1,1)
                  a(1,2) = fd * a(1,2)
                  a(2,0) = fq * a(2,0)
                  a(2,1) = fq * a(2,1)
                  a(2,2) = fq * a(2,2)
c
c     unweighted reaction potential tensor
c
                  gc(1) = a(0,0)
                  gux(1) = xr * a(1,0)
                  guy(1) = yr * a(1,0)
                  guz(1) = zr * a(1,0)
                  gqxx(1) = xr2 * a(2,0)
                  gqyy(1) = yr2 * a(2,0)
                  gqzz(1) = zr2 * a(2,0)
                  gqxy(1) = xr * yr * a(2,0)
                  gqxz(1) = xr * zr * a(2,0)
                  gqyz(1) = yr * zr * a(2,0)
c
c     unweighted reaction potential gradient tensor
c
                  gc(2) = xr * a(0,1)
                  gc(3) = yr * a(0,1)
                  gc(4) = zr * a(0,1)
                  gux(2) = a(1,0) + xr2*a(1,1)
                  gux(3) = xr * yr * a(1,1)
                  gux(4) = xr * zr * a(1,1)
                  guy(2) = gux(3)
                  guy(3) = a(1,0) + yr2*a(1,1)
                  guy(4) = yr * zr * a(1,1)
                  guz(2) = gux(4)
                  guz(3) = guy(4)
                  guz(4) = a(1,0) + zr2*a(1,1)
                  gqxx(2) = xr * (2.0d0*a(2,0)+xr2*a(2,1))
                  gqxx(3) = yr * xr2 * a(2,1)
                  gqxx(4) = zr * xr2 * a(2,1)
                  gqyy(2) = xr * yr2 * a(2,1)
                  gqyy(3) = yr * (2.0d0*a(2,0)+yr2*a(2,1))
                  gqyy(4) = zr * yr2 * a(2,1)
                  gqzz(2) = xr * zr2 * a(2,1)
                  gqzz(3) = yr * zr2 * a(2,1)
                  gqzz(4) = zr * (2.0d0*a(2,0)+zr2*a(2,1))
                  gqxy(2) = yr * (a(2,0)+xr2*a(2,1))
                  gqxy(3) = xr * (a(2,0)+yr2*a(2,1))
                  gqxy(4) = zr * xr * yr * a(2,1)
                  gqxz(2) = zr * (a(2,0)+xr2*a(2,1))
                  gqxz(3) = gqxy(4)
                  gqxz(4) = xr * (a(2,0)+zr2*a(2,1))
                  gqyz(2) = gqxy(4)
                  gqyz(3) = zr * (a(2,0)+yr2*a(2,1))
                  gqyz(4) = yr * (a(2,0)+zr2*a(2,1))
c
c     unweighted second reaction potential gradient tensor
c
                  gc(5) = a(0,1) + xr2*a(0,2)
                  gc(6) = xr * yr * a(0,2)
                  gc(7) = xr * zr * a(0,2)
                  gc(8) = a(0,1) + yr2*a(0,2)
                  gc(9) = yr * zr * a(0,2)
                  gc(10) = a(0,1) + zr2*a(0,2)
                  gux(5) = xr * (a(1,1)+2.0d0*a(1,1)+xr2*a(1,2))
                  gux(6) = yr * (a(1,1)+xr2*a(1,2))
                  gux(7) = zr * (a(1,1)+xr2*a(1,2))
                  gux(8) = xr * (a(1,1)+yr2*a(1,2))
                  gux(9) = zr * xr * yr * a(1,2)
                  gux(10) = xr * (a(1,1)+zr2*a(1,2))
                  guy(5) = yr * (a(1,1)+xr2*a(1,2))
                  guy(6) = xr * (a(1,1)+yr2*a(1,2))
                  guy(7) = gux(9)
                  guy(8) = yr * (a(1,1)+2.0d0*a(1,1)+yr2*a(1,2))
                  guy(9) = zr * (a(1,1)+yr2*a(1,2))
                  guy(10) = yr * (a(1,1)+zr2*a(1,2))
                  guz(5) = zr * (a(1,1)+xr2*a(1,2))
                  guz(6) = gux(9)
                  guz(7) = xr * (a(1,1)+zr2*a(1,2))
                  guz(8) = zr * (a(1,1)+yr2*a(1,2))
                  guz(9) = yr * (a(1,1)+zr2*a(1,2))
                  guz(10) = zr * (a(1,1)+2.0d0*a(1,1)+zr2*a(1,2))
                  gqxx(5) = 2.0d0*a(2,0) + xr2*(5.0d0*a(2,1)+xr2*a(2,2))
                  gqxx(6) = yr * xr *(2.0d0*a(2,1)+xr2*a(2,2))
                  gqxx(7) = zr * xr *(2.0d0*a(2,1)+xr2*a(2,2))
                  gqxx(8) = xr2 * (a(2,1)+yr2*a(2,2))
                  gqxx(9) = zr * yr * xr2 * a(2,2)
                  gqxx(10) = xr2 * (a(2,1)+zr2*a(2,2))
                  gqyy(5) = yr2 * (a(2,1)+xr2*a(2,2))
                  gqyy(6) = xr * yr * (2.0d0*a(2,1)+yr2*a(2,2))
                  gqyy(7) = xr * zr * yr2 * a(2,2)
                  gqyy(8) = 2.0d0*a(2,0) + yr2*(5.0d0*a(2,1)+yr2*a(2,2))
                  gqyy(9) = yr * zr * (2.0d0*a(2,1)+yr2*a(2,2))
                  gqyy(10) = yr2 * (a(2,1)+zr2*a(2,2))
                  gqzz(5) = zr2 * (a(2,1)+xr2*a(2,2))
                  gqzz(6) = xr * yr * zr2 * a(2,2)
                  gqzz(7) = xr * zr * (2.0d0*a(2,1)+zr2*a(2,2))
                  gqzz(8) = zr2 * (a(2,1)+yr2*a(2,2))
                  gqzz(9) = yr * zr * (2.0d0*a(2,1)+zr2*a(2,2))
                  gqzz(10) = 2.0d0*a(2,0)
     &                          + zr2*(5.0d0*a(2,1)+zr2*a(2,2))
                  gqxy(5) = xr * yr * (3.0d0*a(2,1)+xr2*a(2,2))
                  gqxy(6) = a(2,0) + (xr2+yr2)*a(2,1) + xr2*yr2*a(2,2)
                  gqxy(7) = zr * yr * (a(2,1)+xr2*a(2,2))
                  gqxy(8) = xr * yr * (3.0d0*a(2,1)+yr2*a(2,2))
                  gqxy(9) = zr * xr * (a(2,1)+yr2*a(2,2))
                  gqxy(10) = xr * yr * (a(2,1)+zr2*a(2,2))
                  gqxz(5) = xr * zr * (3.0d0*a(2,1)+xr2*a(2,2))
                  gqxz(6) = yr * zr * (a(2,1)+xr2*a(2,2))
                  gqxz(7) = a(2,0) + (xr2+zr2)*a(2,1) + xr2*zr2*a(2,2)
                  gqxz(8) = xr * zr * (a(2,1)+yr2*a(2,2))
                  gqxz(9) = xr * yr * (a(2,1)+zr2*a(2,2))
                  gqxz(10) = xr * zr * (3.0d0*a(2,1)+zr2*a(2,2))
                  gqyz(5) = zr * yr * (a(2,1)+xr2*a(2,2))
                  gqyz(6) = xr * zr * (a(2,1)+yr2*a(2,2))
                  gqyz(7) = xr * yr * (a(2,1)+zr2*a(2,2))
                  gqyz(8) = yr * zr * (3.0d0*a(2,1)+yr2*a(2,2))
                  gqyz(9) = a(2,0) + (yr2+zr2)*a(2,1) + yr2*zr2*a(2,2)
                  gqyz(10) = yr * zr * (3.0d0*a(2,1)+zr2*a(2,2))
c
c     electrostatic solvation free energy of the permanent multipoles
c     in their own GK reaction potential
c
                  esym = ci*ck*gc(1)
     &                     - uxi*(uxk*gux(2)+uyk*guy(2)+uzk*guz(2))
     &                     - uyi*(uxk*gux(3)+uyk*guy(3)+uzk*guz(3))
     &                     - uzi*(uxk*gux(4)+uyk*guy(4)+uzk*guz(4))
                  ewi = ci*(uxk*gc(2)+uyk*gc(3)+uzk*gc(4))
     &                    - ck*(uxi*gux(1)+uyi*guy(1)+uzi*guz(1))
     &               + ci*(qxxk*gc(5)+qyyk*gc(8)+qzzk*gc(10)
     &                  +2.0d0*(qxyk*gc(6)+qxzk*gc(7)+qyzk*gc(9)))
     &               + ck*(qxxi*gqxx(1)+qyyi*gqyy(1)+qzzi*gqzz(1)
     &                  +2.0d0*(qxyi*gqxy(1)+qxzi*gqxz(1)+qyzi*gqyz(1)))
     &               - uxi*(qxxk*gux(5)+qyyk*gux(8)+qzzk*gux(10)
     &                  +2.0d0*(qxyk*gux(6)+qxzk*gux(7)+qyzk*gux(9)))
     &               - uyi*(qxxk*guy(5)+qyyk*guy(8)+qzzk*guy(10)
     &                  +2.0d0*(qxyk*guy(6)+qxzk*guy(7)+qyzk*guy(9)))
     &               - uzi*(qxxk*guz(5)+qyyk*guz(8)+qzzk*guz(10)
     &                  +2.0d0*(qxyk*guz(6)+qxzk*guz(7)+qyzk*guz(9)))
     &               + uxk*(qxxi*gqxx(2)+qyyi*gqyy(2)+qzzi*gqzz(2)
     &                  +2.0d0*(qxyi*gqxy(2)+qxzi*gqxz(2)+qyzi*gqyz(2)))
     &               + uyk*(qxxi*gqxx(3)+qyyi*gqyy(3)+qzzi*gqzz(3)
     &                  +2.0d0*(qxyi*gqxy(3)+qxzi*gqxz(3)+qyzi*gqyz(3)))
     &               + uzk*(qxxi*gqxx(4)+qyyi*gqyy(4)+qzzi*gqzz(4)
     &                  +2.0d0*(qxyi*gqxy(4)+qxzi*gqxz(4)+qyzi*gqyz(4)))
     &               + qxxi*(qxxk*gqxx(5)+qyyk*gqxx(8)+qzzk*gqxx(10)
     &                  +2.0d0*(qxyk*gqxx(6)+qxzk*gqxx(7)+qyzk*gqxx(9)))
     &               + qyyi*(qxxk*gqyy(5)+qyyk*gqyy(8)+qzzk*gqyy(10)
     &                  +2.0d0*(qxyk*gqyy(6)+qxzk*gqyy(7)+qyzk*gqyy(9)))
     &               + qzzi*(qxxk*gqzz(5)+qyyk*gqzz(8)+qzzk*gqzz(10)
     &                  +2.0d0*(qxyk*gqzz(6)+qxzk*gqzz(7)+qyzk*gqzz(9)))
     &          + 2.0d0 * (qxyi*(qxxk*gqxy(5)+qyyk*gqxy(8)+qzzk*gqxy(10)
     &               +2.0d0*(qxyk*gqxy(6)+qxzk*gqxy(7)+qyzk*gqxy(9)))
     &               + qxzi*(qxxk*gqxz(5)+qyyk*gqxz(8)+qzzk*gqxz(10)
     &               +2.0d0*(qxyk*gqxz(6)+qxzk*gqxz(7)+qyzk*gqxz(9)))
     &               + qyzi*(qxxk*gqyz(5)+qyyk*gqyz(8)+qzzk*gqyz(10)
     &               +2.0d0*(qxyk*gqyz(6)+qxzk*gqyz(7)+qyzk*gqyz(9))))
                  ewk = ci*(uxk*gux(1)+uyk*guy(1)+uzk*guz(1))
     &                    - ck*(uxi*gc(2)+uyi*gc(3)+uzi*gc(4))
     &               + ci*(qxxk*gqxx(1)+qyyk*gqyy(1)+qzzk*gqzz(1)
     &                  +2.0d0*(qxyk*gqxy(1)+qxzk*gqxz(1)+qyzk*gqyz(1)))
     &               + ck*(qxxi*gc(5)+qyyi*gc(8)+qzzi*gc(10)
     &                  +2.0d0*(qxyi*gc(6)+qxzi*gc(7)+qyzi*gc(9)))
     &               - uxi*(qxxk*gqxx(2)+qyyk*gqyy(2)+qzzk*gqzz(2)
     &                  +2.0d0*(qxyk*gqxy(2)+qxzk*gqxz(2)+qyzk*gqyz(2)))
     &               - uyi*(qxxk*gqxx(3)+qyyk*gqyy(3)+qzzk*gqzz(3)
     &                  +2.0d0*(qxyk*gqxy(3)+qxzk*gqxz(3)+qyzk*gqyz(3)))
     &               - uzi*(qxxk*gqxx(4)+qyyk*gqyy(4)+qzzk*gqzz(4)
     &                  +2.0d0*(qxyk*gqxy(4)+qxzk*gqxz(4)+qyzk*gqyz(4)))
     &               + uxk*(qxxi*gux(5)+qyyi*gux(8)+qzzi*gux(10)
     &                  +2.0d0*(qxyi*gux(6)+qxzi*gux(7)+qyzi*gux(9)))
     &               + uyk*(qxxi*guy(5)+qyyi*guy(8)+qzzi*guy(10)
     &                  +2.0d0*(qxyi*guy(6)+qxzi*guy(7)+qyzi*guy(9)))
     &               + uzk*(qxxi*guz(5)+qyyi*guz(8)+qzzi*guz(10)
     &                  +2.0d0*(qxyi*guz(6)+qxzi*guz(7)+qyzi*guz(9)))
     &               + qxxi*(qxxk*gqxx(5)+qyyk*gqyy(5)+qzzk*gqzz(5)
     &                  +2.0d0*(qxyk*gqxy(5)+qxzk*gqxz(5)+qyzk*gqyz(5)))
     &               + qyyi*(qxxk*gqxx(8)+qyyk*gqyy(8)+qzzk*gqzz(8)
     &                  +2.0d0*(qxyk*gqxy(8)+qxzk*gqxz(8)+qyzk*gqyz(8)))
     &               + qzzi*(qxxk*gqxx(10)+qyyk*gqyy(10)+qzzk*gqzz(10)
     &               +2.0d0*(qxyk*gqxy(10)+qxzk*gqxz(10)+qyzk*gqyz(10)))
     &          + 2.0d0*(qxyi*(qxxk*gqxx(6)+qyyk*gqyy(6)+qzzk*gqzz(6)
     &               +2.0d0*(qxyk*gqxy(6)+qxzk*gqxz(6)+qyzk*gqyz(6)))
     &               + qxzi*(qxxk*gqxx(7)+qyyk*gqyy(7)+qzzk*gqzz(7)
     &               +2.0d0*(qxyk*gqxy(7)+qxzk*gqxz(7)+qyzk*gqyz(7)))
     &               + qyzi*(qxxk*gqxx(9)+qyyk*gqyy(9)+qzzk*gqzz(9)
     &               +2.0d0*(qxyk*gqxy(9)+qxzk*gqxz(9)+qyzk*gqyz(9))))
c
c     electrostatic solvation free energy of the permenant multipoles
c     in the GK reaction potential of the induced dipoles
c
                  esymi = -uxi*(dxk*gux(2)+dyk*guy(2)+dzk*guz(2))
     &                      - uyi*(dxk*gux(3)+dyk*guy(3)+dzk*guz(3))
     &                      - uzi*(dxk*gux(4)+dyk*guy(4)+dzk*guz(4))
     &                      - uxk*(dxi*gux(2)+dyi*guy(2)+dzi*guz(2))
     &                      - uyk*(dxi*gux(3)+dyi*guy(3)+dzi*guz(3))
     &                      - uzk*(dxi*gux(4)+dyi*guy(4)+dzi*guz(4))
                  ewii = ci*(dxk*gc(2)+dyk*gc(3)+dzk*gc(4))
     &                     - ck*(dxi*gux(1)+dyi*guy(1)+dzi*guz(1))
     &              - dxi*(qxxk*gux(5)+qyyk*gux(8)+qzzk*gux(10)
     &                 +2.0d0*(qxyk*gux(6)+qxzk*gux(7)+qyzk*gux(9)))
     &              - dyi*(qxxk*guy(5)+qyyk*guy(8)+qzzk*guy(10)
     &                 +2.0d0*(qxyk*guy(6)+qxzk*guy(7)+qyzk*guy(9)))
     &              - dzi*(qxxk*guz(5)+qyyk*guz(8)+qzzk*guz(10)
     &                 +2.0d0*(qxyk*guz(6)+qxzk*guz(7)+qyzk*guz(9)))
     &              + dxk*(qxxi*gqxx(2)+qyyi*gqyy(2)+qzzi*gqzz(2)
     &                 +2.0d0*(qxyi*gqxy(2)+qxzi*gqxz(2)+qyzi*gqyz(2)))
     &              + dyk*(qxxi*gqxx(3)+qyyi*gqyy(3)+qzzi*gqzz(3)
     &                 +2.0d0*(qxyi*gqxy(3)+qxzi*gqxz(3)+qyzi*gqyz(3)))
     &              + dzk*(qxxi*gqxx(4)+qyyi*gqyy(4)+qzzi*gqzz(4)
     &                 +2.0d0*(qxyi*gqxy(4)+qxzi*gqxz(4)+qyzi*gqyz(4)))
                  ewki = ci*(dxk*gux(1)+dyk*guy(1)+dzk*guz(1))
     &                     - ck*(dxi*gc(2)+dyi*gc(3)+dzi*gc(4))
     &              - dxi*(qxxk*gqxx(2)+qyyk*gqyy(2)+qzzk*gqzz(2)
     &                 +2.0d0*(qxyk*gqxy(2)+qxzk*gqxz(2)+qyzk*gqyz(2)))
     &              - dyi*(qxxk*gqxx(3)+qyyk*gqyy(3)+qzzk*gqzz(3)
     &                 +2.0d0*(qxyk*gqxy(3)+qxzk*gqxz(3)+qyzk*gqyz(3)))
     &              - dzi*(qxxk*gqxx(4)+qyyk*gqyy(4)+qzzk*gqzz(4)
     &                 +2.0d0*(qxyk*gqxy(4)+qxzk*gqxz(4)+qyzk*gqyz(4)))
     &              + dxk*(qxxi*gux(5)+qyyi*gux(8)+qzzi*gux(10)
     &                 +2.0d0*(qxyi*gux(6)+qxzi*gux(7)+qyzi*gux(9)))
     &              + dyk*(qxxi*guy(5)+qyyi*guy(8)+qzzi*guy(10)
     &                 +2.0d0*(qxyi*guy(6)+qxzi*guy(7)+qyzi*guy(9)))
     &              + dzk*(qxxi*guz(5)+qyyi*guz(8)+qzzi*guz(10)
     &                 +2.0d0*(qxyi*guz(6)+qxzi*guz(7)+qyzi*guz(9)))
c
c     total permanent and induced energies for this interaction
c
                  e = esym + 0.5d0*(ewi+ewk)
                  ei = 0.5d0 * (esymi + 0.5d0*(ewii+ewki))
c
c     scale the interaction based on its group membership
c
                  if (use_group) then
                     e = e * fgrp
                     ei = ei * fgrp
                  end if
c
c     increment the total GK electrostatic solvation energy
c
                  nes = nes + 1
                  if (i .eq. k) then
                     e = 0.5d0 * e
                     ei = 0.5d0 * ei
                     es = es + e + ei
                     aes(i) = aes(i) + e + ei
                     eself(i) = eself(i) + e + ei
                  else
                     es = es + e + ei
                     aes(i) = aes(i) + 0.5d0*(e+ei)
                     aes(k) = aes(k) + 0.5d0*(e+ei)
                     ecross(i) = ecross(i) + 0.5d0*(e+ei)
                     ecross(k) = ecross(k) + 0.5d0*(e+ei)
                  end if
c
c     increment the total intermolecule energy
c
                  if (molcule(i) .ne. molcule(k)) then
                     einter = einter + e + ei
                  end if
               end if
            end if
         end do
      end do
c
c     print the self-energy and cross-energy terms
c
      if (debug) then
         write (iout,10)
   10    format (/,' Generalized Kirkwood Self-Energies and',
     &              ' Cross-Energies :',/)
         do i = 1, n
            write (iout,20)  i,eself(i),ecross(i)
   20       format (i8,1x,2f15.4)
         end do
      end if
      return
      end
c
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine ediff3  --  correct GK for polarization change  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "ediff3" calculates the energy of polarizing the vacuum induced
c     dipoles to their generalized Kirkwood values with energy analysis
c
c
      subroutine ediff3
      implicit none
      include 'sizes.i'
      include 'analyz.i'
      include 'atoms.i'
      include 'bound.i'
      include 'chgpot.i'
      include 'couple.i'
      include 'energi.i'
      include 'group.i'
      include 'mpole.i'
      include 'polar.i'
      include 'polgrp.i'
      include 'polpot.i'
      include 'shunt.i'
      include 'usage.i'
      integer i,j,k
      integer ii,kk
      integer ix,iz
      integer kx,kz
      real*8 ei,f,fikp
      real*8 fgrp,damp
      real*8 r,r2,xr,yr,zr
      real*8 rr1,rr3,rr5,rr7
      real*8 pdi,pti,pgamma
      real*8 ci,dix,diy,diz
      real*8 uix,uiy,uiz
      real*8 qixx,qixy,qixz
      real*8 qiyy,qiyz,qizz
      real*8 ck,dkx,dky,dkz
      real*8 ukx,uky,ukz
      real*8 qkxx,qkxy,qkxz
      real*8 qkyy,qkyz,qkzz
      real*8 qix,qiy,qiz
      real*8 qkx,qky,qkz
      real*8 scale3,scale5
      real*8 scale7
      real*8 sc(6)
      real*8 sci(8)
      real*8 gli(3)
      real*8 pscale(maxatm)
      logical proceed,usei,usek
c
c
c     set conversion factor, cutoff and scaling coefficients
c
      if (npole .eq. 0)  return
      f = electric / dielec
      call switch ('MPOLE')
c
c     set array needed to scale connected atom interactions
c
      do i = 1, n
         pscale(i) = 1.0d0
      end do
c
c     calculate the multipole interaction energy term
c
      do i = 1, npole-1
         ii = ipole(i)
         iz = zaxis(i)
         ix = xaxis(i)
         pdi = pdamp(i)
         pti = thole(i)
         ci = rpole(1,i)
         dix = rpole(2,i)
         diy = rpole(3,i)
         diz = rpole(4,i)
         qixx = rpole(5,i)
         qixy = rpole(6,i)
         qixz = rpole(7,i)
         qiyy = rpole(9,i)
         qiyz = rpole(10,i)
         qizz = rpole(13,i)
         uix = uinds(1,i) - uind(1,i)
         uiy = uinds(2,i) - uind(2,i)
         uiz = uinds(3,i) - uind(3,i)
         usei = (use(ii) .or. use(iz) .or. use(ix))
         do j = 1, n12(ii)
            pscale(i12(j,ii)) = p2scale
         end do
         do j = 1, n13(ii)
            pscale(i13(j,ii)) = p3scale
         end do
         do j = 1, n14(ii)
            pscale(i14(j,ii)) = p4scale
            do k = 1, np11(ii)
                if (i14(j,ii) .eq. ip11(k,ii))
     &            pscale(i14(j,ii)) = 0.5d0 * pscale(i14(j,ii))
            end do
         end do
         do j = 1, n15(ii)
            pscale(i15(j,ii)) = p5scale
         end do
c
c     decide whether to compute the current interaction
c
         do k = i+1, npole
            kk = ipole(k)
            kz = zaxis(k)
            kx = xaxis(k)
            usek = (use(kk) .or. use(kz) .or. use(kx))
            proceed = .true.
            if (use_group)  call groups (proceed,fgrp,ii,kk,0,0,0,0)
            if (.not. use_intra)  proceed = .true.
            if (proceed)  proceed = (usei .or. usek)
c
c     compute the energy contribution for this interaction
c
            if (proceed) then
               xr = x(kk) - x(ii)
               yr = y(kk) - y(ii)
               zr = z(kk) - z(ii)
               call image (xr,yr,zr)
               r2 = xr*xr + yr* yr + zr*zr
               if (r2 .le. off2) then
                  r = sqrt(r2)
                  ck = rpole(1,k)
                  dkx = rpole(2,k)
                  dky = rpole(3,k)
                  dkz = rpole(4,k)
                  qkxx = rpole(5,k)
                  qkxy = rpole(6,k)
                  qkxz = rpole(7,k)
                  qkyy = rpole(9,k)
                  qkyz = rpole(10,k)
                  qkzz = rpole(13,k)
                  ukx = uinds(1,k) - uind(1,k)
                  uky = uinds(2,k) - uind(2,k)
                  ukz = uinds(3,k) - uind(3,k)
c
c     construct some intermediate quadrupole values
c
                  qix = qixx*xr + qixy*yr + qixz*zr
                  qiy = qixy*xr + qiyy*yr + qiyz*zr
                  qiz = qixz*xr + qiyz*yr + qizz*zr
                  qkx = qkxx*xr + qkxy*yr + qkxz*zr
                  qky = qkxy*xr + qkyy*yr + qkyz*zr
                  qkz = qkxz*xr + qkyz*yr + qkzz*zr
c
c     calculate the scalar products for permanent multipoles
c
                  sc(3) = dix*xr + diy*yr + diz*zr
                  sc(4) = dkx*xr + dky*yr + dkz*zr
                  sc(5) = qix*xr + qiy*yr + qiz*zr
                  sc(6) = qkx*xr + qky*yr + qkz*zr
c
c     calculate the scalar products for polarization components
c
                  sci(2) = uix*dkx + dix*ukx + uiy*dky
     &                        + diy*uky + uiz*dkz + diz*ukz
                  sci(3) = uix*xr + uiy*yr + uiz*zr
                  sci(4) = ukx*xr + uky*yr + ukz*zr
                  sci(7) = qix*ukx + qiy*uky + qiz*ukz
                  sci(8) = qkx*uix + qky*uiy + qkz*uiz
c
c     calculate the gl functions for polarization components
c
                  gli(1) = ck*sci(3) - ci*sci(4) + sci(2)
                  gli(2) = 2.0d0*(sci(7)-sci(8)) - sci(3)*sc(4)
     &                        - sc(3)*sci(4)
                  gli(3) = sci(3)*sc(6) - sci(4)*sc(5)
c
c     compute the energy contributions for this interaction
c
                  rr1 = 1.0d0 / r
                  rr3 = rr1 / r2
                  rr5 = 3.0d0 * rr3 / r2
                  rr7 = 5.0d0 * rr5 / r2
                  scale3 = 1.0d0
                  scale5 = 1.0d0
                  scale7 = 1.0d0
                  damp = pdi * pdamp(k)
                  if (damp .ne. 0.0d0) then
                     pgamma = min(pti,thole(k))
                     damp = -pgamma * (r/damp)**3
                     if (damp .gt. -50.0d0) then
                        scale3 = 1.0d0 - exp(damp)
                        scale5 = 1.0d0 - (1.0d0-damp)*exp(damp)
                        scale7 = 1.0d0 - (1.0d0-damp+0.6d0*damp**2)
     &                                          *exp(damp)
                     end if
                  end if
                  ei = gli(1)*rr3*scale3 + gli(2)*rr5*scale5
     &                    + gli(3)*rr7*scale7
c
c     make the adjustment for scaled interactions
c
                  fikp = f * pscale(kk)
                  ei = 0.5d0 * fikp * ei
c
c     scale the interaction based on its group membership;
c     polarization cannot be group scaled as it is not pairwise
c
                  if (use_group) then
c                    ei = ei * fgrp
                  end if
c
c     increment the total GK electrostatic solvation energy
c
                  es = es + ei
                  aes(ii) = aes(ii) + 0.5d0*ei
                  aes(kk) = aes(kk) + 0.5d0*ei
               end if
            end if
         end do
c
c     reset interaction scaling coefficients for connected atoms
c
         do j = 1, n12(ii)
            pscale(i12(j,ii)) = 1.0d0
         end do
         do j = 1, n13(ii)
            pscale(i13(j,ii)) = 1.0d0
         end do
         do j = 1, n14(ii)
            pscale(i14(j,ii)) = 1.0d0
         end do
         do j = 1, n15(ii)
            pscale(i15(j,ii)) = 1.0d0
         end do
      end do
      return
      end
c
c
c     ##############################################################
c     ##                                                          ##
c     ##  subroutine epb3  --  PB polarization energy & analysis  ##
c     ##                                                          ##
c     ##############################################################
c
c
c     "epb3" calculates the implicit solvation energy via the
c     Poisson-Boltzmann model; also partitions the energy among
c     the atoms
c
c
      subroutine epb3
      implicit none
      include 'sizes.i'
      include 'analyz.i'
      include 'atoms.i'
      include 'chgpot.i'
      include 'energi.i'
      include 'mpole.i'
      include 'pbstuf.i'
      include 'polar.i'
      include 'potent.i'
      integer i,ii
      real*8 e
c
c
c     compute the electrostatic energy via Poisson-Boltzmann
c
      if (use_polar) then
         e = 0.0d0
         do i = 1, npole
            ii = ipole(i)
            e = e + uinds(1,i)*pbep(1,ii) + uinds(2,i)*pbep(2,ii)
     &               + uinds(3,i)*pbep(3,ii)
         end do
         e = -0.5d0 * electric * e
         pbe = pbe + e
      else
         call pbempole
      end if
c
c     increment solvation energy and analysis by PB results
c
      es = es + pbe
      do i = 1, n
         aes(i) = aes(i) + apbe(i)
      end do
c
c     correct the solvation energy for vacuum to polarized state
c
      call ediff3
      return
      end
c
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine enp3  --  nonpolar solvation energy & analysis  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "enp3" calculates the nonpolar implicit solvation energy as
c     a sum of cavity and dispersion terms; also partitions the
c     energy among the atoms
c
c
      subroutine enp3 (ecav,aecav,edisp,aedisp)
      implicit none
      include 'sizes.i'
      include 'atmtyp.i'
      include 'atoms.i'
      include 'kvdws.i'
      include 'math.i'
      include 'mpole.i'
      include 'npolar.i'
      include 'shunt.i'
      include 'solute.i'
      integer i
      real*8 ecav,edisp
      real*8 exclude,taper
      real*8 evol,esurf,aevol
      real*8 reff,reff2,reff3
      real*8 reff4,reff5
      real*8 aecav(maxatm)
      real*8 aedisp(maxatm)
      real*8 aesurf(maxatm)
c
c
c     zero out the nonpolar solvation energy and partitioning
c
      ecav = 0.0d0
      edisp = 0.0d0
      do i = 1, n
         aecav(i) = 0.0d0
         aedisp(i) = 0.0d0
      end do
c
c     compute SASA and effective radius needed for cavity term
c
      exclude = 1.4d0
      call surface (esurf,aesurf,rcav,asolv,exclude)
      reff = 0.5d0 * sqrt(esurf/(pi*surften))
      reff2 = reff * reff
      reff3 = reff2 * reff
      reff4 = reff3 * reff
      reff5 = reff4 * reff
c
c     compute solvent excluded volume for needed for small solutes
c
      if (reff .lt. spoff) then
         call volume (evol,rcav,exclude)
         evol = evol * solvprs
         aevol = evol / dble(n)
      end if
c
c     find cavity energy from only the solvent excluded volume
c
      if (reff .le. spcut) then
         ecav = evol
         do i = 1, n
            aecav(i) = aevol
         end do
c
c     find cavity energy from only a tapered volume term
c
      else if (reff.gt.spcut .and. reff.le.stoff) then
         call switch ('GKV')
         taper = c5*reff5 + c4*reff4 + c3*reff3
     &              + c2*reff2 + c1*reff + c0
         ecav = taper * evol
         do i = 1, n
            aecav(i) = taper * aevol
         end do
c
c     find cavity energy using both volume and SASA terms
c
      else if (reff .gt. stoff .and. reff .le. spoff) then
         call switch ('GKV')
         taper = c5*reff5 + c4*reff4 + c3*reff3
     &              + c2*reff2 + c1*reff + c0
         ecav = taper * evol
         call switch ('GKSA')
         taper = c5*reff5 + c4*reff4 + c3*reff3
     &              + c2*reff2 + c1*reff + c0
         taper = 1.0d0 - taper
         ecav = ecav + taper*esurf
         do i = 1, n
            aecav(i) = taper * (aevol+aesurf(i))
         end do
c
c     find cavity energy from only a tapered SASA term
c
      else if (reff.gt.spoff .and. reff.le.stcut) then
         call switch ('GKSA')
         taper = c5*reff5 + c4*reff4 + c3*reff3
     &              + c2*reff2 + c1*reff + c0
         taper = 1.0d0 - taper
         ecav = taper * esurf
         do i = 1, n
            aecav(i) = taper * aesurf(i)
         end do
c
c     find cavity energy from only a SASA-based term
c
      else
         ecav = esurf
         do i = 1, n
            aecav(i) = aesurf(i)
         end do
      end if
c
c     find the implicit dispersion solvation energy
c
      call ewca3 (edisp,aedisp)
c     call ewca3x (edisp,aedisp)
      return
      end
c
c
c     ################################################################
c     ##                                                            ##
c     ##  subroutine ewca3  --  WCA dispersion energy and analysis  ##
c     ##                                                            ##
c     ################################################################
c
c
c     "ewca3" find the Weeks-Chandler-Andersen dispersion energy
c     of a solute; also partitions the energy among the atoms
c
c
      subroutine ewca3 (edisp,aedisp)
      implicit none
      include 'sizes.i'
      include 'atoms.i'
      include 'atmtyp.i'
      include 'deriv.i'
      include 'inform.i'
      include 'iounit.i'
      include 'kvdws.i'
      include 'math.i'
      include 'npolar.i'
      include 'solute.i'
      include 'vdw.i'
      integer i,k
      real*8 edisp,e,idisp
      real*8 xi,yi,zi
      real*8 rk,sk,sk2
      real*8 xr,yr,zr,r,r2
      real*8 sum,term,shctd
      real*8 iwca,irep,offset
      real*8 epsi,rmini,ri,rmax
      real*8 ao,emixo,rmixo,rmixo7
      real*8 ah,emixh,rmixh,rmixh7
      real*8 lik,lik2,lik3,lik4
      real*8 lik5,lik10,lik11,lik12
      real*8 uik,uik2,uik3,uik4
      real*8 uik5,uik10,uik11,uik12
      real*8 aedisp(maxatm)
c
c
c     zero out the WCA dispersion energy and partitioning
c
      edisp = 0.0d0
      do i = 1, n
         aedisp(i) = 0.0d0
      end do
c
c     set overlap scale factor for HCT descreening method
c
      shctd = 0.81d0
      offset = 0.0d0
      do i = 1, n
         rdisp(i) = rad(class(i)) + offset
      end do
c
c     find the Weeks-Chandler-Andersen dispersion energy
c
      do i = 1, n
         epsi = eps(class(i))
         rmini = rad(class(i))
         emixo = 4.0d0 * epso * epsi / ((sqrt(epso)+sqrt(epsi))**2)
         rmixo = 2.0d0 * (rmino**3+rmini**3) / (rmino**2+rmini**2)
         rmixo7 = rmixo**7
         ao = emixo * rmixo7
         emixh = 4.0d0 * epsh * epsi / ((sqrt(epsh)+sqrt(epsi))**2)
         rmixh = 2.0d0 * (rminh**3+rmini**3) / (rminh**2+rmini**2)
         rmixh7 = rmixh**7
         ah = emixh * rmixh7
         xi = x(i)
         yi = y(i)
         zi = z(i)
         ri = rdisp(i)
c
c     remove contribution due to solvent displaced by solute atoms
c
         sum = 0.0d0
         do k = 1, n
            if (i .ne. k) then
               xr = x(k) - xi
               yr = y(k) - yi
               zr = z(k) - zi
               r2 = xr*xr + yr*yr + zr*zr
               r = sqrt(r2)
               rk = rdisp(k)
c              sk = rk * shct(k)
               sk = rk * shctd
               sk2 = sk * sk
               if (ri .lt. r+sk) then
                  rmax = max(ri,r-sk)
                  lik = rmax
                  lik2 = lik * lik
                  lik3 = lik2 * lik
                  lik4 = lik3 * lik
                  if (lik .lt. rmixo) then
                     uik = min(r+sk,rmixo)
                     uik2 = uik * uik
                     uik3 = uik2 * uik
                     uik4 = uik3 * uik
                     term = 4.0d0 * pi / (48.0d0*r)
     &                    * (3.0d0*(lik4-uik4) - 8.0d0*r*(lik3-uik3)
     &                          + 6.0d0*(r2-sk2)*(lik2-uik2))
                     iwca = -emixo * term
                     sum = sum + iwca
                  end if
                  if (lik .lt. rmixh) then
                     uik = min(r+sk,rmixh)
                     uik2 = uik * uik
                     uik3 = uik2 * uik
                     uik4 = uik3 * uik
                     term = 4.0d0 * pi / (48.0d0*r)
     &                    * (3.0d0*(lik4-uik4) - 8.0d0*r*(lik3-uik3)
     &                          + 6.0d0*(r2-sk2)*(lik2-uik2))
                     iwca = -2.0d0 * emixh * term
                     sum = sum + iwca
                  end if
                  uik = r + sk
                  uik2 = uik * uik
                  uik3 = uik2 * uik
                  uik4 = uik3 * uik
                  uik5 = uik4 * uik
                  uik10 = uik5 * uik5
                  uik11 = uik10 * uik
                  uik12 = uik11 * uik
                  if (uik .gt. rmixo) then
                     lik = max(rmax,rmixo)
                     lik2 = lik * lik
                     lik3 = lik2 * lik
                     lik4 = lik3 * lik
                     lik5 = lik4 * lik
                     lik10 = lik5 * lik5
                     lik11 = lik10 * lik
                     lik12 = lik11 * lik
                     term = 4.0d0 * pi / (120.0d0*r*lik5*uik5)
     &                      * (15.0d0*uik*lik*r*(uik4-lik4)
     &                         - 10.0d0*uik2*lik2*(uik3-lik3)
     &                         + 6.0d0*(sk2-r2)*(uik5-lik5))
                     idisp = -2.0d0 * ao * term
                     term = 4.0d0 * pi / (2640.0d0*r*lik12*uik12)
     &                      * (120.0d0*uik*lik*r*(uik11-lik11)
     &                         - 66.0d0*uik2*lik2*(uik10-lik10)
     &                         + 55.0d0*(sk2-r2)*(uik12-lik12))
                     irep = ao * rmixo7 * term
                     sum = sum + irep + idisp
                  end if
                  if (uik .gt. rmixh) then
                     lik = max(rmax,rmixh)
                     lik2 = lik * lik
                     lik3 = lik2 * lik
                     lik4 = lik3 * lik
                     lik5 = lik4 * lik
                     lik10 = lik5 * lik5
                     lik11 = lik10 * lik
                     lik12 = lik11 * lik
                     term = 4.0d0 * pi / (120.0d0*r*lik5*uik5)
     &                      * (15.0d0*uik*lik*r*(uik4-lik4)
     &                         - 10.0d0*uik2*lik2*(uik3-lik3)
     &                         + 6.0d0*(sk2-r2)*(uik5-lik5))
                     idisp = -4.0d0 * ah * term
                     term = 4.0d0 * pi / (2640.0d0*r*lik12*uik12)
     &                      * (120.0d0*uik*lik*r*(uik11-lik11)
     &                         - 66.0d0*uik2*lik2*(uik10-lik10)
     &                         + 55.0d0*(sk2-r2)*(uik12-lik12))
                     irep = 2.0d0 * ah * rmixh7 * term
                     sum = sum + irep + idisp
                  end if
               end if
            end if
         end do
c
c     increment the overall dispersion energy component
c
         e = cdisp(i) - slevy*awater*sum
         aedisp(i) = e
         edisp = edisp + e
      end do
c
c     print the total dispersion energy and energy for each atom
c
      if (debug) then
         write (iout,10)
   10    format (/,' HCT Implicit Solvation Dispersion :',/)
         do i = 1, n
            write (iout,20)  i,aedisp(i)
   20       format (' Dispersion Energy for Atom',i6,' :',2x,f12.4)
         end do
         write (iout,30)  edisp
   30    format (/,' Total HCT Dispersion Energy :',7x,f12.4)
      end if
      return
      end
c
c
c     ################################################################
c     ##                                                            ##
c     ##  subroutine ewca3x  --  alternative WCA energy & analysis  ##
c     ##                                                            ##
c     ################################################################
c
c
c     "ewca3x" finds the Weeks-Chandler-Anderson dispersion energy
c     of a solute using a numerical "onion shell" method; also
c     partitions the energy among the atoms
c
c
      subroutine ewca3x (edisp,aedisp)
      implicit none
      include 'sizes.i'
      include 'atoms.i'
      include 'atmtyp.i'
      include 'couple.i'
      include 'inform.i'
      include 'iounit.i'
      include 'kvdws.i'
      include 'math.i'
      include 'npolar.i'
      include 'solute.i'
      include 'vdw.i'
      integer i,j,k
      real*8 edisp,e
      real*8 t,tinit,offset
      real*8 ratio,rinit
      real*8 rmult,rswitch
      real*8 rmax,shell
      real*8 inner,outer
      real*8 area,fraction
      real*8 epsi,rmini
      real*8 epsoi,rminoi
      real*8 epshi,rminhi
      real*8 oer7,oer14
      real*8 her7,her14
      real*8 roff(maxatm)
      real*8 aedisp(maxatm)
      logical done
c
c
c     zero out the WCA dispersion energy and partitioning
c
      edisp = 0.0d0
      do i = 1, n
         aedisp(i) = 0.0d0
      end do
c
c     set parameters for high accuracy numerical shells
c
c     tinit = 0.2d0
c     rinit = 1.0d0
c     rmult = 1.5d0
c     rswitch = 7.0d0
c     rmax = 12.0d0
c
c     set parameters for medium accuracy numerical shells
c
      tinit = 1.0d0
      rinit = 1.0d0
      rmult = 2.0d0
      rswitch = 5.0d0
      rmax = 9.0d0
c
c     set parameters for low accuracy numerical shells
c
c     tinit = 1.0d0
c     rinit = 1.0d0
c     rmult = 2.0d0
c     rswitch = 4.0d0
c     rmax = 7.0d0
c
c     set parameters for atomic radii and probe radii
c
      offset = 0.55d0
      do i = 1, n
         rdisp(i) = rad(class(i)) + 0.27d0
         roff(i) = rdisp(i) + offset
      end do
c
c     print header for output of the detailed energy components
c
      if (debug) then
         write (iout,10)
   10    format (/,' Onion Shell Implicit Solvation Dispersion :',/)
      end if
c
c     compute the dispersion energy for each atom in the system
c
      do i = 1, n
         epsi = eps(class(i))
         rmini = rad(class(i))
         epsoi = 4.0d0 * epso * epsi / ((sqrt(epso)+sqrt(epsi))**2)
         rminoi = 2.0d0 * (rmino**3+rmini**3) / (rmino**2+rmini**2)
         epshi = 4.0d0 * epsh * epsi / ((sqrt(epsh)+sqrt(epsi))**2)
         rminhi = 2.0d0 * (rminh**3+rmini**3) / (rminh**2+rmini**2)
         her7 = epshi * rminhi**7
         oer7 = epsoi * rminoi**7
         her14 = epshi * rminhi**14
         oer14 = epsoi * rminoi**14
c
c     alter radii values for atoms attached to current atom
c
         roff(i) = rdisp(i)
         do j = 1, n12(i)
            k = i12(j,i)
            roff(k) = rdisp(k)
         end do
         do j = 1, n13(i)
            k = i13(j,i)
            roff(k) = rdisp(k)
         end do
         do j = 1, n14(i)
            k = i14(j,i)
            roff(k) = rdisp(k)
         end do
         do j = 1, n15(i)
            k = i15(j,i)
            roff(k) = rdisp(k)
         end do
c
c     get the dispersion energy via a series of "onion" shells
c
         t = tinit
         ratio = rinit
         e = 0.0d0
         done = .false.
         dowhile (.not. done)
            inner = roff(i)
            outer = inner + t
            roff(i) = 0.5d0 * (inner+outer)
            call surfatom (i,area,roff)
            fraction = area / (4.0d0*pi*roff(i)**2)
            if (outer .lt. rminoi) then
               shell = (outer**3-inner**3)/3.0d0
               e = e - epsoi*fraction*shell
            else if (inner .gt. rminoi) then
               shell = (1.0d0/(inner**4)-1.0d0/(outer**4)) / 4.0d0
               e = e - 2.0d0*oer7*fraction*shell
               shell = (1.0d0/(inner**11)-1.0d0/(outer**11)) / 11.0d0
               e = e + oer14*fraction*shell
            else
               shell = (rminoi**3-inner**3)/3.0d0
               e = e - epsoi*fraction*shell
               shell = (1.0d0/(rminoi**4)-1.0d0/(outer**4)) / 4.0d0
               e = e - 2.0d0*oer7*fraction*shell
               shell = (1.0d0/(rminoi**11)-1.0d0/(outer**11)) / 11.0d0
               e = e + oer14*fraction*shell
            end if
            if (outer .lt. rminhi) then
               shell = (outer**3-inner**3)/3.0d0
               e = e - 2.0d0*epshi*fraction*shell
            else if (inner .gt. rminhi) then
               shell = (1.0d0/(inner**4)-1.0d0/(outer**4)) / 4.0d0
               e = e - 4.0d0*her7*fraction*shell
               shell = (1.0d0/(inner**11)-1.0d0/(outer**11)) / 11.0d0
               e = e + 2.0d0*her14*fraction*shell
            else
               shell = (rminhi**3-inner**3)/3.0d0
               e = e - 2.0d0*epshi*fraction*shell
               shell = (1.0d0/(rminhi**4)-1.0d0/(outer**4)) / 4.0d0
               e = e - 4.0d0*her7*fraction*shell
               shell = (1.0d0/(rminhi**11)-1.0d0/(outer**11)) / 11.0d0
               e = e + 2.0d0*her14*fraction*shell
            end if
            if (outer .gt. rmax)  done = .true.
            if (fraction.gt.0.99d0 .and. outer.gt.rminoi)  done = .true.
            if (done) then
               e = e - 2.0d0*oer7*fraction/(4.0d0*outer**4)
               e = e + oer14*fraction/(11.0d0*outer**11)
               e = e - 4.0d0*her7*fraction/(4.0d0*outer**4)
               e = e + 2.0d0*her14*fraction/(11.0d0*outer**11)
            end if
            roff(i) = roff(i) + 0.5d0*t
            if (outer .gt. rswitch)  ratio = rmult
            t = ratio * t
c
c     print the dispersion energy for the current shell
c
c           if (debug) then
c              write (iout,20)  i,inner,outer,fraction,
c    &                          4.0d0*pi*awater*e
c 20          format (' Onion Shell :',4x,i8,2f9.2,2f12.4)
c           end if
         end do
c
c     increment the overall dispersion energy component
c
         e = 4.0d0 * pi * slevy * awater * e
         aedisp(i) = aedisp(i) + e
         edisp = edisp + e
c
c     reset the radii values for atoms attached to current atom
c
         roff(i) = rdisp(i) + offset
         do j = 1, n12(i)
            k = i12(j,i)
            roff(k) = rdisp(k) + offset
         end do
         do j = 1, n13(i)
            k = i13(j,i)
            roff(k) = rdisp(k) + offset
         end do
         do j = 1, n14(i)
            k = i14(j,i)
            roff(k) = rdisp(k) + offset
         end do
         do j = 1, n15(i)
            k = i15(j,i)
            roff(k) = rdisp(k) + offset
         end do
      end do
c
c     print the total dispersion energy and energy for each atom
c
      if (debug) then
         write (iout,30)
   30    format ()
         do i = 1, n
            write (iout,40)  i,aedisp(i)
   40       format (' Dispersion Energy for Atom',i6,' :',2x,f12.4)
         end do
         write (iout,50)  edisp
   50    format (/,' Total Onion Dispersion Energy :',5x,f12.4)
      end if
      return
      end
c
c
c     ################################################################
c     ##                                                            ##
c     ##  subroutine ehpmf3  --  hydrophobic PMF energy & analysis  ##
c     ##                                                            ##
c     ################################################################
c
c
c     "ehpmf3" calculates the hydrophobic potential of mean force
c     nonpolar energy; also partitions the energy among the atoms
c
c     literature reference:
c
c     M. S. Lin, N. L. Fawzi and T. Head-Gordon, "Hydrophobic
c     Potential of Mean Force as a Solvation Function for Protein
c     Structure Prediction", Structure, 15, 727-740 (2007)
c
c
      subroutine ehpmf3 (ehp,nehp,aehp)
      implicit none
      include 'sizes.i'
      include 'atmtyp.i'
      include 'atoms.i'
      include 'couple.i'
      include 'hpmf.i'
      include 'math.i'
      integer i,j,k,m
      integer ii,jj,kk
      integer nehp,sschk
      integer omit(maxatm)
      real*8 xr,yr,zr,r,r2
      real*8 e,ehp
      real*8 rsurf,pisurf
      real*8 hpmfcut2
      real*8 saterm,sasa
      real*8 rbig,rsmall
      real*8 part,cutv
      real*8 e1,e2,e3,sum
      real*8 arg1,arg2,arg3
      real*8 arg12,arg22,arg32
      real*8 cutmtx(maxatm)
      real*8 aehp(maxatm)
c
c
c     zero out the hydrophobic PMF energy and partitioning
c
      ehp = 0.0d0
      nehp = 0
      do i = 1, n
         aehp(i) = 0.0d0
      end do
c
c     set some values needed during the HPMF calculation
c
      rsurf = rcarbon + 2.0d0*rwater
      pisurf = pi * (rcarbon+rwater)
      hpmfcut2 = hpmfcut * hpmfcut
c
c     get the solvent accessible surface area for each atom
c
      do ii = 1, npmf
         i = ipmf(ii)
         saterm = acsa(i)
         sasa = 1.0d0
         do k = 1, n
            if (i .ne. k) then
               xr = x(i) - x(k)
               yr = y(i) - y(k)
               zr = z(i) - z(k)
               r2 = xr*xr + yr*yr + zr*zr
               rbig = rpmf(k) + rsurf
               if (r2 .le. rbig*rbig) then
                  r = sqrt(r2)         
                  rsmall = rpmf(k) - rcarbon
                  part = pisurf * (rbig-r) * (1.0d0+rsmall/r)
                  sasa = sasa * (1.0d0-saterm*part)
               end if
            end if
         end do
         sasa = acsurf * sasa
         cutv = tanh(tslope*(sasa-toffset))
         cutmtx(i) = 0.5d0 * (1.0d0+cutv)
      end do
c
c     find the hydrophobic PMF energy via a double loop search
c
      do i = 1, n
         omit(i) = 0
      end do
      do ii = 1, npmf-1
         i = ipmf(ii)
         sschk = 0
         do j = 1, n12(i)
            k = i12(j,i)
            omit(k) = i
            if (atomic(k) .eq. 16)  sschk = k
         end do
         do j = 1, n13(i)
            k = i13(j,i)
            omit(k) = i
         end do
         do j = 1, n14(i)
            k = i14(j,i)
            omit(k) = i
            if (sschk .ne. 0) then
               do jj = 1, n12(k)
                  m = i12(jj,k)
                  if (atomic(m) .eq. 16) then
                     do kk = 1, n12(m)
                        if (i12(kk,m) .eq. sschk)  omit(k) = 0
                     end do
                  end if
               end do
            end if
         end do
         do kk = ii+1, npmf
            k = ipmf(kk)
            if (omit(k) .ne. i) then
               xr = x(i) - x(k)
               yr = y(i) - y(k)
               zr = z(i) - z(k)
               r2 = xr*xr + yr*yr + zr*zr
               if (r2 .le. hpmfcut2) then
                  r = sqrt(r2)
                  arg1 = (r-c1) * w1
                  arg12 = arg1 * arg1
                  arg2 = (r-c2) * w2
                  arg22 = arg2 * arg2
                  arg3 = (r-c3) * w3
                  arg32 = arg3 * arg3
                  e1 = h1 * exp(-arg12)
                  e2 = h2 * exp(-arg22)
                  e3 = h3 * exp(-arg32)
                  sum = e1 + e2 + e3
                  e = sum * cutmtx(i) * cutmtx(k)
                  ehp = ehp + e
                  nehp = nehp + 1
                  aehp(i) = aehp(i) + 0.5d0*e
                  aehp(k) = aehp(k) + 0.5d0*e
               end if
            end if
         end do
      end do
      return
      end
