c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1997  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine initprm  --  initialize force field parameters  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "initprm" completely initializes a force field by setting all
c     parameters to zero and using defaults for control values
c
c
      subroutine initprm
      use sizes
      use angpot
      use bndpot
      use chgpot
      use fields
      use kangs
      use katoms
      use kbonds
      use kmulti
      use kopbnd
      use korbs
      use kpitor
      use kpolr
      use kstbnd
      use ktorsn
      use ktrtor
      use kurybr
      use kvdws
      use kvdwpr
      use math
      use mplpot
      use polpot
      use urypot
      use torpot
      use units
      use vdwpot
      implicit none
      integer i,j
      character*3 blank3
      character*8 blank8
      character*12 blank12
      character*16 blank16
      character*20 blank20
      character*24 blank24
c
c
c     define blank character strings of various lengths
c
      blank3 = '   '
      blank8 = '        '
      blank12 = '            '
      blank16 = '                '
      blank20 = '                    '
      blank24 = '                        '
c
c     initialize strings of parameter atom types and classes
c
      do i = 1, maxnvp
         kvpr(i) = blank8
      end do
      do i = 1, maxnb
         kb(i) = blank8
      end do
      do i = 1, maxnb5
         kb5(i) = blank8
      end do
      do i = 1, maxnb4
         kb4(i) = blank8
      end do
      do i = 1, maxnb3
         kb3(i) = blank8
      end do
      do i = 1, maxnel
         kel(i) = blank12
      end do
      do i = 1, maxna
         ka(i) = blank12
      end do
      do i = 1, maxna5
         ka5(i) = blank12
      end do
      do i = 1, maxna4
         ka4(i) = blank12
      end do
      do i = 1, maxna3
         ka3(i) = blank12
      end do
      do i = 1, maxnaf
         kaf(i) = blank12
      end do
      do i = 1, maxnsb
         ksb(i) = blank12
      end do
      do i = 1, maxnu
         ku(i) = blank12
      end do
      do i = 1, maxnopb
         kopb(i) = blank8
      end do
      do i = 1, maxnt
         kt(i) = blank16
      end do
      do i = 1, maxnt5
         kt5(i) = blank16
      end do
      do i = 1, maxnt4
         kt4(i) = blank16
      end do
      do i = 1, maxnpt
         kpt(i) = blank8
      end do
      do i = 1, maxntt
         ktt(i) = blank20
      end do
      do i = 1, maxnmp
         kmp(i) = blank12
      end do
      do i = 1, maxnpi
         kpi(i) = blank8
      end do
      do i = 1, maxnpi5
         kpi5(i) = blank8
      end do
      do i = 1, maxnpi4
         kpi4(i) = blank8
      end do
c
c     initialize values of some force field parameters
c
      forcefield = blank20
      do i = 1, maxtyp
         symbol(i)   = blank3
         atmcls(i)   = 0
         atmnum(i)   = 0
         weight(i)   = 0.0d0
         ligand(i)   = 0
         describe(i) = blank24
         rad(i)      = 0.0d0
         eps(i)      = 0.0d0
         rad4(i)     = 0.0d0
         eps4(i)     = 0.0d0
         reduct(i)   = 0.0d0
         polr(i)     = 0.0d0
         athl(i)     = 0.0d0
         do j = 1, maxbonds
            pgrp(j,i) = 0
         end do
      end do
      do i = 1, maxclass
         do j = 1, 2
            stbn(j,i) = 0.0d0
         end do
         electron(i) = 0.0d0
         ionize(i)   = 0.0d0
         repulse(i)  = 0.0d0
      end do
      do i = 1, maxbio
         biotyp(i) = 0
      end do
c
c     set default control parameters for local geometry terms
c
      bndtyp   = 'HARMONIC'
      bndunit  = 1.0d0
      cbnd     = 0.0d0
      qbnd     = 0.0d0
      angunit  = 1.0d0 / radian**2
      cang     = 0.0d0
      qang     = 0.0d0
      pang     = 0.0d0
      sang     = 0.0d0
      stbnunit = 1.0d0 / radian
      ureyunit = 1.0d0
      cury     = 0.0d0
      qury     = 0.0d0
      aaunit   = 1.0d0 / radian**2
      opbtyp   = 'W-D-C'
      opbunit  = 1.0d0 / radian**2
      copb     = 0.0d0
      qopb     = 0.0d0
      popb     = 0.0d0
      sopb     = 0.0d0
      opdunit  = 1.0d0
      copd     = 0.0d0
      qopd     = 0.0d0
      popd     = 0.0d0
      sopd     = 0.0d0
      idihunit = 1.0d0
      itorunit = 1.0d0
      torsunit = 1.0d0
      ptorunit = 1.0d0
      storunit = 1.0d0
      atorunit = 1.0d0
      ttorunit = 1.0d0
c
c     set default control parameters for van der Waals terms
c
      vdwindex  = 'CLASS'
      vdwtyp    = 'LENNARD-JONES'
      radrule   = 'ARITHMETIC'
      radtyp    = 'R-MIN'
      radsiz    = 'RADIUS'
      epsrule   = 'GEOMETRIC'
      gausstyp  = 'NONE'
      ngauss    = 0
      abuck     = 0.0d0
      bbuck     = 0.0d0
      cbuck     = 0.0d0
      ghal      = 0.12d0
      dhal      = 0.07d0
      v2scale   = 0.0d0
      v3scale   = 0.0d0
      v4scale   = 1.0d0
      v5scale   = 1.0d0
      use_vcorr = .false.
c
c     set default control parameters for charge-charge terms
c
      electric = coulomb
      dielec   = 1.0d0
      ebuffer  = 0.0d0
      c2scale  = 0.0d0
      c3scale  = 0.0d0
      c4scale  = 1.0d0
      c5scale  = 1.0d0
      neutnbr  = .false.
      neutcut  = .false.
c
c     set default control parameters for polarizable multipoles
c
      m2scale  = 0.0d0
      m3scale  = 0.0d0
      m4scale  = 1.0d0
      m5scale  = 1.0d0
      p2scale  = 0.0d0
      p3scale  = 0.0d0
      p4scale  = 1.0d0
      p5scale  = 1.0d0
      p41scale = 0.5d0
c
c     set default control parameters for induced dipoles
c
      poltyp  = 'MUTUAL'
      politer = 500
      poleps  = 0.00001d0
      udiag   = 2.0d0
      d1scale = 0.0d0
      d2scale = 1.0d0
      d3scale = 1.0d0
      d4scale = 1.0d0
      u1scale = 1.0d0
      u2scale = 1.0d0
      u3scale = 1.0d0
      u4scale = 1.0d0
      return
      end
