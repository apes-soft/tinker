c
c
c     ###################################################
c     ##  COPYRIGHT (C)  2003  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ################################################################
c     ##                                                            ##
c     ##  subroutine chkpole  --  check multipoles at chiral sites  ##
c     ##                                                            ##
c     ################################################################
c
c
c     "chkpole" inverts atomic multipole moments as necessary
c     at sites with chiral local reference frame definitions
c
c
      subroutine chkpole
      use sizes
      use atoms
      use mpole
      implicit none
      integer i,k
      integer ia,ib,ic,id
      real*8 xad,yad,zad
      real*8 xbd,ybd,zbd
      real*8 xcd,ycd,zcd
      real*8 c1,c2,c3,vol
      logical check
c
c
c     loop over multipole sites testing for chirality inversion
c
      do i = 1, npole
         check = .true.
         if (polaxe(i) .ne. 'Z-then-X')  check = .false.
         if (yaxis(i) .eq. 0)  check = .false.
         if (check) then
            k = yaxis(i)
            ia = ipole(i)
            ib = zaxis(i)
            ic = xaxis(i)
            id = abs(k)
c
c     compute the signed parallelpiped volume at chiral site
c
            xad = atom(ia)%pos(1) - atom(id)%pos(1)
            yad = atom(ia)%pos(2) - atom(id)%pos(2)
            zad = atom(ia)%pos(3) - atom(id)%pos(3)
            xbd = atom(ib)%pos(1) - atom(id)%pos(1)
            ybd = atom(ib)%pos(2) - atom(id)%pos(2)
            zbd = atom(ib)%pos(3) - atom(id)%pos(3)
            xcd = atom(ic)%pos(1) - atom(id)%pos(1)
            ycd = atom(ic)%pos(2) - atom(id)%pos(2)
            zcd = atom(ic)%pos(3) - atom(id)%pos(3)
            c1 = ybd*zcd - zbd*ycd
            c2 = ycd*zad - zcd*yad
            c3 = yad*zbd - zad*ybd
            vol = xad*c1 + xbd*c2 + xcd*c3
c
c     invert atomic multipole components involving the y-axis
c
            if (k.lt.0.and.vol.gt.0.0d0 .or.
     &          k.gt.0.and.vol.lt.0.0d0) then
               yaxis(i) = -k
               pole(3,i) = -pole(3,i)
               pole(6,i) = -pole(6,i)
               pole(8,i) = -pole(8,i)
               pole(10,i) = -pole(10,i)
               pole(12,i) = -pole(12,i)
            end if
         end if
      end do
      return
      end
