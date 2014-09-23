c
c
c     ###################################################
c     ##  COPYRIGHT (C)  2000  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ###############################################################
c     ##                                                           ##
c     ##  subroutine chkxyz  --  check for coincident coordinates  ##
c     ##                                                           ##
c     ###############################################################
c
c
c     "chkxyz" finds any pairs of atoms with identical Cartesian
c     coordinates, and prints a warning message
c
c
      subroutine chkxyz (clash)
      use sizes
      use atoms
      use iounit
      implicit none
      integer i,j
      real*8 xi,yi,zi
      real*8 eps,r2
      logical clash
      logical header
c
c
c     initialize atom collision flag and distance tolerance
c
      clash = .false.
      eps   = 0.000001d0
c
c     loop over atom pairs testing for identical coordinates
c
      header = .true.
      do i = 1, n-1
         xi = atom(i)%pos(1)
         yi = atom(i)%pos(2)
         zi = atom(i)%pos(3)
         do j = i+1, n
            r2 = (atom(j)%pos(1)-xi)**2 + (atom(j)%pos(2)-yi)**2 
     &           + (atom(j)%pos(3)-zi)**2
            if (r2 .lt. eps) then
               clash = .true.
               if (header) then
                  header = .false.
                  write (iout,10)
   10             format ()
               end if
               write (iout,20)  i,j
   20          format (' CHKXYZ  --  Warning, Atoms',i6,' and',i6,
     &                    ' have Identical Coordinates')
            end if
         end do
      end do
      return
      end
