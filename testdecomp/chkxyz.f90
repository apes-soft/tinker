!
!
!     ###################################################
!     ##  COPYRIGHT (C)  2000  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ###############################################################
!     ##                                                           ##
!     ##  subroutine chkxyz  --  check for coincident coordinates  ##
!     ##                                                           ##
!     ###############################################################
!
!
!     "chkxyz" finds any pairs of atoms with identical Cartesian
!     coordinates, and prints a warning message
!
!


      subroutine chkxyz (clash)

      use sizes
      use atoms
      use iounit

      integer:: i,j
      real (kind=8):: xi,yi,zi
      real (kind=8):: eps,r2
      logical:: clash
      logical:: header
!
!
!     initialize atom collision flag and distance tolerance
!
      clash = .false.
      eps = 0.000001d0
!
!     loop over atom pairs testing for identical coordinates
!
      header = .true.
      do i = 1, n-1
         xi = x(i)
         yi = y(i)
         zi = z(i)
         do j = i+1, n
            r2 = (x(j)-xi)**2 + (y(j)-yi)**2 + (z(j)-zi)**2
            if (r2 .lt. eps) then
               clash = .true.
               if (header) then
                  header = .false.
                  write (iout,10)
   10             format ()
               end if
               write (iout,20)  i,j
   20          format (' CHKXYZ  --  Warning, Atoms',i6,' and',i6, &
                         ' have Identical Coordinates')
            end if
         end do
      end do
      return
      end

