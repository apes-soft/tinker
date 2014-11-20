c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ##########################################################
c     ##                                                      ##
c     ##  subroutine torsions  --  locate and store torsions  ##
c     ##                                                      ##
c     ##########################################################
c
c
c     "torsions" finds the total number of dihedral angles and
c     the numbers of the four atoms defining each dihedral angle
c
c
      subroutine torsions
      use sizes
      use atoms
      use bndstr
      use couple
      use iounit
      use tors
      implicit none
      integer i,j,k
      integer ia,ib,ic,id
c
c
c     perform dynamic allocation of some global arrays
c
      if (.not. allocated(itors))  allocate (itors(4,6*n))
c
c     loop over all bonds, storing the atoms in each torsion
c
      ntors = 0
      do i = 1, nbond
         ib = ibnd(1,i)
         ic = ibnd(2,i)
         do j = 1, n12(ib)
            ia = atom(ib)%i12(j)
            if (ia .ne. ic) then
               do k = 1, n12(ic)
                  id = atom(ic)%i12(k)
                  if (id.ne.ib .and. id.ne.ia) then
                     ntors = ntors + 1
                     if (ntors .gt. maxtors) then
                        write (iout,10)
   10                   format (/,' TORSIONS  --  Too many Torsional',
     &                             ' Angles; Increase MAXTORS')
                        call fatal
                     end if
                     itors(1,ntors) = ia
                     itors(2,ntors) = ib
                     itors(3,ntors) = ic
                     itors(4,ntors) = id
                  end if
               end do
            end if
         end do
      end do
      return
      end
