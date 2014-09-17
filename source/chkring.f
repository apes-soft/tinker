c
c
c     ###################################################
c     ##  COPYRIGHT (C)  2004  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ##############################################################
c     ##                                                          ##
c     ##  subroutine chkring  --  check atom set for small rings  ##
c     ##                                                          ##
c     ##############################################################
c
c
c     "chkring" tests an atom or a set of connected atoms for
c     their presence within a single 3- to 6-membered ring
c
c
      subroutine chkring (iring,ia,ib,ic,id)
      use sizes
      use couple
      use atoms
      implicit none
      integer i,j,k,m,p,q,r
      integer ia,ib,ic,id
      integer iring,nset
c
c
c     initialize the ring size and number of atoms to test
c
      iring = 0
      nset = 0
      if (ia .gt. 0)  nset = 1
      if (ib .gt. 0)  nset = 2
      if (ic .gt. 0)  nset = 3
      if (id .gt. 0)  nset = 4
c
c     cannot be in a ring if the terminal atoms are univalent
c
      if (nset .eq. 1) then
         if (atom(ia)%n12 .le. 1)  nset = 0
      else if (nset .eq. 2) then
         if (min(atom(ia)%n12,atom(ib)%n12) .le. 1)  nset = 0
      else if (nset .eq. 3) then
         if (min(atom(ia)%n12,atom(ic)%n12) .le. 1)  nset = 0
      else if (nset .eq. 4) then
         if (min(atom(ia)%n12,atom(id)%n12) .le. 1)  nset = 0
      end if
c
c     check the input atoms for sequential connectivity
c
      if (nset .gt. 1) then
         do j = 1, atom(ia)%n12
            i = atom(ia)%i12(j)
            if (ib .eq. i) then
               if (nset .eq. 2)  goto 10
               do k = 1, atom(ib)%n12
                  m = atom(ib)%i12(k)
                  if (ic .eq. m) then
                     if (nset .eq. 3)  goto 10
                     do p = 1, atom(ic)%n12
                        q = atom(ic)%i12(p)
                        if (id .eq. q)  goto 10
                     end do
                  end if
               end do
            end if
         end do
         nset = 0
   10    continue
      end if
c
c     check for an atom contained inside a small ring
c
      if (nset .eq. 1) then
         do j = 1, atom(ia)%n12-1
            i = atom(ia)%i12(j)
            do k = j+1, atom(ia)%n12
               m = atom(ia)%i12(k)
               do p = 1, atom(i)%n12
                  if (m .eq. atom(i)%i12(p)) then
                     iring = 3
                     goto 20
                  end if
               end do
            end do
         end do
         do j = 1, atom(ia)%n12-1
            i = atom(ia)%i12(j)
            do k = j+1, atom(ia)%n12
               m = atom(ia)%i12(k)
               do p = 1, atom(i)%n12
                  r = atom(i)%i12(p)
                  if (r .ne. ia) then
                     do q = 1, atom(m)%n12
                        if (r .eq. atom(m)%i12(q)) then
                           iring = 4
                           goto 20
                        end if
                     end do
                  end if
               end do
            end do
         end do
         do j = 1, atom(ia)%n13-1
            i = atom(ia)%i13(j)
            do k = j+1, atom(ia)%n13
               m = atom(ia)%i13(k)
               do p = 1, atom(i)%n12
                  if (m .eq. atom(i)%i12(p)) then
                     iring = 5
                     goto 20
                  end if
               end do
               do p = 1, atom(i)%n13
                  if (m .eq. atom(i)%i13(p)) then
                     iring = 6
                     goto 20
                  end if
               end do
            end do
         end do
   20    continue
c
c     check for a bond contained inside a small ring
c
      else if (nset .eq. 2) then
         do j = 1, atom(ia)%n12
            i = atom(ia)%i12(j)
            do k = 1, atom(ib)%n12
               if (i .eq. atom(ib)%i12(k)) then
                  iring = 3
                  goto 30
               end if
            end do
         end do
         do j = 1, atom(ia)%n12
            i = atom(ia)%i12(j)
            if (ib .ne. i) then
               do k = 1, atom(ib)%n12
                  m = atom(ib)%i12(k)
                  if (ia .ne. m) then
                     do p = 1, atom(i)%n12
                        if (m .eq. atom(i)%i12(p)) then
                           iring = 4
                           goto 30
                        end if
                     end do
                  end if
               end do
            end if
         end do
         do j = 1, atom(ia)%n13
            i = atom(ia)%i13(j)
            do k = 1, atom(ib)%n13
               if (i .eq. atom(ib)%i13(k)) then
                  iring = 5
                  goto 30
               end if
            end do
         end do
         do j = 1, atom(ia)%n12
            i = atom(ia)%i12(j)
            if (ib .ne. i) then
               do k = 1, atom(ib)%n13
                  m = atom(ib)%i13(k)
                  do p = 1, atom(i)%n13
                     if (m .eq. atom(i)%i13(p)) then
                        iring = 6
                        do q = 1, atom(ia)%n12
                           if (m .eq. atom(ia)%i12(q))  iring = 0
                        end do
                        if (iring .eq. 6)  goto 30
                     end if
                  end do
               end do
            end if
         end do
   30    continue
c
c     check for an angle contained inside a small ring
c
      else if (nset .eq. 3) then
         do j = 1, atom(ia)%n12
            if (ic .eq. atom(ia)%i12(j)) then
               iring = 3
               goto 40
            end if
         end do
         do j = 1, atom(ia)%n12
            i = atom(ia)%i12(j)
            if (ib .ne. i) then
               do k = 1, atom(ic)%n12
                  if (i .eq. atom(ic)%i12(k)) then
                     iring = 4
                     goto 40
                  end if
               end do
            end if
         end do
         do j = 1, atom(ia)%n12
            i = atom(ia)%i12(j)
            if (ib .ne. i) then
               do k = 1, atom(ic)%n13
                  if (i .eq. atom(ic)%i13(k)) then
                     iring = 5
                     goto 40
                  end if
               end do
            end if
         end do
         do j = 1, atom(ia)%n13
            i = atom(ia)%i13(j)
            if (ic .ne. i) then
               do k = 1, atom(ic)%n13
                  if (i .eq. atom(ic)%i13(k)) then
                     iring = 6
                     goto 40
                  end if
               end do
            end if
         end do
   40    continue
c
c     check for a torsion contained inside a small ring
c
      else if (nset .eq. 4) then
         do j = 1, atom(ia)%n12
            if (id .eq. atom(ia)%i12(j)) then
               iring = 4
               goto 50
            end if
         end do
         do j = 1, atom(ia)%n12
            i = atom(ia)%i12(j)
            if (ib .ne. i) then
               do k = 1, atom(id)%n12
                  if (i .eq. atom(id)%i12(k)) then
                     iring = 5
                     goto 50
                  end if
               end do
            end if
         end do
         do j = 1, atom(ia)%n12
            i = atom(ia)%i12(j)
            if (ib .ne. i) then
               do k = 1, atom(id)%n13
                  if (i .eq. atom(id)%i13(k)) then
                     iring = 6
                     goto 50
                  end if
               end do
            end if
         end do
   50    continue
      end if
      return
      end
