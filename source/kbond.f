c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ###############################################################
c     ##                                                           ##
c     ##  subroutine kbond  --  bond stretch parameter assignment  ##
c     ##                                                           ##
c     ###############################################################
c
c
c     "kbond" assigns a force constant and ideal bond length
c     to each bond in the structure and processes any new or
c     changed parameter values
c
c
      subroutine kbond
      use sizes
      use atomid
      use atoms
      use bndstr
      use couple
      use fields
      use inform
      use iounit
      use kbonds
      use keys
      use potent
      implicit none
      integer i,j
      integer ia,ib,ita,itb
      integer nb,nb5,nb4,nb3
      integer size,next
      integer minat,iring
      real*8 fc,bd
      logical header,done
      logical use_ring
      character*4 pa,pb
      character*6 label
      character*8 blank,pt
      character*20 keyword
      character*120 record
      character*120 string
c
c
c     process keywords containing bond stretch parameters
c
      blank = '        '
      header = .true.
      do i = 1, nkey
         next = 1
         record = keyline(i)
         call gettext (record,keyword,next)
         call upcase (keyword)
         iring = -1
         if (keyword(1:5) .eq. 'BOND ')  iring = 0
         if (keyword(1:6) .eq. 'BOND5 ')  iring = 5
         if (keyword(1:6) .eq. 'BOND4 ')  iring = 4
         if (keyword(1:6) .eq. 'BOND3 ')  iring = 3
         if (iring .ge. 0) then
            ia = 0
            ib = 0
            fc = 0.0d0
            bd = 0.0d0
            string = record(next:120)
            read (string,*,err=10,end=10)  ia,ib,fc,bd
   10       continue
            if (.not. silent) then
               if (header) then
                  header = .false.
                  write (iout,20)
   20             format (/,' Additional Bond Stretching Parameters :',
     &                    //,5x,'Atom Classes',9x,'K(S)',6x,'Length',/)
               end if
               if (iring .eq. 0) then
                  write (iout,30)  ia,ib,fc,bd
   30             format (6x,2i4,4x,f12.3,f12.4)
               else
                  if (iring .eq. 5)  label = '5-Ring'
                  if (iring .eq. 4)  label = '4-Ring'
                  if (iring .eq. 3)  label = '3-Ring'
                  write (iout,40)  ia,ib,fc,bd,label
   40             format (6x,2i4,4x,f12.3,f12.4,3x,a6)
               end if
            end if
            size = 4
            call numeral (ia,pa,size)
            call numeral (ib,pb,size)
            if (ia .le. ib) then
               pt = pa//pb
            else
               pt = pb//pa
            end if
            if (iring .eq. 0) then
               do j = 1, maxnb
                  if (kb(j).eq.blank .or. kb(j).eq.pt) then
                     kb(j) = pt
                     bcon(j) = fc
                     blen(j) = bd
                     goto 60
                  end if
               end do
               write (iout,50)
   50          format (/,' KBOND  --  Too many Bond Stretching',
     &                       ' Parameters')
               abort = .true.
   60          continue
            else if (iring .eq. 5) then
               do j = 1, maxnb5
                  if (kb5(j).eq.blank .or. kb5(j).eq.pt) then
                     kb5(j) = pt
                     bcon5(j) = fc
                     blen5(j) = bd
                     goto 80
                  end if
               end do
               write (iout,70)
   70          format (/,' KBOND  --  Too many 5-Ring Stretching',
     &                       ' Parameters')
               abort = .true.
   80          continue
            else if (iring .eq. 4) then
               do j = 1, maxnb4
                  if (kb4(j).eq.blank .or. kb4(j).eq.pt) then
                     kb4(j) = pt
                     bcon4(j) = fc
                     blen4(j) = bd
                     goto 100
                  end if
               end do
               write (iout,90)
   90          format (/,' KBOND  --  Too many 4-Ring Stretching',
     &                       ' Parameters')
               abort = .true.
  100          continue
            else if (iring .eq. 3) then
               do j = 1, maxnb3
                  if (kb3(j).eq.blank .or. kb3(j).eq.pt) then
                     kb3(j) = pt
                     bcon3(j) = fc
                     blen3(j) = bd
                     goto 120
                  end if
               end do
               write (iout,110)
  110          format (/,' KBOND  --  Too many 3-Ring Stretching',
     &                       ' Parameters')
               abort = .true.
  120          continue
            end if
         end if
      end do
c
c     determine the total number of forcefield parameters
c
      nb = maxnb
      nb5 = maxnb5
      nb4 = maxnb4
      nb3 = maxnb3
      do i = maxnb, 1, -1
         if (kb(i) .eq. blank)  nb = i - 1
      end do
      do i = maxnb5, 1, -1
         if (kb5(i) .eq. blank)  nb5 = i - 1
      end do
      do i = maxnb4, 1, -1
         if (kb4(i) .eq. blank)  nb4 = i - 1
      end do
      do i = maxnb3, 1, -1
         if (kb3(i) .eq. blank)  nb3 = i - 1
      end do
      use_ring = .false.
      if (min(nb5,nb4,nb3) .ne. 0)  use_ring = .true.
c
c     perform dynamic allocation of some global arrays
c
      if (.not. allocated(bk))  allocate (bk(nbond))
      if (.not. allocated(bl))  allocate (bl(nbond))
c
c     assign ideal bond length and force constant for each bond
c
      header = .true.
      do i = 1, nbond
         ia = ibnd(1,i)
         ib = ibnd(2,i)
         ita = atom(ia)%class
         itb = atom(ib)%class
         size = 4
         call numeral (ita,pa,size)
         call numeral (itb,pb,size)
         if (ita .le. itb) then
            pt = pa//pb
         else
            pt = pb//pa
         end if
         bk(i) = 0.0d0
         bl(i) = 0.0d0
         done = .false.
c
c     make a check for bonds contained inside small rings
c
         iring = 0
         if (use_ring) then
            call chkring (iring,ia,ib,0,0)
            if (iring .eq. 6)  iring = 0
            if (iring.eq.5 .and. nb5.eq.0)  iring = 0
            if (iring.eq.4 .and. nb4.eq.0)  iring = 0
            if (iring.eq.3 .and. nb3.eq.0)  iring = 0
         end if
c
c     assign bond stretching parameters for each bond
c
         if (iring .eq. 0) then
            do j = 1, nb
               if (kb(j) .eq. pt) then
                  bk(i) = bcon(j)
                  bl(i) = blen(j)
                  done = .true.
                  goto 130
               end if
            end do
c
c     assign stretching parameters for 5-membered ring bonds
c
         else if (iring .eq. 5) then
            do j = 1, nb5
               if (kb5(j) .eq. pt) then
                  bk(i) = bcon5(j)
                  bl(i) = blen5(j)
                  done = .true.
                  goto 130
               end if
            end do
c
c     assign stretching parameters for 4-membered ring bonds
c
         else if (iring .eq. 4) then
            do j = 1, nb4
               if (kb4(j) .eq. pt) then
                  bk(i) = bcon4(j)
                  bl(i) = blen4(j)
                  done = .true.
                  goto 130
               end if
            end do
c
c     assign stretching parameters for 3-membered ring bonds
c
         else if (iring .eq. 3) then
            do j = 1, nb3
               if (kb3(j) .eq. pt) then
                  bk(i) = bcon3(j)
                  bl(i) = blen3(j)
                  done = .true.
                  goto 130
               end if
            end do
         end if
c
c     warning if suitable bond stretching parameter not found
c
  130    continue
         minat = min(atom(ia)%atomic,atom(ib)%atomic)
         if (minat .eq. 0)  done = .true.
         if (.not.done) then
            abort = .true.
            if (header) then
               header = .false.
               write (iout,140)
  140          format (/,' Undefined Bond Stretching Parameters :',
     &                 //,' Type',13x,'Atom Names',11x,
     &                    'Atom Classes',/)
            end if
            label = 'Bond  '
            if (iring .eq. 5)  label = '5-Ring'
            if (iring .eq. 4)  label = '4-Ring'
            if (iring .eq. 3)  label = '3-Ring'
            write (iout,150)  label,ia,atom(ia)%name,ib,atom(ib)%name,
     &           ita,itb
  150       format (1x,a6,5x,i6,'-',a3,i6,'-',a3,7x,2i5)
         end if
      end do
c
c     turn off the bond stretch potential if it is not used
c
      if (nbond .eq. 0)  use_bond = .false.
      return
      end
