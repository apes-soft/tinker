c
c
c     ###############################################################
c     ##  COPYRIGHT (C) 2006 by David Gohara & Jay William Ponder  ##
c     ##                    All Rights Reserved                    ##
c     ###############################################################
c
c     ###############################################################
c     ##                                                           ##
c     ##  subroutine nblist  --  maintain pairwise neighbor lists  ##
c     ##                                                           ##
c     ###############################################################
c
c
c     "nblist" constructs and maintains nonbonded pair neighbor lists
c     for vdw, electrostatic and polarization interactions
c
c
      subroutine nblist
      use limits
      use potent
      implicit none
c
c
c     update the vdw and electrostatic neighbor lists
c
      if (use_vdw .and. use_vlist)  call vlist1
      if ((use_charge.or.use_solv) .and. use_clist)  call clist
      if ((use_mpole.or.use_polar.or.use_solv) .and. use_mlist)
     &      call mlist1
      if (use_polar .and. use_ulist)  call ulist1
      return
      end
c
c     ##############################################################
c     ##                                                          ##
c     ##  subroutine vlist1  --  get van der Waals neighbor lists ##
c     ##                                                          ##
c     ##############################################################
c
c
c     "vlist1" performs an update or a complete rebuild of the
c     van der Waals neighbor list
c
c
      subroutine vlist1
      use sizes
      use atoms
      use bound
      use boxes
      use iounit
      use neigh
      use vdw
      use openmp
      implicit none
      integer i,j,k
      integer ii,iv
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 radius
      real*8 rdn,r2

c
c     apply reduction factors to find coordinates for each site
c
!$OMP DO schedule(static,128) private(i,ii,iv,rdn)
      do i = 1, nvdw
         ii = ivdw(i)
         iv = ired(ii)
         rdn = kred(ii)
         xred_th(i) = rdn*(x(ii)-x(iv)) + x(iv)
         yred_th(i) = rdn*(y(ii)-y(iv)) + y(iv)
         zred_th(i) = rdn*(z(ii)-z(iv)) + z(iv)
      end do
!$OMP end DO 
c
c     neighbor list cannot be used with the replicates method
c
!$OMP master
      do i=1,nthread
         do_list(i) = dovlst
      end do
      radius = sqrt(vbuf2)
      call replica (radius)
      if (use_replica) then
         write (iout,10)
   10    format (/,' VLIST  --  Pairwise Neighbor List cannot',
     &              ' be used with Replicas')
         call fatal
      end if
!$OMP end master
!$OMP barrier
c
c     perform a complete list build instead of an update
c
c      if (dovlst) then
      if(do_list(th_id)) then
         do_list(th_id) = .false.
         dovlst = .false.
         if (octahedron) then
            call vbuild1 !(xred_th,yred_th,zred_th)
         else
            call vlight1 !(xred_th,yred_th,zred_th)
         end if
         return
      end if
c
c     test sites for displacement exceeding half the buffer, and
c     rebuild the higher numbered neighbors of updated sites
c

!$OMP DO schedule(guided) private(i,j,k,xi,yi,zi,xr,yr,zr,r2)

      do i = 1, nvdw
         xi = xred_th(i)
         yi = yred_th(i)
         zi = zred_th(i)
         xr = xi - xvold(i)
         yr = yi - yvold(i)
         zr = zi - zvold(i)
         call imagen (xr,yr,zr)
         r2 = xr*xr + yr*yr + zr*zr
         update_omp(i) = .false.
         if (r2 .ge. lbuf2) then
            update_omp(i) = .true.
            xvold(i) = xi
            yvold(i) = yi
            zvold(i) = zi
            nvlst(i) = 0
            do k = i+1, nvdw
               xr = xi - xred_th(k)
               yr = yi - yred_th(k)
               zr = zi - zred_th(k)
               call imagen (xr,yr,zr)
               r2 = xr*xr + yr*yr + zr*zr
               if (r2 .le. vbuf2) then
                  nvlst(i) = nvlst(i) + 1
                  vlst(nvlst(i),i) = k
               end if
            end do
         end if
      end do
!$OMP END DO
c
c     adjust lists of lower numbered neighbors of updated sites
c
!$OMP DO schedule(guided)
      do i = 1, nvdw
         if (update_omp(i)) then
            xi = xred_th(i)
            yi = yred_th(i)
            zi = zred_th(i)
            do k = 1, i-1
               if (.not. update_omp(k)) then
                  xr = xi - xvold(k)
                  yr = yi - yvold(k)
                  zr = zi - zvold(k)
                  call imagen (xr,yr,zr)
                  r2 = xr*xr + yr*yr + zr*zr
                  if (r2 .le. vbuf2) then
!$OMP CRITICAL
                     do j = 1, nvlst(k)
                        if (vlst(j,k) .eq. i)  goto 20
                     end do
                     nvlst(k) = nvlst(k) + 1
                     vlst(nvlst(k),k) = i
   20                continue
!$OMP END CRITICAL
                  else if (r2 .le. vbufx) then
!$OMP CRITICAL
                     do j = 1, nvlst(k)
                        if (vlst(j,k) .eq. i) then
                           vlst(j,k) = vlst(nvlst(k),k)
                           nvlst(k) = nvlst(k) - 1
                           goto 30
                        end if
                     end do
   30                continue
!$OMP END CRITICAL
                  end if
               end if
            end do
         end if
      end do
!$OMP END DO
c
c     check to see if any neighbor lists are too long
c
!$OMP DO schedule(guided)
      do i = 1, nvdw
         if (nvlst(i) .ge. maxvlst) then
            write (iout,40)
   40       format (/,' VLIST  --  Too many Neighbors;',
     &                 ' Increase MAXVLST')
            call fatal
         end if
      end do
!$OMP END DO

      return
      end

c     ##############################################################
c     ##                                                          ##
c     ##  subroutine vlist  --  get van der Waals neighbor lists  ##
c     ##                                                          ##
c     ##############################################################
c
c
c     "vlist" performs an update or a complete rebuild of the
c     van der Waals neighbor list
c
c
      subroutine vlist
      use sizes
      use atoms
      use bound
      use boxes
      use iounit
      use neigh
      use vdw
      implicit none
      integer i,j,k
      integer ii,iv
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 radius
      real*8 rdn,r2
      real*8, allocatable :: xred(:)
      real*8, allocatable :: yred(:)
      real*8, allocatable :: zred(:)
      logical, allocatable :: update(:)
c
c
c     perform dynamic allocation of some local arrays
c
      allocate (xred(n))
      allocate (yred(n))
      allocate (zred(n))
      allocate (update(n))
c
c     apply reduction factors to find coordinates for each site
c
      do i = 1, nvdw
         ii = ivdw(i)
         iv = ired(ii)
         rdn = kred(ii)
         xred(i) = rdn*(x(ii)-x(iv)) + x(iv)
         yred(i) = rdn*(y(ii)-y(iv)) + y(iv)
         zred(i) = rdn*(z(ii)-z(iv)) + z(iv)
      end do
c
c     neighbor list cannot be used with the replicates method
c
      radius = sqrt(vbuf2)
      call replica (radius)
      if (use_replica) then
         write (iout,10)
   10    format (/,' VLIST  --  Pairwise Neighbor List cannot',
     &              ' be used with Replicas')
         call fatal
      end if
c
c     perform a complete list build instead of an update
c
      if (dovlst) then
         dovlst = .false.
         if (octahedron) then
            call vbuild (xred,yred,zred)
         else
            call vlight (xred,yred,zred)
         end if
         return
      end if
c
c     test sites for displacement exceeding half the buffer, and
c     rebuild the higher numbered neighbors of updated sites
c
c!$OMP PARALLEL default(none) shared(nvdw,xred,yred,zred,xvold,
c!$OMP& yvold,zvold,update,lbuf2,nvlst,vbuf2,vlst,vbufx,maxvlst,
c!$OMP& iout) private(i,j,k,xi,yi,zi,xr,yr,zr,r2)
c!$OMP DO schedule(guided)
      do i = 1, nvdw
         xi = xred(i)
         yi = yred(i)
         zi = zred(i)
         xr = xi - xvold(i)
         yr = yi - yvold(i)
         zr = zi - zvold(i)
         call imagen (xr,yr,zr)
         r2 = xr*xr + yr*yr + zr*zr
         update(i) = .false.
         if (r2 .ge. lbuf2) then
            update(i) = .true.
            xvold(i) = xi
            yvold(i) = yi
            zvold(i) = zi
            nvlst(i) = 0
            do k = i+1, nvdw
               xr = xi - xred(k)
               yr = yi - yred(k)
               zr = zi - zred(k)
               call imagen (xr,yr,zr)
               r2 = xr*xr + yr*yr + zr*zr
               if (r2 .le. vbuf2) then
                  nvlst(i) = nvlst(i) + 1
                  vlst(nvlst(i),i) = k
               end if
            end do
         end if
      end do
c!$OMP END DO
c
c     adjust lists of lower numbered neighbors of updated sites
c
c!$OMP DO schedule(guided)
      do i = 1, nvdw
         if (update(i)) then
            xi = xred(i)
            yi = yred(i)
            zi = zred(i)
            do k = 1, i-1
               if (.not. update(k)) then
                  xr = xi - xvold(k)
                  yr = yi - yvold(k)
                  zr = zi - zvold(k)
                  call imagen (xr,yr,zr)
                  r2 = xr*xr + yr*yr + zr*zr
                  if (r2 .le. vbuf2) then
c!$OMP CRITICAL
                     do j = 1, nvlst(k)
                        if (vlst(j,k) .eq. i)  goto 20
                     end do
                     nvlst(k) = nvlst(k) + 1
                     vlst(nvlst(k),k) = i
   20                continue
c!$OMP END CRITICAL
                  else if (r2 .le. vbufx) then
c!$OMP CRITICAL
                     do j = 1, nvlst(k)
                        if (vlst(j,k) .eq. i) then
                           vlst(j,k) = vlst(nvlst(k),k)
                           nvlst(k) = nvlst(k) - 1
                           goto 30
                        end if
                     end do
   30                continue
c!$OMP END CRITICAL
                  end if
               end if
            end do
         end if
      end do
c!$OMP END DO
c
c     check to see if any neighbor lists are too long
c
c!$OMP DO schedule(guided)
      do i = 1, nvdw
         if (nvlst(i) .ge. maxvlst) then
            write (iout,40)
   40       format (/,' VLIST  --  Too many Neighbors;',
     &                 ' Increase MAXVLST')
            call fatal
         end if
      end do
c!$OMP END DO
c!$OMP END PARALLEL
c
c     perform deallocation of some local arrays
c
      deallocate (xred)
      deallocate (yred)
      deallocate (zred)
      deallocate (update)
      return
      end
c
c
c
c     ###########################################################
c     ##                                                       ##
c     ##  subroutine vbuild1  --  build vdw list for all sites  ##
c     ##                                                       ##
c     ###########################################################
c
c
c     "vbuild1" performs a complete rebuild of the van der Waals
c     pair neighbor for each site
c
c
      subroutine vbuild1 !(xred,yred,zred)
      use sizes
      use bound
      use iounit
      use neigh
      use vdw
      use openmp
      implicit none
      integer i,k
      real*8 xi,yi,zi
      real*8 xr,yr,zr,r2
C$$$      real*8 xred(*)
C$$$      real*8 yred(*)
C$$$      real*8 zred(*)
c
c
c     set OpenMP directives for the major loop structure
c
c!$OMP PARALLEL default(shared) private(i,k,xi,yi,zi,xr,yr,zr,r2)
c!$OMP DO schedule(guided)
c
c     store coordinates to reflect update of the site
c
      do i = 1, nvdw
         xi = xred_th(i)
         yi = yred_th(i)
         zi = zred_th(i)
         xvold(i) = xi
         yvold(i) = yi
         zvold(i) = zi
c
c     generate all neighbors for the site being rebuilt
c
         nvlst(i) = 0
         do k = i+1, nvdw
            xr = xi - xred_th(k)
            yr = yi - yred_th(k)
            zr = zi - zred_th(k)
            call imagen (xr,yr,zr)
            r2 = xr*xr + yr*yr + zr*zr
            if (r2 .le. vbuf2) then
               nvlst(i) = nvlst(i) + 1
               vlst(nvlst(i),i) = k
            end if
         end do
c
c     check to see if the neighbor list is too long
c
         if (nvlst(i) .ge. maxvlst) then
            write (iout,10)
   10       format (/,' VBUILD  --  Too many Neighbors;',
     &                 ' Increase MAXVLST')
            call fatal
         end if
      end do
c
c     end OpenMP directives for the major loop structure
c
c!$OMP END DO
c!$OMP END PARALLEL
      return
      end
c
c     ###########################################################
c     ##                                                       ##
c     ##  subroutine vbuild  --  build vdw list for all sites  ##
c     ##                                                       ##
c     ###########################################################
c
c
c     "vbuild" performs a complete rebuild of the van der Waals
c     pair neighbor for each site
c
c
      subroutine vbuild (xred,yred,zred)
      use sizes
      use bound
      use iounit
      use neigh
      use vdw
      implicit none
      integer i,k
      real*8 xi,yi,zi
      real*8 xr,yr,zr,r2
      real*8 xred(*)
      real*8 yred(*)
      real*8 zred(*)
c
c
c     set OpenMP directives for the major loop structure
c
c!$OMP PARALLEL default(shared) private(i,k,xi,yi,zi,xr,yr,zr,r2)
c!$OMP DO schedule(guided)
c
c     store coordinates to reflect update of the site
c
      do i = 1, nvdw
         xi = xred(i)
         yi = yred(i)
         zi = zred(i)
         xvold(i) = xi
         yvold(i) = yi
         zvold(i) = zi
c
c     generate all neighbors for the site being rebuilt
c
         nvlst(i) = 0
         do k = i+1, nvdw
            xr = xi - xred(k)
            yr = yi - yred(k)
            zr = zi - zred(k)
            call imagen (xr,yr,zr)
            r2 = xr*xr + yr*yr + zr*zr
            if (r2 .le. vbuf2) then
               nvlst(i) = nvlst(i) + 1
               vlst(nvlst(i),i) = k
            end if
         end do
c
c     check to see if the neighbor list is too long
c
         if (nvlst(i) .ge. maxvlst) then
            write (iout,10)
   10       format (/,' VBUILD  --  Too many Neighbors;',
     &                 ' Increase MAXVLST')
            call fatal
         end if
      end do
c
c     end OpenMP directives for the major loop structure
c
c!$OMP END DO
c!$OMP END PARALLEL
      return
      end
c
c
c     #############################################################
c     ##                                                         ##
c     ##  subroutine vlight1  --  build vdw pair list via lights  ##
c     ##                                                         ##
c     #############################################################
c
c
c     "vlight1" performs a complete rebuild of the van der Waals
c     pair neighbor list for all sites using the method of lights
c
c
      subroutine vlight1 !(xred,yred,zred)
      use sizes
      use atoms
      use bound
      use cell
      use iounit
      use light
      use neigh
      use vdw
      use openmp
      implicit none
      integer i,j,k
      integer kgy,kgz
      integer start,stop
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 r2,off
c      real*8, allocatable :: xsort(:)
c      real*8, allocatable :: ysort(:)
c      real*8, allocatable :: zsort(:)
      logical repeat
c
c
c     perform dynamic allocation of some local arrays
c
c!$OMP master
c      allocate (xsort(nvdw))
c      allocate (ysort(nvdw))
c      allocate (zsort(nvdw))
c
c     transfer interaction site coordinates to sorting arrays
c
!$OMP DO schedule(static,128)
      do i = 1, nvdw
         nvlst(i) = 0
         xvold(i) = xred_th(i)
         yvold(i) = yred_th(i)
         zvold(i) = zred_th(i)
         xsort_omp(i) = xred_th(i)
         ysort_omp(i) = yred_th(i)
         zsort_omp(i) = zred_th(i)
      end do
!$OMP end DO 
c
c     use the method of lights to generate neighbors
c
!$OMP master
      off = sqrt(vbuf2)
      call lightn (off,nvdw,xsort_omp,ysort_omp,zsort_omp)
c
c     perform deallocation of some local arrays
c
c      deallocate (xsort)
c      deallocate (ysort)
c      deallocate (zsort)
!$OMP end master
!$OMP barrier
c
c     set OpenMP directives for the major loop structure
c

!$OMP DO schedule(guided) private(i,j,k,xi,yi,zi,
!$OMP& xr,yr,zr,r2,kgy,kgz,start,stop,repeat)
c
c     loop over all atoms computing the neighbor lists
c
      do i = 1, nvdw
         xi = xred_th(i)
         yi = yred_th(i)
         zi = zred_th(i)
         if (kbx(i) .le. kex(i)) then
            repeat = .false.
            start = kbx(i)
            stop = kex(i)
         else
            repeat = .true.
            start = 1
            stop = kex(i)
         end if
   10    continue
         do j = start, stop
            k = locx(j)
            if (k .le. i)  goto 20
            kgy = rgy(k)
            if (kby(i) .le. key(i)) then
               if (kgy.lt.kby(i) .or. kgy.gt.key(i))  goto 20
            else
               if (kgy.lt.kby(i) .and. kgy.gt.key(i))  goto 20
            end if
            kgz = rgz(k)
            if (kbz(i) .le. kez(i)) then
               if (kgz.lt.kbz(i) .or. kgz.gt.kez(i))  goto 20
            else
               if (kgz.lt.kbz(i) .and. kgz.gt.kez(i))  goto 20
            end if
            xr = xi - xred_th(k)
            yr = yi - yred_th(k)
            zr = zi - zred_th(k)
            call imagen (xr,yr,zr)
            r2 = xr*xr + yr*yr + zr*zr
            if (r2 .le. vbuf2) then
               nvlst(i) = nvlst(i) + 1
               vlst(nvlst(i),i) = k
            end if
   20       continue
         end do
         if (repeat) then
            repeat = .false.
            start = kbx(i)
            stop = nvdw
            goto 10
         end if
c
c     check to see if the neighbor list is too long
c
         if (nvlst(i) .ge. maxvlst) then
            write (iout,30)
   30       format (/,' VLIGHT  --  Too many Neighbors;',
     &                 ' Increase MAXVLST')
            call fatal
         end if
      end do
c
c     end OpenMP directives for the major loop structure
c
!$OMP END DO
      return
      end

c     #############################################################
c     ##                                                         ##
c     ##  subroutine vlight  --  build vdw pair list via lights  ##
c     ##                                                         ##
c     #############################################################
c
c
c     "vlight" performs a complete rebuild of the van der Waals
c     pair neighbor list for all sites using the method of lights
c
c
      subroutine vlight (xred,yred,zred)
      use sizes
      use atoms
      use bound
      use cell
      use iounit
      use light
      use neigh
      use vdw
      implicit none
      integer i,j,k
      integer kgy,kgz
      integer start,stop
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 r2,off
      real*8 xred(*)
      real*8 yred(*)
      real*8 zred(*)
      real*8, allocatable :: xsort(:)
      real*8, allocatable :: ysort(:)
      real*8, allocatable :: zsort(:)
      logical repeat
c
c
c     perform dynamic allocation of some local arrays
c
      allocate (xsort(nvdw))
      allocate (ysort(nvdw))
      allocate (zsort(nvdw))
c
c     transfer interaction site coordinates to sorting arrays
c
      do i = 1, nvdw
         nvlst(i) = 0
         xvold(i) = xred(i)
         yvold(i) = yred(i)
         zvold(i) = zred(i)
         xsort(i) = xred(i)
         ysort(i) = yred(i)
         zsort(i) = zred(i)
      end do
c
c     use the method of lights to generate neighbors
c
      off = sqrt(vbuf2)
      call lightn (off,nvdw,xsort,ysort,zsort)
c
c     perform deallocation of some local arrays
c
      deallocate (xsort)
      deallocate (ysort)
      deallocate (zsort)
c
c     set OpenMP directives for the major loop structure
c
c!$OMP PARALLEL default(shared) private(i,j,k,xi,yi,zi,
c!$OMP& xr,yr,zr,r2,kgy,kgz,start,stop,repeat)
c!$OMP DO schedule(guided)
c
c     loop over all atoms computing the neighbor lists
c
      do i = 1, nvdw
         xi = xred(i)
         yi = yred(i)
         zi = zred(i)
         if (kbx(i) .le. kex(i)) then
            repeat = .false.
            start = kbx(i)
            stop = kex(i)
         else
            repeat = .true.
            start = 1
            stop = kex(i)
         end if
   10    continue
         do j = start, stop
            k = locx(j)
            if (k .le. i)  goto 20
            kgy = rgy(k)
            if (kby(i) .le. key(i)) then
               if (kgy.lt.kby(i) .or. kgy.gt.key(i))  goto 20
            else
               if (kgy.lt.kby(i) .and. kgy.gt.key(i))  goto 20
            end if
            kgz = rgz(k)
            if (kbz(i) .le. kez(i)) then
               if (kgz.lt.kbz(i) .or. kgz.gt.kez(i))  goto 20
            else
               if (kgz.lt.kbz(i) .and. kgz.gt.kez(i))  goto 20
            end if
            xr = xi - xred(k)
            yr = yi - yred(k)
            zr = zi - zred(k)
            call imagen (xr,yr,zr)
            r2 = xr*xr + yr*yr + zr*zr
            if (r2 .le. vbuf2) then
               nvlst(i) = nvlst(i) + 1
               vlst(nvlst(i),i) = k
            end if
   20       continue
         end do
         if (repeat) then
            repeat = .false.
            start = kbx(i)
            stop = nvdw
            goto 10
         end if
c
c     check to see if the neighbor list is too long
c
         if (nvlst(i) .ge. maxvlst) then
            write (iout,30)
   30       format (/,' VLIGHT  --  Too many Neighbors;',
     &                 ' Increase MAXVLST')
            call fatal
         end if
      end do
c
c     end OpenMP directives for the major loop structure
c
c!$OMP END DO
c!$OMP END PARALLEL
      return
      end
c
c
c
c     ###############################################################
c     ##                                                           ##
c     ##  subroutine clist  --  get partial charge neighbor lists  ##
c     ##                                                           ##
c     ###############################################################
c
c
c     "clist" performs an update or a complete rebuild of the
c     electrostatic neighbor lists for partial charges
c
c
      subroutine clist
      use sizes
      use atoms
      use bound
      use boxes
      use charge
      use iounit
      use neigh
      implicit none
      integer i,j,k
      integer ii,kk
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 radius,r2
      logical, allocatable :: update(:)
c
c
c     perform dynamic allocation of some local arrays
c
      allocate (update(n))
c
c     neighbor list cannot be used with the replicates method
c
      radius = sqrt(cbuf2)
      call replica (radius)
      if (use_replica) then
         write (iout,10)
   10    format (/,' CLIST  --  Pairwise Neighbor List cannot',
     &              ' be used with Replicas')
         call fatal
      end if
c
c     perform a complete list build instead of an update
c
      if (doclst) then
         doclst = .false.
         if (octahedron) then
            call cbuild
         else
            call clight
         end if
         return
      end if
c
c     test sites for displacement exceeding half the buffer, and
c     rebuild the higher numbered neighbors of updated sites
c
c!$OMP PARALLEL default(shared) private(i,j,k,ii,kk,xi,yi,zi,xr,yr,zr,r2)
c!$OMP DO schedule(guided)
      do i = 1, nion
         ii = kion(i)
         xi = x(ii)
         yi = y(ii)
         zi = z(ii)
         xr = xi - xcold(i)
         yr = yi - ycold(i)
         zr = zi - zcold(i)
         call imagen (xr,yr,zr)
         r2 = xr*xr + yr*yr + zr*zr
         update(i) = .false.
         if (r2 .ge. lbuf2) then
            update(i) = .true.
            xcold(i) = xi
            ycold(i) = yi
            zcold(i) = zi
            nelst(i) = 0
            do k = i+1, nion
               kk = kion(k)
               xr = xi - x(kk)
               yr = yi - y(kk)
               zr = zi - z(kk)
               call imagen (xr,yr,zr)
               r2 = xr*xr + yr*yr + zr*zr
               if (r2 .le. cbuf2) then
                  nelst(i) = nelst(i) + 1
                  elst(nelst(i),i) = k
               end if
            end do
         end if
      end do
c!$OMP END DO
c
c     adjust lists of lower numbered neighbors of updated sites
c
c!$OMP DO schedule(guided)
      do i = 1, nion
         if (update(i)) then
            ii = kion(i)
            xi = x(ii)
            yi = y(ii)
            zi = z(ii)
            do k = 1, i-1
               if (.not. update(k)) then
                  xr = xi - xcold(k)
                  yr = yi - ycold(k)
                  zr = zi - zcold(k)
                  call imagen (xr,yr,zr)
                  r2 = xr*xr + yr*yr + zr*zr
                  if (r2 .le. cbuf2) then
c!$OMP CRITICAL
                     do j = 1, nelst(k)
                        if (elst(j,k) .eq. i)  goto 20
                     end do
                     nelst(k) = nelst(k) + 1
                     elst(nelst(k),k) = i
   20                continue
c!$OMP END CRITICAL
                  else if (r2 .le. cbufx) then
c!$OMP CRITICAL
                     do j = 1, nelst(k)
                        if (elst(j,k) .eq. i) then
                           elst(j,k) = elst(nelst(k),k)
                           nelst(k) = nelst(k) - 1
                           goto 30
                        end if
                     end do
   30                continue
c!$OMP END CRITICAL
                  end if
               end if
            end do
         end if
      end do
c!$OMP END DO
c
c     check to see if any neighbor lists are too long
c
c!$OMP DO schedule(guided)
      do i = 1, nion
         if (nelst(i) .ge. maxelst) then
            write (iout,40)
   40       format (/,' CLIST  --  Too many Neighbors;',
     &                 ' Increase MAXELST')
            call fatal
         end if
      end do
c!$OMP END DO
c!$OMP END PARALLEL
c
c     perform deallocation of some local arrays
c
      deallocate (update)
      return
      end
c
c
c     ##############################################################
c     ##                                                          ##
c     ##  subroutine cbuild  --  build charge list for all sites  ##
c     ##                                                          ##
c     ##############################################################
c
c
c     "cbuild" performs a complete rebuild of the partial charge
c     electrostatic neighbor list for all sites
c
c
      subroutine cbuild
      use sizes
      use atoms
      use bound
      use charge
      use iounit
      use neigh
      implicit none
      integer i,k
      integer ii,kk
      real*8 xi,yi,zi
      real*8 xr,yr,zr,r2
c
c
c     set OpenMP directives for the major loop structure
c
c!$OMP PARALLEL default(shared) private(i,k,ii,kk,xi,yi,zi,xr,yr,zr,r2)
c!$OMP DO schedule(guided)
c
c     store new coordinates to reflect update of the site
c
      do i = 1, nion
         ii = kion(i)
         xi = x(ii)
         yi = y(ii)
         zi = z(ii)
         xcold(i) = xi
         ycold(i) = yi
         zcold(i) = zi
c
c     generate all neighbors for the site being rebuilt
c
         nelst(i) = 0
         do k = i+1, nion
            kk = kion(k)
            xr = xi - x(kk)
            yr = yi - y(kk)
            zr = zi - z(kk)
            call imagen (xr,yr,zr)
            r2 = xr*xr + yr*yr + zr*zr
            if (r2 .le. cbuf2) then
               nelst(i) = nelst(i) + 1
               elst(nelst(i),i) = k
            end if
         end do
c
c     check to see if the neighbor list is too long
c
         if (nelst(i) .ge. maxelst) then
            write (iout,10)
   10       format (/,' CBUILD  --  Too many Neighbors;',
     &                 ' Increase MAXELST')
            call fatal
         end if
      end do
c
c     end OpenMP directives for the major loop structure
c
c!$OMP END DO
c!$OMP END PARALLEL
      return
      end
c
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine clight  --  get partial charge list via lights  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "clight" performs a complete rebuild of the partial charge
c     pair neighbor list for all sites using the method of lights
c
c
      subroutine clight
      use sizes
      use atoms
      use bound
      use cell
      use charge
      use iounit
      use light
      use neigh
      implicit none
      integer i,j,k
      integer ii,kk
      integer kgy,kgz
      integer start,stop
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 r2,off
      real*8, allocatable :: xsort(:)
      real*8, allocatable :: ysort(:)
      real*8, allocatable :: zsort(:)
      logical repeat
c
c
c     perform dynamic allocation of some local arrays
c
      allocate (xsort(nion))
      allocate (ysort(nion))
      allocate (zsort(nion))
c
c     transfer interaction site coordinates to sorting arrays
c
      do i = 1, nion
         nelst(i) = 0
         ii = kion(i)
         xcold(i) = x(ii)
         ycold(i) = y(ii)
         zcold(i) = z(ii)
         xsort(i) = x(ii)
         ysort(i) = y(ii)
         zsort(i) = z(ii)
      end do
c
c     use the method of lights to generate neighbors
c
      off = sqrt(cbuf2)
      call lightn (off,nion,xsort,ysort,zsort)
c
c     perform deallocation of some local arrays
c
      deallocate (xsort)
      deallocate (ysort)
      deallocate (zsort)
c
c     set OpenMP directives for the major loop structure
c
c!$OMP PARALLEL default(shared) private(i,j,k,ii,kk,xi,yi,zi,
c!$OMP& xr,yr,zr,r2,kgy,kgz,start,stop,repeat)
c!$OMP DO schedule(guided)
c
c     loop over all atoms computing the neighbor lists
c
      do i = 1, nion
         ii = kion(i)
         xi = x(ii)
         yi = y(ii)
         zi = z(ii)
         if (kbx(i) .le. kex(i)) then
            repeat = .false.
            start = kbx(i)
            stop = kex(i)
         else
            repeat = .true.
            start = 1
            stop = kex(i)
         end if
   10    continue
         do j = start, stop
            k = locx(j)
            if (k .le. i)  goto 20
            kk = kion(k)
            kgy = rgy(k)
            if (kby(i) .le. key(i)) then
               if (kgy.lt.kby(i) .or. kgy.gt.key(i))  goto 20
            else
               if (kgy.lt.kby(i) .and. kgy.gt.key(i))  goto 20
            end if
            kgz = rgz(k)
            if (kbz(i) .le. kez(i)) then
               if (kgz.lt.kbz(i) .or. kgz.gt.kez(i))  goto 20
            else
               if (kgz.lt.kbz(i) .and. kgz.gt.kez(i))  goto 20
            end if
            xr = xi - x(kk)
            yr = yi - y(kk)
            zr = zi - z(kk)
            call imagen (xr,yr,zr)
            r2 = xr*xr + yr*yr + zr*zr
            if (r2 .le. cbuf2) then
               nelst(i) = nelst(i) + 1
               elst(nelst(i),i) = k
            end if
   20       continue
         end do
         if (repeat) then
            repeat = .false.
            start = kbx(i)
            stop = nion
            goto 10
         end if
c
c     check to see if the neighbor list is too long
c
         if (nelst(i) .ge. maxelst) then
            write (iout,30)
   30       format (/,' CLIGHT  --  Too many Neighbors;',
     &                 ' Increase MAXELST')
            call fatal
         end if
      end do
c
c     end OpenMP directives for the major loop structure
c
c!$OMP END DO
c!$OMP END PARALLEL
      return
      end
c
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine mlist  --  get atomic multipole neighbor lists  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "mlist" performs an update or a complete rebuild of the
c     electrostatic neighbor lists for atomic multipoles
c
c
      subroutine mlist
      use sizes
      use atoms
      use bound
      use boxes
      use iounit
      use mpole
      use neigh
      implicit none
      integer i,j,k
      integer ii,kk
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 radius,r2
      logical, allocatable :: update(:)
c
c
c     perform dynamic allocation of some local arrays
c
      allocate (update(n))
c
c     neighbor list cannot be used with the replicates method
c
      radius = sqrt(mbuf2)
      call replica (radius)
      if (use_replica) then
         write (iout,10)
   10    format (/,' MLIST  --  Pairwise Neighbor List cannot',
     &              ' be used with Replicas')
         call fatal
      end if
c
c     perform a complete list build instead of an update
c
      if (domlst) then
         domlst = .false.
         if (octahedron) then
            call mbuild
         else
            call mlight
         end if
         return
      end if
c
c     test sites for displacement exceeding half the buffer, and
c     rebuild the higher numbered neighbors of updated sites
c
c!$OMP PARALLEL default(shared) private(i,j,k,ii,kk,xi,yi,zi,xr,yr,zr,r2)
c!$OMP DO schedule(guided)
      do i = 1, npole
         ii = ipole(i)
         xi = x(ii)
         yi = y(ii)
         zi = z(ii)
         xr = xi - xmold(i)
         yr = yi - ymold(i)
         zr = zi - zmold(i)
         call imagen (xr,yr,zr)
         r2 = xr*xr + yr*yr + zr*zr
         update(i) = .false.
         if (r2 .ge. lbuf2) then
            update(i) = .true.
            xmold(i) = xi
            ymold(i) = yi
            zmold(i) = zi
            nelst(i) = 0
            do k = i+1, npole
               kk = ipole(k)
               xr = xi - x(kk)
               yr = yi - y(kk)
               zr = zi - z(kk)
               call imagen (xr,yr,zr)
               r2 = xr*xr + yr*yr + zr*zr
               if (r2 .le. mbuf2) then
                  nelst(i) = nelst(i) + 1
                  elst(nelst(i),i) = k
               end if
            end do
         end if
      end do
c!$OMP END DO
c
c     adjust lists of lower numbered neighbors of updated sites
c
c!$OMP DO schedule (guided)
      do i = 1, npole
         if (update(i)) then
            ii = ipole(i)
            xi = x(ii)
            yi = y(ii)
            zi = z(ii)
            do k = 1, i-1
               if (.not. update(k)) then
                  xr = xi - xmold(k)
                  yr = yi - ymold(k)
                  zr = zi - zmold(k)
                  call imagen (xr,yr,zr)
                  r2 = xr*xr + yr*yr + zr*zr
                  if (r2 .le. mbuf2) then
c!$OMP CRITICAL
                     do j = 1, nelst(k)
                        if (elst(j,k) .eq. i)  goto 20
                     end do
                     nelst(k) = nelst(k) + 1
                     elst(nelst(k),k) = i
   20                continue
c!$OMP END CRITICAL
                  else if (r2 .le. mbufx) then
c!$OMP CRITICAL
                     do j = 1, nelst(k)
                        if (elst(j,k) .eq. i) then
                           elst(j,k) = elst(nelst(k),k)
                           nelst(k) = nelst(k) - 1
                           goto 30
                        end if
                     end do
   30                continue
c!$OMP END CRITICAL
                  end if
               end if
            end do
         end if
      end do
c!$OMP END DO
c
c     check to see if any neighbor lists are too long
c
c!$OMP DO schedule(guided)
      do i = 1, npole
         if (nelst(i) .ge. maxelst) then
            write (iout,40)
   40       format (/,' MLIST  --  Too many Neighbors;',
     &                 ' Increase MAXELST')
            call fatal
         end if
      end do
c!$OMP END DO
c!$OMP END PARALLEL
c
c     perform deallocation of some local arrays
c
      deallocate (update)
      return
      end

c     #################################################################
c     ##                                                             ##
c     ##  subroutine mlist1  --  get atomic multipole neighbor lists  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "mlist1" performs an update or a complete rebuild of the
c     electrostatic neighbor lists for atomic multipoles
c
c
      subroutine mlist1
      use sizes
      use atoms
      use bound
      use boxes
      use iounit
      use mpole
      use neigh
      use openmp
      implicit none
      integer i,j,k
      integer ii,kk
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 radius,r2

c
c     neighbor list cannot be used with the replicates method
c
!$OMP master
      radius = sqrt(mbuf2)
      call replica (radius)
      if (use_replica) then
         write (iout,10)
   10    format (/,' MLIST  --  Pairwise Neighbor List cannot',
     &              ' be used with Replicas')
         call fatal
      end if

      do i=1,nthread
         do_list(i) = domlst
      end do
!$OMP end master
!$OMP barrier
c
c     perform a complete list build instead of an update
c
c      if (domlst) then
      if(do_list(th_id)) then
         do_list(th_id) = .false.
         domlst = .false.
         if (octahedron) then
            call mbuild
         else
            call mlight1
         end if
         return
      end if
c
c     test sites for displacement exceeding half the buffer, and
c     rebuild the higher numbered neighbors of updated sites
c


!$OMP DO schedule(guided) private(i,j,k,ii,kk,xi,yi,zi,xr,yr,zr,r2)
      do i = 1, npole
         ii = ipole(i)
         xi = x(ii)
         yi = y(ii)
         zi = z(ii)
         xr = xi - xmold(i)
         yr = yi - ymold(i)
         zr = zi - zmold(i)
         call imagen (xr,yr,zr)
         r2 = xr*xr + yr*yr + zr*zr
         update_omp(i) = .false.
         if (r2 .ge. lbuf2) then
            update_omp(i) = .true.
            xmold(i) = xi
            ymold(i) = yi
            zmold(i) = zi
            nelst(i) = 0
            do k = i+1, npole
               kk = ipole(k)
               xr = xi - x(kk)
               yr = yi - y(kk)
               zr = zi - z(kk)
               call imagen (xr,yr,zr)
               r2 = xr*xr + yr*yr + zr*zr
               if (r2 .le. mbuf2) then
                  nelst(i) = nelst(i) + 1
                  elst(nelst(i),i) = k
               end if
            end do
         end if
      end do
!$OMP END DO
c
c     adjust lists of lower numbered neighbors of updated sites
c

!$OMP DO schedule (guided)
      do i = 1, npole
         if (update_omp(i)) then
            ii = ipole(i)
            xi = x(ii)
            yi = y(ii)
            zi = z(ii)
            do k = 1, i-1
               if (.not. update_omp(k)) then
                  xr = xi - xmold(k)
                  yr = yi - ymold(k)
                  zr = zi - zmold(k)
                  call imagen (xr,yr,zr)
                  r2 = xr*xr + yr*yr + zr*zr
                  if (r2 .le. mbuf2) then
!$OMP CRITICAL
                     do j = 1, nelst(k)
                        if (elst(j,k) .eq. i)  goto 20
                     end do
                     nelst(k) = nelst(k) + 1
                     elst(nelst(k),k) = i
   20                continue
!$OMP END CRITICAL
                  else if (r2 .le. mbufx) then
!$OMP CRITICAL
                     do j = 1, nelst(k)
                        if (elst(j,k) .eq. i) then
                           elst(j,k) = elst(nelst(k),k)
                           nelst(k) = nelst(k) - 1
                           goto 30
                        end if
                     end do
   30                continue
!$OMP END CRITICAL
                  end if
               end if
            end do
         end if
      end do
!$OMP END DO
c
c     check to see if any neighbor lists are too long
c
!$OMP DO schedule(guided)
      do i = 1, npole
         if (nelst(i) .ge. maxelst) then
            write (iout,40)
   40       format (/,' MLIST  --  Too many Neighbors;',
     &                 ' Increase MAXELST')
            call fatal
         end if
      end do
!$OMP END DO
      return
      end
c
c
c     #############################################################
c     ##                                                         ##
c     ##  subroutine mbuild  --  build mpole list for all sites  ##
c     ##                                                         ##
c     #############################################################
c
c
c     "mbuild" performs a complete rebuild of the atomic multipole
c     electrostatic neighbor list for all sites
c
c
      subroutine mbuild
      use sizes
      use atoms
      use bound
      use iounit
      use mpole
      use neigh
      implicit none
      integer i,k
      integer ii,kk
      real*8 xi,yi,zi
      real*8 xr,yr,zr,r2
c
c
c     set OpenMP directives for the major loop structure
c
c!$OMP PARALLEL default(shared) private(i,k,ii,kk,xi,yi,zi,xr,yr,zr,r2)
c!$OMP DO schedule(guided)
c
c     store new coordinates to reflect update of the site
c
      do i = 1, npole
         ii = ipole(i)
         xi = x(ii)
         yi = y(ii)
         zi = z(ii)
         xmold(i) = xi
         ymold(i) = yi
         zmold(i) = zi
c
c     generate all neighbors for the site being rebuilt
c
         nelst(i) = 0
         do k = i+1, npole
            kk = ipole(k)
            xr = xi - x(kk)
            yr = yi - y(kk)
            zr = zi - z(kk)
            call imagen (xr,yr,zr)
            r2 = xr*xr + yr*yr + zr*zr
            if (r2 .le. mbuf2) then
               nelst(i) = nelst(i) + 1
               elst(nelst(i),i) = k
            end if
         end do
c
c     check to see if the neighbor list is too long
c
         if (nelst(i) .ge. maxelst) then
            write (iout,10)
   10       format (/,' MBUILD  --  Too many Neighbors;',
     &                 ' Increase MAXELST')
            call fatal
         end if
      end do
c
c     end OpenMP directives for the major loop structure
c
c!$OMP END DO
c!$OMP END PARALLEL
      return
      end
c
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine mlight  --  get multipole pair list via lights  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "mlight" performs a complete rebuild of the atomic multipole
c     pair neighbor list for all sites using the method of lights
c
c
      subroutine mlight
      use sizes
      use atoms
      use bound
      use cell
      use iounit
      use light
      use mpole
      use neigh
      implicit none
      integer i,j,k
      integer ii,kk
      integer kgy,kgz
      integer start,stop
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 r2,off
      real*8, allocatable :: xsort(:)
      real*8, allocatable :: ysort(:)
      real*8, allocatable :: zsort(:)
      logical repeat
c
c
c     perform dynamic allocation of some local arrays
c
      allocate (xsort(npole))
      allocate (ysort(npole))
      allocate (zsort(npole))
c
c     transfer interaction site coordinates to sorting arrays
c
      do i = 1, npole
         nelst(i) = 0
         ii = ipole(i)
         xmold(i) = x(ii)
         ymold(i) = y(ii)
         zmold(i) = z(ii)
         xsort(i) = x(ii)
         ysort(i) = y(ii)
         zsort(i) = z(ii)
      end do
c
c     use the method of lights to generate neighbors
c
      off = sqrt(mbuf2)
      call lightn (off,npole,xsort,ysort,zsort)
c
c     perform deallocation of some local arrays
c
      deallocate (xsort)
      deallocate (ysort)
      deallocate (zsort)
c
c     set OpenMP directives for the major loop structure
c
c!$OMP PARALLEL default(shared) private(i,j,k,ii,kk,xi,yi,zi,
c!$OMP& xr,yr,zr,r2,kgy,kgz,start,stop,repeat)
c!$OMP DO schedule(guided)
c
c     loop over all atoms computing the neighbor lists
c
      do i = 1, npole
         ii = ipole(i)
         xi = x(ii)
         yi = y(ii)
         zi = z(ii)
         if (kbx(i) .le. kex(i)) then
            repeat = .false.
            start = kbx(i)
            stop = kex(i)
         else
            repeat = .true.
            start = 1
            stop = kex(i)
         end if
   10    continue
         do j = start, stop
            k = locx(j)
            if (k .le. i)  goto 20
            kk = ipole(k)
            kgy = rgy(k)
            if (kby(i) .le. key(i)) then
               if (kgy.lt.kby(i) .or. kgy.gt.key(i))  goto 20
            else
               if (kgy.lt.kby(i) .and. kgy.gt.key(i))  goto 20
            end if
            kgz = rgz(k)
            if (kbz(i) .le. kez(i)) then
               if (kgz.lt.kbz(i) .or. kgz.gt.kez(i))  goto 20
            else
               if (kgz.lt.kbz(i) .and. kgz.gt.kez(i))  goto 20
            end if
            xr = xi - x(kk)
            yr = yi - y(kk)
            zr = zi - z(kk)
            call imagen (xr,yr,zr)
            r2 = xr*xr + yr*yr + zr*zr
            if (r2 .le. mbuf2) then
               nelst(i) = nelst(i) + 1
               elst(nelst(i),i) = k
            end if
   20       continue
         end do
         if (repeat) then
            repeat = .false.
            start = kbx(i)
            stop = npole
            goto 10
         end if
c
c     check to see if the neighbor list is too long
c
         if (nelst(i) .ge. maxelst) then
            write (iout,30)
   30       format (/,' MLIGHT  --  Too many Neighbors;',
     &                 ' Increase MAXELST')
            call fatal
         end if
      end do
c
c     end OpenMP directives for the major loop structure
c
c!$OMP END DO
c!$OMP END PARALLEL
      return
      end
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine mlight1  --  get multipole pair list via lights  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "mlight1" performs a complete rebuild of the atomic multipole
c     pair neighbor list for all sites using the method of lights
c
c
      subroutine mlight1
      use sizes
      use atoms
      use bound
      use cell
      use iounit
      use light
      use mpole
      use neigh
      use openmp
      implicit none
      integer i,j,k
      integer ii,kk
      integer kgy,kgz
      integer start,stop
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 r2,off
c      real*8, allocatable :: xsort(:)
c      real*8, allocatable :: ysort(:)
c      real*8, allocatable :: zsort(:)
      logical repeat
c
c
c     perform dynamic allocation of some local arrays
c
c!$OMP master
c      allocate (xsort(npole))
c      allocate (ysort(npole))
c      allocate (zsort(npole))
c
c     transfer interaction site coordinates to sorting arrays
c
!$OMP DO private(ii) schedule(static,128)
      do i = 1, npole
         nelst(i) = 0
         ii = ipole(i)
         xmold(i) = x(ii)
         ymold(i) = y(ii)
         zmold(i) = z(ii)
         xsort_omp(i) = x(ii)
         ysort_omp(i) = y(ii)
         zsort_omp(i) = z(ii)
      end do
!$OMP end DO 
c
c     use the method of lights to generate neighbors
c
!$OMP master
      off = sqrt(mbuf2)
      call lightn (off,npole,xsort_omp,ysort_omp,zsort_omp)
!$OMP end master
!$OMP barrier
c
c     perform deallocation of some local arrays
c
c      deallocate (xsort)
c      deallocate (ysort)
c      deallocate (zsort)
c!$OMP end master
c!$OMP barrier
c
c     set OpenMP directives for the major loop structure

!$OMP DO schedule(guided)private(i,j,k,ii,kk,xi,yi,zi,
!$OMP& xr,yr,zr,r2,kgy,kgz,start,stop,repeat)
c
c     loop over all atoms computing the neighbor lists
c
      do i = 1, npole
         ii = ipole(i)
         xi = x(ii)
         yi = y(ii)
         zi = z(ii)
         if (kbx(i) .le. kex(i)) then
            repeat = .false.
            start = kbx(i)
            stop = kex(i)
         else
            repeat = .true.
            start = 1
            stop = kex(i)
         end if
   10    continue
         do j = start, stop
            k = locx(j)
            if (k .le. i)  goto 20
            kk = ipole(k)
            kgy = rgy(k)
            if (kby(i) .le. key(i)) then
               if (kgy.lt.kby(i) .or. kgy.gt.key(i))  goto 20
            else
               if (kgy.lt.kby(i) .and. kgy.gt.key(i))  goto 20
            end if
            kgz = rgz(k)
            if (kbz(i) .le. kez(i)) then
               if (kgz.lt.kbz(i) .or. kgz.gt.kez(i))  goto 20
            else
               if (kgz.lt.kbz(i) .and. kgz.gt.kez(i))  goto 20
            end if
            xr = xi - x(kk)
            yr = yi - y(kk)
            zr = zi - z(kk)
            call imagen (xr,yr,zr)
            r2 = xr*xr + yr*yr + zr*zr
            if (r2 .le. mbuf2) then
               nelst(i) = nelst(i) + 1
               elst(nelst(i),i) = k
            end if
   20       continue
         end do
         if (repeat) then
            repeat = .false.
            start = kbx(i)
            stop = npole
            goto 10
         end if
c
c     check to see if the neighbor list is too long
c
         if (nelst(i) .ge. maxelst) then
            write (iout,30)
   30       format (/,' MLIGHT  --  Too many Neighbors;',
     &                 ' Increase MAXELST')
            call fatal
         end if
      end do
c
c     end OpenMP directives for the major loop structure
c
!$OMP END DO
      return
      end
c
c
c     ###############################################################
c     ##                                                           ##
c     ##  subroutine ulist  --  get preconditioner neighbor lists  ##
c     ##                                                           ##
c     ###############################################################
c
c
c     "ulist" performs an update or a complete rebuild of the
c     neighbor lists for the polarization preconditioner
c
c
      subroutine ulist
      use sizes
      use atoms
      use bound
      use boxes
      use iounit
      use mpole
      use neigh
      implicit none
      integer i,j,k
      integer ii,kk
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 radius,r2
      logical, allocatable :: update(:)
c
c
c     perform dynamic allocation of some local arrays
c
      allocate (update(n))
c
c     neighbor list cannot be used with the replicates method
c
      radius = sqrt(ubuf2)
      call replica (radius)
      if (use_replica) then
         write (iout,10)
   10    format (/,' ULIST  --  Pairwise Neighbor List cannot',
     &              ' be used with Replicas')
         call fatal
      end if
c
c     perform a complete list build instead of an update
c
      if (doulst) then
         doulst = .false.
         if (octahedron) then
            call ubuild
         else
            call ulight
         end if
         return
      end if
c
c     test sites for displacement exceeding half the buffer, and
c     rebuild the higher numbered neighbors of updated sites
c
c!$OMP PARALLEL default(shared) private(i,j,k,ii,kk,xi,yi,zi,xr,yr,zr,r2)
c!$OMP DO schedule(guided)
      do i = 1, npole
         ii = ipole(i)
         xi = x(ii)
         yi = y(ii)
         zi = z(ii)
         xr = xi - xuold(i)
         yr = yi - yuold(i)
         zr = zi - zuold(i)
         call imagen (xr,yr,zr)
         r2 = xr*xr + yr*yr + zr*zr
         update(i) = .false.
         if (r2 .ge. pbuf2) then
            update(i) = .true.
            xuold(i) = xi
            yuold(i) = yi
            zuold(i) = zi
            nulst(i) = 0
            do k = i+1, npole
               kk = ipole(k)
               xr = xi - x(kk)
               yr = yi - y(kk)
               zr = zi - z(kk)
               call imagen (xr,yr,zr)
               r2 = xr*xr + yr*yr + zr*zr
               if (r2 .le. ubuf2) then
                  nulst(i) = nulst(i) + 1
                  ulst(nulst(i),i) = k
               end if
            end do
         end if
      end do
c!$OMP END DO
c
c     adjust lists of lower numbered neighbors of updated sites
c
c!$OMP DO schedule(guided)
      do i = 1, npole
         if (update(i)) then
            ii = ipole(i)
            xi = x(ii)
            yi = y(ii)
            zi = z(ii)
            do k = 1, i-1
               if (.not. update(k)) then
                  xr = xi - xuold(k)
                  yr = yi - yuold(k)
                  zr = zi - zuold(k)
                  call imagen (xr,yr,zr)
                  r2 = xr*xr + yr*yr + zr*zr
                  if (r2 .le. ubuf2) then
c!$OMP CRITICAL
                     do j = 1, nulst(k)
                        if (ulst(j,k) .eq. i)  goto 20
                     end do
                     nulst(k) = nulst(k) + 1
                     ulst(nulst(k),k) = i
   20                continue
c!$OMP END CRITICAL
                  else if (r2 .le. ubufx) then
c!$OMP CRITICAL
                     do j = 1, nulst(k)
                        if (ulst(j,k) .eq. i) then
                           ulst(j,k) = ulst(nulst(k),k)
                           nulst(k) = nulst(k) - 1
                           goto 30
                        end if
                     end do
   30                continue
c!$OMP END CRITICAL
                  end if
               end if
            end do
         end if
      end do
c!$OMP END DO
c
c     check to see if any neighbor lists are too long
c
c!$OMP DO schedule(guided)
      do i = 1, npole
         if (nulst(i) .ge. maxulst) then
            write (iout,40)
   40       format (/,' ULIST  --  Too many Neighbors;',
     &                 ' Increase MAXULST')
            call fatal
         end if
      end do
c!$OMP END DO
c!$OMP END PARALLEL
c
c     perform deallocation of some local arrays
c
      deallocate (update)
      return
      end
c     ###############################################################
c     ##                                                           ##
c     ##  subroutine ulist1  --  get preconditioner neighbor lists  ##
c     ##                                                           ##
c     ###############################################################
c
c
c     "ulist1" performs an update or a complete rebuild of the
c     neighbor lists for the polarization preconditioner
c
c
      subroutine ulist1
      use sizes
      use atoms
      use bound
      use boxes
      use iounit
      use mpole
      use neigh
      use openmp
      implicit none
      integer i,j,k
      integer ii,kk
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 radius,r2
c
c     neighbor list cannot be used with the replicates method
c
!$OMP master
      radius = sqrt(ubuf2)
      call replica (radius)
      if (use_replica) then
         write (iout,10)
   10    format (/,' ULIST  --  Pairwise Neighbor List cannot',
     &              ' be used with Replicas')
         call fatal
      end if

      do i=1,nthread
         do_list(i) = doulst
      end do
!$OMP end master
!$OMP barrier

c
c     perform a complete list build instead of an update
c
      if(do_list(th_id))then
         do_list(th_id) = .false.     
         doulst = .false.
         if (octahedron) then
            call ubuild
         else
            call ulight1
         end if
         return
      end if

c
c     test sites for displacement exceeding half the buffer, and
c     rebuild the higher numbered neighbors of updated sites
c

!$OMP DO schedule(guided) private(i,j,k,ii,kk,xi,yi,zi,xr,yr,zr,r2)
      do i = 1, npole
         ii = ipole(i)
         xi = x(ii)
         yi = y(ii)
         zi = z(ii)
         xr = xi - xuold(i)
         yr = yi - yuold(i)
         zr = zi - zuold(i)
         call imagen (xr,yr,zr)
         r2 = xr*xr + yr*yr + zr*zr
         update_omp(i) = .false.
         if (r2 .ge. pbuf2) then
            update_omp(i) = .true.
            xuold(i) = xi
            yuold(i) = yi
            zuold(i) = zi
            nulst(i) = 0
            do k = i+1, npole
               kk = ipole(k)
               xr = xi - x(kk)
               yr = yi - y(kk)
               zr = zi - z(kk)
               call imagen (xr,yr,zr)
               r2 = xr*xr + yr*yr + zr*zr
               if (r2 .le. ubuf2) then
                  nulst(i) = nulst(i) + 1
                  ulst(nulst(i),i) = k
               end if
            end do
         end if
      end do
!$OMP END DO
c
c     adjust lists of lower numbered neighbors of updated sites
c
!$OMP DO schedule(guided) private(i,j,k,ii,kk,xi,yi,zi,xr,yr,zr,r2)
      do i = 1, npole
         if (update_omp(i)) then
            ii = ipole(i)
            xi = x(ii)
            yi = y(ii)
            zi = z(ii)
            do k = 1, i-1
               if (.not. update_omp(k)) then
                  xr = xi - xuold(k)
                  yr = yi - yuold(k)
                  zr = zi - zuold(k)
                  call imagen (xr,yr,zr)
                  r2 = xr*xr + yr*yr + zr*zr
                  if (r2 .le. ubuf2) then
!$OMP CRITICAL
                     do j = 1, nulst(k)
                        if (ulst(j,k) .eq. i)  goto 20
                     end do
                     nulst(k) = nulst(k) + 1
                     ulst(nulst(k),k) = i
   20                continue
!$OMP END CRITICAL
                  else if (r2 .le. ubufx) then
!$OMP CRITICAL
                     do j = 1, nulst(k)
                        if (ulst(j,k) .eq. i) then
                           ulst(j,k) = ulst(nulst(k),k)
                           nulst(k) = nulst(k) - 1
                           goto 30
                        end if
                     end do
   30                continue
!$OMP END CRITICAL
                  end if
               end if
            end do
         end if
      end do
!$OMP END DO
c
c     check to see if any neighbor lists are too long
c
!$OMP DO schedule(guided)
      do i = 1, npole
         if (nulst(i) .ge. maxulst) then
            write (iout,40)
   40       format (/,' ULIST  --  Too many Neighbors;',
     &                 ' Increase MAXULST')
            call fatal
         end if
      end do
!$OMP END DO
      return
      end
c
c
c     ################################################################
c     ##                                                            ##
c     ##  subroutine ubuild  --  preconditioner list for all sites  ##
c     ##                                                            ##
c     ################################################################
c
c
c     "ubuild" performs a complete rebuild of the polarization
c     preconditioner neighbor list for all sites
c
c
      subroutine ubuild
      use sizes
      use atoms
      use bound
      use iounit
      use mpole
      use neigh
      implicit none
      integer i,k
      integer ii,kk
      real*8 xi,yi,zi
      real*8 xr,yr,zr,r2
c
c
c     set OpenMP directives for the major loop structure
c
c!$OMP PARALLEL default(shared) private(i,k,ii,kk,xi,yi,zi,xr,yr,zr,r2)
c!$OMP DO schedule(guided)
c
c     store new coordinates to reflect update of the site
c
      do i = 1, npole
         ii = ipole(i)
         xi = x(ii)
         yi = y(ii)
         zi = z(ii)
         xuold(i) = xi
         yuold(i) = yi
         zuold(i) = zi
c
c     generate all neighbors for the site being rebuilt
c
         nulst(i) = 0
         do k = i+1, npole
            kk = ipole(k)
            xr = xi - x(kk)
            yr = yi - y(kk)
            zr = zi - z(kk)
            call imagen (xr,yr,zr)
            r2 = xr*xr + yr*yr + zr*zr
            if (r2 .le. ubuf2) then
               nulst(i) = nulst(i) + 1
               ulst(nulst(i),i) = k
            end if
         end do
c
c     check to see if the neighbor list is too long
c
         if (nulst(i) .ge. maxulst) then
            write (iout,10)
   10       format (/,' UBUILD  --  Too many Neighbors;',
     &                 ' Increase MAXULST')
            call fatal
         end if
      end do
c
c     end OpenMP directives for the major loop structure
c
c!$OMP END DO
c!$OMP END PARALLEL
      return
      end
c
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine ulight  --  get preconditioner list via lights  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "ulight" performs a complete rebuild of the polarization
c     preconditioner pair neighbor list for all sites using the
c     method of lights
c
c
      subroutine ulight
      use sizes
      use atoms
      use bound
      use cell
      use iounit
      use light
      use mpole
      use neigh
      implicit none
      integer i,j,k
      integer ii,kk
      integer kgy,kgz
      integer start,stop
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 r2,off
      real*8, allocatable :: xsort(:)
      real*8, allocatable :: ysort(:)
      real*8, allocatable :: zsort(:)
      logical repeat
c
c
c     perform dynamic allocation of some local arrays
c
      allocate (xsort(npole))
      allocate (ysort(npole))
      allocate (zsort(npole))
c
c     transfer interaction site coordinates to sorting arrays
c
      do i = 1, npole
         nulst(i) = 0
         ii = ipole(i)
         xuold(i) = x(ii)
         yuold(i) = y(ii)
         zuold(i) = z(ii)
         xsort(i) = x(ii)
         ysort(i) = y(ii)
         zsort(i) = z(ii)
      end do
c
c     use the method of lights to generate neighbors
c
      off = sqrt(ubuf2)
      call lightn (off,npole,xsort,ysort,zsort)
c
c     perform deallocation of some local arrays
c
      deallocate (xsort)
      deallocate (ysort)
      deallocate (zsort)
c
c     set OpenMP directives for the major loop structure
c
c!$OMP PARALLEL default(shared) private(i,j,k,ii,kk,xi,yi,zi,
c!$OMP& xr,yr,zr,r2,kgy,kgz,start,stop,repeat)
c!$OMP DO schedule(guided)
c
c     loop over all atoms computing the neighbor lists
c
      do i = 1, npole
         ii = ipole(i)
         xi = x(ii)
         yi = y(ii)
         zi = z(ii)
         if (kbx(i) .le. kex(i)) then
            repeat = .false.
            start = kbx(i)
            stop = kex(i)
         else
            repeat = .true.
            start = 1
            stop = kex(i)
         end if
   10    continue
         do j = start, stop
            k = locx(j)
            if (k .le. i)  goto 20
            kk = ipole(k)
            kgy = rgy(k)
            if (kby(i) .le. key(i)) then
               if (kgy.lt.kby(i) .or. kgy.gt.key(i))  goto 20
            else
               if (kgy.lt.kby(i) .and. kgy.gt.key(i))  goto 20
            end if
            kgz = rgz(k)
            if (kbz(i) .le. kez(i)) then
               if (kgz.lt.kbz(i) .or. kgz.gt.kez(i))  goto 20
            else
               if (kgz.lt.kbz(i) .and. kgz.gt.kez(i))  goto 20
            end if
            xr = xi - x(kk)
            yr = yi - y(kk)
            zr = zi - z(kk)
            call imagen (xr,yr,zr)
            r2 = xr*xr + yr*yr + zr*zr
            if (r2 .le. ubuf2) then
               nulst(i) = nulst(i) + 1
               ulst(nulst(i),i) = k
            end if
   20       continue
         end do
         if (repeat) then
            repeat = .false.
            start = kbx(i)
            stop = npole
            goto 10
         end if
c
c     check to see if the neighbor list is too long
c
         if (nulst(i) .ge. maxulst) then
            write (iout,30)
   30       format (/,' ULIGHT  --  Too many Neighbors;',
     &                 ' Increase MAXULST')
            call fatal
         end if
      end do
c
c     end OpenMP directives for the major loop structure
c
c!$OMP END DO
c!$OMP END PARALLEL
      return
      end
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine ulight1  --  get preconditioner list via lights  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "ulight1" performs a complete rebuild of the polarization
c     preconditioner pair neighbor list for all sites using the
c     method of lights
c
c
      subroutine ulight1
      use sizes
      use atoms
      use bound
      use cell
      use iounit
      use light
      use mpole
      use neigh
      use openmp
      implicit none
      integer i,j,k
      integer ii,kk
      integer kgy,kgz
      integer start,stop
      real*8 xi,yi,zi
      real*8 xr,yr,zr
      real*8 r2,off
c      real*8, allocatable :: xsort(:)
c      real*8, allocatable :: ysort(:)
c      real*8, allocatable :: zsort(:)
      logical repeat
c
c
c     perform dynamic allocation of some local arrays
c
c!$OMP master
c      allocate (xsort(npole))
c      allocate (ysort(npole))
c      allocate (zsort(npole))
c
c     transfer interaction site coordinates to sorting arrays
c
!$OMP DO private(ii) schedule(static,128)
      do i = 1, npole
         nulst(i) = 0
         ii = ipole(i)
         xuold(i) = x(ii)
         yuold(i) = y(ii)
         zuold(i) = z(ii)
         xsort_omp(i) = x(ii)
         ysort_omp(i) = y(ii)
         zsort_omp(i) = z(ii)
      end do
!$OMP end DO
c
c     use the method of lights to generate neighbors
c
!$OMP master
      off = sqrt(ubuf2)
      call lightn (off,npole,xsort_omp,ysort_omp,zsort_omp)
!$OMP end master
!$OMP barrier
c
c     perform deallocation of some local arrays
c
c      deallocate (xsort)
c      deallocate (ysort)
c      deallocate (zsort)

c
c     set OpenMP directives for the major loop structure

!$OMP DO schedule(guided) private(i,j,k,ii,kk,xi,yi,zi,
!$OMP& xr,yr,zr,r2,kgy,kgz,start,stop,repeat)
c
c     loop over all atoms computing the neighbor lists
c
      do i = 1, npole
         ii = ipole(i)
         xi = x(ii)
         yi = y(ii)
         zi = z(ii)
         if (kbx(i) .le. kex(i)) then
            repeat = .false.
            start = kbx(i)
            stop = kex(i)
         else
            repeat = .true.
            start = 1
            stop = kex(i)
         end if
   10    continue
         do j = start, stop
            k = locx(j)
            if (k .le. i)  goto 20
            kk = ipole(k)
            kgy = rgy(k)
            if (kby(i) .le. key(i)) then
               if (kgy.lt.kby(i) .or. kgy.gt.key(i))  goto 20
            else
               if (kgy.lt.kby(i) .and. kgy.gt.key(i))  goto 20
            end if
            kgz = rgz(k)
            if (kbz(i) .le. kez(i)) then
               if (kgz.lt.kbz(i) .or. kgz.gt.kez(i))  goto 20
            else
               if (kgz.lt.kbz(i) .and. kgz.gt.kez(i))  goto 20
            end if
            xr = xi - x(kk)
            yr = yi - y(kk)
            zr = zi - z(kk)
            call imagen (xr,yr,zr)
            r2 = xr*xr + yr*yr + zr*zr
            if (r2 .le. ubuf2) then
               nulst(i) = nulst(i) + 1
               ulst(nulst(i),i) = k
            end if
   20       continue
         end do
         if (repeat) then
            repeat = .false.
            start = kbx(i)
            stop = npole
            goto 10
         end if
c
c     check to see if the neighbor list is too long
c
         if (nulst(i) .ge. maxulst) then
            write (iout,30)
   30       format (/,' ULIGHT  --  Too many Neighbors;',
     &                 ' Increase MAXULST')
            call fatal
         end if
      end do
c
c     end OpenMP directives for the major loop structure
c
!$OMP END DO
      return
      end
c
c
c
c     #################################################################
c     ##                                                             ##
c     ##  subroutine lightn  --  method of lights for list building  ##
c     ##                                                             ##
c     #################################################################
c
c
c     "lightn" computes the set of nearest neighbor interactions
c     using the method of lights algorithm
c
c     note this is a special version for neighbor list generation
c     which includes each pair in both directions, (A,B) and (B,A)
c
c     literature reference:
c
c     F. Sullivan, R. D. Mountain and J. O'Connell, "Molecular
c     Dynamics on Vector Computers", Journal of Computational
c     Physics, 61, 138-153 (1985)
c
c
      subroutine lightn (cutoff,nsite,xsort,ysort,zsort)
      use sizes
      use bound
      use boxes
      use cell
      use iounit
      use light
      implicit none
      integer i,j,k
      integer nsite
      integer extent
      real*8 cutoff,box
      real*8 xcut,ycut,zcut
      real*8 xsort(*)
      real*8 ysort(*)
      real*8 zsort(*)
      real*8, allocatable :: xfrac(:)
      real*8, allocatable :: yfrac(:)
      real*8, allocatable :: zfrac(:)
c
c
c     truncated octahedron periodicity is not handled at present
c
      if (use_bounds) then
         if (octahedron) then
            write (iout,10)
   10       format (/,' LIGHTS  --  Truncated Octahedron not',
     &                 ' Supported by Method of Lights')
            call fatal
         end if
      end if
c
c     set the light width based on input distance cutoff
c
      xcut = cutoff
      ycut = cutoff
      zcut = cutoff
      if (use_bounds) then
         if (monoclinic) then
            zcut = zcut / beta_sin
            xcut = xcut + zcut*abs(beta_cos)
         else if (triclinic) then
            zcut = zcut / gamma_term
            ycut = (ycut + zcut*abs(beta_term)) / gamma_sin
            xcut = xcut + ycut*abs(gamma_cos) + zcut*abs(beta_cos)
         end if
         xcut = min(xcut,xcell2)
         ycut = min(ycut,ycell2)
         zcut = min(zcut,zcell2)
      end if
c
c     perform dynamic allocation of some local arrays
c
      allocate (xfrac(nsite))
      allocate (yfrac(nsite))
      allocate (zfrac(nsite))
c
c     find fractional coordinates for the unitcell atoms
c
      if (use_bounds) then
         if (orthogonal) then
            do i = 1, nsite
               zfrac(i) = zsort(i)
               yfrac(i) = ysort(i)
               xfrac(i) = xsort(i)
            end do
         else if (monoclinic) then
            do i = 1, nsite
               zfrac(i) = zsort(i) / beta_sin
               yfrac(i) = ysort(i)
               xfrac(i) = xsort(i) - zfrac(i)*beta_cos
            end do
         else if (triclinic) then
            do i = 1, nsite
               zfrac(i) = zsort(i) / gamma_term
               yfrac(i) = (ysort(i) - zfrac(i)*beta_term) / gamma_sin
               xfrac(i) = xsort(i) - yfrac(i)*gamma_cos
     &                       - zfrac(i)*beta_cos
            end do
         end if
      end if
c
c     use images to move coordinates into periodic cell
c
      if (use_bounds) then
         do i = 1, nsite
            xsort(i) = xfrac(i)
            ysort(i) = yfrac(i)
            zsort(i) = zfrac(i)
            do while (abs(xsort(i)) .gt. xcell2)
               xsort(i) = xsort(i) - sign(xcell,xsort(i))
            end do
            do while (abs(ysort(i)) .gt. ycell2)
               ysort(i) = ysort(i) - sign(ycell,ysort(i))
            end do
            do while (abs(zsort(i)) .gt. zcell2)
               zsort(i) = zsort(i) - sign(zcell,zsort(i))
            end do
         end do
      end if
c
c     perform deallocation of some local arrays
c
      deallocate (xfrac)
      deallocate (yfrac)
      deallocate (zfrac)
c
c     perform dynamic allocation of some global arrays
c
      nlight = nsite
      extent = 0
      if (allocated(rgx))  extent = size(rgx)
      if (extent .lt. nlight) then
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
         allocate (kbx(nsite))
         allocate (kby(nsite))
         allocate (kbz(nsite))
         allocate (kex(nsite))
         allocate (key(nsite))
         allocate (kez(nsite))
         allocate (locx(nlight))
         allocate (locy(nlight))
         allocate (locz(nlight))
         allocate (rgx(nlight))
         allocate (rgy(nlight))
         allocate (rgz(nlight))
      end if
c
c     sort the coordinate components into ascending order
c
      call sort2 (nlight,xsort,locx)
      call sort2 (nlight,ysort,locy)
      call sort2 (nlight,zsort,locz)
c
c     index the position of each atom in the sorted coordinates
c
      do i = 1, nlight
         rgx(locx(i)) = i
         rgy(locy(i)) = i
         rgz(locz(i)) = i
      end do
c
c     find the negative x-coordinate boundary for each atom
c
      j = nlight
      box = 0.0d0
      do i = nlight, 1, -1
         k = locx(i)
         do while (xsort(i)-xsort(j)+box .le. xcut)
            if (j .eq. 1) then
               if (use_bounds) then
                  j = nlight + 1
                  box = xcell
               end if
            end if
            j = j - 1
            if (j .lt. 1)  goto 20
         end do
   20    continue
         j = j + 1
         if (j .gt. nlight) then
            j = 1
            box = 0.0d0
         end if
         kbx(k) = j
      end do
c
c     find the positive x-coordinate boundary for each atom
c
      j = 1
      box = 0.0d0
      do i = 1, nlight
         k = locx(i)
         do while (xsort(j)-xsort(i)+box .lt. xcut)
            if (j .eq. nlight) then
               if (use_bounds) then
                  j = 0
                  box = xcell
               end if
            end if
            j = j + 1
            if (j .gt. nlight)  goto 30
         end do
   30    continue
         j = j - 1
         if (j .lt. 1) then
            j = nlight
            box = 0.0d0
         end if
         kex(k) = j
      end do
c
c     find the negative y-coordinate boundary for each atom
c
      j = nlight
      box = 0.0d0
      do i = nlight, 1, -1
         k = locy(i)
         do while (ysort(i)-ysort(j)+box .le. ycut)
            if (j .eq. 1) then
               if (use_bounds) then
                  j = nlight + 1
                  box = ycell
               end if
            end if
            j = j - 1
            if (j .lt. 1)  goto 40
         end do
   40    continue
         j = j + 1
         if (j .gt. nlight) then
            j = 1
            box = 0.0d0
         end if
         kby(k) = j
      end do
c
c     find the positive y-coordinate boundary for each atom
c
      j = 1
      box = 0.0d0
      do i = 1, nlight
         k = locy(i)
         do while (ysort(j)-ysort(i)+box .lt. ycut)
            if (j .eq. nlight) then
               if (use_bounds) then
                  j = 0
                  box = ycell
               end if
            end if
            j = j + 1
            if (j .gt. nlight)  goto 50
         end do
   50    continue
         j = j - 1
         if (j .lt. 1) then
            j = nlight
            box = 0.0d0
         end if
         key(k) = j
      end do
c
c     find the negative z-coordinate boundary for each atom
c
      j = nlight
      box = 0.0d0
      do i = nlight, 1, -1
         k = locz(i)
         do while (zsort(i)-zsort(j)+box .le. zcut)
            if (j .eq. 1) then
               if (use_bounds) then
                  j = nlight + 1
                  box = zcell
               end if
            end if
            j = j - 1
            if (j .lt. 1)  goto 60
         end do
   60    continue
         j = j + 1
         if (j .gt. nlight) then
            j = 1
            box = 0.0d0
         end if
         kbz(k) = j
      end do
c
c     find the positive z-coordinate boundary for each atom
c
      j = 1
      box = 0.0d0
      do i = 1, nlight
         k = locz(i)
         do while (zsort(j)-zsort(i)+box .lt. zcut)
            if (j .eq. nlight) then
               if (use_bounds) then
                  j = 0
                  box = zcell
               end if
            end if
            j = j + 1
            if (j .gt. nlight)  goto 70
         end do
   70    continue
         j = j - 1
         if (j .lt. 1) then
            j = nlight
            box = 0.0d0
         end if
         kez(k) = j
      end do
      return
      end
c
c
c     ##############################################################
c     ##                                                          ##
c     ##  subroutine imagen  --  neighbor minimum image distance  ##
c     ##                                                          ##
c     ##############################################################
c
c
c     "imagen" takes the components of pairwise distance between
c     two points and converts to the components of the minimum
c     image distance
c
c     note this is a fast version for neighbor list generation
c     which only returns the correct component magnitudes
c
c
      subroutine imagen (xr,yr,zr)
      use boxes
      implicit none
      real*8 xr,yr,zr
c
c
c     for orthogonal lattice, find the desired image directly
c
      if (orthogonal) then
         xr = abs(xr)
         yr = abs(yr)
         zr = abs(zr)
         if (xr .gt. xbox2)  xr = xr - xbox
         if (yr .gt. ybox2)  yr = yr - ybox
         if (zr .gt. zbox2)  zr = zr - zbox
c
c     for monoclinic lattice, convert "xr" and "zr" specially
c
      else if (monoclinic) then
         zr = zr / beta_sin
         yr = abs(yr)
         xr = xr - zr*beta_cos
         if (abs(xr) .gt. xbox2)  xr = xr - sign(xbox,xr)
         if (yr .gt. ybox2)  yr = yr - ybox
         if (abs(zr) .gt. zbox2)  zr = zr - sign(zbox,zr)
         xr = xr + zr*beta_cos
         zr = zr * beta_sin
c
c     for triclinic lattice, use general conversion equations
c
      else if (triclinic) then
         zr = zr / gamma_term
         yr = (yr - zr*beta_term) / gamma_sin
         xr = xr - yr*gamma_cos - zr*beta_cos
         if (abs(xr) .gt. xbox2)  xr = xr - sign(xbox,xr)
         if (abs(yr) .gt. ybox2)  yr = yr - sign(ybox,yr)
         if (abs(zr) .gt. zbox2)  zr = zr - sign(zbox,zr)
         xr = xr + yr*gamma_cos + zr*beta_cos
         yr = yr*gamma_sin + zr*beta_term
         zr = zr * gamma_term
c
c     for truncated octahedron, remove the corner pieces
c
      else if (octahedron) then
         if (abs(xr) .gt. xbox2)  xr = xr - sign(xbox,xr)
         if (abs(yr) .gt. ybox2)  yr = yr - sign(ybox,yr)
         if (abs(zr) .gt. zbox2)  zr = zr - sign(zbox,zr)
         if (abs(xr)+abs(yr)+abs(zr) .gt. box34) then
            xr = xr - sign(xbox2,xr)
            yr = yr - sign(ybox2,yr)
            zr = zr - sign(zbox2,zr)
         end if
      end if
      return
      end
