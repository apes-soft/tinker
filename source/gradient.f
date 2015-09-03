c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ##################################################################
c     ##                                                              ##
c     ##  subroutine gradient  --  find energy & gradient components  ##
c     ##                                                              ##
c     ##################################################################
c
c
c     "gradient" calls subroutines to calculate the potential energy
c     and first derivatives with respect to Cartesian coordinates
c
c
      subroutine gradient (energy,derivs)
      use sizes
      use atoms
      use bound
      use bndstr
      use bndpot
      use group
      use couple
      use deriv
      use energi
      use inter
      use iounit
      use limits
      use potent
      use rigid
      use vdwpot
      use virial
      use usage
      use openmp

      use warp 
      implicit none
      integer i,j,k
      real*8 energy,cutoff
      real*8 derivs(3,*)
      real*8 elrc, vlrc
!$    integer omp_get_thread_num


c
c     zero out each of the potential energy components
c
      eb = 0.0d0
      ea = 0.0d0
      eba = 0.0d0
      eub = 0.0d0
      eopb = 0.0d0
      eit = 0.0d0
      et = 0.0d0
      ept = 0.0d0
      ett = 0.0d0
      ev = 0.0d0
      em = 0.0d0
      ep = 0.0d0

c
c     perform dynamic allocation of some global arrays
c
      if (allocated(desum)) then
         if (size(desum) .lt. 3*n) then
            deallocate (desum)
            deallocate (dev)
            deallocate (dem)
            deallocate (dep)
         end if
      end if
      if (.not. allocated(desum)) then
         allocate (desum(3,n))
         allocate (dev(3,n))
         allocate (dem(3,n))
         allocate (dep(3,n))
      end if

      if(.not. allocated(vir_th)) allocate(vir_th(nthread,3,3))
      if(.not. allocated(drv_th)) allocate(drv_th(nthread,3,n))
      if(.not. allocated(en_th)) allocate(en_th(nthread))
c
c     zero out the virial and the intermolecular energy
c
      do i = 1, 3
         do j = 1, 3
            vir(j,i) = 0.0d0
         end do
      end do

      einter = 0.0d0

      if (use_bounds .and. .not.use_rigid) call bounds ! no omp - used
     
      
      cutoff = 0.0d0
      call replica (cutoff)     ! no omp
c
c     many implicit solvation models require Born radii
c
c      if (use_born)  call born  ! no omp - not used
        
c
c     alter bond and torsion constants for pisystem
c
c      if (use_orbit) call picalc ! no omp - not used
        

      if (use_list)  call nblist


C$$$!$OMP parallel default(none) shared(n, deb, dea, deba, deub, deaa, 
C$$$!$OMP& deopb, deopd, deid, deit, det, dept, debt, deat, dett, dev, 
C$$$!$OMP& dec, decd, ded, dem, dep, des, delf, deg, dex, vir, einter, 
C$$$!$OMP& use_bounds, use_rigid, use_list, der, cutoff, use_born, 
C$$$!$OMP& use_orbit, use_bond, use_angle, vdwtyp, use_charge, use_chgdpl,
C$$$!$OMP& use_dipole, use_mpole, use_polar, use_rxnfld, use_solv, use_geom, 
C$$$!$OMP& use_metal, use_strbnd, use_urey, use_angang, use_opbend, 
C$$$!$OMP& use_opdist,use_improp, use_imptor, use_tors, use_pitors,
C$$$!$OMP& use_strtor, use_angtor,use_tortor, use_vdw, use_extra, esum,  
C$$$!$OMP& eb, ea,eba, eub, eaa, eopb, eopd, eid, eit, et, ept, ebt, eat, 
C$$$!$OMP& ett,ev, ec, ecd, ed,em,ep,er,es,elf,eg, ex,energy, desum) 

!$OMP parallel default(shared) 
      
      th_id = 1
!$      th_id = omp_get_thread_num() + 1

      do i=1,3
         do j=1,3
            vir_th(th_id,j,i) = 0.0d0
         end do
      end do

        do i=1,n
         do j=1,3
            drv_th(th_id,j,i) = 0.0d0
         end do
      end do

      en_th(th_id) = 0.0d0
c
c     zero out each of the first derivative components
c
      
!$OMP DO schedule(guided)
      do i = 1, n
         do j = 1, 3
            dev(j,i) = 0.0d0
            dem(j,i) = 0.0d0
            dep(j,i) = 0.0d0
         end do
      end do
!$OMP END DO


c
c     call the local geometry energy and gradient routines
c

      if (use_bond)  call ebond1 
      if (use_angle)  call eangle1
      if (use_strbnd)  call estrbnd1
      if (use_urey)  call eurey1           
      if (use_opbend)  call eopbend1 
      if(use_tors) call etors1
      if (use_pitors)  call epitors1
      if (use_tortor)  call etortor1

c the following subroutines are not used by bench7

C$$$      if (use_angang)  call eangang1 ! no omp
C$$$      if (use_opdist)  call eopdist1 ! no omp
C$$$      if (use_improp)  call eimprop1 ! no omp
C$$$      if (use_imptor)  call eimptor1 ! no omp
C$$$      if (use_strtor)  call estrtor1 ! no omp
C$$$      if (use_angtor)  call eangtor1 ! no omp
   
c
c     call the van der Waals energy and gradient routines
c
      if (use_vdw) then
         if (vdwtyp .eq. 'LENNARD-JONES')  call elj1
         if (vdwtyp .eq. 'BUCKINGHAM')  call ebuck1
         if (vdwtyp .eq. 'MM3-HBOND')  call emm3hb1
         if (vdwtyp .eq. 'BUFFERED-14-7')  call ehal1
         if (vdwtyp .eq. 'GAUSSIAN')  call egauss1
      end if
!$OMP END PARALLEL
c
c     call the electrostatic energy and gradient routines
c
      if (use_charge)  call echarge1
      if (use_chgdpl)  call echgdpl1
      if (use_dipole)  call edipole1
      if (use_mpole .or. use_polar)  call empole1
      if (use_rxnfld)  call erxnfld1
c
c     call any miscellaneous energy and gradient routines
c
      if (use_solv)  call esolv1
      if (use_metal)  call emetal1
      if (use_geom)  call egeom1
c      if (use_extra)  call extra1
c
c     sum up to get the total energy and first derivatives


      esum = ea + eba + eub + eopb 
     &          + et  + ett + ev + ept
     &          + em + ep + eb 
    
      esum  = esum + sum(en_th) 
     

      

C     putting together the first derivatives
      do i = 1, n
         do j = 1, 3
            desum(j,i) = dev(j,i) + dem(j,i) + dep(j,i) 
            do k=1,nthread
               desum(j,i) = desum(j,i) + drv_th(k,j,i)
            end do
         end do
      end do


      do i =1,3
         do j = 1,3
            do k = 1,nthread
               vir(j,i) = vir(j,i) + vir_th(k,j,i)
            end do
         end do
      end do


      energy = esum
        
        do i=1, n
           do j=1,3
              derivs(j,i) = desum(j,i)
           end do 
        end do
  
c
c     check for an illegal value for the total energy
c
c     if (isnan(esum)) then
      if (esum .ne. esum) then
         write (iout,10)
   10    format (/,' GRADIENT  --  Illegal Value for the Total',
     &              ' Potential Energy')
         call fatal
      end if
      return
      end
