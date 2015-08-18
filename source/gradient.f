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

      use warp 
      implicit none
      integer i,j
      real*8 energy,cutoff
      real*8 derivs(3,*)
      real*8 elrc, vlrc
c
c
c     zero out each of the potential energy components
c
      eb = 0.0d0
      ea = 0.0d0
      eba = 0.0d0
      eub = 0.0d0
      eaa = 0.0d0
      eopb = 0.0d0
      eopd = 0.0d0
      eid = 0.0d0
      eit = 0.0d0
      et = 0.0d0
      ept = 0.0d0
      ebt = 0.0d0
      eat = 0.0d0
      ett = 0.0d0
      ev = 0.0d0
      ec = 0.0d0
      ecd = 0.0d0
      ed = 0.0d0
      em = 0.0d0
      ep = 0.0d0
      er = 0.0d0
      es = 0.0d0
      elf = 0.0d0
      eg = 0.0d0
      ex = 0.0d0
c
c     perform dynamic allocation of some global arrays
c
      if (allocated(desum)) then
         if (size(desum) .lt. 3*n) then
            deallocate (desum)
            deallocate (deb)
            deallocate (dea)
            deallocate (deba)
            deallocate (deub)
            deallocate (deaa)
            deallocate (deopb)
            deallocate (deopd)
            deallocate (deid)
            deallocate (deit)
            deallocate (det)
            deallocate (dept)
            deallocate (debt)
            deallocate (deat)
            deallocate (dett)
            deallocate (dev)
            deallocate (dec)
            deallocate (decd)
            deallocate (ded)
            deallocate (dem)
            deallocate (dep)
            deallocate (der)
            deallocate (des)
            deallocate (delf)
            deallocate (deg)
            deallocate (dex)
         end if
      end if
      if (.not. allocated(desum)) then
         allocate (desum(3,n))
         allocate (deb(3,n))
         allocate (dea(3,n))
         allocate (deba(3,n))
         allocate (deub(3,n))
         allocate (deaa(3,n))
         allocate (deopb(3,n))
         allocate (deopd(3,n))
         allocate (deid(3,n))
         allocate (deit(3,n))
         allocate (det(3,n))
         allocate (dept(3,n))
         allocate (debt(3,n))
         allocate (deat(3,n))
         allocate (dett(3,n))
         allocate (dev(3,n))
         allocate (dec(3,n))
         allocate (decd(3,n))
         allocate (ded(3,n))
         allocate (dem(3,n))
         allocate (dep(3,n))
         allocate (der(3,n))
         allocate (des(3,n))
         allocate (delf(3,n))
         allocate (deg(3,n))
         allocate (dex(3,n))
      end if

c
c     zero out the virial and the intermolecular energy
c
      do i = 1, 3
         do j = 1, 3
            vir(j,i) = 0.0d0
         end do
      end do
      einter = 0.0d0


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
C$$$!$OMP& reduction(+:deb,vir)

!$OMP parallel default(shared) 
ccshared(nbond,ibnd,bl,bk,use,
cc!$OMP& x,y,z,cbnd,qbnd,bndtyp,bndunit,use_group,use_polymer)
ccc!$OMP& reduction(+:deb,vir,eb)
    
c
c     zero out each of the first derivative components
c

!$OMP DO schedule(guided)
      do i = 1, n
         do j = 1, 3
            deb(j,i) = 0.0d0
            dea(j,i) = 0.0d0
            deba(j,i) = 0.0d0
            deub(j,i) = 0.0d0
            deaa(j,i) = 0.0d0
            deopb(j,i) = 0.0d0
            deopd(j,i) = 0.0d0
            deid(j,i) = 0.0d0
            deit(j,i) = 0.0d0
            det(j,i) = 0.0d0
            dept(j,i) = 0.0d0
            debt(j,i) = 0.0d0
            deat(j,i) = 0.0d0
            dett(j,i) = 0.0d0
            dev(j,i) = 0.0d0
            dec(j,i) = 0.0d0
            decd(j,i) = 0.0d0
            ded(j,i) = 0.0d0
            dem(j,i) = 0.0d0
            dep(j,i) = 0.0d0
            der(j,i) = 0.0d0
            des(j,i) = 0.0d0
            delf(j,i) = 0.0d0
            deg(j,i) = 0.0d0
            dex(j,i) = 0.0d0
         end do
      end do
!$OMP END DO


c
c     maintain any periodic boundary conditions
c

      if (use_bounds .and. .not.use_rigid)  call bounds ! no omp
c
c     update the pairwise interaction neighbor lists
c

!$OMP master       
      if (use_list)  call nblist
!$OMP end master


c
c     remove any previous use of the replicates method
c
      cutoff = 0.0d0
      call replica (cutoff)  ! no omp
c
c     many implicit solvation models require Born radii
c
      if (use_born)  call born ! no omp 
c
c     alter bond and torsion constants for pisystem
c
      if (use_orbit)  call picalc  ! no omp

!$OMP barrier
!$OMP flush
c
c     call the local geometry energy and gradient routines
c

      if (use_bond)  call ebond1
      if (use_angle)  call eangle1
      if (use_strbnd)  call estrbnd1
      if (use_urey)  call eurey1
!$OMP master
      if (use_angang)  call eangang1 ! no omp
      if (use_opbend)  call eopbend1 ! no omp
      if (use_opdist)  call eopdist1 ! no omp
      if (use_improp)  call eimprop1 ! no omp
      if (use_imptor)  call eimptor1 ! no omp
!$OMP end master
c!$OMP END PARALLEL
!$OMP barrier
c!$OMP flush

c      if(use_tors) call etors1
      if (use_tors)  then
         if (use_smooth) then
            call etors1b
         else
            call etors1a
         end if
      end if
!$OMP END PARALLEL

      if (use_pitors)  call epitors1 ! no omp
      if (use_strtor)  call estrtor1 ! no omp
      if (use_angtor)  call eangtor1 ! no omp
      if (use_tortor)  call etortor1 ! no omp
c
c     call the van der Waals energy and gradient routines
c
      if (use_vdw) then
         if (vdwtyp .eq. 'LENNARD-JONES')  call elj1
         if (vdwtyp .eq. 'BUCKINGHAM')  call ebuck1
         if (vdwtyp .eq. 'MM3-HBOND')  call emm3hb1
         if (vdwtyp .eq. 'BUFFERED-14-7')  call ehal1
    
C$$$      if (vdwtyp .eq. 'BUFFERED-14-7')  then
C$$$           if (use_lights) then
C$$$               call ehal1b
C$$$            else if (use_vlist) then
C$$$               call ehal1c
C$$$            else
C$$$               call ehal1a
C$$$            end if
C$$$c     
C$$$c     apply long range van der Waals correction if desired
C$$$c     
C$$$            if (use_vcorr) then
C$$$               call evcorr1 (elrc,vlrc)
C$$$               ev = ev + elrc
C$$$               vir(1,1) = vir(1,1) + vlrc
C$$$               vir(2,2) = vir(2,2) + vlrc
C$$$               vir(3,3) = vir(3,3) + vlrc
C$$$            end if
C$$$         end if
         if (vdwtyp .eq. 'GAUSSIAN')  call egauss1
      end if
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
      if (use_extra)  call extra1
c
c     sum up to get the total energy and first derivatives
c
      esum = eb + ea + eba + eub + eaa + eopb + eopd + eid + eit
     &          + et + ept + ebt + eat + ett + ev + ec + ecd + ed
     &          + em + ep + er + es + elf + eg + ex
    

ccc!$OMP DO schedule(guided)      
      do i = 1, n
         do j = 1, 3
            desum(j,i) = deb(j,i) + dea(j,i) + deba(j,i)
     &                      + deub(j,i) + deaa(j,i) + deopb(j,i)
     &                      + deopd(j,i) + deid(j,i) + deit(j,i)
     &                      + det(j,i) + dept(j,i) + debt(j,i)
     &                      + deat(j,i) + dett(j,i) + dev(j,i)
     &                      + dec(j,i) + decd(j,i) + ded(j,i)
     &                      + dem(j,i) + dep(j,i) + der(j,i)
     &                      + des(j,i) + delf(j,i) + deg(j,i)
     &                      + dex(j,i)
         end do
      end do

ccc!$OMP END DO


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
