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
      use couple
      use deriv
      use energi
      use inter
      use iounit
      use limits
      use mpiparams
      use potent
      use rigid
      use vdwpot
      use virial
      implicit none
      integer i,j
      real*8 energy,cutoff
      real*8 derivs(3,n)
      real*8 tmpdvs(3,n)  ! Temporary derivative sums
      real*8 sumtmp       ! Temporary energy sum
      real*8 viro(3,3)
      real*8 partmp
      real*8 sumdvs(3,n)
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
      
      ebtmp = 0.0d0
      virtemp = 0.0d0
      etmp = 0.0d0
      eatmp = 0.0d0
    
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
      
      allocate (detmp(3,n))
      allocate (debtmp(3,n))
      allocate (deatmp(3,n))

      ! zero out each of the first derivative components
      deb = 0.0d0
      dea = 0.0d0
      deba = 0.0d0
      deub = 0.0d0
      deaa = 0.0d0
      deopb = 0.0d0
      deopd = 0.0d0
      deid = 0.0d0
      deit = 0.0d0
      det = 0.0d0
      dept = 0.0d0
      debt = 0.0d0
      deat = 0.0d0
      dett = 0.0d0
      dev = 0.0d0
      dec = 0.0d0
      decd = 0.0d0
      ded = 0.0d0
      dem = 0.0d0
      dep = 0.0d0
      der = 0.0d0
      des = 0.0d0
      delf = 0.0d0
      deg = 0.0d0
      dex = 0.0d0

      emtmp = 0.0d0
      debtmp = 0.0d0
      deatmp = 0.0d0
      

      ! zero out the virial and the intermolecular energy
      vir = 0.0d0
      einter = 0.0d0
   
c
c     maintain any periodic boundary conditions
c
      if (use_bounds .and. .not.use_rigid)  call bounds
c
c     update the pairwise interaction neighbor lists
c
      if (use_list)  call nblist
c
c     remove any previous use of the replicates method
c
      cutoff = 0.0d0
      call replica (cutoff)
c
c     many implicit solvation models require Born radii
c
      if (use_born)  call born
c
c     alter bond and torsion constants for pisystem
c
      if (use_orbit)  call picalc
c
c     call the local geometry energy and gradient routines
c
      if (use_bond)  call ebond1
      if (use_angle)  call eangle1
      if (use_strbnd)  call estrbnd1
      if (use_urey)  call eurey1
      if (use_angang)  call eangang1
      if (use_opbend)  call eopbend1
      if (use_opdist)  call eopdist1
      if (use_improp)  call eimprop1
      if (use_imptor)  call eimptor1
      if (use_tors)  call etors1
      if (use_pitors)  call epitors1
      if (use_strtor)  call estrtor1
      if (use_angtor)  call eangtor1
      if (use_tortor)  call etortor1
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

      ! Currently using tmp variables to perform sums - eventually
      ! these can be removed when all the code has been migrated
      ! to use loops that have been split and the partial results
      ! need to be summed up.

       sumtmp = 0.0d0
       ! em, ep give problems => modifications other than in ereal1d?
       ! ev from ehal1c also gives problems.

       partmp = 0.0d0
       partmp = ev + emtmp + ebtmp + eatmp + eba!+ eb
       
c       print*, "emtmp from id", emtmp

       call MPI_Allreduce(partmp, sumtmp, 1, MPI_DOUBLE_PRECISION,
     &                   MPI_SUM, MPI_COMM_WORLD, ierror)
       ev = 0.0d0
       eba = 0.0d0
       !eb = 0.0d0
c       em = 0.0d0

c       print*, "ev from id", ev, rank
c       print*, "ev summed", sumtmp, rank
       

      ! sum up to get the total energy and first derivatives
      esum = eb + ea + eba + eub + eaa + eopb + eopd + eid + eit +
     &       et + ept + ebt + eat + ett+ ev + ec + ecd + ed +
     &       em + ep + er + es + elf + eg + ex + sumtmp

      

      energy = esum

!      esum = eb + ea + eba + eub + eaa + eopb + eopd + eid + eit
!     &          + et + ept + ebt + eat + ett + ev + ec + ecd + ed
!     &          + em + ep + er + es + elf + eg + ex
!      energy = esum

      tmpdvs = 0.0d0
      tmpdvs = dev + detmp + debtmp + deatmp + deba !+ deb
      sumdvs = 0.0d0
      call MPI_Allreduce(tmpdvs, sumdvs, 3*n, MPI_DOUBLE_PRECISION,
     &     MPI_SUM, MPI_COMM_WORLD, ierror)

      dev = 0.0d0
      deba = 0.0d0
      !deb = 0.0d0
      
      desum = deb + dea + deba +
     &             deub + deaa + deopb +
     &             deopd + deid + deit +
     &             det + dept + debt +
     &             deat + dett + dev +
     &             dec + decd + ded +
     &             dem + dep + der +
     &             des + delf + deg +
     &             dex + sumdvs
      derivs = desum

!      do i = 1, n
!         do j = 1, 3
!            desum(j,i) = deb(j,i) + dea(j,i) + deba(j,i)
!     &                      + deub(j,i) + deaa(j,i) + deopb(j,i)
!     &                      + deopd(j,i) + deid(j,i) + deit(j,i)
!     &                      + det(j,i) + dept(j,i) + debt(j,i)
!     &                      + deat(j,i) + dett(j,i) + dev(j,i)
!     &                      + dec(j,i) + decd(j,i) + ded(j,i)
!     &                      + dem(j,i) + dep(j,i) + der(j,i)
!     &                      + des(j,i) + delf(j,i) + deg(j,i)
!     &                      + dex(j,i)
!            derivs(j,i) = desum(j,i)
!         end do
!      end do

      
      call MPI_Allreduce(virtemp, viro, 9, MPI_DOUBLE_PRECISION,
     &     MPI_SUM, MPI_COMM_WORLD, ierror)

      vir = vir + viro

      sumtmp = 0.0d0
      call MPI_Allreduce(etmp, sumtmp, 1, MPI_DOUBLE_PRECISION,
     &     MPI_SUM, MPI_COMM_WORLD, ierror)


      einter = einter + em + ep  + sumtmp
      deallocate(detmp)
      deallocate(debtmp)
      deallocate(deatmp)

      !print*, "einter", einter
      !print*, "vir", vir


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
