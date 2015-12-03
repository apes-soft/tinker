c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ###############################################################
c     ##                                                           ##
c     ##  module virial  --  components of internal virial tensor  ##
c     ##                                                           ##
c     ###############################################################
c
c
c     vir    total internal virial Cartesian tensor components
c
c
      module virial
      implicit none
      real*8 vir(3,3)
      real*8 vir_omp(3,3)
      real*8 vir_tmp(3,3)
      real*8 ff
      real*8, allocatable :: vir_th(:,:,:)
      integer, allocatable :: lck_drv(:)
      real*8, allocatable :: mscale_omp(:)
      real*8, allocatable :: pscale_omp(:)
      real*8, allocatable :: dscale_omp(:)
      real*8, allocatable :: uscale_omp(:)
!$OMP threadprivate(vir_tmp)
!$OMP threadprivate(ff)      
      save
      end
