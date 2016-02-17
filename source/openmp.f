c
c
c     ###################################################
c     ##  COPYRIGHT (C)  2010  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     #############################################################
c     ##                                                         ##
c     ##  module openmp  --  OpenMP processor and thread values  ##
c     ##                                                         ##
c     #############################################################
c
c
c     nproc     number of processors available to OpenMP
c     nthread   number of threads to be used with OpenMP 
c
c
      module openmp
      implicit none
      integer nproc
      integer nthread
      integer th_id
      real*8, allocatable :: xred_th(:)
      real*8, allocatable :: yred_th(:)
      real*8, allocatable :: zred_th(:)
      real*8, allocatable :: vscale_th(:)
      integer, allocatable :: iv14_th(:)
      real*8, allocatable :: fieldt_omp(:,:)
      real*8, allocatable :: fieldtp_omp(:,:)
      integer, allocatable :: offset_omp(:)
      integer toffset0
      integer nlocal
      real*8, allocatable :: ilocal_omp(:,:,:)
      real*8, allocatable :: dlocal_omp(:,:,:)
      real*8, allocatable :: field_omp(:,:)
      real*8, allocatable :: fieldp_omp(:,:)
      real*8, allocatable :: udir_omp(:,:)
      real*8, allocatable :: udirp_omp(:,:)
      real*8 eintra_omp
      real*8 em_omp, ep_omp
      real*8, allocatable :: uind_omp(:,:)
      real*8, allocatable :: uinp_omp(:,:)
      real*8, allocatable :: poli_omp(:)
      real*8, allocatable :: rsd_omp(:,:)
      real*8, allocatable :: rsdp_omp(:,:)
      real*8, allocatable :: zrsd_omp(:,:)
      real*8, allocatable :: zrsdp_omp(:,:)
      real*8, allocatable :: zrsdtp_omp(:,:)
      real*8, allocatable :: zrsdt_omp(:,:)
      real*8, allocatable :: conjp_omp(:,:)
      real*8, allocatable :: conj_omp(:,:)
      real*8, allocatable :: vec_omp(:,:)
      real*8, allocatable :: vecp_omp(:,:)
      logical done_omp
      real*8 a_omp,ap_omp,sum_omp,sump_omp      
      save
!$OMP threadprivate(th_id)
!$OMP threadprivate(nlocal)
      end
