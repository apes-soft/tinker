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
      real*8 a_omp,ap_omp,sum_omp,sump_omp,b_omp,bp_omp
      real*8 epsd_omp,epsp_omp
      real*8, allocatable :: fuind_omp(:,:)
      real*8, allocatable :: fuinp_omp(:,:)
      real*8, allocatable :: fdip_phi1_omp(:,:)
      real*8, allocatable :: fdip_phi2_omp(:,:)
      real*8, allocatable :: fdip_sum_phi_omp(:,:)
      real*8, allocatable :: dipfield1_omp(:,:)
      real*8, allocatable :: dipfield2_omp(:,:) 
      real*8, allocatable :: fmp_omp(:,:)
      real*8, allocatable :: cmp_omp(:,:)
      real*8 vxx_omp,vyx_omp,vzx_omp
      real*8 vyy_omp,vzy_omp,vzz_omp
      real*8 ctf_omp(10,10)
      real*8, allocatable :: cphi_omp(:,:)
      real*8, allocatable :: fphi_omp(:,:)
      real*8, allocatable :: frc_omp(:,:)
      real*8, allocatable :: trq_omp(:,:)
      logical, allocatable :: update_omp(:)
      logical, allocatable :: do_list(:)
      real*8, allocatable :: xsort_omp(:)
      real*8, allocatable :: ysort_omp(:)
      real*8, allocatable :: zsort_omp(:)
c      real*8, allocatable :: qgrid_omp(:,:,:,:,:)
c      real*8, allocatable :: qgrip_omp(:,:,:,:)
      real*8 e_omp
      save
!$OMP threadprivate(th_id)
!$OMP threadprivate(nlocal)
      end
