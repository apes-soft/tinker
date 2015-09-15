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
      save
!$OMP threadprivate(th_id)
!$OMP threadprivate(vscale_th)
      end
