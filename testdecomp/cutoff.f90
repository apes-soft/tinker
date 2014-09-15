!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ################################################################
!     ##                                                            ##
!     ##  cutoff.f90  --  cutoff distances for energy interactions  ##
!     ##                                                            ##
!     ################################################################
!


module cutoff

 implicit none

  real (kind=8):: vdwcut   ! cutoff distance for van der Waals interactions
  real (kind=8):: chgcut   ! cutoff distance for charge-charge interactions
  real (kind=8):: dplcut   ! cutoff distance for dipole-dipole interactions
  real (kind=8):: mpolecut ! cutoff distance for atomic multipole interactions
  real (kind=8):: vdwtaper ! distance at which van der Waals switching begins
  real (kind=8):: chgtaper ! distance at which charge-charge switching begins
  real (kind=8):: dpltaper ! distance at which dipole-dipole switching begins
  real (kind=8):: mpoletaper ! distance at which atomic multipole switching 
                           ! begins   
  real (kind=8):: ewaldcut ! cutoff distance for direct space Ewald summation
  real (kind=8):: usolvcut ! cutoff distance for dipole solver preconditioner
  logical:: use_ewald      ! logical flag governing use of Ewald summation
  logical:: use_lights     ! logical flag governing use of method of lights
  logical:: use_list       ! logical flag governing use of any neighbor lists
  logical:: use_vlist      ! logical flag governing use of vdw neighbor list
  logical:: use_clist      ! logical flag governing use of charge neighbor list
  logical:: use_mlist      ! logical flag governing use of multipole neighbor 
                           ! list 
  logical:: use_ulist      ! logical flag governing use of preconditioner list
  save

contains


!     ################################################################
!     ##                                                            ##
!     ##  subroutine cutoffs  --  set distance and Hessian cutoffs  ##
!     ##                                                            ##
!     ################################################################
!
!
!     "cutoffs" initializes and stores spherical energy cutoff
!     distance windows, Hessian element and Ewald sum cutoffs,
!     and the pairwise neighbor generation method


subroutine cutoffs

  use sizes
  use atoms
  use bound
  use hescut
  use keys
  use neigh
  use polpot
  use tarray

  implicit none

  integer:: i,next
  real (kind=8):: big,value
  logical:: truncate
  character (len=20):: keyword
  character (len=120):: record
  character (len=120):: string


  ! set defaults for spherical energy cutoff distances

  big = 1.0d12
  if (use_bounds) then
     vdwcut   = 9.0d0
     chgcut   = 9.0d0
     dplcut   = 9.0d0
     mpolecut = 9.0d0
  else
     vdwcut   = big
     chgcut   = big
     dplcut   = big
     mpolecut = big
  end if
  ewaldcut = 7.0d0
  usolvcut = 4.5d0

  ! set defaults for tapering, Hessian cutoff and neighbor buffers

  vdwtaper   = 0.90d0
  chgtaper   = 0.65d0
  dpltaper   = 0.75d0
  mpoletaper = 0.65d0
  hesscut    = 0.0d0
  lbuffer    = 2.0d0
  pbuffer    = 2.0d0

  ! set defaults for Ewald sum, tapering style and neighbor method

  use_ewald  = .false.
  truncate   = .false.
  use_lights = .false.
  use_list   = .false.
  use_vlist  = .false.
  use_clist  = .false.
  use_mlist  = .false.
  use_ulist  = .false.
  dovlst     = .true.
  doclst     = .true.
  domlst     = .true.
  doulst     = .true.

  ! search the keywords for various cutoff parameters

  do i = 1, nkey
     next = 1
     record = keyline(i)
     call gettext (record,keyword,next)
     call upcase (keyword)
     string = record(next:120)
     ! get values related to use of Ewald summation
     if (keyword(1:6) .eq. 'EWALD ') then
        use_ewald = .true.
     else if (keyword(1:13) .eq. 'EWALD-CUTOFF ') then
        read (string,*,err=10,end=10)  ewaldcut
     ! get cutoff for preconditioner of dipole solver
     else if (keyword(1:14) .eq. 'USOLVE-CUTOFF ') then
        read (string,*,err=10,end=10)  usolvcut
     ! get values for the tapering style and neighbor method
     else if (keyword(1:9) .eq. 'TRUNCATE ') then
        truncate = .true.
     else if (keyword(1:7) .eq. 'LIGHTS ') then
        use_lights = .true.
     else if (keyword(1:14) .eq. 'NEIGHBOR-LIST ') then
        use_list  = .true.
        use_vlist = .true.
        use_clist = .true.
        use_mlist = .true.
        use_ulist = .true.
     else if (keyword(1:9) .eq. 'VDW-LIST ') then
        use_list  = .true.
        use_vlist = .true.
     else if (keyword(1:9) .eq. 'CHG-LIST ') then
        use_list  = .true.
        use_clist = .true.
     else if (keyword(1:11) .eq. 'MPOLE-LIST ') then
        use_list  = .true.
        use_mlist = .true.
        use_ulist = .true.
     ! get cutoff for the magnitude of Hessian elements
     else if (keyword(1:12) .eq. 'HESS-CUTOFF ') then
        read (string,*,err=10,end=10)  hesscut
     ! get the cutoff radii for potential energy functions
     else if (keyword(1:7) .eq. 'CUTOFF ') then
        read (string,*,err=10,end=10)  value
        vdwcut   = value
        chgcut   = value
        dplcut   = value
        mpolecut = value
        ewaldcut = value
     else if (keyword(1:11) .eq. 'VDW-CUTOFF ') then
        read (string,*,err=10,end=10)  vdwcut
     else if (keyword(1:11) .eq. 'CHG-CUTOFF ') then
        read (string,*,err=10,end=10)  chgcut
     else if (keyword(1:11) .eq. 'DPL-CUTOFF ') then
        read (string,*,err=10,end=10)  dplcut
     else if (keyword(1:13) .eq. 'MPOLE-CUTOFF ') then
        read (string,*,err=10,end=10)  mpolecut
     ! get distance for initialization of energy switching
     else if (keyword(1:6) .eq. 'TAPER ') then
        read (string,*,err=10,end=10)  value
        vdwtaper   = value
        chgtaper   = value
        dpltaper   = value
        mpoletaper = value
     else if (keyword(1:10) .eq. 'VDW-TAPER ') then
        read (string,*,err=10,end=10)  vdwtaper
     else if (keyword(1:10) .eq. 'CHG-TAPER ') then
        read (string,*,err=10,end=10)  chgtaper
     else if (keyword(1:10) .eq. 'DPL-TAPER ') then
        read (string,*,err=10,end=10)  dpltaper
     else if (keyword(1:12) .eq. 'MPOLE-TAPER ') then
        read (string,*,err=10,end=10)  mpoletaper
     ! get buffer width for use with pairwise neighbor lists
     else if (keyword(1:12) .eq. 'LIST-BUFFER ') then
        read (string,*,err=10,end=10)  lbuffer
     else if (keyword(1:14) .eq. 'USOLVE-BUFFER ') then
        read (string,*,err=10,end=10)  pbuffer
     end if
10   continue
  end do
  
  ! preconditioner list only needed for mutual polarization
  if (poltyp .ne. 'MUTUAL')  use_ulist = .false.
  if (use_list)  usolvcut = usolvcut - pbuffer

  ! apply any Ewald cutoff to charge and multipole terms
  if (use_ewald) then
     chgcut   = ewaldcut
     mpolecut = ewaldcut
  end if

  ! convert any tapering percentages to absolute distances
  if (vdwtaper .lt. 1.0d0)  vdwtaper = vdwtaper * vdwcut
  if (chgtaper .lt. 1.0d0)  chgtaper = chgtaper * chgcut
  if (dpltaper .lt. 1.0d0)  dpltaper = dpltaper * dplcut
  if (mpoletaper .lt. 1.0d0)  mpoletaper = mpoletaper * mpolecut

  ! apply truncation cutoffs if they were requested
  if (truncate) then
     vdwtaper   = big
     chgtaper   = big
     dpltaper   = big
     mpoletaper = big
  end if

  ! set buffer region limits for pairwise neighbor lists
  lbuf2 = (0.5d0*lbuffer)**2
  pbuf2 = (0.5d0*pbuffer)**2
  vbuf2 = (vdwcut+lbuffer)**2
  cbuf2 = (chgcut+lbuffer)**2
  mbuf2 = (mpolecut+lbuffer)**2
  ubuf2 = (usolvcut+pbuffer)**2
  vbufx = (vdwcut+2.0d0*lbuffer)**2
  cbufx = (chgcut+2.0d0*lbuffer)**2
  mbufx = (mpolecut+2.0d0*lbuffer)**2
  ubufx = (usolvcut+2.0d0*pbuffer)**2

  ! perform dynamic allocation of some pointer arrays
  if (use_vlist) then
     if (associated(nvlst))  deallocate (nvlst)
     if (associated(vlst))  deallocate (vlst)
     if (associated(xvold))  deallocate (xvold)
     if (associated(yvold))  deallocate (yvold)
     if (associated(zvold))  deallocate (zvold)
     allocate (nvlst(n))
     allocate (vlst(maxvlst,n))
     allocate (xvold(n))
     allocate (yvold(n))
     allocate (zvold(n))
  end if
  if (use_clist .or. use_mlist) then
     if (associated(nelst))  deallocate (nelst)
     if (associated(elst))  deallocate (elst)
     allocate (nelst(n))
     allocate (elst(maxelst,n))
  end if
  if (use_clist) then
     if (associated(xcold))  deallocate (xcold)
     if (associated(ycold))  deallocate (ycold)
     if (associated(zcold))  deallocate (zcold)
     allocate (xcold(n))
     allocate (ycold(n))
     allocate (zcold(n))
  end if
  if (use_mlist) then
     if (associated(xmold))  deallocate (xmold)
     if (associated(ymold))  deallocate (ymold)
     if (associated(zmold))  deallocate (zmold)
     allocate (xmold(n))
     allocate (ymold(n))
     allocate (zmold(n))
     if (poltyp .eq. 'MUTUAL') then
        if (associated(tindex))  deallocate (tindex)
        if (associated(tdipdip))  deallocate (tdipdip)
        allocate (tindex(2,n*maxelst))
        allocate (tdipdip(6,n*maxelst))
     end if
  end if
  if (use_ulist) then
     if (associated(nulst))  deallocate (nulst)
     if (associated(ulst))  deallocate (ulst)
     if (associated(xuold))  deallocate (xuold)
     if (associated(yuold))  deallocate (yuold)
     if (associated(zuold))  deallocate (zuold)
     allocate (nulst(n))
     allocate (ulst(maxulst,n))
     allocate (xuold(n))
     allocate (yuold(n))
     allocate (zuold(n))
  end if
  return
      
end subroutine cutoffs

end module cutoff
