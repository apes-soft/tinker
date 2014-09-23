!
!
!     ################################################################
!     ##  COPYRIGHT (C) 2006 by Michael Schnieders & Jay W. Ponder  ##
!     ##                     All Rights Reserved                    ##
!     ################################################################
!
!     #################################################################
!     ##                                                             ##
!     ##  neigh.f90  --  pairwise neighbor list indices and storage  ##
!     ##                                                             ##
!     #################################################################
!
!
!     lbuffer     width of the neighbor list buffer region
!     pbuffer     width of the preconditioner list buffer region
!     lbuf2       square of half the neighbor list buffer width
!     pbuf2       square of half the preconditioner list buffer width
!     vbuf2       square of vdw cutoff plus neighbor list buffer
!     cbuf2       square of charge cutoff plus neighbor list buffer
!     mbuf2       square of multipole cutoff plus neighbor list buffer
!     ubuf2       square of preconditioner cutoff plus neighbor buffer
!     vbufx       square of vdw cutoff plus twice the list buffer
!     cbufx       square of charge cutoff plus twice the list buffer
!     mbufx       square of multipole cutoff plus twice the list buffer
!     ubufx       square of preconditioner cutoff plus twice the buffer
!     xvold       x-coordinate at last vdw neighbor list update
!     yvold       y-coordinate at last vdw neighbor list update
!     zvold       z-coordinate at last vdw neighbor list update
!     xcold       x-coordinate at last charge neighbor list update
!     ycold       y-coordinate at last charge neighbor list update
!     zcold       z-coordinate at last charge neighbor list update
!     xmold       x-coordinate at last multipole neighbor list update
!     ymold       y-coordinate at last multipole neighbor list update
!     zmold       z-coordinate at last multipole neighbor list update
!     xuold       x-coordinate at last preconditioner neighbor update
!     yuold       y-coordinate at last preconditioner neighbor update
!     zuold       z-coordinate at last preconditioner neighbor update
!     nvlst       number of sites in list for each vdw site
!     vlst        site numbers in neighbor list of each vdw site
!     nelst       number of sites in list for each electrostatic site
!     elst        site numbers in list of each electrostatic site
!     nulst       number of sites in list for each preconditioner site
!     ulst        site numbers in list of each preconditioner site
!     dovlst      logical flag to rebuild vdw neighbor list
!     doclst      logical flag to rebuild charge neighbor list
!     domlst      logical flag to rebuild multipole neighbor list
!     doulst      logical flag to rebuild preconditioner neighbor list
!
!

module neigh

  implicit none

  integer, pointer :: nvlst(:)
  integer, pointer :: vlst(:,:)
  integer, pointer :: nelst(:)
  integer, pointer :: elst(:,:)
  integer, pointer :: nulst(:)
  integer, pointer :: ulst(:,:)
  real (kind=8):: lbuffer,pbuffer
  real (kind=8):: lbuf2,pbuf2
  real (kind=8):: vbuf2,cbuf2
  real (kind=8):: mbuf2,ubuf2
  real (kind=8):: vbufx,cbufx
  real (kind=8):: mbufx,ubufx
  real (kind=8), pointer :: xvold(:)
  real (kind=8), pointer :: yvold(:)
  real (kind=8), pointer :: zvold(:)
  real (kind=8), pointer :: xcold(:)
  real (kind=8), pointer :: ycold(:)
  real (kind=8), pointer :: zcold(:)
  real (kind=8), pointer :: xmold(:)
  real (kind=8), pointer :: ymold(:)
  real (kind=8), pointer :: zmold(:)
  real (kind=8), pointer :: xuold(:)
  real (kind=8), pointer :: yuold(:)
  real (kind=8), pointer :: zuold(:)
  logical:: dovlst,doclst
  logical:: domlst,doulst

  save

end module neigh
