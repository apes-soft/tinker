!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ##################################################################
!     ##                                                              ##
!     ##  boxes.f90  --  parameters for periodic boundary conditions  ##
!     ##                                                              ##
!     ##################################################################
!


module boxes
   implicit none

   real (kind=8):: xbox         ! length of a-axis of periodic box in Angstroms
   real (kind=8):: ybox         ! length of b-axis of periodic box in Angstroms
   real (kind=8):: zbox         ! length of c-axis of periodic box in Angstroms
   real (kind=8):: alpha        ! angle between b- and c-axes of box in degrees
   real (kind=8):: beta         ! angle between a- and c-axes of box in degrees
   real (kind=8):: gamma        ! angle between a- and b-axes of box in degrees
   real (kind=8):: xbox2        ! half of the a-axis length of periodic box
   real (kind=8):: ybox2        ! half of the b-axis length of periodic box
   real (kind=8):: zbox2        ! half of the c-axis length of periodic box
   real (kind=8):: box34        ! 3/4 axis length of truncated octahedron
   real (kind=8):: volbox       ! volume in Ang**3 of the periodic box
   real (kind=8), dimension(3,3):: lvec         ! real space lattice vectors as matrix rows
   real (kind=8), dimension(3,3):: recip        ! reciprocal lattice vectors as matrix columns
   real (kind=8):: beta_sin     ! sine of the beta periodic box angle
   real (kind=8):: beta_cos     ! cosine of the beta periodic box angle
   real (kind=8):: gamma_sin    ! sine of the gamma periodic box angle
   real (kind=8):: gamma_cos    ! cosine of the gamma periodic box angle
   real (kind=8):: beta_term    ! term used in generating triclinic box
   real (kind=8):: gamma_term   ! term used in generating triclinic box
   logical:: orthogonal         ! flag to mark periodic box as orthogonal
   logical:: monoclinic         ! flag to mark periodic box as monoclinic
   logical:: triclinic          ! flag to mark periodic box as triclinic
   logical:: octahedron         ! flag to mark box as truncated octahedron
   character(len=10):: spacegrp ! space group symbol for the unitcell type 


end module boxes
