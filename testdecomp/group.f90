!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1997  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ############################################################
!     ##                                                        ##
!     ##  group.i  --  partitioning of system into atom groups  ##
!     ##                                                        ##
!     ############################################################
!
!
!     grpmass     total mass of all the atoms in each group
!     wgrp        weight for each set of group-group interactions
!     ngrp        total number of atom groups in the system
!     kgrp        contiguous list of the atoms in each group
!     igrp        first and last atom of each group in the list
!     grplist     number of the group to which each atom belongs
!     use_group   flag to use partitioning of system into groups
!     use_intra   flag to include only intragroup interactions
!     use_inter   flag to include only intergroup interactions
!
!

module group

  use sizes, ONLY: maxgrp,maxatm

  implicit none

  integer:: ngrp
  integer, dimension(maxatm):: kgrp
  integer, dimension(2,0:maxgrp):: igrp
  integer, dimension(maxatm):: grplist
  real (kind=8), dimension(maxgrp):: grpmass
  integer, dimension(0:maxgrp,0:maxgrp):: wgrp
  logical:: use_group
  logical:: use_intra
  logical:: use_inter

  save

end module group
