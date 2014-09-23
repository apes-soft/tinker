!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ###############################################################
!     ##                                                           ##
!     ##  atmtyp.f90  --  atomic properties for each current atom  ##
!     ##                                                           ##
!     ###############################################################
!


module atmtyp
       use sizes, ONLY: maxatm

 integer, dimension(maxatm):: tag                ! integer atom labels
 integer, dimension(maxatm):: class              ! atom class number
 integer, dimension(maxatm):: atomic             ! atomic number 
 integer, dimension(maxatm):: valence            ! valence number
 real (kind=8), dimension(maxatm):: mass         ! atomic weight
 character (len=3), dimension(maxatm):: name     ! atom name
 character (len=24), dimension(maxatm):: story   ! descriptive type

end module atmtyp
