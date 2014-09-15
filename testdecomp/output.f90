!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ################################################################
!     ##                                                            ##
!     ##  output.f90  --  control of coordinate output file format  ##
!     ##                                                            ##
!     ################################################################
!

module output

  implicit none

  logical:: archive    ! flag to save structures in an archive        
  logical:: noversion  ! flag governing use of filename versions      
  logical:: overwrite  ! flag to overwrite intermediate files inplace 
  logical:: cyclesave  ! flag to mark use of numbered cycle files     
  character (len=9):: coordtype ! selects Cartesian, internal, rigid body or 
                                ! none
  save

end module output
