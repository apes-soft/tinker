!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     #################################################################
!     ##                                                             ##
!     ##  polpot.f90  --  specifics of polarization functional form  ##
!     ##                                                             ##
!     #################################################################
!
!
!     poleps    
!     p2scale   
!     p3scale   
!     p4scale   
!     p5scale   
!     p41scale  
!     d1scale   
!     d2scale   
!     d3scale   
!     d4scale   
!     u1scale   
!     u2scale   
!     u3scale   
!     u4scale   
!     udiag     
!     poltyp    
!
!

module polpot

  implicit none

  integer:: politer          ! maximum number of induced dipole SCF iterations
  real (kind=8):: poleps     ! induced dipole convergence criterion 
                             ! (rms Debyes/atom) 
  real (kind=8):: p2scale    ! scale factor for 1-2 polarization energy 
                             ! interactions  
  real (kind=8):: p3scale    ! scale factor for 1-3 polarization energy 
                             ! interactions  
  real (kind=8):: p4scale    ! scale factor for 1-4 polarization energy 
                             ! interactions  
  real (kind=8):: p5scale    ! scale factor for 1-5 polarization energy 
                             ! interactions  
  real (kind=8):: p41scale   ! additional factor for 1-4 intragroup polarization
  real (kind=8):: d1scale    ! scale factor for intra-group direct induction   
  real (kind=8):: d2scale    ! scale factor for 1-2 group direct induction 
  real (kind=8):: d3scale    ! scale factor for 1-3 group direct induction
  real (kind=8):: d4scale    ! scale factor for 1-4 group direct induction 
  real (kind=8):: u1scale    ! scale factor for intra-group mutual induction 
  real (kind=8):: u2scale    ! scale factor for 1-2 group mutual induction 
  real (kind=8):: u3scale    ! scale factor for 1-3 group mutual induction 
  real (kind=8):: u4scale    ! scale factor for 1-4 group mutual induction 
  real (kind=8):: udiag      ! acceleration factor for induced dipole SCF 
                             ! iterations
  character (len=6):: poltyp ! type of polarization potential 
                             ! (direct or mutual)   
  save

end module polpot
