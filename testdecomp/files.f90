!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     #################################################################
!     ##                                                             ##
!     ##  files.f90  --  name and number of current structure files  ##
!     ##                                                             ##
!     #################################################################
!

module files
   implicit none

   integer:: nprior          ! number of previously existing cycle files
   integer, save:: ldir      ! length in characters of the directory name
   integer, save:: leng      ! length in characters of the base filename

   ! base filename used by default for all files
   character(len=120), save:: filename 

   ! output filename used for intermediate results
   character(len=120), save:: outfile  

end module files
