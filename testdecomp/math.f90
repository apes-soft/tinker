!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ############################################################
!     ##                                                        ##
!     ##  math.f90  --  mathematical and geometrical constants  ##
!     ##                                                        ##
!     ############################################################
!
!
!     radian   conversion factor from radians to degrees
!     pi       numerical value of the geometric constant
!     sqrtpi   numerical value of the square root of Pi
!     logten   numerical value of the natural log of ten
!     sqrttwo  numerical value of the square root of two
!     twosix   numerical value of the sixth root of two
!
!

module math

   implicit none

   real (kind=8), parameter:: radian=57.29577951308232088d0
   real (kind=8), parameter:: pi=3.141592653589793238d0
   real (kind=8), parameter:: sqrtpi=1.772453850905516027d0
   real (kind=8), parameter:: logten=2.302585092994045684d0
   real (kind=8), parameter:: sqrttwo=1.414213562373095049d0
   real (kind=8), parameter:: twosix=1.122462048309372981d0
   save

contains

function log2(x)

   implicit none

   real (kind=8):: log2
   real (kind=8):: x

   log2 = log(x)/log(2.0)

end function log2

end module math
