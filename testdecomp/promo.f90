!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ###############################################################
!     ##                                                           ##
!     ##  subroutine promo  --  version info and copywrite notice  ##
!     ##                                                           ##
!     ###############################################################
!
!
!     "promo" writes a short message containing information
!     about the TINKER version number and the copyright notice
!
!
      subroutine promo

      use iounit

      implicit none

!
!
!     print out the informational header message
!
      write (iout,10)
   10 format (/,5x,70('#'),                                        &
              /,3x,74('#'),                                        &
              /,2x,'###',70x,'###',                                &
              /,1x,'###',12x,'TINKER  ---  Software Tools for',    &
                 ' Molecular Design',12x,'###',                    &
              /,1x,'##',74x,'##',                                  &
              /,1x,'##',24x,'Version 6.3  February 2014',24x,'##', &
              /,1x,'##',74x,'##',                                  &
              /,1x,'##',15x,'Copyright (c)  Jay William Ponder',   &
                 '  1990-2014',15x,'##',                           &
              /,1x,'###',27x,'All Rights Reserved',26x,'###',      &
              /,2x,'###',70x,'###',                                &
              /,3x,74('#'),                                        &
              /,5x,70('#'),/)
      return
      end
