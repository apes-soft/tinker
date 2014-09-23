!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1993  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ##############################################################
!     ##                                                          ##
!     ##  subroutine fatal  --  terminate the program abnormally  ##
!     ##                                                          ##
!     ##############################################################
!
!
!     "fatal" terminates execution due to a user request, a severe
!     error or some other nonstandard condition


   subroutine fatal

   use parallelparams
   use iounit

   implicit none

   integer:: errcode=99  ! Specify an error code

   ! print a final warning message, then quit
   write (iout,10)
10 format (/,' TINKER is Unable to Continue; Terminating', &
              ' the Current Calculation',/)

   ! Abort the MPI program
   call MPI_Abort(MPI_COMM_WORLD,errcode,ierror)

   stop
   end subroutine fatal

