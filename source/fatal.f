c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1993  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ##############################################################
c     ##                                                          ##
c     ##  subroutine fatal  --  terminate the program abnormally  ##
c     ##                                                          ##
c     ##############################################################
c
c
c     "fatal" terminates execution due to a user request, a severe
c     error or some other nonstandard condition
c
c
      subroutine fatal(errcode)

      use iounit
      use parallelparams

      implicit none

      integer, optional, intent(in):: errcode  ! Specify an error code

c
c
c     print a final warning message, then quit
c
      write (iout,10)
   10 format (/,' TINKER is Unable to Continue; Terminating',
     &           ' the Current Calculation',/)

      ! Abort the MPI program
      call MPI_Abort(MPI_COMM_WORLD,errcode,ierror)

      stop
      end
