!
!
!     ###################################################
!     ##  COPYRIGHT (C)  2002  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ###################################################################
!     ##                                                               ##
!     ##  socket.f90  --  control parameters for socket communication  ##
!     ##                                                               ##
!     ###################################################################
!
!
!     runtyp      calculation type for passing socket information
!     cstep       current optimization or dynamics step number
!     cdt         current dynamics cumulative simulation time
!     cenergy     current potential energy from simulation
!     cdx         current gradient components along the x-axis
!     cdy         current gradient components along the y-axis
!     cdz         current gradient components along the z-axis
!     use_socket  logical flag governing use of external sockets
!     skt_init    logical flag to indicate socket initialization
!     skt_close   logical flag to indicate socket shutdown
!
!

module socket

  use sizes, ONLY: maxatm

  implicit none

  integer:: runtyp
  integer:: cstep
  real (kind=8)::  cdt
  real (kind=8):: cenergy
  real (kind=8), dimension(maxatm):: cdx
  real (kind=8), dimension(maxatm):: cdy
  real (kind=8), dimension(maxatm):: cdz
  logical:: use_socket
  logical:: skt_init
  logical:: skt_close
  save

end module socket
