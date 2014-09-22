!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ###############################################################
!     ##                                                           ##
!     ##  bath.i  --  temperature and pressure control parameters  ##
!     ##                                                           ##
!     ###############################################################
!
!
!     maxnose     maximum length of Nose-Hoover thermostat chain
!
!     kelvin      target value for the system temperature (K)
!     atmsph      target value for the system pressure (atm)
!     tautemp     time constant for Berendsen thermostat (psec)
!     taupres     time constant for Berendsen barostat (psec)
!     compress    isothermal compressibility of medium (atm-1)
!     collide     collision frequency for Andersen thermostat
!     vnh         velocity of each chained Nose-Hoover thermostat
!     qnh         mass for each chained Nose-Hoover thermostat
!     gnh         force for each chained Nose-Hoover thermostat
!     vbar        velocity of log volume for Nose-Hoover barostat
!     qbar        mass of the volume for Nose-Hoover barostat
!     gbar        force for the volume for Nose-Hoover barostat
!     eta         velocity value for Bussi-Parrinello barostat
!     volmove     maximum volume move for Monte Carlo barostat (Ang**3)
!     voltrial    mean number of steps between Monte Carlo moves
!     isothermal  logical flag governing use of temperature control
!     isobaric    logical flag governing use of pressure control
!     anisotrop   logical flag governing use of anisotropic pressure
!     thermostat  choice of temperature control method to be used
!     barostat    choice of pressure control method to be used
!     volscale    choice of scaling method for Monte Carlo barostat
!
!

module bath

  implicit none

  integer, parameter:: maxnose=4
  integer:: voltrial
  real (kind=8):: kelvin
  real (kind=8):: atmsph
  real (kind=8):: tautemp
  real (kind=8):: taupres
  real (kind=8):: compress
  real (kind=8):: collide
  real (kind=8), dimension(maxnose):: vnh
  real (kind=8), dimension(maxnose):: qnh
  real (kind=8), dimension(maxnose):: gnh
  real (kind=8):: vbar
  real (kind=8):: qbar
  real (kind=8):: gbar
  real (kind=8):: eta
  real (kind=8):: volmove
  logical:: isothermal
  logical:: isobaric
  logical:: anisotrop
  character (len=9):: volscale
  character (len=11):: barostat
  character (len=11):: thermostat

end module bath
