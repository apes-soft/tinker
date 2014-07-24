c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ###############################################################
c     ##                                                           ##
c     ##  module bath  --  thermostat and barostat control values  ##
c     ##                                                           ##
c     ###############################################################
c
c
c     maxnose     maximum length of Nose-Hoover thermostat chain
c
c     voltrial    mean number of steps between Monte Carlo moves
c     kelvin      target value for the system temperature (K)
c     atmsph      target value for the system pressure (atm)
c     tautemp     time constant for Berendsen thermostat (psec)
c     taupres     time constant for Berendsen barostat (psec)
c     compress    isothermal compressibility of medium (atm-1)
c     collide     collision frequency for Andersen thermostat
c     eta         velocity value for Bussi-Parrinello barostat
c     volmove     maximum volume move for Monte Carlo barostat (Ang**3)
c     vbar        velocity of log volume for Nose-Hoover barostat
c     qbar        mass of the volume for Nose-Hoover barostat
c     gbar        force for the volume for Nose-Hoover barostat
c     vnh         velocity of each chained Nose-Hoover thermostat
c     qnh         mass for each chained Nose-Hoover thermostat
c     gnh         force for each chained Nose-Hoover thermostat
c     isothermal  logical flag governing use of temperature control
c     isobaric    logical flag governing use of pressure control
c     anisotrop   logical flag governing use of anisotropic pressure
c     thermostat  choice of temperature control method to be used
c     barostat    choice of pressure control method to be used
c     volscale    choice of scaling method for Monte Carlo barostat
c
c
      module bath

      implicit none

      integer, parameter:: maxnose=4
      integer:: voltrial
      real (kind=8):: kelvin,atmsph
      real (kind=8):: tautemp,taupres
      real (kind=8):: compress,collide
      real (kind=8):: eta,volmove
      real (kind=8):: vbar,qbar,gbar
      real (kind=8), dimension(maxnose):: vnh
      real (kind=8), dimension(maxnose):: qnh
      real (kind=8), dimension(maxnose):: gnh
      logical isothermal
      logical isobaric
      logical anisotrop
      character (LEN=9):: volscale
      character (LEN=11):: barostat
      character (LEN=11):: thermostat

      save
      end
