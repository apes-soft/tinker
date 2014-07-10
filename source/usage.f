c     Print a usage message.
c

      subroutine usage

      use iounit 

      implicit none

      write (iout,*)
      write (iout,*) " Usage: "
      write (iout,*)
      write (iout,*) "  dynamic [file] [nstep] [dt] [dtdump] [temp]", 
     &               " [pres]"
      write (iout,*)
      write (iout,*) " where:"
      write (iout,*) 
      write (iout,*) " file   : base name for .xyz coordinate file for",
     &               " the simulation system."
      write (iout,*) " nstep  : integer number of steps to be taken."
      write (iout,*) " dt     : real number value of timestep length",
     &               " in femtoseconds."
      write (iout,*) " dtdump : real number value of time between data",
     &               " writes in picoseconds."
      write (iout,*) " temp   : real number value of temperature of",
     &               " the system in Kelvin."
      write (iout,*) " pres   : real number value of pressure of the",
     &               " system in atmospheres."


      end subroutine usage

 
