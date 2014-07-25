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
      write (iout,*)
      write (iout,*) "Other valid flags that can be used are:"
      write (iout,*)
      write (iout,*) "-K keyfile : to specify the key file."
      write (iout,*) "-V : sets verbose mode."
      write (iout,*) "-D : sets debug mode (also switches on verbose mode)."
      write (iout,*)


      end subroutine usage

 
