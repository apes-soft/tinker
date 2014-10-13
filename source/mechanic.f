c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ###############################################################
c     ##                                                           ##
c     ##  subroutine mechanic  --  initialize molecular mechanics  ##
c     ##                                                           ##
c     ###############################################################
c
c
c     "mechanic" sets up needed parameters for the potential energy
c     calculation and reads in many of the user selectable options
c
c
      subroutine mechanic
      use inform
      use iounit
      use limits
      use vdwpot
      implicit none
c
c
c     set the bonded connectivity lists and active atoms
c
      call attach
c
c     find bonds, angles, torsions, bitorsions and small rings
c
      call bonds
      call angles
      call torsions
      call bitors
      call rings
c
c     get the base force field from parameter file and keyfile
c
      call field
c
c     find unit cell type, lattice parameters and cutoff values
c
      call unitcell
      call lattice
c
c     assign atom types, classes and other atomic information
c
      call katom
c
c     assign atoms to molecules and set the atom groups
c
      call molecule
c
c     assign bond, angle and cross term potential parameters
c
      call kbond
      call kangle
      call kstrbnd
      call kurey
c
c     assign out-of-plane deformation potential parameters
c
      call kopbend
c
c     assign torsion and torsion cross term potential parameters
c
      call ktors
      call kpitors
      call ktortor
c
c     assign van der Waals and electrostatic potential parameters
c
      call kvdw
      call kmpole
      call kpolar
      call kewald
c
c     quit if essential parameter information is missing
c
      if (abort) then
         write (iout,10)
   10    format (/,' MECHANIC  --  Some Required Potential Energy',
     &              ' Parameters are Undefined')
         call fatal
      end if
      return
      end
