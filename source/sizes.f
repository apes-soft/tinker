c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ############################################################
c     ##                                                        ##
c     ##  module sizes  --  parameters to set array dimensions  ##
c     ##                                                        ##
c     ############################################################
c
c
c     "sizes" sets values for critical array dimensions used
c     throughout the software; these parameters will fix the size
c     of the largest systems that can be handled; values too large
c     for the computer memory or swap space to accomodate will
c     result in poor performance or outright failure
c
c     parameter:      maximum allowed number of:
c
c     maxatm          atoms in the molecular system
c     maxbonds        atoms directly bonded to an atom
c     maxgrp          user-defined groups of atoms
c     maxref          stored reference molecular systems
c     maxtyp          force field atom type definitions
c     maxclass        force field atom class definitions
c     maxprm          lines in the parameter file
c     maxkey          lines in the keyword file
c     maxrot          bonds for torsional rotation
c     maxvar          optimization variables (vector storage)
c     maxopt          optimization variables (matrix storage)
c     maxlight        sites for method of lights neighbors
c     maxvlst         neighbors in van der Waals pair list
c     maxelst         neighbors in electrostatics pair list
c     maxulst         neighbors in dipole preconditioner list
c     maxfft          grid points in each FFT dimension
c     maxfix          geometric constraints and restraints
c     maxvib          vibrational frequencies
c     maxgeo          distance geometry points
c     maxcell         unit cells in replicated crystal
c     maxring         3-, 4-, or 5-membered rings
c     maxbio          biopolymer atom definitions
c     maxres          residues in the macromolecule
c     maxele          elements in periodic table
c     maxamino        amino acid residue types
c     maxnuc          nucleic acid residue types
c     maxbnd          covalent bonds in molecular system
c     maxang          bond angles in molecular system
c     maxtors         torsional angles in molecular system
c     maxbitor        bitorsions in molecular system
c
c
      module sizes
      implicit none
      integer, parameter:: maxatm=25000
      integer, parameter:: maxbonds=8
      integer, parameter:: maxgrp=1000
      integer, parameter:: maxref=10
      integer, parameter:: maxtyp=5000
      integer, parameter:: maxclass=1000
      integer, parameter:: maxprm=25000
      integer, parameter:: maxkey=25000
      integer, parameter:: maxrot=1000
      integer, parameter:: maxvar=3*maxatm
      integer, parameter:: maxopt=1000
      integer, parameter:: maxlight=8*maxatm
      integer, parameter:: maxvlst=1800
      integer, parameter:: maxelst=1200
      integer, parameter:: maxulst=100
      integer, parameter:: maxfft=250
      integer, parameter:: maxfix=maxatm
      integer, parameter:: maxvib=1000
      integer, parameter:: maxgeo=2500
      integer, parameter:: maxcell=10000
      integer, parameter:: maxring=10000
      integer, parameter:: maxbio=10000
      integer, parameter:: maxres=10000
      integer, parameter:: maxele=112
      integer, parameter:: maxamino=38
      integer, parameter:: maxnuc=12
      integer, parameter:: maxbnd=2*maxatm
      integer, parameter:: maxang=4*maxatm
      integer, parameter:: maxtors=6*maxatm
      integer, parameter:: maxbitor=8*maxatm

      save
      end
