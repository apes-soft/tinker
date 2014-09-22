!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ###############################################################
!     ##                                                           ##
!     ##  sizes.f90  --  parameter values to set array dimensions  ##
!     ##                                                           ##
!     ###############################################################
!
!
!     "sizes.mod" sets values for critical array dimensions used
!     throughout the software; these parameters will fix the size
!     of the largest systems that can be handled; values too large
!     for the computer memory or swap space to accomodate will
!     result in poor performance or outright failure
!

module sizes
       implicit none

 integer, parameter:: maxatm=100000   ! atoms in the molecular system
 integer, parameter:: maxbonds=8      ! atoms directly bonded to an atom
 integer, parameter:: maxgrp=1000     ! user-defined groups of atoms
 integer, parameter:: maxref=10       ! stored reference molecular systems
 integer, parameter:: maxtyp=5000     ! force field atom type definitions
 integer, parameter:: maxclass=1000   ! force field atom class definitions
 integer, parameter:: maxprm=25000    ! lines in the parameter file		
 integer, parameter:: maxkey=25000    ! lines in the keyword file
 integer, parameter:: maxrot=1000     ! bonds for torsional rotation		
 integer, parameter:: maxvar=3*maxatm ! optimization variables (vector storage)
 integer, parameter:: maxopt=1000     ! optimization variables (matrix storage)
 integer, parameter:: maxhess=1000000 ! off-diagonal Hessian elements		
 integer, parameter:: maxlight=8*maxatm ! sites for method of lights neighbors	
 integer, parameter:: maxvlst=1800    ! neighbors in van der Waals pair list	
 integer, parameter:: maxelst=1200    ! neighbors in electrostatics pair list	
 integer, parameter:: maxulst=100     ! neighbors in dipole preconditioner list
 integer, parameter:: maxfft=250      ! grid points in each FFT dimension	
 integer, parameter:: maxfix=maxatm   ! geometric constraints and restraints	
 integer, parameter:: maxvib=1000     ! vibrational frequencies	
 integer, parameter:: maxgeo=2500     ! distance geometry points	
 integer, parameter:: maxcell=10000   ! unit cells in replicated crystal	
 integer, parameter:: maxring=10000   ! 3-, 4-, or 5-membered rings		
 integer, parameter:: maxbio=10000    ! biopolymer atom definitions		
 integer, parameter:: maxres=10000    ! residues in the macromolecule		
 integer, parameter:: maxele=112      ! elements in periodic table		
 integer, parameter:: maxamino=38     ! amino acid residue types	
 integer, parameter:: maxnuc=12       ! nucleic acid residue types		
 integer, parameter:: maxbnd=2*maxatm ! covalent bonds in molecular system	
 integer, parameter:: maxang=4*maxatm ! bond angles in molecular system	
 integer, parameter:: maxtors=6*maxatm  ! torsional angles in molecular system	
 integer, parameter:: maxbitor=8*maxatm ! bitorsions in molecular system

end module sizes

