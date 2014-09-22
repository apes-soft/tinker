!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ################################################################
!     ##                                                            ##
!     ##  pdb.f90  --  definition of a Protein Data Bank structure  ##
!     ##                                                            ##
!     ################################################################
!
!
!     xpdb      x-coordinate of each atom stored in PDB format
!     ypdb      y-coordinate of each atom stored in PDB format
!     zpdb      z-coordinate of each atom stored in PDB format
!     npdb      number of atoms stored in Protein Data Bank format
!     nres      number of residues stored in Protein Data Bank format
!     resnum    number of the residue to which each atom belongs
!     resatm    number of first and last atom in each residue
!     npdb12    number of atoms directly bonded to each CONECT atom
!     ipdb12    atom numbers of atoms connected to each CONECT atom
!     pdblist   list of the Protein Data Bank atom number of each atom
!     pdbtyp    Protein Data Bank record type assigned to each atom
!     atmnam    Protein Data Bank atom name assigned to each atom
!     resnam    Protein Data Bank residue name assigned to each atom
!     chnsym    string with PDB chain identifiers to be included
!     altsym    string with PDB alternate locations to be included
!     instyp    string with PDB insertion records to be included
!
!

module pdb

  use sizes, ONLY: maxatm,maxbonds

  implicit none

  integer:: npdb
  integer:: nres
  integer, dimension(maxatm):: resnum
  integer, dimension(2,maxatm):: resatm
  integer, dimension(maxatm):: npdb12
  integer, dimension(maxbonds,maxatm):: ipdb12
  integer, dimension(maxatm):: pdblist
  real (kind=8), dimension(maxatm):: xpdb
  real (kind=8), dimension(maxatm):: ypdb
  real (kind=8), dimension(maxatm):: zpdb
  character (len=1):: altsym
  character (len=3), dimension(maxatm):: resnam
  character (len=4), dimension(maxatm):: atmnam
  character (len=6), dimension(maxatm):: pdbtyp
  character (len=20):: chnsym
  character (len=20):: instyp
  save

end module pdb
