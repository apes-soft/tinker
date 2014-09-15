!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     #############################################################
!     ##                                                         ##
!     ##  sequen.f90  --  sequence information for a biopolymer  ##
!     ##                                                         ##
!     #############################################################
!
!
!     nseq     total number of residues in biopolymer sequences
!     nchain   number of separate biopolymer sequence chains
!     ichain   first and last residue in each biopolymer chain
!     seqtyp   residue type for each residue in the sequence
!     seq      three-letter code for each residue in the sequence
!     chnnam   one-letter identifier for each sequence chain
!     chntyp   contents of each chain (GENERIC, PEPTIDE or NUCLEIC)
!
!

module sequen

  use sizes, ONLY: maxres

  implicit none

  integer:: nseq
  integer, dimension(maxres):: seqtyp
  integer:: nchain
  integer, dimension(2, maxres):: ichain
  character (len=1), dimension(maxres):: chnnam
  character (len=3), dimension(maxres):: seq
  character (len=7), dimension(maxres):: chntyp
  save

end module sequen
