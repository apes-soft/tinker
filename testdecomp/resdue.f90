!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ##################################################################
!     ##                                                              ##
!     ##  resdue.f90  --  biopolymer residue names and biotype codes  ##
!     ##                                                              ##
!     ##################################################################
!
!
!     ntyp     biotypes for mid-chain peptide backbone N atoms
!     catyp    biotypes for mid-chain peptide backbone CA atoms
!     ctyp     biotypes for mid-chain peptide backbone C atoms
!     hntyp    biotypes for mid-chain peptide backbone HN atoms
!     otyp     biotypes for mid-chain peptide backbone O atoms
!     hatyp    biotypes for mid-chain peptide backbone HA atoms
!     cbtyp    biotypes for mid-chain peptide backbone CB atoms
!     nntyp    biotypes for N-terminal peptide backbone N atoms
!     cantyp   biotypes for N-terminal peptide backbone CA atoms
!     cntyp    biotypes for N-terminal peptide backbone C atoms
!     hnntyp   biotypes for N-terminal peptide backbone HN atoms
!     ontyp    biotypes for N-terminal peptide backbone O atoms
!     hantyp   biotypes for N-terminal peptide backbone HA atoms
!     nctyp    biotypes for C-terminal peptide backbone N atoms
!     cactyp   biotypes for C-terminal peptide backbone CA atoms
!     cctyp    biotypes for C-terminal peptide backbone C atoms
!     hnctyp   biotypes for C-terminal peptide backbone HN atoms
!     octyp    biotypes for C-terminal peptide backbone O atoms
!     hactyp   biotypes for C-terminal peptide backbone HA atoms
!     o5typ    biotypes for nucleotide backbone and sugar O5' atoms
!     c5typ    biotypes for nucleotide backbone and sugar C5' atoms
!     h51typ   biotypes for nucleotide backbone and sugar H5' atoms
!     h52typ   biotypes for nucleotide backbone and sugar H5'' atoms
!     c4typ    biotypes for nucleotide backbone and sugar C4' atoms
!     h4typ    biotypes for nucleotide backbone and sugar H4' atoms
!     o4typ    biotypes for nucleotide backbone and sugar O4' atoms
!     c1typ    biotypes for nucleotide backbone and sugar C1' atoms
!     h1typ    biotypes for nucleotide backbone and sugar H1' atoms
!     c3typ    biotypes for nucleotide backbone and sugar C3' atoms
!     h3typ    biotypes for nucleotide backbone and sugar H3' atoms
!     c2typ    biotypes for nucleotide backbone and sugar C2' atoms
!     h21typ   biotypes for nucleotide backbone and sugar H2' atoms
!     o2typ    biotypes for nucleotide backbone and sugar O2' atoms
!     h22typ   biotypes for nucleotide backbone and sugar H2'' atoms
!     o3typ    biotypes for nucleotide backbone and sugar O3' atoms
!     ptyp     biotypes for nucleotide backbone and sugar P atoms
!     optyp    biotypes for nucleotide backbone and sugar OP atoms
!     h5ttyp   biotypes for nucleotide backbone and sugar H5T atoms
!     h3ttyp   biotypes for nucleotide backbone and sugar H3T atoms
!     amino    three-letter abbreviations for amino acids types
!     nuclz    three-letter abbreviations for nucleic acids types
!     amino1   one-letter abbreviations for amino acids types
!     nuclz1   one-letter abbreviations for nucleic acids types
!
!

module resdue

  use sizes, ONLY: maxamino,maxnuc

  implicit none

  integer, dimension(maxamino):: ntyp,catyp
  integer, dimension(maxamino):: ctyp,hntyp
  integer, dimension(maxamino):: otyp,hatyp,cbtyp
  integer, dimension(maxamino):: nntyp,cantyp,cntyp
  integer, dimension(maxamino):: hnntyp,ontyp,hantyp
  integer, dimension(maxamino):: nctyp,cactyp,cctyp
  integer, dimension(maxamino):: hnctyp,octyp,hactyp
  integer, dimension(maxnuc):: o5typ,c5typ,h51typ
  integer, dimension(maxnuc):: h52typ,c4typ,h4typ
  integer, dimension(maxnuc):: o4typ,c1typ,h1typ
  integer, dimension(maxnuc):: c3typ,h3typ,c2typ
  integer, dimension(maxnuc):: h21typ,o2typ,h22typ
  integer, dimension(maxnuc):: o3typ,ptyp,optyp
  integer, dimension(maxnuc):: h5ttyp,h3ttyp
  character (len=1), dimension(maxamino):: amino1
  character (len=1), dimension(maxnuc):: nuclz1
  character (len=3), dimension(maxamino):: amino
  character (len=3), dimension(maxnuc):: nuclz

  save

end module resdue
