!
!
!     ###################################################
!     ##  COPYRIGHT (C)  2012  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ##############################################################
!     ##                                                          ##
!     ##  subroutine initatom  --  setup atoms in periodic table  ##
!     ##                                                          ##
!     ##############################################################
!
!
!     "initatom" sets the atomic symbol for each element in the
!     periodic table
!
!

subroutine initatom

   use sizes, ONLY: maxele
   use ptable

   implicit none

   character (len=3), dimension(maxele):: atmsym

   ! supported atomic symbols for the elements

   data atmsym  / 'H  ', 'He ', 'Li ', 'Be ', 'B  ', 'C  ', 'N  ', &
                  'O  ', 'F  ', 'Ne ', 'Na ', 'Mg ', 'Al ', 'Si ', &
                  'P  ', 'S  ', 'Cl ', 'Ar ', 'K  ', 'Ca ', 'Sc ', &
                  'Ti ', 'V  ', 'Cr ', 'Mn ', 'Fe ', 'Co ', 'Ni ', &
                  'Cu ', 'Zn ', 'Ga ', 'Ge ', 'As ', 'Se ', 'Br ', &
                  'Kr ', 'Rb ', 'Sr ', 'Y  ', 'Zr ', 'Nb ', 'Mo ', &
                  'Tc ', 'Ru ', 'Rh ', 'Pd ', 'Ag ', 'Cd ', 'In ', &
                  'Sn ', 'Sb ', 'Te ', 'I  ', 'Xe ', 'Cs ', 'Ba ', &
                  'La ', 'Ce ', 'Pr ', 'Nd ', 'Pm ', 'Sm ', 'Eu ', &
                  'Gd ', 'Tb ', 'Dy ', 'Ho ', 'Er ', 'Tm ', 'Yb ', &
                  'Lu ', 'Hf ', 'Ta ', 'W  ', 'Re ', 'Os ', 'Ir ', &
                  'Pt ', 'Au ', 'Hg ', 'Tl ', 'Pb ', 'Bi ', 'Po ', &
                  'At ', 'Rn ', 'Fr ', 'Ra ', 'Ac ', 'Th ', 'Pa ', &
                  'U  ', 'Np ', 'Pu ', 'Am ', 'Cm ', 'Bk ', 'Cf ', &
                  'Es ', 'Fm ', 'Md ', 'No ', 'Lr ', 'Rf ', 'Db ', &
                  'Sg ', 'Bh ', 'Hs ', 'Mt ', 'Ds ', 'Rg ', 'Cn ' /


   ! set atomic symbols for each element in periodic table
   elemnt = atmsym

   return

end subroutine initatom
