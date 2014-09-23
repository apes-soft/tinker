c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ################################################################
c     ##                                                            ##
c     ##  subroutine field  --  get the potential energy functions  ##
c     ##                                                            ##
c     ################################################################
c
c
c     "field" sets the force field potential energy functions from
c     a parameter file and modifications specified in a keyfile
c
c
      subroutine field
      use sizes
      use keys
      use potent
      implicit none
      integer i
      character*120 record
c
c
c     set the default values for the active potentials
c
      use_bond   = .true.
      use_angle  = .true.
      use_strbnd = .true.
      use_urey   = .true.
      use_angang = .false.
      use_opbend = .true.
      use_opdist = .false.
      use_improp = .false.
      use_imptor = .false.
      use_tors   = .true.
      use_pitors = .true.
      use_strtor = .false.
      use_angtor = .false.
      use_tortor = .true.
      use_vdw    = .true.
      use_charge = .false.
      use_chgdpl = .false.
      use_dipole = .false.
      use_mpole  = .true.
      use_polar  = .true.
      use_rxnfld = .false.
      use_solv   = .false.
      use_metal  = .false.
      use_geom   = .false.
      use_extra  = .false.
c
c     read the potential energy force field parameter file
c
      call getprm
c
c     check keywords for potential function control parameters
c
      do i = 1, nkey
         record = keyline(i)
         call prmkey (record)
      end do
      return
      end
