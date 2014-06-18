c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ################################################################
c     ##                                                            ##
c     ##  subroutine initial  --  initial values and program setup  ##
c     ##                                                            ##
c     ################################################################
c
c
c     "initial" sets up original values for some parameters
c     and variables that might not otherwise get initialized
c
c
      subroutine initial
      use sizes
      use atoms
      use bath
      use boxes
      use cell
      use files
      use inform
      use iounit
      use keys
      use molcul
      use neigh
      use openmp
      use output
      use params
      use zclose
      implicit none
!$    integer omp_get_num_procs
      integer omp_get_num_threads
      real*8 precise
c
c
c     cores, thread count and options for OpenMP
c
c      nproc = 1 
     
       nthread = 1
!$OMP PARALLEL shared(nthread) 
!$OMP MASTER 
       nthread = omp_get_num_threads()
!$OMP END MASTER
!$OMP END PARALLEL 

c
c     Intel compiler extensions to OpenMP standard
c
!!$    call kmp_set_stacksize_s (2**28)
!!$    call kmp_set_blocktime (0)
c
c     default unit numbers for input and output
c
      input = 5
      iout = 6
c
c     display program banner and copyright notice
c
      call promo
c
c     command line arguments to the program
c
      call command
c
c     number of lines in the keyfile
c
      nkey = 0
c
c     number of lines in the parameter file
c
      nprm = 0
c
c     number of atoms in the system
c
      n = 0
c
c     number of molecules in the system
c
      nmol = 0
c
c     number of unit cell replicates
c
      ncell = 0
c
c     number of bonds added or deleted from Z-matrix
c
      nadd = 0
      ndel = 0
c
c     highest numbered previous cycle file
c
      nprior = 0
c
c     flags for information levels within the program
c
      silent = .false.
      verbose = .false.
      debug = .false.
      abort = .false.
c
c     default values for unitcell dimensions
c
      xbox = 0.0d0
      ybox = 0.0d0
      zbox = 0.0d0
      alpha = 0.0d0
      beta = 0.0d0
      gamma = 0.0d0
c
c     flags for temperature and pressure baths
c
      isothermal = .false.
      isobaric = .false.
c
c     flags for rebuilding of neighbor lists
c
      dovlst = .true.
      doclst = .true.
      domlst = .true.
      doulst = .true.
c
c     type of coordinates file
c
      coordtype = 'NONE'
c
c     atomic symbols for elements
c
      call initatom
c
c     default values used by optimizations
c
      iprint = -1
      iwrite = -1
      return
      end
