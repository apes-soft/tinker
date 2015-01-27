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
c     "initial" sets up original values for some parameters and
c     variables that might not otherwise get initialized
c
c     note the calls below to "kmp_set" routines are only needed by
c     the Intel compiler and must be commented for other compilers
c
c
      subroutine initial
      use sizes
      use align
      use atoms
      use bath
      use bound
      use boxes
      use cell
      use files
      use group
      use inform
      use iounit
      use keys
      use linmin
      use minima
      use molcul
      use mpiparams
      use mutant
      use neigh
      use openmp
      use output
      use params
      use pdb
      use precis
      use rigid
      use scales
      use sequen
      use socket
      use warp
      use zclose
      implicit none
!$    integer omp_get_num_procs
      integer omp_get_num_threads
      real*8 precise
      logical first
      save first
      data first  / .true. /


      ! display program banner and copyright notice
      ! only process 0 should do this.
      if (first.and.rank.eq.0)  call promo

      ! command line arguments to the program
      if (first)  call command
      if (first)  first = .false.
c
c     cores, thread count and options for OpenMP
c
c      nproc = 1 
     
       nthread = 1
!$OMP PARALLEL shared(nthread) 
!$OMP MASTER 
!$       nthread = omp_get_num_threads()
!$OMP END MASTER
!$OMP END PARALLEL 

       ! In general we do not want the number of threads to
       ! be set to the total number of cores available on 
       ! the system. Better to set this using the OpenMP 
       ! environment variable OMP_NUMTHREADS or to use the
       ! TINKER OPENMP-THREADS key word.
 
c      nproc = omp_get_num_procs ()
c!$    nthread = nproc
c!$    call omp_set_num_threads (nthread)


      ! Allow nested parallelism
!$    call omp_set_nested (.true.)
 
      ! Intel compiler extensions to OpenMP standard
      ! Commented out - better to set the OpenMP environment
      ! variable OMP_STACKSIZE and to ensure that 
      ! is set in the shell ulimit -s unlimited
c!$    call kmp_set_stacksize_s (2**28)
c!$    call kmp_set_blocktime (0)

      ! values of machine precision constants
      tiny  = precise (1)
      small = precise (2)
      huge  = precise (3)

      ! number of lines in the keyfile
      nkey = 0

      ! number of lines in the parameter file
      nprm = 0

      ! number of atoms in the system
      n = 0

      ! number of molecules in the system
      nmol = 0

      ! number of unit cell replicates
      ncell = 0

      ! number of atoms used in superposition
      nfit = 0

      ! number of mutated atoms in the system
      nmut = 0

      ! number of bonds added or deleted from Z-matrix
      nadd = 0
      ndel = 0

      ! number of atoms in Protein Data Bank format
      npdb = 0

      ! number of residues and chains in biopolymer sequence
      nseq   = 0
      nchain = 0

      ! highest numbered previous cycle file
      nprior = 0

      ! flags for information levels within the program
      silent  = .false.
      verbose = .false.
      debug   = .false.
      abort   = .false.

      ! flag for use of atom groups
      use_group = .false.

      ! flags for periodic boundaries
      use_bounds  = .false.
      use_replica = .false.
      use_polymer = .false.

      ! default values for unitcell dimensions
      xbox  = 0.0d0
      ybox  = 0.0d0
      zbox  = 0.0d0
      alpha = 0.0d0
      beta  = 0.0d0
      gamma = 0.0d0

      ! flags for temperature and pressure baths
      isothermal = .false.
      isobaric   = .false.

      ! flags for rebuilding of neighbor lists
      dovlst = .true.
      doclst = .true.
      domlst = .true.
      doulst = .true.

      ! flag for use of rigid bodies
      use_rigid = .false.

      ! flag to show setting of optimization scale factors
      set_scale = .false.

      ! flags for external Java socket communication
      skt_init   = .false.
      use_socket = .false.

      ! flags for potential energy smoothing
      use_smooth  = .false.
      use_dem     = .false.
      use_gda     = .false.
      use_tophat  = .false.
      use_stophat = .false.

      ! type of coordinates file
      coordtype = 'NONE'

      ! atomic symbols for elements
      call initatom

      ! names of biopolymer residue types
      call initres

      ! default values used by optimizations
      fctmin   = 0.0d0
      maxiter  = 0
      nextiter = 0
      iprint   = -1
      iwrite   = -1
      stpmax   = 0.0d0
      return
      end
