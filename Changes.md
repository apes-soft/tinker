# Cumulative changes made to the code

Using this to develop `tinkerLite` - a cut down version of tinker, basically `dynamic`
with restricted options of what can be run. The main aim of this work is to:

1. Try and improve serial performance and scalability.
2. Produce a distributed memory version of `dynamic` using MPI.

# Changes Made

Enumerate in reverse chronological order the rationale for
changes made to the `tinkerLite` code base.

* Made the derived data type atomtype into allocatable structure. 
  The atomtype variable atom is initialised inside the readxyz.f file and
  has n elements - the actual number of atoms in the modelled system (rather 
  than the maxatm). Deallocation is done inside the final.f file.     

* Removed the redundant atomid.f file. All the information is now in atoms.f 
  file, stored as the elements of atomtype.  
 
* Added class, atomic number, valence, story and name elements to the atomtype,
  which resulted in small degradation in perfromance compared to previous 
  versions.Overall, using the atomtype with the following elements:
  pos, mass, tag, type, class, atomic, valence, story and name degraded the 
  perfromance by up to 4.5% depending on the number of OpenMP threads used.    

* Added tag and type elements to the atomtype - perfromance slightly degraded 
  for larger number of threads. Can access tag and type information through
  atom(N)%tag and atom(N)%type.  

* Added mass element to the atomtype which resulted in a small degradation in 
  perfromance for up to 8 threads and improvement for larger numbers of threads.
  The mass elament is accessed by atom(N)%mass.  

* Created derived data type - atomtype to keep the information about atoms
  in one place. The elements of the atomtype have been added incrementally 
  starting with position of the atoms. The pos(3)(N) array has been 
  transformed into array element of atom derived data type - atom(N)%pos(3). 
  The difference in the perfromance is negligible. After the modification the
  code is slightly faster for up to 10 threads and slightly slower for larger 
  numbers.    

* Have collapsed the distinct `x(N)`, `y(N)` and `z(N)`
  into a single array `pos(3)(N)`. Testing has shown that this has
  no major impact on performance but the change will simplify a
  distributed memory parallelisation by reducing the number of explicit
  message passes required to exchange coordinate information.
* Removed `freeunit.f` and moved the definition of specific 
  unit numbers to deal with io of different types to `iounit.f`.
* Had a further cull of unused variables as reported by the
  `gfortran` compiler with the `-Wall` flag.
* Added a test directory. Only have one valid regression test
  `dhfr` otherwise known as JAC. Could not import the files 
  from the main `tinker` distribution as that uses a `Modified Beeman 
  Algorithm` while `tinkerLite` uses a `Velocity Verlet Algorithm`
  which gives a different result - enough to cause the test to 
  fail. Have taken an output from the current version, which has
  not had many modifications, as the basis for future regression 
  testing.
* Have tried to make the lack of provision of any inputs to be a
  fatal condition as opposed to going to standard input to ask
  the user to provide a value. In an HPC context this could 
  potentially be a problem - user submits a job, misconfigures the
  inputs and the program goes to standard input to ask the user
  and, as the user, has not access to the back end nodes they are
  blissfully ignorant that their program has stalled. This could
  burn a lot of hours unnecessarily ergo it is better to go into
  a terminal state.
* Renamed `F77` to `FC` and `F77FLAGS` to `FCFLAGS` which is a more
  standard way of referring to the Fortran compiler in `Makefile`s.
* Introduced conditionals to the `Makefile` to specify the compiler to
  use depending on the host name. This makes it easier to pick
  specific compiler options depending on what host is being used. 
  This is just meant to make things easier for development though
  it could be made more robust, i.e. user sets variable at the 
  top which specifies which compiler option to use as opposed to
  having to comment in/out different options.
* Made several changes to the Makefile. I think the way that the tinker
  library is linked in is the correct way now. Also, only make the 
  files that are newer than the library so do not have to recompile
  all the source code as was done before.
* Moved initialization of `input` and `iout` from `init.f` to `iounit.f`.

# Possible changes

Park thoughts about future changes that could be made.

* Migrate to an `.f90` (free source format) version for the files.
* Process the keywords in one place as opposed to throughout the
  distribution. Could use a `SELECT CASE` statement in Fortran.
  This would mean that the keys array would not have to be 
  processed multiple times and it would allow for proper 
  checking of all keywords, e.g. ones that were not valid could
  be flagged up as errors.
