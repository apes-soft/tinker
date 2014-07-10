# Cumulative changes made to the code

Using this to develop `tinkerLite` - a cut down version of tinker, basically `dynamic`
with restricted options of what can be run. The main aim of this work is to:

1. Try and improve serial performance and scalability.
2. Produce a distributed memory version of `dynamic` using MPI.

# Changes Made

Enumerate in reverse chronological order the rationale for
changes made to the `tinkerLite` code base.

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
  `dhfr` otherwise know as JAC. Could not import the files 
  from the main `tiker` distribution as that usess a `Modified Beeman 
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
