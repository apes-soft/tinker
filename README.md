TINKER
======

TINKER Software Tools for Molecular Design. This is development
version of the public GitHub [repository](https://github.com/jayponder/tinker) 
maintained by Jay Ponder.

This is the `repmem` branch which is exploring a replicated memory version
of:

* `dynamic`

Any other application may no longer work. We may also have a look at:

* `bar` which reads in two trajectories and two key files (i.e. different
lambda windows for free energy calculations), and calculates the energies
of trajectory 1 with key file 2, and vice versa. The read-in is very slow
on this, up to a few hours for ~2000 structures. Richard has been thinking
for a while that ideally the energy evaluation should be done as an option
to dynamic - i.e. whenever the energy of a structure is printed out, why
not evaluate the energy of the same structure at different lambda windows
too - this would save all the costly trajectory post-processing later.


Questions
=========

* Had a discussion with Weronika as to whether we should make 
  modifications to file in situ or change renamed copies. We
  opted for the former but if several utilities use a single
  file and only one of the apps gets converted to MPI then 
  this will not work. 
* Should we use Fortran 90 derived types? This would reduce the number
  of communication calls that would be required. 

Changes
=======

Notable changes from the TINKER `master` branch are:

* Moved the `control` and `getkey` subroutines from 
  `basefile.f` to `getxyz.f` - it just seems to be 
  so much more logical to call them from there rather 
  than `basefile.f`.
* Only process 0 reads the key file in `getkey.f` and then 
  broadcasts the value of `nkey` (actual number of key lines)
  and the `keyline` array which contains the key contents.
* Added file `mpiparams.f` - a module to store the MPI parameters.
* Added file `help.f` that prints out usage information.
* Removed (or in the process of removing) stdin input to the 
  application. These will normally run in a batch environment
  where there is no direct connection between the user running
  the program and the application that is running. Attempts to
  get input would stall execution and waste CPU cycles.
* Including a `Makefile` in the `source` directory. Have included 
  a fairly large number of changes from the `Makefile` included 
  in the tinker distribution.
* The `fftw` has been moved from a top level directory in the TINKER
  distribution to live under the `thirdparty` subdirectory otherwise
  it confuses GitHub into thinking that TINKER is a C application.

