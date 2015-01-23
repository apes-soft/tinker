TINKER
======

TINKER Software Tools for Molecular Design. This is development
version of the public GitHub [repository](https://github.com/jayponder/tinker) 
maintained by Jay Ponder.

This is the `repmem` branch which is exploring a replicated memory version
of:

* `dynamic`

Any other application may no longer work.

Questions
=========

* Had a discussion with Weronika as to whether we should make 
  modifications to file in situ or change renamed copies. We
  opted for the former but if several utilities use a single
  file and only one of the apps gets converted to MPI then 
  this will not work. 
* Should we use fortran 90 derived types? This would reduce the number
  of communication calls that would be required. 

Changes
=======

The major changes from the TINKER `master` branch are:

* Including a `Makefile` in the `source` directory. Have included 
  a fairly large number of changes from the `Makefile` included 
  in the tinker distribution.
* The `fftw` has been moved from a top level directory in the TINKER
  distribution to live under the `thirdparty` subdirectory otherwise
  it confuses GitHub into thinking that TINKER is a C application.

