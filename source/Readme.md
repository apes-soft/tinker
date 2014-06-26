# Changes

Trying to enumerate in reverse chronological order the rationale for
changes made to the code base.

* Renamed `F77` to `FC` and `F77FLAGS` to `FCFLAGS` which is a more
  standard way of referring to the Fortran compiler in `Makefile`s.
* Introduced conditionals to the `Makefile` to specify the compiler to
  use depending on the host name. This will make it easier to pick
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
