# Tinker Lite

This is a stripped down version of Tinker created mostly for the
purpose of readability and simplification, but also for small
performance gains.  This software is taken from Tinker 7.0 off of Jay
Ponder's GitHub [tinker repository](https://github.com/jayponder/tinker).  
This software will only perform the Tinker executable `dynamic` and
most options within `dynamic` are already made (see below for
details).  Using GNU compilers and 12 cores Alex Albaugh was able to
achieve ~5% speedups from the base Tinker for a system of 32000 water
molecules and for the JAC benchmark.  Any comments, questions, or
concerns can be direction to [Alex
Albaugh](mailto:aalbaugh@berkeley.edu).


# Compiling

If you do not have a compiled version of `fftw` then you will need to
compile this first. You should be able to do this by just running the
commands in the `fftw` directory (note the backticks that surround 
the `pwd` command should execute the command before being passed to 
the `configure` script):

      make distclean
      configure --prefix=`pwd`/fftw --enable-threads
      make
      make install

You can also use `--enable-openmp` instead of `--enable-thread` for an
OpenMP version of the libaries.

Modify the included `Makefile` to your desired paths and compilers, just
like with base Tinker.  The `make` command will then create a single 
executable called `dynamic.x`.


# Running
`dynamic.x` can be run from the command line or within a script in the 
following way:

    [path]/dynamic.x [file] [nstep] [dt] [dtdump] [temp] [pres]

where 

* `path`: the directory path to the executable
* `file`: `.xyz` coordinate file for the simulation system
* `nstep`: integer number of steps to be taken
* `dt`: real number value of timestep length in femtoseconds
* `dtdump`: real number value of time between data writes in picoseconds
* `temp`: real number value of temperature of the system in Kelvin
* `pres`: real number value of pressure of the system in atmospheres

This scheme is the same as base Tinker except you no longer need to 
specify the thermodynamic ensemble, the system will always be NPT (see
below).


# Features
Many of the decisions for `dynamic` in the base Tinker are already made.  The
following is a list of these features:

* NPT ensemble
* cubic system
* periodic boundaries, tinfoil, no replicas (specify a box length)
* neighbor lists (no need for any list keywords)
* Nose-Hoover themostat (no need for 'thermostat' keyword)
* Berendsen barostat (no need for `barostat` keywork)
* particle-mesh Ewald (no need for `ewald` keyword)
* Verlet integrator (no need for 'integrator keyword)
* mutual polarization (no need for `polarization` keyword)
* no energy/gradient calls to functions that don't correspond directly to 
   the `amoebapro13` or a`moebabio09` force field, will only call:
  * bonds
  * angles
  * angle-bending
  * Urey-Bradley
  * out-of-plane bending
  * torsional
  * pi-orbital torsion
  * torsional-torsional
  * 14-7 van der Waals
  * multipoles with mutual polarization
* pre-conditioning for the conjugate-gradient solver
  
Notable removals from 'dynamic' include the following:

* energy/gradient functions not listed above
* most debug/verbose sections
* restraints
* constraints
* polymers
* smoothing
* non-cubic box geometries
* implicit solvent
* socket (outside interfacing)
* atom grouping
* inactive atoms (all atoms are active)
* unused thermostats and barostats
  
In general most keywords that require a number value such as 
`openmp-threads`, box sizes, cutoffs values, etc. are still valid and active.  
Most keywords that specify certain features such as barostats, 
integrators, etc. are not.


# Comments and future work

* Some of the files included with this distribution may not be used.  Alex 
  have removed many unused files from the original Tinker distribution, 
  but probably not all of them.

* Some of the header modules ('use whatever') within files may be unused,
 as well.  Alex has done his best to clear out the obvious ones.
 
* The `Makefile` is a bit of a mess and could probably use some work. It 
 should be functional, though.
 
* In general comments with a `!` are mine and comments with a `c` are 
 original.  Most sections Alex removed have been completely deleted, but 
 some are commented out.

Original `README` from the Tinker distribution:

                   ------------------------------------------
                   Fortran Source Code for the TINKER Package
                   ------------------------------------------

         This subdirectory contains the Fortran 95 source code for the
         current version of the TINKER program package.

         The code is in a standard Fortran dialect that should compile
         unchanged on most machines. Script files to build the package
         on a variety of systems are in subdirectories named for the
         machine/operating system.

         Only a few source files may require editing prior to building:
         "sizes.f" which contains some master array dimensions used
         throughout the package, and "openend.f" which is a system
         dependent routine to open a file at the end.

         In addition, if your system does not support the iargc/getarg
         mechanism for command line arguments, then comment out the
         call to the subroutine "command" at the bottom of the source
         file "initial.f".

         If you are building an OpenMP-capable version of TINKER with
         a compiler other than the Intel Fortran compiler, then it is
         necessary to remove the calls to the Intel-specific extensions
         "kmp_set_stacksize" and "kmp-set-blocksize" near the top of
         the file "initial.f".

