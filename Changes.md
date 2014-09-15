# Cumulative changes made to the code

Using this to develop `tinkerLite` - a cut down version of tinker,
basically `dynamic` with restricted options of what can be run. The main
aim of this work is to:

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

# Outstanding issues

List issues that need looking at or resolving.

* `version.f` is asking for user input: "filename for the coordiante output".
   Need to disable this.

## Outstanding compilation warnings

### Picked up by the pgf90 compiler

* Predefined intrinsic maxval loses intrinsic property (sizes.f: 58) -
  variable masks the Fortran 90 `maxval` intrinsic.
* Predefined intrinsic digits loses intrinsic property (inform.f: 28) -
  variable masks the Fortran 90 `digits` intrinsic.

### gfortran

Switched on as many compilation switches to find out if there are
potential issues. Essentially though `-Wall` probably suffices to generate
these. The following warnings are outstanding which may be worth examining
at some point.

```
gfortran -Wall -Warray-temporaries -Wcharacter-truncation -Wextra -Wsurprising   -ffast-math -fopenmp -O3 cspline.f -o cspline.o 
cspline.f: In function ‘cytsyp’:
cspline.f:212:0: warning: ‘temp2’ may be used uninitialized in this function [-Wmaybe-uninitialized]
```

```
gfortran -Wall -Warray-temporaries -Wcharacter-truncation -Wextra -Wsurprising   -ffast-math -fopenmp -O3 eangle1.f -o eangle1.o 
eangle1.f: In function ‘eangle1’:
eangle1.f:164:0: warning: ‘e’ may be used uninitialized in this function [-Wmaybe-uninitialized]
eangle1.f:151:0: warning: ‘deddt’ may be used uninitialized in this function [-Wmaybe-uninitialized]
```

```
gfortran -Wall -Warray-temporaries -Wcharacter-truncation -Wextra -Wsurprising   -ffast-math -fopenmp -O3 ehal1.f -o ehal1.o 
ehal1.f: In function ‘ehal1c_._omp_fn.0’:
ehal1.f:318:0: warning: ‘vscale.offset’ may be used uninitialized in this function [-Wmaybe-uninitialized]
ehal1.f:97:0: note: ‘vscale.offset’ was declared here
ehal1.f:189:0: warning: ‘iv14.offset’ may be used uninitialized in this function [-Wmaybe-uninitialized]
ehal1.f:75:0: note: ‘iv14.offset’ was declared here
```

```
gfortran -Wall -Warray-temporaries -Wcharacter-truncation -Wextra -Wsurprising   -ffast-math -fopenmp -O3 empole1.f -o empole1.o 
empole1.f: In function ‘ereal1d_._omp_fn.0’:
empole1.f:1111:0: warning: ‘mscale.offset’ may be used uninitialized in this function [-Wmaybe-uninitialized]
empole1.f:236:0: note: ‘mscale.offset’ was declared here
empole1.f:1112:0: warning: ‘pscale.offset’ may be used uninitialized in this function [-Wmaybe-uninitialized]
empole1.f:237:0: note: ‘pscale.offset’ was declared here
empole1.f:1127:0: warning: ‘dscale.offset’ may be used uninitialized in this function [-Wmaybe-uninitialized]
empole1.f:238:0: note: ‘dscale.offset’ was declared here
empole1.f:1128:0: warning: ‘uscale.offset’ may be used uninitialized in this function [-Wmaybe-uninitialized]
empole1.f:239:0: note: ‘uscale.offset’ was declared here
```

```
gfortran -Wall -Warray-temporaries -Wcharacter-truncation -Wextra -Wsurprising   -ffast-math -fopenmp -O3 erf.f -o erf.o 
erf.f:19.6:

      function erf (x)                                                  
      1
Warning: 'erf' declared at (1) is also the name of an intrinsic.  It can only be called via an explicit interface or if declared EXTERNAL.
erf.f:46.6:

      function erfc (x)                                                 
      1
Warning: 'erfc' declared at (1) is also the name of an intrinsic.  It can only be called via an explicit interface or if declared EXTERNAL.
```

```
gfortran -Wall -Warray-temporaries -Wcharacter-truncation -Wextra -Wsurprising   -ffast-math -fopenmp -O3 etortor1.f -o etortor1.o 
etortor1.f: In function ‘chkttor’:
etortor1.f:393:0: warning: ‘k’ may be used uninitialized in this function [-Wmaybe-uninitialized]
```

```
gfortran -Wall -Warray-temporaries -Wcharacter-truncation -Wextra -Wsurprising   -ffast-math -fopenmp -O3 getnumb.f -o getnumb.o 
getnumb.f: In function ‘getnumb’:
getnumb.f:31:0: warning: ‘digit’ may be used uninitialized in this function [-Wmaybe-uninitialized]
```

```
gfortran -Wall -Warray-temporaries -Wcharacter-truncation -Wextra -Wsurprising   -ffast-math -fopenmp -O3 induce.f -o induce.o 
induce.f:1271.16:

            m = mindex(i)                                               
                1
Warning: Possible change of value in conversion from REAL(8) to INTEGER(4) at (1)
induce.f:1375.16:

            m = mindex(i)                                               
                1
Warning: Possible change of value in conversion from REAL(8) to INTEGER(4) at (1)
induce.f: In function ‘uscale0b_._omp_fn.3’:
induce.f:1421:0: warning: ‘dscale.offset’ may be used uninitialized in this function [-Wmaybe-uninitialized]
induce.f:1233:0: note: ‘dscale.offset’ was declared here
induce.f: In function ‘udirect2b_._omp_fn.0’:
induce.f:894:0: warning: ‘pscale.offset’ may be used uninitialized in this function [-Wmaybe-uninitialized]
induce.f:642:0: note: ‘pscale.offset’ was declared here
induce.f:910:0: warning: ‘dscale.offset’ may be used uninitialized in this function [-Wmaybe-uninitialized]
induce.f:643:0: note: ‘dscale.offset’ was declared here
induce.f:909:0: warning: ‘uscale.offset’ may be used uninitialized in this function [-Wmaybe-uninitialized]
induce.f:644:0: note: ‘uscale.offset’ was declared here
```

```
kpolar.f: In function ‘kpolar’:
kpolar.f:83:0: warning: ‘npg’ may be used uninitialized in this function [-Wmaybe-uninitialized]
```

```
gfortran -Wall -Warray-temporaries -Wcharacter-truncation -Wextra -Wsurprising   -ffast-math -fopenmp -O3 kstrbnd.f -o kstrbnd.o 
kstrbnd.f: In function ‘kstrbnd’:
kstrbnd.f:147:0: warning: ‘nbc’ may be used uninitialized in this function [-Wmaybe-uninitialized]
kstrbnd.f:146:0: warning: ‘nba’ may be used uninitialized in this function [-Wmaybe-uninitialized]
```

```
gfortran -Wall -Warray-temporaries -Wcharacter-truncation -Wextra -Wsurprising   -ffast-math -fopenmp -O3 pressure.f -o pressure.o 
pressure.f:77.39:

      subroutine pscale (dt,pres,stress)                                
                                       1
Warning: Unused dummy argument 'stress' at (1)
pressure.f:20.34:

      subroutine pressure (dt,epot,ekin,temp,pres,stress)               
                                  1
Warning: Unused dummy argument 'epot' at (1)
pressure.f:20.44:

      subroutine pressure (dt,epot,ekin,temp,pres,stress)               
                                            1
Warning: Unused dummy argument 'temp' at (1)
```

```
gfortran -Wall -Warray-temporaries -Wcharacter-truncation -Wextra -Wsurprising   -ffast-math -fopenmp -O3 readprm.f -o readprm.o 
readprm.f:965.5:

  520          format (/,' READPRM  --  Too many Biopolymer Types;',    
     1
Warning: Label 520 at (1) defined but not used
```

```
gfortran -Wall -Warray-temporaries -Wcharacter-truncation -Wextra -Wsurprising   -ffast-math -fopenmp -O3 torque.f -o torque.o 
torque.f: In function ‘torque’:
torque.f:47:0: warning: ‘wssin’ may be used uninitialized in this function [-Wmaybe-uninitialized]
torque.f:43:0: warning: ‘wscos’ may be used uninitialized in this function [-Wmaybe-uninitialized]
torque.f:358:0: warning: ‘vssin’ may be used uninitialized in this function [-Wmaybe-uninitialized]
torque.f:43:0: warning: ‘vscos’ may be used uninitialized in this function [-Wmaybe-uninitialized]
torque.f:46:0: warning: ‘ursin’ may be used uninitialized in this function [-Wmaybe-uninitialized]
torque.f:48:0: warning: ‘ut2sin’ may be used uninitialized in this function [-Wmaybe-uninitialized]
torque.f:48:0: warning: ‘ut1sin’ may be used uninitialized in this function [-Wmaybe-uninitialized]
torque.f: In function ‘torque3’:
torque.f:726:0: warning: ‘wssin’ may be used uninitialized in this function [-Wmaybe-uninitialized]
torque.f:722:0: warning: ‘wscos’ may be used uninitialized in this function [-Wmaybe-uninitialized]
torque.f:726:0: warning: ‘vssin’ may be used uninitialized in this function [-Wmaybe-uninitialized]
torque.f:722:0: warning: ‘vscos’ may be used uninitialized in this function [-Wmaybe-uninitialized]
torque.f:725:0: warning: ‘ursin’ may be used uninitialized in this function [-Wmaybe-uninitialized]
torque.f:727:0: warning: ‘ut2sin’ may be used uninitialized in this function [-Wmaybe-uninitialized]
torque.f:727:0: warning: ‘ut1sin’ may be used uninitialized in this function [-Wmaybe-uninitialized]
```
### ifort

Can play the same game with the intel compiler with the `-warn all` flag but it does not 
return as many warnings:

```
ifort: command line remark #10382: option '-xHOST' setting '-xAVX'
iounit.f(17): warning #5194: Source line truncated.
      integer, parameter:: input =  5 ! Fortran I/O unit for main input (default=5)
------------------------------------------------------------------------^
```

```
ifort -warn all -c  -O3 -no-ipo -no-prec-div -recursive -openmp -xHost pressure.f -o pressure.o 
ifort: command line remark #10382: option '-xHOST' setting '-xAVX'
pressure.f(20): remark #7712: This variable has not been used.   [EPOT]
      subroutine pressure (dt,epot,ekin,temp,pres,stress)
------------------------------^
pressure.f(20): remark #7712: This variable has not been used.   [TEMP]
      subroutine pressure (dt,epot,ekin,temp,pres,stress)
----------------------------------------^
pressure.f(77): remark #7712: This variable has not been used.   [STRESS]
      subroutine pscale (dt,pres,stress)
---------------------------------^
```

