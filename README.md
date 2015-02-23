# TINKER

# Table of Contents

* [Overview](#overview)
* [Parallelisation strategy](#parallelisation-strategy)
  * [Strategy overview](#strategy-overview)
  * [Stage 1](#stage-1)
    * [Execution path and profiling](#execution-path-and-profiling)
    * [Splitting the loops](#splitting-the-loops)
* [To do](#to-do)
* [Questions](#questions)
* [Changes](#changes)


# Overview

TINKER Software Tools for Molecular Design. This is development
version of the public GitHub [repository](https://github.com/jayponder/tinker) 
maintained by Jay Ponder.

This is the `repmem` branch which is exploring a replicated memory version
of:

* `dynamic`

Because of the modifications that we are making to files any of the
other tinker applications may no longer work when compiled in this
branch.

If we have time we may also try to improve the performance of:

* `bar` which reads in two trajectories and two key files (i.e. different
lambda windows for free energy calculations), and calculates the energies
of trajectory 1 with key file 2, and vice versa. The read-in is very slow
on this, up to a few hours for ~2000 structures. Richard has been thinking
for a while that ideally the energy evaluation should be done as an option
to dynamic - i.e. whenever the energy of a structure is printed out, why
not evaluate the energy of the same structure at different lambda windows
too - this would save all the costly trajectory post-processing later.

# Parallelisation strategy

## Strategy overview

Focus on routines that are used by Richard and Omar where possible.
Look at the OpenMP regions to determine where the current CPU cost is
incurred as well as profiling a couple of use cases with
vanilla-tinker to find out where the cost is.

* WF to profile:
  * JAC 
  * Water model with Verlet (as done by Omar)
  * Use case from Richard Bradshaw of an equilibrated staph nuclease
    structure ready for NPT simulation, with the sidechain of residue
    D21 mutated with ele-lambda 0.5 (half switched on) and vdw-lambda
    1.0 (fully switched on).
* From the regions that are parallelised using OpenMP:
  * VdW in `ehal1.f`
  * Electrostatics and polarization in:
    * `empole1.f`
    * `induced.f`
  
## Stage 1

This stage will allow us to evaluate whether this strategy will
work. Focus on the JAC execution path for now. General steps are:

1. **Profile** the latest version of `dynamic` for: JAC without OpenMP. Need to understand quantitavely the performance, the use case provided by Richard.
2. Revisit the **execution path** of `dynamic` for the JAC benchmark.
3. **Split the loops** currently parallelised using OpenMP with MPI. 
4. Check that the correct output is still being produced from multiple-process runs compared to that from a single process.
5. Obtain a speed-up scalability curve. Consider cases with/without OpenMP (compiled with/without). Need to establish the efficacy of a hybrid approach.
6. Evaluate whether this is a good strategy.

If this works well we could move to a stage 2 that might take the
execution path produced with use cases provided by Richard.

### Execution path and profiling

Current content comes from an early write up by Weronika. Need to narrow
this down a bit.

The execution path for `dynamic.f` running JAC:

1. Set up the system (call `initial`, `getxyz`, `mechanic`).
2. Initialize variables and check for keywords.
3. Get the number of time steps, their length, the time between trajectory 
   snapshot coordinate dumps, the type of statistical ensemble, and constant 
   energy or temperature.
4. Initialize constraints and dynamics
5. Call integrator `nstep` times

Each integrator (`verlet`, `stochastic`, `bussi`, `nose` (Nose-Hoover),
`ghmcstep`, `rigidbody`, `respa` and `beeman`) calls the `gradient`
subroutine which then calls all the other subroutines to calculate the
potential energies and first derivatives.

in `gradient.f`:

1. Energy component are zeroed out.
2. Check for periodic conditions -> call `bounds`.
3. Update neighbor list -> call `nblist` (inside depending on parameters a 
   specific list is used, e.g. `vlist`, `clist`, `mlist`, `ulist`)
4. Replicates method -> call `replica`.
5. Check if Born radii is required (used in implicit solvation models) -> call `born`.
6. Alter bond and torsion constants (it seems that it only happens for pi systems) 
   -> call `picalc`
7. Call local geometry energy and gradient routines (13 possible subroutines)
8. Van der Waals energy calculations (5 possible subroutines)
9. Electrostatic energy calculations (5 possible subroutines)
10. Miscellaneous energy (4 possible subroutines).
11. summing all of the components of energy and first derivatives.

It seems that all of the subroutines that are called in `gradient.f`
need to be (?) specified in the key input file. However, not all the
possible options specified in key files are used in gradient
(e.g. `ewald`).

### Splitting the loops

Splitting up loops evenly is not going to suffice because iterating over
atoms does not lead to equal amounts of work. As the interactions are
symmetrical an atom at the beginning of the loop will interact with its
counterparts and its interactions will be taken into account so that as
one progresses over the neighbour list most of the interactions will have
already been taken into account entailing less and less work. Omar has,
more or less, run into this problem.

Current thinking is to associate a *cost* to each atom. WF currently 
working on this. This will then help us perform a load balanced split 
of the loops across the different processes. 

By traversing the neighbour list it may be possible to associate this 
cost once at the start although it may be necessary to have more than
one cost if these prove to be different for the different type of
calculations.

# To do

* Look at taking out direct string comparisons in conditionals in
  loops that are called a lot of times - replace with boolean flags
  which should be cheaper.


# Questions

* Had a discussion with Weronika as to whether we should make 
  modifications to file in situ or change renamed copies. We
  opted for the former but if several utilities use a single
  file and only one of the apps gets converted to MPI then 
  this will not work. 
* Should we use Fortran 90 derived types? This would reduce the number
  of communication calls that would be required. 

# Changes

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

