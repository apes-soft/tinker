# Reproducible Memory Process

## Key points

* Sometimes it is expedient to replicate computation across processes rather than parallelise and resynchronise data using MPI, i.e. the MPI communication can introduce a higher overhead than just replicating the computation.
* There is no interactive input with the user. Parallel jobs are most often put through a batch system which will disallow direct interaction with the user running the application. A job may be left hanging waiting for user input when there is no possibility of it.

## Code flow

This gives a rough overview as to what is happening in terms of the code:

* `dynamic.f`:
  * Starts off the MPI using `MPI_Init_thread` with `MPI_THREAD_FUNNELED` which informs the compiler that any calls to MPI will be outside OpenMP parallel regions or, as the standard puts it: the process may be multi-threaded, but only the main thread will make MPI calls (all MPI calls are funneled to the main thread).
  * The process `rank`, a unique number within the given communicator from 0 to the number of processes-1 and total number of processes, `nprocs` is established. This information is stored in the [mpiparams` module](source/mpiparams.f).
  * `initial.f`
    * Only rank 0 calls the `promo` subroutine otherwise you get multiple outputs.
    * `getxyz.f`:
      * `getkey.f`: only process 0 reads the key file and broadcasts the length of the key file and the contents to the other processes. The call to `getkey` used to be in the `basefile` subroutine but there was no need so it was promoted up one level.
      * Only process 0 opens the `xyz` file.
        * `readxyz.f`: process 0 reads the number of atoms, the file title, atom data, etc and then broadcasts these to the other processes.

## Suggested changes

Some suggested changes of Tinker:

* Processing of Tinker words in a single location as opposed to distributed throughout the code. This would allow the syntax of key files to be checked, e.g. typos could be picked up as an unknown keyword. Very difficult to do this at the moment as the processing of the key files is delegated to the subroutines themselves. This would also allow a single pass of the file as opposed to having to do multiple passes which is the current situation.