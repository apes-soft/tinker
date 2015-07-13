# Reproducible Memory Process

## Key points

* Sometimes it is expedient to replicate computation across processes rather to parallelise and resynchronise using MPI, i.e. the MPI can introduce a higher overhead than just replicating the computation.
* There is no interactive input with the user. Parallel jobs are often put through a batch system

## Code flow

This gives a rough overview as to what is happening in terms of the code:

* `dynamic.f`:
  * Starts off using `MPI_Init_thread` with `MPI_THREAD_FUNNELED` so that the MPI calls must be outside parallel regions.
  * The `rank` and number of processes, `nprocs` is established. This information is stored in the `mpiparams` module.
  * `initial.f`
    * Only rank 0 calls the `promo` subroutine otherwise you get multiple outputs.
    * `getxyz.f`:
      * `getkey.f`: only process 0 reads the key file and broadcasts the length of the key file and the contents to the other processes. The call to `getkey` used to be in the `basefile` subroutine but there was no need so it was promoted up one level.
      * Only process 0 opens the `xyz` file.
        * `readxyz.f`: process 0 reads the number of atoms, the file title, atom data, etc and then broadcasts these to the other processes.
   




## Suggested changes

Some suggested changes of Tinker:

* Processing of Tinker words in a single location as opposed to distributed throughout the code. This would allow the syntax of key files to be checked, e.g. typos could be picked up as an unknown keyword. Very difficult to do this at the moment as the processing of the key files is delegated to the subroutines themselves. This would also allow a single pass of the file as opposed to having to do multiple passes which is the current situation.