# Table of Contents

* [Aims](#aims)
* [Parallelisation strategies](#strategies)
* [Test parallel decomposition code](#test-decomp-code)
* [I/O Model](#io-model)
    * [Reading in the `.key` file](#key-file)
    * [Reading in the `.xyz` file](#xyz-file)
* [Decomposition](#decomposition)
* [Assumptions](#assumptions)
* [Things tried and abandoned](#abandoned)

<a name="aims"></a>
# Aims

The main aim of this piece of work is not to come up with a full parallelisation of TINKER but rather to come up with a parallel framework for the `dynamic` executable of TINKER.

<a name="strategies"></a>
# Parallelisation strategies

The following parallelisation strategies:

* **Replicated data** - this would largely preserve the existing structure of the TINKER code base and allow us to parallelise the I/O. We would probably achieve limited scalability but might provide a stepping stone to going to a full replicated data approach.
* **Distributed memory** - this will probably require radical refactoring of the existing code base. We would need to think of a number of factors from the start.

The `testdecomp` directory is exploring a *distributed memory* implementation based on a *spatial decomposition*. This is trying to explore what is possible, log the difficulties encountered and is not necessarily meant to present any final code.

<a name="test-decomp-code"></a>
# Test parallel decomposition code

Area to try out various things on a small scale. This is trying to substantiate a [design document](https://docs.google.com/document/d/17IP3aXcQ-RITnxtoriwrHS0-yiyqTkpu-mX0MWDERKY/edit#) of sorts (restricted access). Thus far we have:

* Use of Fortran 90 - I think improves the encapsulation. 
    * Pros
        * Can have `use modulename, ONLY: variable list` so that only required variables are visible.
        * No longer need common blocks.
        * Comments can be in-lined with code which I think makes code more readable.
    * Cons
        * Modules use an `f90` extension which separates the include file distinction.
        * Modules have to be compiled in the right order if there is a dependency between them, i.e. one of them uses another.
     * Possibles
        * Use of derived Fortran data types - using these instead of multiple separate arrays will help swapping data between processes.
* Trying out an Orthogonal Recursive Bisection (ORB) like decomposition.

<a name="io-model"></a>
# I/O model

<a name="key-file"></a>
## Reading in the `.key` file
The current I/O model is channelling everything through process 0. Process 0 reads the entire contents of the `.key` file and the broadcasts this to all the other processes. Each process ends up with the entire contents of the `.key` file.

<a name="xyz-file"></a>
## Reading in the `.xyz` file

Possible methodologies:

* Each process could open the `.xyz` file on its own and get the corresponding atoms it needs but this is not a good solution for large number of processes where too many processes may be trying to open the file at the same time.
*  An alternatively might be for process 0 to read the whole of the `.xyz` file and then redistribute but this means that the largest system that could ever be modelled would be one that fits into the memory of one process. 
  
The solution that has been adopted is:

* Process 0 reads the `.xyz` file in chunks and then send these out to the other processes. This means that process 0 only ever needs to have a fraction of the total number of atoms that are to be modelled by the system. It does however introduce a sequential bottleneck in the reading of data but if this is shown to be a problem one could use a subset of processes that read the file in parallel and redistribute the atoms.

Thus for reading in the atoms each process gets:

    dn=Natoms/Nprocs

where `dn` is the number of atoms located in a specific process,`Natons` is the total number of atoms as specified at the top of the `.xyz` file and `Nprocs` is the number of processes involved in the computation. If there is a remainder, then processes with a rank less than the remainder in the division will get an additional atom. This distributes the atom excess to the lower rank processes. 

Process 0 reads the corresponding `dn` atoms and redistributes the atom blocks to the other processes. These are distributed in the order in which they occur in the file. 

If the optional box parameters are present in the file these are read in by process 0 and broadcast to all the other processes.

<a name="decomposition"></a>
# Decomposition

Using an *Orthogonal Recursive Bisection* as proposed in John Salmon's dissertation on [Parallel hierarchical N-body methods](http://thesis.library.caltech.edu/6291). Once the atoms have been read in they have to be redistributed so each process will end up with a set of spatially localised atoms it processes with a set of replicated atoms from the neighbouring spatial regions required to compute the forces on the locally held atoms.

* We assume that the processes can mimic a hypercube interconnect topology which restricts to the parallel algorithm running on a power of 2 number of processes.
* We split the domain in two along the longest Cartesian direction. Half the processes are assigned to each domain.
* Each process calculates a corresponding neighbour process using a Gray code.
* If atoms lie on the other side of the domain then processes swap atoms (including halo atoms).
* Each subdomain is split into two with half the processes assigned to each half. The processes swap atoms. This continues until a process has been assigned its own spatial domain.

<a href="assumptions"></a>
# Assumptions

These are assumptions that are being made that may turn out to be false.

* If `dn` is the number of atoms per process then for the time being we are assuming that that the number of atoms at any one time will be of order `2*(dn+1)`. The halo distance may impact on this.
* Going to use a *median* along a Cartesian direction to determine the *splitting point* along the *splitting direction* (use the longest dimension to split at each point). In principle could use a weighed median where, if some atoms are more *expensive* to calculate contribute more.

<a name="abandoned"></a>
# Things tried and abandoned

* Use of MPI-IO does not work well with text files - you need to specify how many of each particular data types you are going to read. So for the tinker `.xyz` files you can specify that you require 120 `MPI_CHARACTER` types. If you do not have exactly those numbers of characters in a line it will move to the next until it has got the 120 characters. This did not work well with `.xyz` files. If at some point I/O is done in binary it will be worth moving to that. You also want this for much larger data sets than what is currently being used.

<a name="ToDo"></a>
# To do items

A list of items that need to be sorted out:

* Licensing: work developed under this project is supposed to be done under the BSD 2-clause licence. Need to reconcile how this fits in with Jay's TINKER licence.
* The `sortAtoms.f90` source implements a quick sort from this [web site](http://balfortran.org/qsort_optimized.f90) although the code pertains to have free and programming articles there is no clear licensing of the code. May need to rewrite this code or clarify the licensing.
* In the `initial.f90` subroutine the `precise` function defined in  the `precis.f90` file could probably be replaced by Fortran 90 intrinsics. 
* Absorb the`getkey` subroutine in the `getkey.f90` subroutine into the keys module in the `keys.f90` file. It would seem to me more logical to put these together.
