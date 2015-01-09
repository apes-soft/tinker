# Tinker Lite - mpiapp branch

This is the `mpiapp` branch, a development branch, of `tinkerLite`. It
is building a spatial decomposition of `tinkerLite`.

# Table of contents

* [Parallelisation strategy](#parallelisation-strategy)
* [Restriction and changes](#restrctions-and-changes)
* [Possible issues](#possible-issues)
* [References](#references)

# Parallelisation strategy

Use an **Orthogonal Recursive Bisection** (ORB) to distribute atoms across the processes involved.

# Restrictions and changes

* Requires that the number of processes involved is a power of 2. Currently there is a code check for this in `dynamic.f`. Execution will terminate if this condition is not met.
* Only process 0 displays the tinker promotional banner.
* There is no interactive input of parameters - generally these codes will run in a batch system and there will be no access to standard input. A program waiting for input could result in the whole application stalling and large amounts of CPU time being wasted. If a required parameter is missing this will result in the application terminating.
* Only process 0 reads the `.key` file and broadcasts the information to the other processes.
* Process 0 reads the `.xyz` file and distributes the atom information to the other processes in blocks.

# Possible issues

* Need to deal with periodic boundary conditions
* Need a mechanism to deal with molecules that span the whole system
  * Could enumerate molecules in the `.xyz` file by preprocessing
<!-- ![image](imgs/test.svg) -->

# References

* [<a id="ORB"">Salmon</a>] Salmon, John K. (1991) Parallel hierarchical N-body methods. Dissertation (Ph.D.), California Institute of Technology. [http://resolver.caltech.edu/CaltechTHESIS:04112011-113813016](ttp://resolver.caltech.edu/CaltechTHESIS:04112011-113813016)

