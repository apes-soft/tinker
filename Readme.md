# Tinker Lite - mpiapp branch

This is the `mpiapp` branch, a development branch, of `tinkerLite`. It
is putting MPI calls into files derived from `dev-mario` and `derivedType`.

# Parallelisation Strategy

Use an **Orthogonal Recursive Bisection** (ORB) to distribute atoms across the processes involved.

# Restrictions

* Requires that the number of processes involved is a power of 2. Currently there is a check in `dynamic.f` for this. Execution will terminate if this condition is not met.
* Only process 0 reads the `.key` file and broadcasts the information to the other processes.
* Process 0 reads the `.xyz` file and distributes the atom information to the other processes in blocks.

# Issues

* Need to deal with periodic boundary conditions

<!-- ![image](imgs/test.svg) -->

# References