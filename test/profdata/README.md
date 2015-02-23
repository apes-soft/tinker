# Index

These are a number of different profiles obtained using the [callgrind](http://valgrind.org/info/tools.html#callgrind) tool of [valgrind](http://valgrind.org/). Analysis of this profile data is available in this internal [project wiki](https://www.wiki.ed.ac.uk/display/PES/Replicated+memory) page.

1. `callgrind.out.5428`: used indy, run for 10 time steps with gfortran of JAC.
2. `callgrind.out.64353`: used indy, run for 10 times steps of JAC with gfortran and with cache access emulation.
3. `callgrind.out.30379`: used gx745ma (Linux desktop system), run for 2 time steps of JAC compiled with gfortran and using cache access emulation.
4. `callgrind.out.22430`: used phi with an intel compiler, run for 2 times steps of JAC with ifort and cache access emulation. Wanted to see if the comparing of strings was expensive.
5. `callgrind.out.30488`: used indy to run 10 time steps of the 3BDC from Richard.

