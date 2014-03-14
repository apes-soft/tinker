TINKER
======

TINKER Software Tools for Molecular Design. This is development
version of the public GitHub [repository](https://github.com/jayponder/tinker) 
maintained by Jay Ponder which in turn is a mirror of his SVN repository.

This branch explores various OpenMP optimisations to the TINKER code.
The following optimisations have been applied to the OpenMP implementation of Tinker. 

1) Inside the `pmestuff.f`
  
     #subroutines `pchg`, `grid_mpole` and `grid_uind`: 
        - OMP PARALLEL DO schedule(dynamic) instead separate PARALLEL and DO sections
        - OMP ATOMIC UPDATE to ensure that there is no race condition when qgrid is being updated 
  
     #subroutines `fphi_mpole` and `fphi_uind`:
        - OMP PARALLEL DO schedule(dynamic,16)
  

2) Inside `induce.f`

     #subroutines `udirect2b` and `umutual2b`:
        - OMP DO schedule(dynamic,16) for both DO loops
        - OMP END DO NOWAIT after the second DO loop   
      
     #subroutine `uscale0b`: 
        - OMP DO schedule(dynamic,16) for both DO loops in first parallel region
        - OMP END DO NOWAIT after the second DO loop in the first parallel region 
        - OMP PARALLEL DO schedule(dynamic, 16) in the second parallel region

3) Inside `empole1.f`

     #subroutine ereal1d:
         - OMP PARALLEL DO shedule(dynamic,16)

4) Inside `ehall.f`

     #subroutine `ehal1c`

         - OMP DO schedule(dynamic,16)
         - OMP DO NOWAIT

5) Inside `initial.f`

     #subroutine `initial`:

         - OMP PARALLEL shared(nthread)
         - OMP MASTER to get number of thread using omp_get_num_threads() 

