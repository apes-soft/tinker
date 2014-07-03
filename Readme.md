# Tinker Lite - collapse_xyz

This branch will collapse the distinct `x(N)`, `y(N)` and `z(N)`
into a single array `pos(3)(N)` to see whether that has an impact on
performance. This will also simplify a distributed memory parallelisation
by reducing the number of explicit message passes required to exchange
coordinate information.

