TINKER
======

TINKER Software Tools for Molecular Design. This is development
version of the public GitHub [repository](https://github.com/jayponder/tinker) 
maintained by Jay Ponder which in turn is a mirror of his SVN repository.

This branch contains small modifications made by the applications group to add
functionality or print out additional data. It does not contain the implicit
solvent optimisations made by Justs Zarins, which are available in a separate
branch.

1) analyze with the 'p' option now prints out polarisation groups up to 40 atoms
   on one line (previously 20). This was required as the tinker_to_amber program
   reads the analyze output to generate an Amber parameter file with correct
   polarisation groups.

2) Intra-mutant vdW interactions can now be scaled with lambda. This is the
   equivalent to switching off vdW interactions entirely rather than decoupling
   the mutant from the environment (eg. deprotonating a covalently bound acidic
   residue compared to decoupling a ligand non-covalently bound to a protein).
   The choice is of scaling is determined by the definition of mutated atoms in
   the keyfile - the 'mutate' keyword scales intra-mutant interactions while
   'ligand' does not (original behaviour).

3) A small modification to temper.f to correct the Nose-Hoover thermostat - see
   Ref. 2 in temper.f

4) A small modification to random.f to print out the assigned random seed at
   the beginning of every output file (if one is used) to allow debugging. 
