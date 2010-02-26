/* kimptor.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    doublereal mass[25000];
    integer tag[25000], class__[25000], atomic[25000], valence[25000];
    char name__[75000], story[600000];
} atmtyp_;

#define atmtyp_1 atmtyp_

struct {
    doublereal x[25000], y[25000], z__[25000];
    integer n, type__[25000];
} atoms_;

#define atoms_1 atoms_

struct {
    integer n12[25000], i12[200000]	/* was [8][25000] */, n13[25000], i13[
	    600000]	/* was [24][25000] */, n14[25000], i14[1800000]	/* 
	    was [72][25000] */, n15[25000], i15[5400000]	/* was [216][
	    25000] */;
} couple_;

#define couple_1 couple_

struct {
    doublereal itors1[400000]	/* was [4][100000] */, itors2[400000]	/* 
	    was [4][100000] */, itors3[400000]	/* was [4][100000] */;
    integer nitors, iitors[400000]	/* was [4][100000] */;
} imptor_;

#define imptor_1 imptor_

struct {
    integer digits, iprint, iwrite, isend;
    logical verbose, debug, holdup, abort;
} inform_;

#define inform_1 inform_

struct {
    integer iout, input;
} iounit_;

#define iounit_1 iounit_

struct {
    integer nkey;
    char keyline[1200000];
} keys_;

#define keys_1 keys_

struct {
    doublereal ti1[1000]	/* was [2][500] */, ti2[1000]	/* was [2][
	    500] */, ti3[1000]	/* was [2][500] */;
    char kti[8000];
} kitors_;

#define kitors_1 kitors_

struct {
    logical use_bond__, use_angle__, use_strbnd__, use_urey__, use_angang__, 
	    use_opbend__, use_opdist__, use_improp__, use_imptor__, 
	    use_tors__, use_pitors__, use_strtor__, use_tortor__, use_vdw__, 
	    use_charge__, use_chgdpl__, use_dipole__, use_mpole__, 
	    use_polar__, use_rxnfld__, use_solv__, use_metal__, use_geom__, 
	    use_extra__, use_born__, use_orbit__;
} potent_;

#define potent_1 potent_

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;
static integer c__5 = 5;
static integer c__4 = 4;



/*     ################################################### */
/*     ##  COPYRIGHT (C)  1991  by  Jay William Ponder  ## */
/*     ##              All Rights Reserved              ## */
/*     ################################################### */

/*     ########################################################### */
/*     ##                                                       ## */
/*     ##  subroutine kimptor  --  improper torsion parameters  ## */
/*     ##                                                       ## */
/*     ########################################################### */


/*     "kimptor" assigns torsional parameters to each improper */
/*     torsion in the structure and processes any changed values */


/* Subroutine */ int kimptor_(void)
{
    /* Format strings */
    static char fmt_20[] = "(/,\002 Additional Improper Torsion Parameters "
	    ":\002,//,5x,\002Atom Classes\002,15x,\0021-Fold\002,12x,\0022-Fo"
	    "ld\002,12x,\0023-Fold\002,/)";
    static char fmt_30[] = "(4x,4i4,2x,3(f11.3,f7.1))";
    static char fmt_40[] = "(/,\002 KIMPTOR  --  Too many Improper Torsio"
	    "n\002,\002 Parameters\002)";

    /* System generated locals */
    address a__1[4];
    integer i__1, i__2, i__3[4];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rsli(icilist *), do_lio(
	    integer *, integer *, char *, ftnlen), e_rsli(void);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer s_wsfe(cilist *), e_wsfe(void), do_fio(integer *, char *, ftnlen);
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    extern /* Subroutine */ int torphase_(integer *, doublereal *, doublereal 
	    *);
    static integer i__, j, k, ia, ib, ic, id;
    static char pa[4], pb[4], pc[4];
    static integer ft[3];
    static char pd[4], pt[16*6];
    static doublereal st[3], vt[3];
    static char pt0[16], pt1[16], pt2[16], pt3[16];
    static integer ita, itb, itc, itd, nti;
    static char pti[16];
    static logical done;
    static integer size, next;
    static doublereal symm, angle;
    static char blank[16], zeros[4];
    static logical header;
    static char record[120];
    extern /* Subroutine */ int upcase_(char *, ftnlen);
    static char string[120];
    extern /* Subroutine */ int numeral_(integer *, char *, integer *, ftnlen)
	    ;
    static char keyword[20];
    extern /* Subroutine */ int gettext_(char *, char *, integer *, ftnlen, 
	    ftnlen);

    /* Fortran I/O blocks */
    static icilist io___17 = { 1, string, 1, 0, 120, 1 };
    static cilist io___24 = { 0, 0, 0, fmt_20, 0 };
    static cilist io___25 = { 0, 0, 0, fmt_30, 0 };
    static cilist io___26 = { 0, 0, 0, fmt_40, 0 };



#define i12_ref(a_1,a_2) couple_1.i12[(a_2)*8 + a_1 - 9]
#define pt_ref(a_0,a_1) &pt[(a_1)*16 + a_0 - 16]
#define ti1_ref(a_1,a_2) kitors_1.ti1[(a_2)*2 + a_1 - 3]
#define ti2_ref(a_1,a_2) kitors_1.ti2[(a_2)*2 + a_1 - 3]
#define ti3_ref(a_1,a_2) kitors_1.ti3[(a_2)*2 + a_1 - 3]
#define kti_ref(a_0,a_1) &kitors_1.kti[(a_1)*16 + a_0 - 16]
#define itors1_ref(a_1,a_2) imptor_1.itors1[(a_2)*4 + a_1 - 5]
#define itors2_ref(a_1,a_2) imptor_1.itors2[(a_2)*4 + a_1 - 5]
#define itors3_ref(a_1,a_2) imptor_1.itors3[(a_2)*4 + a_1 - 5]
#define iitors_ref(a_1,a_2) imptor_1.iitors[(a_2)*4 + a_1 - 5]
#define keyline_ref(a_0,a_1) &keys_1.keyline[(a_1)*120 + a_0 - 120]



/*     ################################################### */
/*     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ## */
/*     ##              All Rights Reserved              ## */
/*     ################################################### */

/*     ############################################################# */
/*     ##                                                         ## */
/*     ##  sizes.i  --  parameter values to set array dimensions  ## */
/*     ##                                                         ## */
/*     ############################################################# */


/*     "sizes.i" sets values for critical array dimensions used */
/*     throughout the software; these parameters will fix the size */
/*     of the largest systems that can be handled; values too large */
/*     for the computer's memory and/or swap space to accomodate */
/*     will result in poor performance or outright failure */

/*     parameter:      maximum allowed number of: */

/*     maxatm          atoms in the molecular system */
/*     maxval          atoms directly bonded to an atom */
/*     maxgrp          user-defined groups of atoms */
/*     maxref          stored reference molecular systems */
/*     maxtyp          force field atom type definitions */
/*     maxclass        force field atom class definitions */
/*     maxprm          lines in the parameter file */
/*     maxkey          lines in the keyword file */
/*     maxrot          bonds for torsional rotation */
/*     maxvar          optimization variables (vector storage) */
/*     maxopt          optimization variables (matrix storage) */
/*     maxhess         off-diagonal Hessian elements */
/*     maxlight        sites for method of lights neighbors */
/*     maxvlst         atom neighbors in van der Waals pair list */
/*     maxelst         atom neighbors in electrostatics pair list */
/*     maxfft          grid points in each FFT dimension */
/*     maxfix          geometric constraints and restraints */
/*     maxvib          vibrational frequencies */
/*     maxgeo          distance geometry points */
/*     maxcell         unit cells in replicated crystal */
/*     maxring         3-, 4-, or 5-membered rings */
/*     maxbio          biopolymer atom definitions */
/*     maxres          residues in the macromolecule */
/*     maxamino        amino acid residue types */
/*     maxnuc          nucleic acid residue types */
/*     maxbnd          covalent bonds in molecular system */
/*     maxang          bond angles in molecular system */
/*     maxtors         torsional angles in molecular system */
/*     maxbitor        bitorsions in molecular system */
/*     maxpi           atoms in conjugated pisystem */
/*     maxpib          covalent bonds involving pisystem */
/*     maxpit          torsional angles involving pisystem */




/*     ################################################### */
/*     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ## */
/*     ##              All Rights Reserved              ## */
/*     ################################################### */

/*     ############################################################# */
/*     ##                                                         ## */
/*     ##  atmtyp.i  --  atomic properties for each current atom  ## */
/*     ##                                                         ## */
/*     ############################################################# */


/*     mass      atomic weight for each atom in the system */
/*     tag       integer atom labels from input coordinates file */
/*     class     atom class number for each atom in the system */
/*     atomic    atomic number for each atom in the system */
/*     valence   valence number for each atom in the system */
/*     name      atom name for each atom in the system */
/*     story     descriptive type for each atom in system */




/*     ################################################### */
/*     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ## */
/*     ##              All Rights Reserved              ## */
/*     ################################################### */

/*     ############################################################### */
/*     ##                                                           ## */
/*     ##  atoms.i  --  number, position and type of current atoms  ## */
/*     ##                                                           ## */
/*     ############################################################### */


/*     x       current x-coordinate for each atom in the system */
/*     y       current y-coordinate for each atom in the system */
/*     z       current z-coordinate for each atom in the system */
/*     n       total number of atoms in the current system */
/*     type    atom type number for each atom in the system */




/*     ################################################### */
/*     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ## */
/*     ##              All Rights Reserved              ## */
/*     ################################################### */

/*     ############################################################ */
/*     ##                                                        ## */
/*     ##  couple.i  --  near-neighbor atom connectivity lists   ## */
/*     ##                                                        ## */
/*     ############################################################ */


/*     maxn13   maximum number of atoms 1-3 connected to an atom */
/*     maxn14   maximum number of atoms 1-4 connected to an atom */
/*     maxn15   maximum number of atoms 1-5 connected to an atom */

/*     n12      number of atoms directly bonded to each atom */
/*     i12      atom numbers of atoms 1-2 connected to each atom */
/*     n13      number of atoms in a 1-3 relation to each atom */
/*     i13      atom numbers of atoms 1-3 connected to each atom */
/*     n14      number of atoms in a 1-4 relation to each atom */
/*     i14      atom numbers of atoms 1-4 connected to each atom */
/*     n15      number of atoms in a 1-5 relation to each atom */
/*     i15      atom numbers of atoms 1-5 connected to each atom */




/*     ################################################### */
/*     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ## */
/*     ##              All Rights Reserved              ## */
/*     ################################################### */

/*     ################################################################ */
/*     ##                                                            ## */
/*     ##  imptor.i  --  improper torsions in the current structure  ## */
/*     ##                                                            ## */
/*     ################################################################ */


/*     itors1   1-fold amplitude and phase for each improper torsion */
/*     itors2   2-fold amplitude and phase for each improper torsion */
/*     itors3   3-fold amplitude and phase for each improper torsion */
/*     nitors   total number of improper torsional angles in the system */
/*     iitors   numbers of the atoms in each improper torsional angle */




/*     ################################################### */
/*     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ## */
/*     ##              All Rights Reserved              ## */
/*     ################################################### */

/*     ############################################################# */
/*     ##                                                         ## */
/*     ##  inform.i  --  control values for I/O and program flow  ## */
/*     ##                                                         ## */
/*     ############################################################# */


/*     digits    decimal places output for energy and coordinates */
/*     iprint    steps between status printing (0=no printing) */
/*     iwrite    steps between coordinate dumps (0=no dumps) */
/*     isend     steps between socket communication (0=no sockets) */
/*     verbose   logical flag to turn on extra information */
/*     debug     logical flag to turn on full debug printing */
/*     holdup    logical flag to wait for carriage return on exit */
/*     abort     logical flag to stop execution at next chance */




/*     ################################################### */
/*     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ## */
/*     ##              All Rights Reserved              ## */
/*     ################################################### */

/*     ############################################################# */
/*     ##                                                         ## */
/*     ##  iounit.i  --  Fortran input/output (I/O) unit numbers  ## */
/*     ##                                                         ## */
/*     ############################################################# */


/*     iout    Fortran I/O unit for main output (default=6) */
/*     input   Fortran I/O unit for main input (default=5) */




/*     ################################################### */
/*     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ## */
/*     ##              All Rights Reserved              ## */
/*     ################################################### */

/*     ############################################################## */
/*     ##                                                          ## */
/*     ##  keys.i  --  contents of current keyword parameter file  ## */
/*     ##                                                          ## */
/*     ############################################################## */


/*     nkey      number of nonblank lines in the keyword file */
/*     keyline   contents of each individual keyword file line */




/*     ################################################### */
/*     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ## */
/*     ##              All Rights Reserved              ## */
/*     ################################################### */

/*     ################################################################# */
/*     ##                                                             ## */
/*     ##  kitors.i  --  forcefield parameters for improper torsions  ## */
/*     ##                                                             ## */
/*     ################################################################# */


/*     maxnti   maximum number of improper torsion parameter entries */

/*     ti1      torsional parameters for improper 1-fold rotation */
/*     ti2      torsional parameters for improper 2-fold rotation */
/*     ti3      torsional parameters for improper 3-fold rotation */
/*     kti      string of atom classes for improper torsional parameters */




/*     ################################################### */
/*     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ## */
/*     ##              All Rights Reserved              ## */
/*     ################################################### */

/*     ########################################################## */
/*     ##                                                      ## */
/*     ##  math.i  --  mathematical and geometrical constants  ## */
/*     ##                                                      ## */
/*     ########################################################## */


/*     radian   conversion factor from radians to degrees */
/*     pi       numerical value of the geometric constant */
/*     sqrtpi   numerical value of the square root of Pi */
/*     logten   numerical value of the natural log of ten */
/*     sqrttwo  numerical value of the square root of two */
/*     twosix   numerical value of the sixth root of two */




/*     ################################################### */
/*     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ## */
/*     ##              All Rights Reserved              ## */
/*     ################################################### */

/*     ############################################################## */
/*     ##                                                          ## */
/*     ##  potent.i  --  usage of each potential energy component  ## */
/*     ##                                                          ## */
/*     ############################################################## */


/*     use_bond    logical flag governing use of bond stretch potential */
/*     use_angle   logical flag governing use of angle bend potential */
/*     use_strbnd  logical flag governing use of stretch-bend potential */
/*     use_urey    logical flag governing use of Urey-Bradley potential */
/*     use_angang  logical flag governing use of angle-angle cross term */
/*     use_opbend  logical flag governing use of out-of-plane bend term */
/*     use_opdist  logical flag governing use of out-of-plane distance */
/*     use_improp  logical flag governing use of improper dihedral term */
/*     use_imptor  logical flag governing use of improper torsion term */
/*     use_tors    logical flag governing use of torsional potential */
/*     use_pitors  logical flag governing use of pi-orbital torsion term */
/*     use_strtor  logical flag governing use of stretch-torsion term */
/*     use_tortor  logical flag governing use of torsion-torsion term */
/*     use_vdw     logical flag governing use of vdw der Waals potential */
/*     use_charge  logical flag governing use of charge-charge potential */
/*     use_chgdpl  logical flag governing use of charge-dipole potential */
/*     use_dipole  logical flag governing use of dipole-dipole potential */
/*     use_mpole   logical flag governing use of multipole potential */
/*     use_polar   logical flag governing use of polarization term */
/*     use_rxnfld  logical flag governing use of reaction field term */
/*     use_solv    logical flag governing use of continuum solvation */
/*     use_metal   logical flag governing use of ligand field term */
/*     use_geom    logical flag governing use of geometric restraints */
/*     use_extra   logical flag governing use of extra potential term */
/*     use_born    logical flag governing use of Born radii values */
/*     use_orbit   logical flag governing use of pisystem computation */




/*     process keywords containing improper torsion parameters */

    s_copy(blank, "                ", (ftnlen)16, (ftnlen)16);
    s_copy(zeros, "0000", (ftnlen)4, (ftnlen)4);
    header = TRUE_;
    i__1 = keys_1.nkey;
    for (i__ = 1; i__ <= i__1; ++i__) {
	next = 1;
	s_copy(record, keyline_ref(0, i__), (ftnlen)120, (ftnlen)120);
	gettext_(record, keyword, &next, (ftnlen)120, (ftnlen)20);
	upcase_(keyword, (ftnlen)20);
	if (s_cmp(keyword, "IMPTORS ", (ftnlen)8, (ftnlen)8) == 0) {
	    ia = 0;
	    ib = 0;
	    ic = 0;
	    id = 0;
	    for (j = 1; j <= 3; ++j) {
		vt[j - 1] = 0.;
		st[j - 1] = 0.;
		ft[j - 1] = 0;
	    }
	    s_copy(string, record + (next - 1), (ftnlen)120, 120 - (next - 1))
		    ;
	    i__2 = s_rsli(&io___17);
	    if (i__2 != 0) {
		goto L10;
	    }
	    i__2 = do_lio(&c__3, &c__1, (char *)&ia, (ftnlen)sizeof(integer));
	    if (i__2 != 0) {
		goto L10;
	    }
	    i__2 = do_lio(&c__3, &c__1, (char *)&ib, (ftnlen)sizeof(integer));
	    if (i__2 != 0) {
		goto L10;
	    }
	    i__2 = do_lio(&c__3, &c__1, (char *)&ic, (ftnlen)sizeof(integer));
	    if (i__2 != 0) {
		goto L10;
	    }
	    i__2 = do_lio(&c__3, &c__1, (char *)&id, (ftnlen)sizeof(integer));
	    if (i__2 != 0) {
		goto L10;
	    }
	    for (j = 1; j <= 3; ++j) {
		i__2 = do_lio(&c__5, &c__1, (char *)&vt[j - 1], (ftnlen)
			sizeof(doublereal));
		if (i__2 != 0) {
		    goto L10;
		}
		i__2 = do_lio(&c__5, &c__1, (char *)&st[j - 1], (ftnlen)
			sizeof(doublereal));
		if (i__2 != 0) {
		    goto L10;
		}
		i__2 = do_lio(&c__3, &c__1, (char *)&ft[j - 1], (ftnlen)
			sizeof(integer));
		if (i__2 != 0) {
		    goto L10;
		}
	    }
	    i__2 = e_rsli();
	    if (i__2 != 0) {
		goto L10;
	    }
L10:
	    size = 4;
	    numeral_(&ia, pa, &size, (ftnlen)4);
	    numeral_(&ib, pb, &size, (ftnlen)4);
	    numeral_(&ic, pc, &size, (ftnlen)4);
	    numeral_(&id, pd, &size, (ftnlen)4);
/* Writing concatenation */
	    i__3[0] = 4, a__1[0] = pa;
	    i__3[1] = 4, a__1[1] = pb;
	    i__3[2] = 4, a__1[2] = pc;
	    i__3[3] = 4, a__1[3] = pd;
	    s_cat(pti, a__1, i__3, &c__4, (ftnlen)16);
	    torphase_(ft, vt, st);
	    if (header) {
		header = FALSE_;
		io___24.ciunit = iounit_1.iout;
		s_wsfe(&io___24);
		e_wsfe();
	    }
	    io___25.ciunit = iounit_1.iout;
	    s_wsfe(&io___25);
	    do_fio(&c__1, (char *)&ia, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&ib, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&ic, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&id, (ftnlen)sizeof(integer));
	    for (j = 1; j <= 3; ++j) {
		do_fio(&c__1, (char *)&vt[j - 1], (ftnlen)sizeof(doublereal));
		do_fio(&c__1, (char *)&st[j - 1], (ftnlen)sizeof(doublereal));
	    }
	    e_wsfe();
	    for (j = 1; j <= 500; ++j) {
		if (s_cmp(kti_ref(0, j), blank, (ftnlen)16, (ftnlen)16) == 0 
			|| s_cmp(kti_ref(0, j), pti, (ftnlen)16, (ftnlen)16) 
			== 0) {
		    s_copy(kti_ref(0, j), pti, (ftnlen)16, (ftnlen)16);
		    ti1_ref(1, j) = vt[0];
		    ti1_ref(2, j) = st[0];
		    ti2_ref(1, j) = vt[1];
		    ti2_ref(2, j) = st[1];
		    ti3_ref(1, j) = vt[2];
		    ti3_ref(2, j) = st[2];
		    goto L50;
		}
	    }
	    io___26.ciunit = iounit_1.iout;
	    s_wsfe(&io___26);
	    e_wsfe();
	    inform_1.abort = TRUE_;
L50:
	    ;
	}
    }

/*     determine the total number of forcefield parameters */

    nti = 500;
    for (i__ = 500; i__ >= 1; --i__) {
	if (s_cmp(kti_ref(0, i__), blank, (ftnlen)16, (ftnlen)16) == 0) {
	    nti = i__ - 1;
	}
    }

/*     assign improper torsional parameters for each improper torsion; */
/*     multiple symmetrical parameters are given partial weights */

    imptor_1.nitors = 0;
    if (nti != 0) {
	i__1 = atoms_1.n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (couple_1.n12[i__ - 1] == 3) {
		ia = i12_ref(1, i__);
		ib = i12_ref(2, i__);
		ic = i__;
		id = i12_ref(3, i__);
		ita = atmtyp_1.class__[ia - 1];
		itb = atmtyp_1.class__[ib - 1];
		itc = atmtyp_1.class__[ic - 1];
		itd = atmtyp_1.class__[id - 1];
		size = 4;
		numeral_(&ita, pa, &size, (ftnlen)4);
		numeral_(&itb, pb, &size, (ftnlen)4);
		numeral_(&itc, pc, &size, (ftnlen)4);
		numeral_(&itd, pd, &size, (ftnlen)4);
/* Writing concatenation */
		i__3[0] = 4, a__1[0] = pa;
		i__3[1] = 4, a__1[1] = pb;
		i__3[2] = 4, a__1[2] = pc;
		i__3[3] = 4, a__1[3] = pd;
		s_cat(pt_ref(0, 1), a__1, i__3, &c__4, (ftnlen)16);
/* Writing concatenation */
		i__3[0] = 4, a__1[0] = pb;
		i__3[1] = 4, a__1[1] = pa;
		i__3[2] = 4, a__1[2] = pc;
		i__3[3] = 4, a__1[3] = pd;
		s_cat(pt_ref(0, 2), a__1, i__3, &c__4, (ftnlen)16);
/* Writing concatenation */
		i__3[0] = 4, a__1[0] = pa;
		i__3[1] = 4, a__1[1] = pd;
		i__3[2] = 4, a__1[2] = pc;
		i__3[3] = 4, a__1[3] = pb;
		s_cat(pt_ref(0, 3), a__1, i__3, &c__4, (ftnlen)16);
/* Writing concatenation */
		i__3[0] = 4, a__1[0] = pd;
		i__3[1] = 4, a__1[1] = pa;
		i__3[2] = 4, a__1[2] = pc;
		i__3[3] = 4, a__1[3] = pb;
		s_cat(pt_ref(0, 4), a__1, i__3, &c__4, (ftnlen)16);
/* Writing concatenation */
		i__3[0] = 4, a__1[0] = pb;
		i__3[1] = 4, a__1[1] = pd;
		i__3[2] = 4, a__1[2] = pc;
		i__3[3] = 4, a__1[3] = pa;
		s_cat(pt_ref(0, 5), a__1, i__3, &c__4, (ftnlen)16);
/* Writing concatenation */
		i__3[0] = 4, a__1[0] = pd;
		i__3[1] = 4, a__1[1] = pb;
		i__3[2] = 4, a__1[2] = pc;
		i__3[3] = 4, a__1[3] = pa;
		s_cat(pt_ref(0, 6), a__1, i__3, &c__4, (ftnlen)16);
/* Writing concatenation */
		i__3[0] = 4, a__1[0] = zeros;
		i__3[1] = 4, a__1[1] = zeros;
		i__3[2] = 4, a__1[2] = pc;
		i__3[3] = 4, a__1[3] = pd;
		s_cat(pt3, a__1, i__3, &c__4, (ftnlen)16);
/* Writing concatenation */
		i__3[0] = 4, a__1[0] = zeros;
		i__3[1] = 4, a__1[1] = zeros;
		i__3[2] = 4, a__1[2] = pc;
		i__3[3] = 4, a__1[3] = pb;
		s_cat(pt2, a__1, i__3, &c__4, (ftnlen)16);
/* Writing concatenation */
		i__3[0] = 4, a__1[0] = zeros;
		i__3[1] = 4, a__1[1] = zeros;
		i__3[2] = 4, a__1[2] = pc;
		i__3[3] = 4, a__1[3] = pa;
		s_cat(pt1, a__1, i__3, &c__4, (ftnlen)16);
/* Writing concatenation */
		i__3[0] = 4, a__1[0] = zeros;
		i__3[1] = 4, a__1[1] = zeros;
		i__3[2] = 4, a__1[2] = pc;
		i__3[3] = 4, a__1[3] = zeros;
		s_cat(pt0, a__1, i__3, &c__4, (ftnlen)16);
		symm = 1.;
		if (s_cmp(pa, pb, (ftnlen)4, (ftnlen)4) == 0 || s_cmp(pa, pd, 
			(ftnlen)4, (ftnlen)4) == 0 || s_cmp(pb, pd, (ftnlen)4,
			 (ftnlen)4) == 0) {
		    symm = 2.;
		}
		if (s_cmp(pa, pb, (ftnlen)4, (ftnlen)4) == 0 && s_cmp(pa, pd, 
			(ftnlen)4, (ftnlen)4) == 0 && s_cmp(pb, pd, (ftnlen)4,
			 (ftnlen)4) == 0) {
		    symm = 6.;
		}
		done = FALSE_;
		i__2 = nti;
		for (j = 1; j <= i__2; ++j) {
		    if (s_cmp(kti_ref(8, j), pc, (ftnlen)4, (ftnlen)4) == 0) {
			for (k = 1; k <= 6; ++k) {
			    if (s_cmp(kti_ref(0, j), pt_ref(0, k), (ftnlen)16,
				     (ftnlen)16) == 0) {
				++imptor_1.nitors;
				iitors_ref(3, imptor_1.nitors) = ic;
				if (k == 1) {
				    iitors_ref(1, imptor_1.nitors) = ia;
				    iitors_ref(2, imptor_1.nitors) = ib;
				    iitors_ref(4, imptor_1.nitors) = id;
				} else if (k == 2) {
				    iitors_ref(1, imptor_1.nitors) = ib;
				    iitors_ref(2, imptor_1.nitors) = ia;
				    iitors_ref(4, imptor_1.nitors) = id;
				} else if (k == 3) {
				    iitors_ref(1, imptor_1.nitors) = ia;
				    iitors_ref(2, imptor_1.nitors) = id;
				    iitors_ref(4, imptor_1.nitors) = ib;
				} else if (k == 4) {
				    iitors_ref(1, imptor_1.nitors) = id;
				    iitors_ref(2, imptor_1.nitors) = ia;
				    iitors_ref(4, imptor_1.nitors) = ib;
				} else if (k == 5) {
				    iitors_ref(1, imptor_1.nitors) = ib;
				    iitors_ref(2, imptor_1.nitors) = id;
				    iitors_ref(4, imptor_1.nitors) = ia;
				} else if (k == 6) {
				    iitors_ref(1, imptor_1.nitors) = id;
				    iitors_ref(2, imptor_1.nitors) = ib;
				    iitors_ref(4, imptor_1.nitors) = ia;
				}
				itors1_ref(1, imptor_1.nitors) = ti1_ref(1, j)
					 / symm;
				itors1_ref(2, imptor_1.nitors) = ti1_ref(2, j)
					;
				itors2_ref(1, imptor_1.nitors) = ti2_ref(1, j)
					 / symm;
				itors2_ref(2, imptor_1.nitors) = ti2_ref(2, j)
					;
				itors3_ref(1, imptor_1.nitors) = ti3_ref(1, j)
					 / symm;
				itors3_ref(2, imptor_1.nitors) = ti3_ref(2, j)
					;
				done = TRUE_;
			    }
			}
		    }
		}
		if (! done) {
		    i__2 = nti;
		    for (j = 1; j <= i__2; ++j) {
			if (s_cmp(kti_ref(0, j), pt1, (ftnlen)16, (ftnlen)16) 
				== 0) {
			    symm = 3.;
			    for (k = 1; k <= 3; ++k) {
				++imptor_1.nitors;
				iitors_ref(3, imptor_1.nitors) = ic;
				if (k == 1) {
				    iitors_ref(1, imptor_1.nitors) = ia;
				    iitors_ref(2, imptor_1.nitors) = ib;
				    iitors_ref(4, imptor_1.nitors) = id;
				} else if (k == 2) {
				    iitors_ref(1, imptor_1.nitors) = ib;
				    iitors_ref(2, imptor_1.nitors) = id;
				    iitors_ref(4, imptor_1.nitors) = ia;
				} else if (k == 3) {
				    iitors_ref(1, imptor_1.nitors) = id;
				    iitors_ref(2, imptor_1.nitors) = ia;
				    iitors_ref(4, imptor_1.nitors) = ib;
				}
				itors1_ref(1, imptor_1.nitors) = ti1_ref(1, j)
					 / symm;
				itors1_ref(2, imptor_1.nitors) = ti1_ref(2, j)
					;
				itors2_ref(1, imptor_1.nitors) = ti2_ref(1, j)
					 / symm;
				itors2_ref(2, imptor_1.nitors) = ti2_ref(2, j)
					;
				itors3_ref(1, imptor_1.nitors) = ti3_ref(1, j)
					 / symm;
				itors3_ref(2, imptor_1.nitors) = ti3_ref(2, j)
					;
			    }
			    done = TRUE_;
			} else if (s_cmp(kti_ref(0, j), pt2, (ftnlen)16, (
				ftnlen)16) == 0) {
			    symm = 3.;
			    for (k = 1; k <= 3; ++k) {
				++imptor_1.nitors;
				iitors_ref(3, imptor_1.nitors) = ic;
				if (k == 1) {
				    iitors_ref(1, imptor_1.nitors) = ia;
				    iitors_ref(2, imptor_1.nitors) = ib;
				    iitors_ref(4, imptor_1.nitors) = id;
				} else if (k == 2) {
				    iitors_ref(1, imptor_1.nitors) = ib;
				    iitors_ref(2, imptor_1.nitors) = id;
				    iitors_ref(4, imptor_1.nitors) = ia;
				} else if (k == 3) {
				    iitors_ref(1, imptor_1.nitors) = id;
				    iitors_ref(2, imptor_1.nitors) = ia;
				    iitors_ref(4, imptor_1.nitors) = ib;
				}
				itors1_ref(1, imptor_1.nitors) = ti1_ref(1, j)
					 / symm;
				itors1_ref(2, imptor_1.nitors) = ti1_ref(2, j)
					;
				itors2_ref(1, imptor_1.nitors) = ti2_ref(1, j)
					 / symm;
				itors2_ref(2, imptor_1.nitors) = ti2_ref(2, j)
					;
				itors3_ref(1, imptor_1.nitors) = ti3_ref(1, j)
					 / symm;
				itors3_ref(2, imptor_1.nitors) = ti3_ref(2, j)
					;
			    }
			    done = TRUE_;
			} else if (s_cmp(kti_ref(0, j), pt3, (ftnlen)16, (
				ftnlen)16) == 0) {
			    symm = 3.;
			    for (k = 1; k <= 3; ++k) {
				++imptor_1.nitors;
				iitors_ref(3, imptor_1.nitors) = ic;
				if (k == 1) {
				    iitors_ref(1, imptor_1.nitors) = ia;
				    iitors_ref(2, imptor_1.nitors) = ib;
				    iitors_ref(4, imptor_1.nitors) = id;
				} else if (k == 2) {
				    iitors_ref(1, imptor_1.nitors) = ib;
				    iitors_ref(2, imptor_1.nitors) = id;
				    iitors_ref(4, imptor_1.nitors) = ia;
				} else if (k == 3) {
				    iitors_ref(1, imptor_1.nitors) = id;
				    iitors_ref(2, imptor_1.nitors) = ia;
				    iitors_ref(4, imptor_1.nitors) = ib;
				}
				itors1_ref(1, imptor_1.nitors) = ti1_ref(1, j)
					 / symm;
				itors1_ref(2, imptor_1.nitors) = ti1_ref(2, j)
					;
				itors2_ref(1, imptor_1.nitors) = ti2_ref(1, j)
					 / symm;
				itors2_ref(2, imptor_1.nitors) = ti2_ref(2, j)
					;
				itors3_ref(1, imptor_1.nitors) = ti3_ref(1, j)
					 / symm;
				itors3_ref(2, imptor_1.nitors) = ti3_ref(2, j)
					;
			    }
			    done = TRUE_;
			}
		    }
		}
		if (! done) {
		    i__2 = nti;
		    for (j = 1; j <= i__2; ++j) {
			if (s_cmp(kti_ref(0, j), pt0, (ftnlen)16, (ftnlen)16) 
				== 0) {
			    symm = 3.;
			    for (k = 1; k <= 3; ++k) {
				++imptor_1.nitors;
				iitors_ref(3, imptor_1.nitors) = ic;
				if (k == 1) {
				    iitors_ref(1, imptor_1.nitors) = ia;
				    iitors_ref(2, imptor_1.nitors) = ib;
				    iitors_ref(4, imptor_1.nitors) = id;
				} else if (k == 2) {
				    iitors_ref(1, imptor_1.nitors) = ib;
				    iitors_ref(2, imptor_1.nitors) = id;
				    iitors_ref(4, imptor_1.nitors) = ia;
				} else if (k == 3) {
				    iitors_ref(1, imptor_1.nitors) = id;
				    iitors_ref(2, imptor_1.nitors) = ia;
				    iitors_ref(4, imptor_1.nitors) = ib;
				}
				itors1_ref(1, imptor_1.nitors) = ti1_ref(1, j)
					 / symm;
				itors1_ref(2, imptor_1.nitors) = ti1_ref(2, j)
					;
				itors2_ref(1, imptor_1.nitors) = ti2_ref(1, j)
					 / symm;
				itors2_ref(2, imptor_1.nitors) = ti2_ref(2, j)
					;
				itors3_ref(1, imptor_1.nitors) = ti3_ref(1, j)
					 / symm;
				itors3_ref(2, imptor_1.nitors) = ti3_ref(2, j)
					;
			    }
			}
		    }
		}
	    }
	}
    }

/*     find the cosine and sine of the phase angle for each torsion */

    i__1 = imptor_1.nitors;
    for (i__ = 1; i__ <= i__1; ++i__) {
	angle = itors1_ref(2, i__) / 57.29577951308232088;
	itors1_ref(3, i__) = cos(angle);
	itors1_ref(4, i__) = sin(angle);
	angle = itors2_ref(2, i__) / 57.29577951308232088;
	itors2_ref(3, i__) = cos(angle);
	itors2_ref(4, i__) = sin(angle);
	angle = itors3_ref(2, i__) / 57.29577951308232088;
	itors3_ref(3, i__) = cos(angle);
	itors3_ref(4, i__) = sin(angle);
    }

/*     turn off the improper torsional potential if it is not used */

    if (imptor_1.nitors == 0) {
	potent_1.use_imptor__ = FALSE_;
    }
    return 0;
} /* kimptor_ */

#undef keyline_ref
#undef iitors_ref
#undef itors3_ref
#undef itors2_ref
#undef itors1_ref
#undef kti_ref
#undef ti3_ref
#undef ti2_ref
#undef ti1_ref
#undef pt_ref
#undef i12_ref


