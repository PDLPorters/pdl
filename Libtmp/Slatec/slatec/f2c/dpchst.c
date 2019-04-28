/* dpchst.f -- translated by f2c (version 20060506).
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

/* DECK DPCHST */
doublereal dpchst_(doublereal *arg1, doublereal *arg2)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal one = 1.;

    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

/* ***BEGIN PROLOGUE  DPCHST */
/* ***SUBSIDIARY */
/* ***PURPOSE  DPCHIP Sign-Testing Routine */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***TYPE      DOUBLE PRECISION (PCHST-S, DPCHST-D) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/* ***DESCRIPTION */

/*         DPCHST:  DPCHIP Sign-Testing Routine. */


/*     Returns: */
/*        -1. if ARG1 and ARG2 are of opposite sign. */
/*         0. if either argument is zero. */
/*        +1. if ARG1 and ARG2 are of the same sign. */

/*     The object is to do this without multiplying ARG1*ARG2, to avoid */
/*     possible over/underflow problems. */

/*  Fortran intrinsics used:  SIGN. */

/* ***SEE ALSO  DPCHCE, DPCHCI, DPCHCS, DPCHIM */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   811103  DATE WRITTEN */
/*   820805  Converted to SLATEC library version. */
/*   870813  Minor cosmetic changes. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900328  Added TYPE section.  (WRB) */
/*   910408  Updated AUTHOR and DATE WRITTEN sections in prologue.  (WRB) */
/*   930503  Improved purpose.  (FNF) */
/* ***END PROLOGUE  DPCHST */

/* **End */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*  PERFORM THE TEST. */

/* ***FIRST EXECUTABLE STATEMENT  DPCHST */
    ret_val = d_sign(&one, arg1) * d_sign(&one, arg2);
    if (*arg1 == zero || *arg2 == zero) {
	ret_val = zero;
    }

    return ret_val;
/* ------------- LAST LINE OF DPCHST FOLLOWS ----------------------------- */
} /* dpchst_ */

