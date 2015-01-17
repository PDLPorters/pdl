/* pchkt.f -- translated by f2c (version 20060506).
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

/* DECK PCHKT */
/* Subroutine */ int pchkt_(integer *n, real *x, integer *knotyp, real *t)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer j, k;
    static real hbeg, hend;
    static integer ndim;

/* ***BEGIN PROLOGUE  PCHKT */
/* ***SUBSIDIARY */
/* ***PURPOSE  Compute B-spline knot sequence for PCHBS. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***CATEGORY  E3 */
/* ***TYPE      SINGLE PRECISION (PCHKT-S, DPCHKT-D) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/* ***DESCRIPTION */

/*     Set a knot sequence for the B-spline representation of a PCH */
/*     function with breakpoints X.  All knots will be at least double. */
/*     Endknots are set as: */
/*        (1) quadruple knots at endpoints if KNOTYP=0; */
/*        (2) extrapolate the length of end interval if KNOTYP=1; */
/*        (3) periodic if KNOTYP=2. */

/*  Input arguments:  N, X, KNOTYP. */
/*  Output arguments:  T. */

/*  Restrictions/assumptions: */
/*     1. N.GE.2 .  (not checked) */
/*     2. X(i).LT.X(i+1), i=1,...,N .  (not checked) */
/*     3. 0.LE.KNOTYP.LE.2 .  (Acts like KNOTYP=0 for any other value.) */

/* ***SEE ALSO  PCHBS */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   870701  DATE WRITTEN */
/*   900405  Converted Fortran to upper case. */
/*   900410  Converted prologue to SLATEC 4.0 format. */
/*   900410  Minor cosmetic changes. */
/*   930514  Changed NKNOTS from an output to an input variable.  (FNF) */
/*   930604  Removed unused variable NKNOTS from argument list.  (FNF) */
/* ***END PROLOGUE  PCHKT */

/* *Internal Notes: */

/*  Since this is subsidiary to PCHBS, which validates its input before */
/*  calling, it is unnecessary for such validation to be done here. */

/* **End */

/*  Declare arguments. */


/*  Declare local variables. */

/* ***FIRST EXECUTABLE STATEMENT  PCHKT */

/*  Initialize. */

    /* Parameter adjustments */
    --t;
    --x;

    /* Function Body */
    ndim = *n << 1;

/*  Set interior knots. */

    j = 1;
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	j += 2;
	t[j] = x[k];
	t[j + 1] = t[j];
/* L20: */
    }
/*     Assertion:  At this point T(3),...,T(NDIM+2) have been set and */
/*                 J=NDIM+1. */

/*  Set end knots according to KNOTYP. */

    hbeg = x[2] - x[1];
    hend = x[*n] - x[*n - 1];
    if (*knotyp == 1) {
/*          Extrapolate. */
	t[2] = x[1] - hbeg;
	t[ndim + 3] = x[*n] + hend;
    } else if (*knotyp == 2) {
/*          Periodic. */
	t[2] = x[1] - hend;
	t[ndim + 3] = x[*n] + hbeg;
    } else {
/*          Quadruple end knots. */
	t[2] = x[1];
	t[ndim + 3] = x[*n];
    }
    t[1] = t[2];
    t[ndim + 4] = t[ndim + 3];

/*  Terminate. */

    return 0;
/* ------------- LAST LINE OF PCHKT FOLLOWS ------------------------------ */
} /* pchkt_ */

