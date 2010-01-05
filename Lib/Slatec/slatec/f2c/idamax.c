/* idamax.f -- translated by f2c (version 20060506).
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

/* DECK IDAMAX */
integer idamax_(integer *n, doublereal *dx, integer *incx)
{
    /* System generated locals */
    integer ret_val, i__1;
    doublereal d__1;

    /* Local variables */
    static integer i__, ix;
    static doublereal dmax__, xmag;

/* ***BEGIN PROLOGUE  IDAMAX */
/* ***PURPOSE  Find the smallest index of that component of a vector */
/*            having the maximum magnitude. */
/* ***LIBRARY   SLATEC (BLAS) */
/* ***CATEGORY  D1A2 */
/* ***TYPE      DOUBLE PRECISION (ISAMAX-S, IDAMAX-D, ICAMAX-C) */
/* ***KEYWORDS  BLAS, LINEAR ALGEBRA, MAXIMUM COMPONENT, VECTOR */
/* ***AUTHOR  Lawson, C. L., (JPL) */
/*           Hanson, R. J., (SNLA) */
/*           Kincaid, D. R., (U. of Texas) */
/*           Krogh, F. T., (JPL) */
/* ***DESCRIPTION */

/*                B L A S  Subprogram */
/*    Description of Parameters */

/*     --Input-- */
/*        N  number of elements in input vector(s) */
/*       DX  double precision vector with N elements */
/*     INCX  storage spacing between elements of DX */

/*     --Output-- */
/*   IDAMAX  smallest index (zero if N .LE. 0) */

/*     Find smallest index of maximum magnitude of double precision DX. */
/*     IDAMAX = first I, I = 1 to N, to maximize ABS(DX(IX+(I-1)*INCX)), */
/*     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX. */

/* ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T. */
/*                 Krogh, Basic linear algebra subprograms for Fortran */
/*                 usage, Algorithm No. 539, Transactions on Mathematical */
/*                 Software 5, 3 (September 1979), pp. 308-323. */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791001  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900821  Modified to correct problem with a negative increment. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  IDAMAX */
/* ***FIRST EXECUTABLE STATEMENT  IDAMAX */
    /* Parameter adjustments */
    --dx;

    /* Function Body */
    ret_val = 0;
    if (*n <= 0) {
	return ret_val;
    }
    ret_val = 1;
    if (*n == 1) {
	return ret_val;
    }

    if (*incx == 1) {
	goto L20;
    }

/*     Code for increments not equal to 1. */

    ix = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    dmax__ = (d__1 = dx[ix], abs(d__1));
    ix += *incx;
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	xmag = (d__1 = dx[ix], abs(d__1));
	if (xmag > dmax__) {
	    ret_val = i__;
	    dmax__ = xmag;
	}
	ix += *incx;
/* L10: */
    }
    return ret_val;

/*     Code for increments equal to 1. */

L20:
    dmax__ = abs(dx[1]);
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	xmag = (d__1 = dx[i__], abs(d__1));
	if (xmag > dmax__) {
	    ret_val = i__;
	    dmax__ = xmag;
	}
/* L30: */
    }
    return ret_val;
} /* idamax_ */

