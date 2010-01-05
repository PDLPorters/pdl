/* isamax.f -- translated by f2c (version 20060506).
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

/* DECK ISAMAX */
integer isamax_(integer *n, real *sx, integer *incx)
{
    /* System generated locals */
    integer ret_val, i__1;
    real r__1;

    /* Local variables */
    static integer i__, ix;
    static real xmag, smax;

/* ***BEGIN PROLOGUE  ISAMAX */
/* ***PURPOSE  Find the smallest index of that component of a vector */
/*            having the maximum magnitude. */
/* ***LIBRARY   SLATEC (BLAS) */
/* ***CATEGORY  D1A2 */
/* ***TYPE      SINGLE PRECISION (ISAMAX-S, IDAMAX-D, ICAMAX-C) */
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
/*       SX  single precision vector with N elements */
/*     INCX  storage spacing between elements of SX */

/*     --Output-- */
/*   ISAMAX  smallest index (zero if N .LE. 0) */

/*     Find smallest index of maximum magnitude of single precision SX. */
/*     ISAMAX = first I, I = 1 to N, to maximize  ABS(SX(IX+(I-1)*INCX)), */
/*     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX. */

/* ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T. */
/*                 Krogh, Basic linear algebra subprograms for Fortran */
/*                 usage, Algorithm No. 539, Transactions on Mathematical */
/*                 Software 5, 3 (September 1979), pp. 308-323. */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791001  DATE WRITTEN */
/*   861211  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900821  Modified to correct problem with a negative increment. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/*   920618  Slight restructuring of code.  (RWC, WRB) */
/* ***END PROLOGUE  ISAMAX */
/* ***FIRST EXECUTABLE STATEMENT  ISAMAX */
    /* Parameter adjustments */
    --sx;

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

/*     Code for increment not equal to 1. */

    ix = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    smax = (r__1 = sx[ix], dabs(r__1));
    ix += *incx;
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	xmag = (r__1 = sx[ix], dabs(r__1));
	if (xmag > smax) {
	    ret_val = i__;
	    smax = xmag;
	}
	ix += *incx;
/* L10: */
    }
    return ret_val;

/*     Code for increments equal to 1. */

L20:
    smax = dabs(sx[1]);
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	xmag = (r__1 = sx[i__], dabs(r__1));
	if (xmag > smax) {
	    ret_val = i__;
	    smax = xmag;
	}
/* L30: */
    }
    return ret_val;
} /* isamax_ */

