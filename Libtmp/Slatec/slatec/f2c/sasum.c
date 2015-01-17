/* sasum.f -- translated by f2c (version 20060506).
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

/* DECK SASUM */
doublereal sasum_(integer *n, real *sx, integer *incx)
{
    /* System generated locals */
    integer i__1;
    real ret_val, r__1, r__2, r__3, r__4, r__5, r__6;

    /* Local variables */
    static integer i__, m, ix, mp1;

/* ***BEGIN PROLOGUE  SASUM */
/* ***PURPOSE  Compute the sum of the magnitudes of the elements of a */
/*            vector. */
/* ***LIBRARY   SLATEC (BLAS) */
/* ***CATEGORY  D1A3A */
/* ***TYPE      SINGLE PRECISION (SASUM-S, DASUM-D, SCASUM-C) */
/* ***KEYWORDS  BLAS, LINEAR ALGEBRA, SUM OF MAGNITUDES OF A VECTOR */
/* ***AUTHOR  Lawson, C. L., (JPL) */
/*           Hanson, R. J., (SNLA) */
/*           Kincaid, D. R., (U. of Texas) */
/*           Krogh, F. T., (JPL) */
/* ***DESCRIPTION */

/*                B L A S  Subprogram */
/*    Description of Parameters */

/*     --Input-- */
/*        N  number of elements in input vector(S) */
/*       SX  single precision vector with N elements */
/*     INCX  storage spacing between elements of SX */

/*     --Output-- */
/*    SASUM  single precision result (zero if N .LE. 0) */

/*     Returns sum of magnitudes of single precision SX. */
/*     SASUM = sum from 0 to N-1 of ABS(SX(IX+I*INCX)), */
/*     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX. */

/* ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T. */
/*                 Krogh, Basic linear algebra subprograms for Fortran */
/*                 usage, Algorithm No. 539, Transactions on Mathematical */
/*                 Software 5, 3 (September 1979), pp. 308-323. */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791001  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900821  Modified to correct problem with a negative increment. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  SASUM */
/* ***FIRST EXECUTABLE STATEMENT  SASUM */
    /* Parameter adjustments */
    --sx;

    /* Function Body */
    ret_val = 0.f;
    if (*n <= 0) {
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
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ret_val += (r__1 = sx[ix], dabs(r__1));
	ix += *incx;
/* L10: */
    }
    return ret_val;

/*     Code for increment equal to 1. */

/*     Clean-up loop so remaining vector length is a multiple of 6. */

L20:
    m = *n % 6;
    if (m == 0) {
	goto L40;
    }
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ret_val += (r__1 = sx[i__], dabs(r__1));
/* L30: */
    }
    if (*n < 6) {
	return ret_val;
    }
L40:
    mp1 = m + 1;
    i__1 = *n;
    for (i__ = mp1; i__ <= i__1; i__ += 6) {
	ret_val = ret_val + (r__1 = sx[i__], dabs(r__1)) + (r__2 = sx[i__ + 1]
		, dabs(r__2)) + (r__3 = sx[i__ + 2], dabs(r__3)) + (r__4 = sx[
		i__ + 3], dabs(r__4)) + (r__5 = sx[i__ + 4], dabs(r__5)) + (
		r__6 = sx[i__ + 5], dabs(r__6));
/* L50: */
    }
    return ret_val;
} /* sasum_ */

