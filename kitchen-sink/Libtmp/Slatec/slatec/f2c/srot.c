/* srot.f -- translated by f2c (version 20060506).
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

/* DECK SROT */
/* Subroutine */ int srot_(integer *n, real *sx, integer *incx, real *sy, 
	integer *incy, real *sc, real *ss)
{
    /* Initialized data */

    static real zero = 0.f;
    static real one = 1.f;

    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__;
    static real w, z__;
    static integer kx, ky, nsteps;

/* ***BEGIN PROLOGUE  SROT */
/* ***PURPOSE  Apply a plane Givens rotation. */
/* ***LIBRARY   SLATEC (BLAS) */
/* ***CATEGORY  D1A8 */
/* ***TYPE      SINGLE PRECISION (SROT-S, DROT-D, CSROT-C) */
/* ***KEYWORDS  BLAS, GIVENS ROTATION, GIVENS TRANSFORMATION, */
/*             LINEAR ALGEBRA, PLANE ROTATION, VECTOR */
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
/*       SY  single precision vector with N elements */
/*     INCY  storage spacing between elements of SY */
/*       SC  element of rotation matrix */
/*       SS  element of rotation matrix */

/*     --Output-- */
/*       SX  rotated vector SX (unchanged if N .LE. 0) */
/*       SY  rotated vector SY (unchanged if N .LE. 0) */

/*     Multiply the 2 x 2 matrix  ( SC SS) times the 2 x N matrix (SX**T) */
/*                                (-SS SC)                        (SY**T) */
/*     where **T indicates transpose.  The elements of SX are in */
/*     SX(LX+I*INCX), I = 0 to N-1, where LX = 1 if INCX .GE. 0, else */
/*     LX = 1+(1-N)*INCX, and similarly for SY using LY and INCY. */

/* ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T. */
/*                 Krogh, Basic linear algebra subprograms for Fortran */
/*                 usage, Algorithm No. 539, Transactions on Mathematical */
/*                 Software 5, 3 (September 1979), pp. 308-323. */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791001  DATE WRITTEN */
/*   861211  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   920310  Corrected definition of LX in DESCRIPTION.  (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  SROT */
    /* Parameter adjustments */
    --sy;
    --sx;

    /* Function Body */
/* ***FIRST EXECUTABLE STATEMENT  SROT */
    if (*n <= 0 || *ss == zero && *sc == one) {
	goto L40;
    }
    if (! (*incx == *incy && *incx > 0)) {
	goto L20;
    }

/*          Code for equal and positive increments. */

    nsteps = *incx * *n;
    i__1 = nsteps;
    i__2 = *incx;
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
	w = sx[i__];
	z__ = sy[i__];
	sx[i__] = *sc * w + *ss * z__;
	sy[i__] = -(*ss) * w + *sc * z__;
/* L10: */
    }
    goto L40;

/*     Code for unequal or nonpositive increments. */

L20:
    kx = 1;
    ky = 1;

    if (*incx < 0) {
	kx = 1 - (*n - 1) * *incx;
    }
    if (*incy < 0) {
	ky = 1 - (*n - 1) * *incy;
    }

    i__2 = *n;
    for (i__ = 1; i__ <= i__2; ++i__) {
	w = sx[kx];
	z__ = sy[ky];
	sx[kx] = *sc * w + *ss * z__;
	sy[ky] = -(*ss) * w + *sc * z__;
	kx += *incx;
	ky += *incy;
/* L30: */
    }
L40:

    return 0;
} /* srot_ */

