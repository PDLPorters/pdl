/* srotg.f -- translated by f2c (version 20060506).
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

/* DECK SROTG */
/* Subroutine */ int srotg_(real *sa, real *sb, real *sc, real *ss)
{
    /* System generated locals */
    real r__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static real r__, u, v;

/* ***BEGIN PROLOGUE  SROTG */
/* ***PURPOSE  Construct a plane Givens rotation. */
/* ***LIBRARY   SLATEC (BLAS) */
/* ***CATEGORY  D1B10 */
/* ***TYPE      SINGLE PRECISION (SROTG-S, DROTG-D, CROTG-C) */
/* ***KEYWORDS  BLAS, GIVENS ROTATION, GIVENS TRANSFORMATION, */
/*             LINEAR ALGEBRA, VECTOR */
/* ***AUTHOR  Lawson, C. L., (JPL) */
/*           Hanson, R. J., (SNLA) */
/*           Kincaid, D. R., (U. of Texas) */
/*           Krogh, F. T., (JPL) */
/* ***DESCRIPTION */

/*                B L A S  Subprogram */
/*    Description of Parameters */

/*     --Input-- */
/*       SA  single precision scalar */
/*       SB  single precision scalar */

/*     --Output-- */
/*       SA  single precision result R */
/*       SB  single precision result Z */
/*       SC  single precision result */
/*       SS  single precision result */

/*     Construct the Givens transformation */

/*         ( SC  SS ) */
/*     G = (        ) ,    SC**2 + SS**2 = 1 , */
/*         (-SS  SC ) */

/*     which zeros the second entry of the 2-vector  (SA,SB)**T. */

/*     The quantity R = (+/-)SQRT(SA**2 + SB**2) overwrites SA in */
/*     storage.  The value of SB is overwritten by a value Z which */
/*     allows SC and SS to be recovered by the following algorithm: */

/*           If Z=1  set  SC=0.0  and  SS=1.0 */
/*           If ABS(Z) .LT. 1  set  SC=SQRT(1-Z**2)  and  SS=Z */
/*           If ABS(Z) .GT. 1  set  SC=1/Z  and  SS=SQRT(1-SC**2) */

/*     Normally, the subprogram SROT(N,SX,INCX,SY,INCY,SC,SS) will */
/*     next be called to apply the transformation to a 2 by N matrix. */

/* ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T. */
/*                 Krogh, Basic linear algebra subprograms for Fortran */
/*                 usage, Algorithm No. 539, Transactions on Mathematical */
/*                 Software 5, 3 (September 1979), pp. 308-323. */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791001  DATE WRITTEN */
/*   861211  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  SROTG */
/* ***FIRST EXECUTABLE STATEMENT  SROTG */
    if (dabs(*sa) <= dabs(*sb)) {
	goto L10;
    }

/* *** HERE ABS(SA) .GT. ABS(SB) *** */

    u = *sa + *sa;
    v = *sb / u;

/*     NOTE THAT U AND R HAVE THE SIGN OF SA */

/* Computing 2nd power */
    r__1 = v;
    r__ = sqrt(r__1 * r__1 + .25f) * u;

/*     NOTE THAT SC IS POSITIVE */

    *sc = *sa / r__;
    *ss = v * (*sc + *sc);
    *sb = *ss;
    *sa = r__;
    return 0;

/* *** HERE ABS(SA) .LE. ABS(SB) *** */

L10:
    if (*sb == 0.f) {
	goto L20;
    }
    u = *sb + *sb;
    v = *sa / u;

/*     NOTE THAT U AND R HAVE THE SIGN OF SB */
/*     (R IS IMMEDIATELY STORED IN SA) */

/* Computing 2nd power */
    r__1 = v;
    *sa = sqrt(r__1 * r__1 + .25f) * u;

/*     NOTE THAT SS IS POSITIVE */

    *ss = *sb / *sa;
    *sc = v * (*ss + *ss);
    if (*sc == 0.f) {
	goto L15;
    }
    *sb = 1.f / *sc;
    return 0;
L15:
    *sb = 1.f;
    return 0;

/* *** HERE SA = SB = 0.0 *** */

L20:
    *sc = 1.f;
    *ss = 0.f;
    return 0;

} /* srotg_ */

