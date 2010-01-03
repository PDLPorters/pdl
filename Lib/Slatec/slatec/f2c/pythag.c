/* pythag.f -- translated by f2c (version 20060506).
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

/* DECK PYTHAG */
doublereal pythag_(real *a, real *b)
{
    /* System generated locals */
    real ret_val, r__1, r__2;

    /* Local variables */
    static real p, q, r__, s, t;

/* ***BEGIN PROLOGUE  PYTHAG */
/* ***SUBSIDIARY */
/* ***PURPOSE  Compute the complex square root of a complex number without */
/*            destructive overflow or underflow. */
/* ***LIBRARY   SLATEC */
/* ***TYPE      SINGLE PRECISION (PYTHAG-S) */
/* ***AUTHOR  (UNKNOWN) */
/* ***DESCRIPTION */

/*     Finds sqrt(A**2+B**2) without overflow or destructive underflow */

/* ***SEE ALSO  EISDOC */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   811101  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900402  Added TYPE section.  (WRB) */
/* ***END PROLOGUE  PYTHAG */

/* ***FIRST EXECUTABLE STATEMENT  PYTHAG */
/* Computing MAX */
    r__1 = dabs(*a), r__2 = dabs(*b);
    p = dmax(r__1,r__2);
/* Computing MIN */
    r__1 = dabs(*a), r__2 = dabs(*b);
    q = dmin(r__1,r__2);
    if (q == 0.f) {
	goto L20;
    }
L10:
/* Computing 2nd power */
    r__1 = q / p;
    r__ = r__1 * r__1;
    t = r__ + 4.f;
    if (t == 4.f) {
	goto L20;
    }
    s = r__ / t;
    p += p * 2.f * s;
    q *= s;
    goto L10;
L20:
    ret_val = p;
    return ret_val;
} /* pythag_ */

