/* pchdf.f -- translated by f2c (version 20060506).
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

/* Table of constant values */

static integer c__1 = 1;

/* DECK PCHDF */
doublereal pchdf_(integer *k, real *x, real *s, integer *ierr)
{
    /* Initialized data */

    static real zero = 0.f;

    /* System generated locals */
    integer i__1, i__2;
    real ret_val;

    /* Local variables */
    static integer i__, j;
    static real value;
    extern /* Subroutine */ int xermsg_(char *, char *, char *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  PCHDF */
/* ***SUBSIDIARY */
/* ***PURPOSE  Computes divided differences for PCHCE and PCHSP */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***TYPE      SINGLE PRECISION (PCHDF-S, DPCHDF-D) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/* ***DESCRIPTION */

/*          PCHDF:   PCHIP Finite Difference Formula */

/*     Uses a divided difference formulation to compute a K-point approx- */
/*     imation to the derivative at X(K) based on the data in X and S. */

/*     Called by  PCHCE  and  PCHSP  to compute 3- and 4-point boundary */
/*     derivative approximations. */

/* ---------------------------------------------------------------------- */

/*     On input: */
/*        K      is the order of the desired derivative approximation. */
/*               K must be at least 3 (error return if not). */
/*        X      contains the K values of the independent variable. */
/*               X need not be ordered, but the values **MUST** be */
/*               distinct.  (Not checked here.) */
/*        S      contains the associated slope values: */
/*                  S(I) = (F(I+1)-F(I))/(X(I+1)-X(I)), I=1(1)K-1. */
/*               (Note that S need only be of length K-1.) */

/*     On return: */
/*        S      will be destroyed. */
/*        IERR   will be set to -1 if K.LT.2 . */
/*        PCHDF  will be set to the desired derivative approximation if */
/*               IERR=0 or to zero if IERR=-1. */

/* ---------------------------------------------------------------------- */

/* ***SEE ALSO  PCHCE, PCHSP */
/* ***REFERENCES  Carl de Boor, A Practical Guide to Splines, Springer- */
/*                 Verlag, New York, 1978, pp. 10-16. */
/* ***ROUTINES CALLED  XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820503  DATE WRITTEN */
/*   820805  Converted to SLATEC library version. */
/*   870813  Minor cosmetic changes. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890411  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   900328  Added TYPE section.  (WRB) */
/*   910408  Updated AUTHOR and DATE WRITTEN sections in prologue.  (WRB) */
/*   920429  Revised format and order of references.  (WRB,FNF) */
/*   930503  Improved purpose.  (FNF) */
/* ***END PROLOGUE  PCHDF */

/* **End */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */

    /* Parameter adjustments */
    --s;
    --x;

    /* Function Body */

/*  CHECK FOR LEGAL VALUE OF K. */

/* ***FIRST EXECUTABLE STATEMENT  PCHDF */
    if (*k < 3) {
	goto L5001;
    }

/*  COMPUTE COEFFICIENTS OF INTERPOLATING POLYNOMIAL. */

    i__1 = *k - 1;
    for (j = 2; j <= i__1; ++j) {
	i__2 = *k - j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    s[i__] = (s[i__ + 1] - s[i__]) / (x[i__ + j] - x[i__]);
/* L9: */
	}
/* L10: */
    }

/*  EVALUATE DERIVATIVE AT X(K). */

    value = s[1];
    i__1 = *k - 1;
    for (i__ = 2; i__ <= i__1; ++i__) {
	value = s[i__] + value * (x[*k] - x[i__]);
/* L20: */
    }

/*  NORMAL RETURN. */

    *ierr = 0;
    ret_val = value;
    return ret_val;

/*  ERROR RETURN. */

L5001:
/*     K.LT.3 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "PCHDF", "K LESS THAN THREE", ierr, &c__1, (ftnlen)6, (
	    ftnlen)5, (ftnlen)17);
    ret_val = zero;
    return ret_val;
/* ------------- LAST LINE OF PCHDF FOLLOWS ------------------------------ */
} /* pchdf_ */

