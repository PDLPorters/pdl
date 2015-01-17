/* dchfdv.f -- translated by f2c (version 20060506).
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

/* DECK DCHFDV */
/* Subroutine */ int dchfdv_(doublereal *x1, doublereal *x2, doublereal *f1, 
	doublereal *f2, doublereal *d1, doublereal *d2, integer *ne, 
	doublereal *xe, doublereal *fe, doublereal *de, integer *next, 
	integer *ierr)
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static doublereal h__;
    static integer i__;
    static doublereal x, c2, c3, c2t2, c3t3, xma, xmi, del1, del2, delta;
    extern /* Subroutine */ int xermsg_(char *, char *, char *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  DCHFDV */
/* ***PURPOSE  Evaluate a cubic polynomial given in Hermite form and its */
/*            first derivative at an array of points.  While designed for */
/*            use by DPCHFD, it may be useful directly as an evaluator */
/*            for a piecewise cubic Hermite function in applications, */
/*            such as graphing, where the interval is known in advance. */
/*            If only function values are required, use DCHFEV instead. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***CATEGORY  E3, H1 */
/* ***TYPE      DOUBLE PRECISION (CHFDV-S, DCHFDV-D) */
/* ***KEYWORDS  CUBIC HERMITE DIFFERENTIATION, CUBIC HERMITE EVALUATION, */
/*             CUBIC POLYNOMIAL EVALUATION, PCHIP */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */
/* ***DESCRIPTION */

/*        DCHFDV:  Cubic Hermite Function and Derivative Evaluator */

/*     Evaluates the cubic polynomial determined by function values */
/*     F1,F2 and derivatives D1,D2 on interval (X1,X2), together with */
/*     its first derivative, at the points  XE(J), J=1(1)NE. */

/*     If only function values are required, use DCHFEV, instead. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        INTEGER  NE, NEXT(2), IERR */
/*        DOUBLE PRECISION  X1, X2, F1, F2, D1, D2, XE(NE), FE(NE), */
/*                          DE(NE) */

/*        CALL  DCHFDV (X1,X2, F1,F2, D1,D2, NE, XE, FE, DE, NEXT, IERR) */

/*   Parameters: */

/*     X1,X2 -- (input) endpoints of interval of definition of cubic. */
/*           (Error return if  X1.EQ.X2 .) */

/*     F1,F2 -- (input) values of function at X1 and X2, respectively. */

/*     D1,D2 -- (input) values of derivative at X1 and X2, respectively. */

/*     NE -- (input) number of evaluation points.  (Error return if */
/*           NE.LT.1 .) */

/*     XE -- (input) real*8 array of points at which the functions are to */
/*           be evaluated.  If any of the XE are outside the interval */
/*           [X1,X2], a warning error is returned in NEXT. */

/*     FE -- (output) real*8 array of values of the cubic function */
/*           defined by  X1,X2, F1,F2, D1,D2  at the points  XE. */

/*     DE -- (output) real*8 array of values of the first derivative of */
/*           the same function at the points  XE. */

/*     NEXT -- (output) integer array indicating number of extrapolation */
/*           points: */
/*            NEXT(1) = number of evaluation points to left of interval. */
/*            NEXT(2) = number of evaluation points to right of interval. */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           "Recoverable" errors: */
/*              IERR = -1  if NE.LT.1 . */
/*              IERR = -2  if X1.EQ.X2 . */
/*                (Output arrays have not been changed in either case.) */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   811019  DATE WRITTEN */
/*   820803  Minor cosmetic changes for release 1. */
/*   870707  Corrected XERROR calls for d.p. names(s). */
/*   870813  Minor cosmetic changes. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   891006  Cosmetic changes to prologue.  (WRB) */
/*   891006  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/* ***END PROLOGUE  DCHFDV */
/*  Programming notes: */

/*     To produce a single precision version, simply: */
/*        a. Change DCHFDV to CHFDV wherever it occurs, */
/*        b. Change the double precision declaration to real, and */
/*        c. Change the constant ZERO to single precision. */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */

    /* Parameter adjustments */
    --next;
    --de;
    --fe;
    --xe;

    /* Function Body */

/*  VALIDITY-CHECK ARGUMENTS. */

/* ***FIRST EXECUTABLE STATEMENT  DCHFDV */
    if (*ne < 1) {
	goto L5001;
    }
    h__ = *x2 - *x1;
    if (h__ == zero) {
	goto L5002;
    }

/*  INITIALIZE. */

    *ierr = 0;
    next[1] = 0;
    next[2] = 0;
    xmi = min(zero,h__);
    xma = max(zero,h__);

/*  COMPUTE CUBIC COEFFICIENTS (EXPANDED ABOUT X1). */

    delta = (*f2 - *f1) / h__;
    del1 = (*d1 - delta) / h__;
    del2 = (*d2 - delta) / h__;
/*                                           (DELTA IS NO LONGER NEEDED.) */
    c2 = -(del1 + del1 + del2);
    c2t2 = c2 + c2;
    c3 = (del1 + del2) / h__;
/*                               (H, DEL1 AND DEL2 ARE NO LONGER NEEDED.) */
    c3t3 = c3 + c3 + c3;

/*  EVALUATION LOOP. */

    i__1 = *ne;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x = xe[i__] - *x1;
	fe[i__] = *f1 + x * (*d1 + x * (c2 + x * c3));
	de[i__] = *d1 + x * (c2t2 + x * c3t3);
/*          COUNT EXTRAPOLATION POINTS. */
	if (x < xmi) {
	    ++next[1];
	}
	if (x > xma) {
	    ++next[2];
	}
/*        (NOTE REDUNDANCY--IF EITHER CONDITION IS TRUE, OTHER IS FALSE.) */
/* L500: */
    }

/*  NORMAL RETURN. */

    return 0;

/*  ERROR RETURNS. */

L5001:
/*     NE.LT.1 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "DCHFDV", "NUMBER OF EVALUATION POINTS LESS THAN ONE", 
	    ierr, &c__1, (ftnlen)6, (ftnlen)6, (ftnlen)41);
    return 0;

L5002:
/*     X1.EQ.X2 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "DCHFDV", "INTERVAL ENDPOINTS EQUAL", ierr, &c__1, (
	    ftnlen)6, (ftnlen)6, (ftnlen)24);
    return 0;
/* ------------- LAST LINE OF DCHFDV FOLLOWS ----------------------------- */
} /* dchfdv_ */

