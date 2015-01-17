/* pchid.f -- translated by f2c (version 20060506).
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

/* DECK PCHID */
doublereal pchid_(integer *n, real *x, real *f, real *d__, integer *incfd, 
	logical *skip, integer *ia, integer *ib, integer *ierr)
{
    /* Initialized data */

    static real zero = 0.f;
    static real half = .5f;
    static real six = 6.f;

    /* System generated locals */
    integer f_dim1, f_offset, d_dim1, d_offset, i__1;
    real ret_val;

    /* Local variables */
    static real h__;
    static integer i__, iup, low;
    static real sum, value;
    extern /* Subroutine */ int xermsg_(char *, char *, char *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  PCHID */
/* ***PURPOSE  Evaluate the definite integral of a piecewise cubic */
/*            Hermite function over an interval whose endpoints are data */
/*            points. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***CATEGORY  E3, H2A1B2 */
/* ***TYPE      SINGLE PRECISION (PCHID-S, DPCHID-D) */
/* ***KEYWORDS  CUBIC HERMITE INTERPOLATION, NUMERICAL INTEGRATION, PCHIP, */
/*             QUADRATURE */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */
/* ***DESCRIPTION */

/*          PCHID:  Piecewise Cubic Hermite Integrator, Data limits */

/*     Evaluates the definite integral of the cubic Hermite function */
/*     defined by  N, X, F, D  over the interval [X(IA), X(IB)]. */

/*     To provide compatibility with PCHIM and PCHIC, includes an */
/*     increment between successive values of the F- and D-arrays. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  N, IA, IB, IERR */
/*        REAL  X(N), F(INCFD,N), D(INCFD,N) */
/*        LOGICAL  SKIP */

/*        VALUE = PCHID (N, X, F, D, INCFD, SKIP, IA, IB, IERR) */

/*   Parameters: */

/*     VALUE -- (output) value of the requested integral. */

/*     N -- (input) number of data points.  (Error return if N.LT.2 .) */

/*     X -- (input) real array of independent variable values.  The */
/*           elements of X must be strictly increasing: */
/*                X(I-1) .LT. X(I),  I = 2(1)N. */
/*           (Error return if not.) */

/*     F -- (input) real array of function values.  F(1+(I-1)*INCFD) is */
/*           the value corresponding to X(I). */

/*     D -- (input) real array of derivative values.  D(1+(I-1)*INCFD) is */
/*           the value corresponding to X(I). */

/*     INCFD -- (input) increment between successive values in F and D. */
/*           (Error return if  INCFD.LT.1 .) */

/*     SKIP -- (input/output) logical variable which should be set to */
/*           .TRUE. if the user wishes to skip checks for validity of */
/*           preceding parameters, or to .FALSE. otherwise. */
/*           This will save time in case these checks have already */
/*           been performed (say, in PCHIM or PCHIC). */
/*           SKIP will be set to .TRUE. on return with IERR = 0 or -4. */

/*     IA,IB -- (input) indices in X-array for the limits of integration. */
/*           both must be in the range [1,N].  (Error return if not.) */
/*           No restrictions on their relative values. */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           "Recoverable" errors: */
/*              IERR = -1  if N.LT.2 . */
/*              IERR = -2  if INCFD.LT.1 . */
/*              IERR = -3  if the X-array is not strictly increasing. */
/*              IERR = -4  if IA or IB is out of range. */
/*                (VALUE will be zero in any of these cases.) */
/*               NOTE:  The above errors are checked in the order listed, */
/*                   and following arguments have **NOT** been validated. */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820723  DATE WRITTEN */
/*   820804  Converted to SLATEC library version. */
/*   870813  Minor cosmetic changes. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890703  Corrected category record.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   930504  Corrected to set VALUE=0 when IERR.ne.0.  (FNF) */
/* ***END PROLOGUE  PCHID */

/*  Programming notes: */
/*  1. This routine uses a special formula that is valid only for */
/*     integrals whose limits coincide with data values.  This is */
/*     mathematically equivalent to, but much more efficient than, */
/*     calls to CHFIE. */
/* **End */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*  INITIALIZE. */

    /* Parameter adjustments */
    --x;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;
    f_dim1 = *incfd;
    f_offset = 1 + f_dim1;
    f -= f_offset;

    /* Function Body */
/* ***FIRST EXECUTABLE STATEMENT  PCHID */
    value = zero;

/*  VALIDITY-CHECK ARGUMENTS. */

    if (*skip) {
	goto L5;
    }

    if (*n < 2) {
	goto L5001;
    }
    if (*incfd < 1) {
	goto L5002;
    }
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__] <= x[i__ - 1]) {
	    goto L5003;
	}
/* L1: */
    }

/*  FUNCTION DEFINITION IS OK, GO ON. */

L5:
    *skip = TRUE_;
    if (*ia < 1 || *ia > *n) {
	goto L5004;
    }
    if (*ib < 1 || *ib > *n) {
	goto L5004;
    }
    *ierr = 0;

/*  COMPUTE INTEGRAL VALUE. */

    if (*ia != *ib) {
	low = min(*ia,*ib);
	iup = max(*ia,*ib) - 1;
	sum = zero;
	i__1 = iup;
	for (i__ = low; i__ <= i__1; ++i__) {
	    h__ = x[i__ + 1] - x[i__];
	    sum += h__ * (f[i__ * f_dim1 + 1] + f[(i__ + 1) * f_dim1 + 1] + (
		    d__[i__ * d_dim1 + 1] - d__[(i__ + 1) * d_dim1 + 1]) * (
		    h__ / six));
/* L10: */
	}
	value = half * sum;
	if (*ia > *ib) {
	    value = -value;
	}
    }

/*  NORMAL RETURN. */

L5000:
    ret_val = value;
    return ret_val;

/*  ERROR RETURNS. */

L5001:
/*     N.LT.2 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "PCHID", "NUMBER OF DATA POINTS LESS THAN TWO", ierr, &
	    c__1, (ftnlen)6, (ftnlen)5, (ftnlen)35);
    goto L5000;

L5002:
/*     INCFD.LT.1 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "PCHID", "INCREMENT LESS THAN ONE", ierr, &c__1, (
	    ftnlen)6, (ftnlen)5, (ftnlen)23);
    goto L5000;

L5003:
/*     X-ARRAY NOT STRICTLY INCREASING. */
    *ierr = -3;
    xermsg_("SLATEC", "PCHID", "X-ARRAY NOT STRICTLY INCREASING", ierr, &c__1,
	     (ftnlen)6, (ftnlen)5, (ftnlen)31);
    goto L5000;

L5004:
/*     IA OR IB OUT OF RANGE RETURN. */
    *ierr = -4;
    xermsg_("SLATEC", "PCHID", "IA OR IB OUT OF RANGE", ierr, &c__1, (ftnlen)
	    6, (ftnlen)5, (ftnlen)21);
    goto L5000;
/* ------------- LAST LINE OF PCHID FOLLOWS ------------------------------ */
} /* pchid_ */

