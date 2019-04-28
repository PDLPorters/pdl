/* dpchia.f -- translated by f2c (version 20060506).
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

/* DECK DPCHIA */
doublereal dpchia_(integer *n, doublereal *x, doublereal *f, doublereal *d__, 
	integer *incfd, logical *skip, doublereal *a, doublereal *b, integer *
	ierr)
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer f_dim1, f_offset, d_dim1, d_offset, i__1, i__2;
    doublereal ret_val;

    /* Local variables */
    static integer i__, ia, ib, il;
    static doublereal xa, xb;
    static integer ir, ierd;
    static doublereal value;
    extern doublereal dchfie_(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), dpchid_(integer *, doublereal *, doublereal *, 
	    doublereal *, integer *, logical *, integer *, integer *, integer 
	    *);
    extern /* Subroutine */ int xermsg_(char *, char *, char *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  DPCHIA */
/* ***PURPOSE  Evaluate the definite integral of a piecewise cubic */
/*            Hermite function over an arbitrary interval. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***CATEGORY  E3, H2A1B2 */
/* ***TYPE      DOUBLE PRECISION (PCHIA-S, DPCHIA-D) */
/* ***KEYWORDS  CUBIC HERMITE INTERPOLATION, NUMERICAL INTEGRATION, PCHIP, */
/*             QUADRATURE */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */
/* ***DESCRIPTION */

/*          DPCHIA:  Piecewise Cubic Hermite Integrator, Arbitrary limits */

/*     Evaluates the definite integral of the cubic Hermite function */
/*     defined by  N, X, F, D  over the interval [A, B]. */

/*     To provide compatibility with DPCHIM and DPCHIC, includes an */
/*     increment between successive values of the F- and D-arrays. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  N, IERR */
/*        DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N), A, B */
/*        DOUBLE PRECISION  VALUE, DPCHIA */
/*        LOGICAL  SKIP */

/*        VALUE = DPCHIA (N, X, F, D, INCFD, SKIP, A, B, IERR) */

/*   Parameters: */

/*     VALUE -- (output) value of the requested integral. */

/*     N -- (input) number of data points.  (Error return if N.LT.2 .) */

/*     X -- (input) real*8 array of independent variable values.  The */
/*           elements of X must be strictly increasing: */
/*                X(I-1) .LT. X(I),  I = 2(1)N. */
/*           (Error return if not.) */

/*     F -- (input) real*8 array of function values.  F(1+(I-1)*INCFD) is */
/*           the value corresponding to X(I). */

/*     D -- (input) real*8 array of derivative values.  D(1+(I-1)*INCFD) */
/*           is the value corresponding to X(I). */

/*     INCFD -- (input) increment between successive values in F and D. */
/*           (Error return if  INCFD.LT.1 .) */

/*     SKIP -- (input/output) logical variable which should be set to */
/*           .TRUE. if the user wishes to skip checks for validity of */
/*           preceding parameters, or to .FALSE. otherwise. */
/*           This will save time in case these checks have already */
/*           been performed (say, in DPCHIM or DPCHIC). */
/*           SKIP will be set to .TRUE. on return with IERR.GE.0 . */

/*     A,B -- (input) the limits of integration. */
/*           NOTE:  There is no requirement that [A,B] be contained in */
/*                  [X(1),X(N)].  However, the resulting integral value */
/*                  will be highly suspect, if not. */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           Warning errors: */
/*              IERR = 1  if  A  is outside the interval [X(1),X(N)]. */
/*              IERR = 2  if  B  is outside the interval [X(1),X(N)]. */
/*              IERR = 3  if both of the above are true.  (Note that this */
/*                        means that either [A,B] contains data interval */
/*                        or the intervals do not intersect at all.) */
/*           "Recoverable" errors: */
/*              IERR = -1  if N.LT.2 . */
/*              IERR = -2  if INCFD.LT.1 . */
/*              IERR = -3  if the X-array is not strictly increasing. */
/*                (VALUE will be zero in any of these cases.) */
/*               NOTE:  The above errors are checked in the order listed, */
/*                   and following arguments have **NOT** been validated. */
/*              IERR = -4  in case of an error return from DPCHID (which */
/*                         should never occur). */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  DCHFIE, DPCHID, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820730  DATE WRITTEN */
/*   820804  Converted to SLATEC library version. */
/*   870707  Corrected XERROR calls for d.p. name(s). */
/*   870707  Corrected conversion to double precision. */
/*   870813  Minor cosmetic changes. */
/*   890206  Corrected XERROR calls. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890703  Corrected category record.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   891006  Cosmetic changes to prologue.  (WRB) */
/*   891006  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   930503  Corrected to set VALUE=0 when IERR.lt.0.  (FNF) */
/*   930504  Changed DCHFIV to DCHFIE.  (FNF) */
/* ***END PROLOGUE  DPCHIA */

/*  Programming notes: */
/*  1. The error flag from DPCHID is tested, because a logic flaw */
/*     could conceivably result in IERD=-4, which should be reported. */
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
/* ***FIRST EXECUTABLE STATEMENT  DPCHIA */
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
    *ierr = 0;
    if (*a < x[1] || *a > x[*n]) {
	++(*ierr);
    }
    if (*b < x[1] || *b > x[*n]) {
	*ierr += 2;
    }

/*  COMPUTE INTEGRAL VALUE. */

    if (*a != *b) {
	xa = min(*a,*b);
	xb = max(*a,*b);
	if (xb <= x[2]) {
/*           INTERVAL IS TO LEFT OF X(2), SO USE FIRST CUBIC. */
/*                   --------------------------------------- */
	    value = dchfie_(&x[1], &x[2], &f[f_dim1 + 1], &f[(f_dim1 << 1) + 
		    1], &d__[d_dim1 + 1], &d__[(d_dim1 << 1) + 1], a, b);
/*                   --------------------------------------- */
	} else if (xa >= x[*n - 1]) {
/*           INTERVAL IS TO RIGHT OF X(N-1), SO USE LAST CUBIC. */
/*                   ------------------------------------------ */
	    value = dchfie_(&x[*n - 1], &x[*n], &f[(*n - 1) * f_dim1 + 1], &f[
		    *n * f_dim1 + 1], &d__[(*n - 1) * d_dim1 + 1], &d__[*n * 
		    d_dim1 + 1], a, b);
/*                   ------------------------------------------ */
	} else {
/*           'NORMAL' CASE -- XA.LT.XB, XA.LT.X(N-1), XB.GT.X(2). */
/*      ......LOCATE IA AND IB SUCH THAT */
/*               X(IA-1).LT.XA.LE.X(IA).LE.X(IB).LE.XB.LE.X(IB+1) */
	    ia = 1;
	    i__1 = *n - 1;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (xa > x[i__]) {
		    ia = i__ + 1;
		}
/* L10: */
	    }
/*             IA = 1 IMPLIES XA.LT.X(1) .  OTHERWISE, */
/*             IA IS LARGEST INDEX SUCH THAT X(IA-1).LT.XA,. */

	    ib = *n;
	    i__1 = ia;
	    for (i__ = *n; i__ >= i__1; --i__) {
		if (xb < x[i__]) {
		    ib = i__ - 1;
		}
/* L20: */
	    }
/*             IB = N IMPLIES XB.GT.X(N) .  OTHERWISE, */
/*             IB IS SMALLEST INDEX SUCH THAT XB.LT.X(IB+1) . */

/*     ......COMPUTE THE INTEGRAL. */
	    if (ib < ia) {
/*              THIS MEANS IB = IA-1 AND */
/*                 (A,B) IS A SUBSET OF (X(IB),X(IA)). */
/*                      ------------------------------------------- */
		value = dchfie_(&x[ib], &x[ia], &f[ib * f_dim1 + 1], &f[ia * 
			f_dim1 + 1], &d__[ib * d_dim1 + 1], &d__[ia * d_dim1 
			+ 1], a, b);
/*                      ------------------------------------------- */
	    } else {

/*              FIRST COMPUTE INTEGRAL OVER (X(IA),X(IB)). */
/*                (Case (IB .EQ. IA) is taken care of by initialization */
/*                 of VALUE to ZERO.) */
		if (ib > ia) {
/*                         --------------------------------------------- */
		    value = dpchid_(n, &x[1], &f[f_offset], &d__[d_offset], 
			    incfd, skip, &ia, &ib, &ierd);
/*                         --------------------------------------------- */
		    if (ierd < 0) {
			goto L5004;
		    }
		}

/*              THEN ADD ON INTEGRAL OVER (XA,X(IA)). */
		if (xa < x[ia]) {
/* Computing MAX */
		    i__1 = 1, i__2 = ia - 1;
		    il = max(i__1,i__2);
		    ir = il + 1;
/*                                 ------------------------------------- */
		    value += dchfie_(&x[il], &x[ir], &f[il * f_dim1 + 1], &f[
			    ir * f_dim1 + 1], &d__[il * d_dim1 + 1], &d__[ir *
			     d_dim1 + 1], &xa, &x[ia]);
/*                                 ------------------------------------- */
		}

/*              THEN ADD ON INTEGRAL OVER (X(IB),XB). */
		if (xb > x[ib]) {
/* Computing MIN */
		    i__1 = ib + 1;
		    ir = min(i__1,*n);
		    il = ir - 1;
/*                                 ------------------------------------- */
		    value += dchfie_(&x[il], &x[ir], &f[il * f_dim1 + 1], &f[
			    ir * f_dim1 + 1], &d__[il * d_dim1 + 1], &d__[ir *
			     d_dim1 + 1], &x[ib], &xb);
/*                                 ------------------------------------- */
		}

/*              FINALLY, ADJUST SIGN IF NECESSARY. */
		if (*a > *b) {
		    value = -value;
		}
	    }
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
    xermsg_("SLATEC", "DPCHIA", "NUMBER OF DATA POINTS LESS THAN TWO", ierr, &
	    c__1, (ftnlen)6, (ftnlen)6, (ftnlen)35);
    goto L5000;

L5002:
/*     INCFD.LT.1 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "DPCHIA", "INCREMENT LESS THAN ONE", ierr, &c__1, (
	    ftnlen)6, (ftnlen)6, (ftnlen)23);
    goto L5000;

L5003:
/*     X-ARRAY NOT STRICTLY INCREASING. */
    *ierr = -3;
    xermsg_("SLATEC", "DPCHIA", "X-ARRAY NOT STRICTLY INCREASING", ierr, &
	    c__1, (ftnlen)6, (ftnlen)6, (ftnlen)31);
    goto L5000;

L5004:
/*     TROUBLE IN DPCHID.  (SHOULD NEVER OCCUR.) */
    *ierr = -4;
    xermsg_("SLATEC", "DPCHIA", "TROUBLE IN DPCHID", ierr, &c__1, (ftnlen)6, (
	    ftnlen)6, (ftnlen)17);
    goto L5000;
/* ------------- LAST LINE OF DPCHIA FOLLOWS ----------------------------- */
} /* dpchia_ */

