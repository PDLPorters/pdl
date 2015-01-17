/* dpchcm.f -- translated by f2c (version 20060506).
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

static integer c__3 = 3;
static integer c__1 = 1;

/* DECK DPCHCM */
/* Subroutine */ int dpchcm_(integer *n, doublereal *x, doublereal *f, 
	doublereal *d__, integer *incfd, logical *skip, integer *ismon, 
	integer *ierr)
{
    /* System generated locals */
    integer f_dim1, f_offset, d_dim1, d_offset, i__1;

    /* Builtin functions */
    integer i_sign(integer *, integer *);

    /* Local variables */
    static integer i__, nseg;
    static doublereal delta;
    extern integer dchfcm_(doublereal *, doublereal *, doublereal *);
    extern /* Subroutine */ int xermsg_(char *, char *, char *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  DPCHCM */
/* ***PURPOSE  Check a cubic Hermite function for monotonicity. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***CATEGORY  E3 */
/* ***TYPE      DOUBLE PRECISION (PCHCM-S, DPCHCM-D) */
/* ***KEYWORDS  CUBIC HERMITE INTERPOLATION, MONOTONE INTERPOLATION, */
/*             PCHIP, PIECEWISE CUBIC INTERPOLATION, UTILITY ROUTINE */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Computing & Mathematics Research Division */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */
/* ***DESCRIPTION */

/* *Usage: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  N, ISMON(N), IERR */
/*        DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N) */
/*        LOGICAL  SKIP */

/*        CALL  DPCHCM (N, X, F, D, INCFD, SKIP, ISMON, IERR) */

/* *Arguments: */

/*     N:IN  is the number of data points.  (Error return if N.LT.2 .) */

/*     X:IN  is a real*8 array of independent variable values.  The */
/*           elements of X must be strictly increasing: */
/*                X(I-1) .LT. X(I),  I = 2(1)N. */
/*           (Error return if not.) */

/*     F:IN  is a real*8 array of function values.  F(1+(I-1)*INCFD) is */
/*           the value corresponding to X(I). */

/*     D:IN  is a real*8 array of derivative values.  D(1+(I-1)*INCFD) is */
/*           is the value corresponding to X(I). */

/*     INCFD:IN  is the increment between successive values in F and D. */
/*           (Error return if  INCFD.LT.1 .) */

/*     SKIP:INOUT  is a logical variable which should be set to */
/*           .TRUE. if the user wishes to skip checks for validity of */
/*           preceding parameters, or to .FALSE. otherwise. */
/*           This will save time in case these checks have already */
/*           been performed. */
/*           SKIP will be set to .TRUE. on normal return. */

/*     ISMON:OUT  is an integer array indicating on which intervals the */
/*           PCH function defined by  N, X, F, D  is monotonic. */
/*           For data interval [X(I),X(I+1)], */
/*             ISMON(I) = -3  if function is probably decreasing; */
/*             ISMON(I) = -1  if function is strictly decreasing; */
/*             ISMON(I) =  0  if function is constant; */
/*             ISMON(I) =  1  if function is strictly increasing; */
/*             ISMON(I) =  2  if function is non-monotonic; */
/*             ISMON(I) =  3  if function is probably increasing. */
/*                If ABS(ISMON)=3, this means that the D-values are near */
/*                the boundary of the monotonicity region.  A small */
/*                increase produces non-monotonicity; decrease, strict */
/*                monotonicity. */
/*           The above applies to I=1(1)N-1.  ISMON(N) indicates whether */
/*              the entire function is monotonic on [X(1),X(N)]. */

/*     IERR:OUT  is an error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           "Recoverable" errors: */
/*              IERR = -1  if N.LT.2 . */
/*              IERR = -2  if INCFD.LT.1 . */
/*              IERR = -3  if the X-array is not strictly increasing. */
/*          (The ISMON-array has not been changed in any of these cases.) */
/*               NOTE:  The above errors are checked in the order listed, */
/*                   and following arguments have **NOT** been validated. */

/* *Description: */

/*          DPCHCM:  Piecewise Cubic Hermite -- Check Monotonicity. */

/*     Checks the piecewise cubic Hermite function defined by  N,X,F,D */
/*     for monotonicity. */

/*     To provide compatibility with DPCHIM and DPCHIC, includes an */
/*     increment between successive values of the F- and D-arrays. */

/* *Cautions: */
/*     This provides the same capability as old DPCHMC, except that a */
/*     new output value, -3, was added February 1989.  (Formerly, -3 */
/*     and +3 were lumped together in the single value 3.)  Codes that */
/*     flag nonmonotonicity by "IF (ISMON.EQ.2)" need not be changed. */
/*     Codes that check via "IF (ISMON.GE.3)" should change the test to */
/*     "IF (IABS(ISMON).GE.3)".  Codes that declare monotonicity via */
/*     "IF (ISMON.LE.1)" should change to "IF (IABS(ISMON).LE.1)". */

/* ***REFERENCES  F. N. Fritsch and R. E. Carlson, Monotone piecewise */
/*                 cubic interpolation, SIAM Journal on Numerical Ana- */
/*                 lysis 17, 2 (April 1980), pp. 238-246. */
/* ***ROUTINES CALLED  DCHFCM, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820518  DATE WRITTEN */
/*   820804  Converted to SLATEC library version. */
/*   831201  Reversed order of subscripts of F and D, so that the */
/*           routine will work properly when INCFD.GT.1 .  (Bug!) */
/*   870707  Corrected XERROR calls for d.p. name(s). */
/*   890206  Corrected XERROR calls. */
/*   890209  Added possible ISMON value of -3 and modified code so */
/*           that 1,3,-1 produces ISMON(N)=2, rather than 3. */
/*   890306  Added caution about changed output. */
/*   890407  Changed name from DPCHMC to DPCHCM, as requested at the */
/*           March 1989 SLATEC CML meeting, and made a few other */
/*           minor modifications necessitated by this change. */
/*   890407  Converted to new SLATEC format. */
/*   890407  Modified DESCRIPTION to LDOC format. */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   920429  Revised format and order of references.  (WRB,FNF) */
/* ***END PROLOGUE  DPCHCM */

/*  Fortran intrinsics used:  ISIGN. */
/*  Other routines used:  CHFCM, XERMSG. */

/* ---------------------------------------------------------------------- */

/*  Programming notes: */

/*     An alternate organization would have separate loops for computing */
/*     ISMON(i), i=1,...,NSEG, and for the computation of ISMON(N).  The */
/*     first loop can be readily parallelized, since the NSEG calls to */
/*     CHFCM are independent.  The second loop can be cut short if */
/*     ISMON(N) is ever equal to 2, for it cannot be changed further. */

/*     To produce a single precision version, simply: */
/*        a. Change DPCHCM to PCHCM wherever it occurs, */
/*        b. Change DCHFCM to CHFCM wherever it occurs, and */
/*        c. Change the double precision declarations to real. */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*  VALIDITY-CHECK ARGUMENTS. */

/* ***FIRST EXECUTABLE STATEMENT  DPCHCM */
    /* Parameter adjustments */
    --ismon;
    --x;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;
    f_dim1 = *incfd;
    f_offset = 1 + f_dim1;
    f -= f_offset;

    /* Function Body */
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
    *skip = TRUE_;

/*  FUNCTION DEFINITION IS OK -- GO ON. */

L5:
    nseg = *n - 1;
    i__1 = nseg;
    for (i__ = 1; i__ <= i__1; ++i__) {
	delta = (f[(i__ + 1) * f_dim1 + 1] - f[i__ * f_dim1 + 1]) / (x[i__ + 
		1] - x[i__]);
/*                   ------------------------------- */
	ismon[i__] = dchfcm_(&d__[i__ * d_dim1 + 1], &d__[(i__ + 1) * d_dim1 
		+ 1], &delta);
/*                   ------------------------------- */
	if (i__ == 1) {
	    ismon[*n] = ismon[1];
	} else {
/*           Need to figure out cumulative monotonicity from following */
/*           "multiplication table": */

/*                    +        I S M O N (I) */
/*                     +  -3  -1   0   1   3   2 */
/*                      +------------------------+ */
/*               I   -3 I -3  -3  -3   2   2   2 I */
/*               S   -1 I -3  -1  -1   2   2   2 I */
/*               M    0 I -3  -1   0   1   3   2 I */
/*               O    1 I  2   2   1   1   3   2 I */
/*               N    3 I  2   2   3   3   3   2 I */
/*              (N)   2 I  2   2   2   2   2   2 I */
/*                      +------------------------+ */
/*           Note that the 2 row and column are out of order so as not */
/*           to obscure the symmetry in the rest of the table. */

/*           No change needed if equal or constant on this interval or */
/*           already declared nonmonotonic. */
	    if (ismon[i__] != ismon[*n] && ismon[i__] != 0 && ismon[*n] != 2) 
		    {
		if (ismon[i__] == 2 || ismon[*n] == 0) {
		    ismon[*n] = ismon[i__];
		} else if (ismon[i__] * ismon[*n] < 0) {
/*                 This interval has opposite sense from curve so far. */
		    ismon[*n] = 2;
		} else {
/*                 At this point, both are nonzero with same sign, and */
/*                 we have already eliminated case both +-1. */
		    ismon[*n] = i_sign(&c__3, &ismon[*n]);
		}
	    }
	}
/* L90: */
    }

/*  NORMAL RETURN. */

    *ierr = 0;
    return 0;

/*  ERROR RETURNS. */

L5001:
/*     N.LT.2 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "DPCHCM", "NUMBER OF DATA POINTS LESS THAN TWO", ierr, &
	    c__1, (ftnlen)6, (ftnlen)6, (ftnlen)35);
    return 0;

L5002:
/*     INCFD.LT.1 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "DPCHCM", "INCREMENT LESS THAN ONE", ierr, &c__1, (
	    ftnlen)6, (ftnlen)6, (ftnlen)23);
    return 0;

L5003:
/*     X-ARRAY NOT STRICTLY INCREASING. */
    *ierr = -3;
    xermsg_("SLATEC", "DPCHCM", "X-ARRAY NOT STRICTLY INCREASING", ierr, &
	    c__1, (ftnlen)6, (ftnlen)6, (ftnlen)31);
    return 0;
/* ------------- LAST LINE OF DPCHCM FOLLOWS ----------------------------- */
} /* dpchcm_ */

