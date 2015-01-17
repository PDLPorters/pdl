/* pchfd.f -- translated by f2c (version 20060506).
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
static integer c__2 = 2;

/* DECK PCHFD */
/* Subroutine */ int pchfd_(integer *n, real *x, real *f, real *d__, integer *
	incfd, logical *skip, integer *ne, real *xe, real *fe, real *de, 
	integer *ierr)
{
    /* System generated locals */
    integer f_dim1, f_offset, d_dim1, d_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j, nj, ir, ierc, next[2];
    extern /* Subroutine */ int chfdv_(real *, real *, real *, real *, real *,
	     real *, integer *, real *, real *, real *, integer *, integer *);
    static integer jfirst;
    extern /* Subroutine */ int xermsg_(char *, char *, char *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  PCHFD */
/* ***PURPOSE  Evaluate a piecewise cubic Hermite function and its first */
/*            derivative at an array of points.  May be used by itself */
/*            for Hermite interpolation, or as an evaluator for PCHIM */
/*            or PCHIC.  If only function values are required, use */
/*            PCHFE instead. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***CATEGORY  E3, H1 */
/* ***TYPE      SINGLE PRECISION (PCHFD-S, DPCHFD-D) */
/* ***KEYWORDS  CUBIC HERMITE DIFFERENTIATION, CUBIC HERMITE EVALUATION, */
/*             HERMITE INTERPOLATION, PCHIP, PIECEWISE CUBIC EVALUATION */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */
/* ***DESCRIPTION */

/*          PCHFD:  Piecewise Cubic Hermite Function and Derivative */
/*                  evaluator */

/*     Evaluates the cubic Hermite function defined by  N, X, F, D,  to- */
/*     gether with its first derivative, at the points  XE(J), J=1(1)NE. */

/*     If only function values are required, use PCHFE, instead. */

/*     To provide compatibility with PCHIM and PCHIC, includes an */
/*     increment between successive values of the F- and D-arrays. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  N, NE, IERR */
/*        REAL  X(N), F(INCFD,N), D(INCFD,N), XE(NE), FE(NE), DE(NE) */
/*        LOGICAL  SKIP */

/*        CALL  PCHFD (N, X, F, D, INCFD, SKIP, NE, XE, FE, DE, IERR) */

/*   Parameters: */

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
/*           SKIP will be set to .TRUE. on normal return. */

/*     NE -- (input) number of evaluation points.  (Error return if */
/*           NE.LT.1 .) */

/*     XE -- (input) real array of points at which the functions are to */
/*           be evaluated. */


/*          NOTES: */
/*           1. The evaluation will be most efficient if the elements */
/*              of XE are increasing relative to X; */
/*              that is,   XE(J) .GE. X(I) */
/*              implies    XE(K) .GE. X(I),  all K.GE.J . */
/*           2. If any of the XE are outside the interval [X(1),X(N)], */
/*              values are extrapolated from the nearest extreme cubic, */
/*              and a warning error is returned. */

/*     FE -- (output) real array of values of the cubic Hermite function */
/*           defined by  N, X, F, D  at the points  XE. */

/*     DE -- (output) real array of values of the first derivative of */
/*           the same function at the points  XE. */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           Warning error: */
/*              IERR.GT.0  means that extrapolation was performed at */
/*                 IERR points. */
/*           "Recoverable" errors: */
/*              IERR = -1  if N.LT.2 . */
/*              IERR = -2  if INCFD.LT.1 . */
/*              IERR = -3  if the X-array is not strictly increasing. */
/*              IERR = -4  if NE.LT.1 . */
/*           (Output arrays have not been changed in any of these cases.) */
/*               NOTE:  The above errors are checked in the order listed, */
/*                   and following arguments have **NOT** been validated. */
/*              IERR = -5  if an error has occurred in the lower-level */
/*                         routine CHFDV.  NB: this should never happen. */
/*                         Notify the author **IMMEDIATELY** if it does. */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  CHFDV, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   811020  DATE WRITTEN */
/*   820803  Minor cosmetic changes for release 1. */
/*   870707  Minor cosmetic changes to prologue. */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/* ***END PROLOGUE  PCHFD */
/*  Programming notes: */

/*     1. To produce a double precision version, simply: */
/*        a. Change PCHFD to DPCHFD, and CHFDV to DCHFDV, wherever they */
/*           occur, */
/*        b. Change the real declaration to double precision, */

/*     2. Most of the coding between the call to CHFDV and the end of */
/*        the IR-loop could be eliminated if it were permissible to */
/*        assume that XE is ordered relative to X. */

/*     3. CHFDV does not assume that X1 is less than X2.  thus, it would */
/*        be possible to write a version of PCHFD that assumes a strict- */
/*        ly decreasing X-array by simply running the IR-loop backwards */
/*        (and reversing the order of appropriate tests). */

/*     4. The present code has a minor bug, which I have decided is not */
/*        worth the effort that would be required to fix it. */
/*        If XE contains points in [X(N-1),X(N)], followed by points .LT. */
/*        X(N-1), followed by points .GT.X(N), the extrapolation points */
/*        will be counted (at least) twice in the total returned in IERR. */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*  VALIDITY-CHECK ARGUMENTS. */

/* ***FIRST EXECUTABLE STATEMENT  PCHFD */
    /* Parameter adjustments */
    --x;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;
    f_dim1 = *incfd;
    f_offset = 1 + f_dim1;
    f -= f_offset;
    --xe;
    --fe;
    --de;

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

/*  FUNCTION DEFINITION IS OK, GO ON. */

L5:
    if (*ne < 1) {
	goto L5004;
    }
    *ierr = 0;
    *skip = TRUE_;

/*  LOOP OVER INTERVALS.        (   INTERVAL INDEX IS  IL = IR-1  . ) */
/*                              ( INTERVAL IS X(IL).LE.X.LT.X(IR) . ) */
    jfirst = 1;
    ir = 2;
L10:

/*     SKIP OUT OF LOOP IF HAVE PROCESSED ALL EVALUATION POINTS. */

    if (jfirst > *ne) {
	goto L5000;
    }

/*     LOCATE ALL POINTS IN INTERVAL. */

    i__1 = *ne;
    for (j = jfirst; j <= i__1; ++j) {
	if (xe[j] >= x[ir]) {
	    goto L30;
	}
/* L20: */
    }
    j = *ne + 1;
    goto L40;

/*     HAVE LOCATED FIRST POINT BEYOND INTERVAL. */

L30:
    if (ir == *n) {
	j = *ne + 1;
    }

L40:
    nj = j - jfirst;

/*     SKIP EVALUATION IF NO POINTS IN INTERVAL. */

    if (nj == 0) {
	goto L50;
    }

/*     EVALUATE CUBIC AT XE(I),  I = JFIRST (1) J-1 . */

/*       ---------------------------------------------------------------- */
    chfdv_(&x[ir - 1], &x[ir], &f[(ir - 1) * f_dim1 + 1], &f[ir * f_dim1 + 1],
	     &d__[(ir - 1) * d_dim1 + 1], &d__[ir * d_dim1 + 1], &nj, &xe[
	    jfirst], &fe[jfirst], &de[jfirst], next, &ierc);
/*       ---------------------------------------------------------------- */
    if (ierc < 0) {
	goto L5005;
    }

    if (next[1] == 0) {
	goto L42;
    }
/*        IF (NEXT(2) .GT. 0)  THEN */
/*           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(2) TO THE */
/*           RIGHT OF X(IR). */

    if (ir < *n) {
	goto L41;
    }
/*           IF (IR .EQ. N)  THEN */
/*              THESE ARE ACTUALLY EXTRAPOLATION POINTS. */
    *ierr += next[1];
    goto L42;
L41:
/*           ELSE */
/*              WE SHOULD NEVER HAVE GOTTEN HERE. */
    goto L5005;
/*           ENDIF */
/*        ENDIF */
L42:

    if (next[0] == 0) {
	goto L49;
    }
/*        IF (NEXT(1) .GT. 0)  THEN */
/*           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(1) TO THE */
/*           LEFT OF X(IR-1). */

    if (ir > 2) {
	goto L43;
    }
/*           IF (IR .EQ. 2)  THEN */
/*              THESE ARE ACTUALLY EXTRAPOLATION POINTS. */
    *ierr += next[0];
    goto L49;
L43:
/*           ELSE */
/*              XE IS NOT ORDERED RELATIVE TO X, SO MUST ADJUST */
/*              EVALUATION INTERVAL. */

/*              FIRST, LOCATE FIRST POINT TO LEFT OF X(IR-1). */
    i__1 = j - 1;
    for (i__ = jfirst; i__ <= i__1; ++i__) {
	if (xe[i__] < x[ir - 1]) {
	    goto L45;
	}
/* L44: */
    }
/*              NOTE-- CANNOT DROP THROUGH HERE UNLESS THERE IS AN ERROR */
/*                     IN CHFDV. */
    goto L5005;

L45:
/*              RESET J.  (THIS WILL BE THE NEW JFIRST.) */
    j = i__;

/*              NOW FIND OUT HOW FAR TO BACK UP IN THE X-ARRAY. */
    i__1 = ir - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (xe[j] < x[i__]) {
	    goto L47;
	}
/* L46: */
    }
/*              NB-- CAN NEVER DROP THROUGH HERE, SINCE XE(J).LT.X(IR-1). */

L47:
/*              AT THIS POINT, EITHER  XE(J) .LT. X(1) */
/*                 OR      X(I-1) .LE. XE(J) .LT. X(I) . */
/*              RESET IR, RECOGNIZING THAT IT WILL BE INCREMENTED BEFORE */
/*              CYCLING. */
/* Computing MAX */
    i__1 = 1, i__2 = i__ - 1;
    ir = max(i__1,i__2);
/*           ENDIF */
/*        ENDIF */
L49:

    jfirst = j;

/*     END OF IR-LOOP. */

L50:
    ++ir;
    if (ir <= *n) {
	goto L10;
    }

/*  NORMAL RETURN. */

L5000:
    return 0;

/*  ERROR RETURNS. */

L5001:
/*     N.LT.2 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "PCHFD", "NUMBER OF DATA POINTS LESS THAN TWO", ierr, &
	    c__1, (ftnlen)6, (ftnlen)5, (ftnlen)35);
    return 0;

L5002:
/*     INCFD.LT.1 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "PCHFD", "INCREMENT LESS THAN ONE", ierr, &c__1, (
	    ftnlen)6, (ftnlen)5, (ftnlen)23);
    return 0;

L5003:
/*     X-ARRAY NOT STRICTLY INCREASING. */
    *ierr = -3;
    xermsg_("SLATEC", "PCHFD", "X-ARRAY NOT STRICTLY INCREASING", ierr, &c__1,
	     (ftnlen)6, (ftnlen)5, (ftnlen)31);
    return 0;

L5004:
/*     NE.LT.1 RETURN. */
    *ierr = -4;
    xermsg_("SLATEC", "PCHFD", "NUMBER OF EVALUATION POINTS LESS THAN ONE", 
	    ierr, &c__1, (ftnlen)6, (ftnlen)5, (ftnlen)41);
    return 0;

L5005:
/*     ERROR RETURN FROM CHFDV. */
/*   *** THIS CASE SHOULD NEVER OCCUR *** */
    *ierr = -5;
    xermsg_("SLATEC", "PCHFD", "ERROR RETURN FROM CHFDV -- FATAL", ierr, &
	    c__2, (ftnlen)6, (ftnlen)5, (ftnlen)32);
    return 0;
/* ------------- LAST LINE OF PCHFD FOLLOWS ------------------------------ */
} /* pchfd_ */

