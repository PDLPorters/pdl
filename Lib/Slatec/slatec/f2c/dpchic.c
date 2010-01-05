/* dpchic.f -- translated by f2c (version 20060506).
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

/* DECK DPCHIC */
/* Subroutine */ int dpchic_(integer *ic, doublereal *vc, doublereal *
	switch__, integer *n, doublereal *x, doublereal *f, doublereal *d__, 
	integer *incfd, doublereal *wk, integer *nwk, integer *ierr)
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer f_dim1, f_offset, d_dim1, d_offset, i__1;

    /* Local variables */
    static integer i__, ibeg, iend, nless1;
    extern /* Subroutine */ int dpchce_(integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     integer *), dpchci_(integer *, doublereal *, doublereal *, 
	    doublereal *, integer *), dpchcs_(doublereal *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *), 
	    xermsg_(char *, char *, char *, integer *, integer *, ftnlen, 
	    ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  DPCHIC */
/* ***PURPOSE  Set derivatives needed to determine a piecewise monotone */
/*            piecewise cubic Hermite interpolant to given data. */
/*            User control is available over boundary conditions and/or */
/*            treatment of points where monotonicity switches direction. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***CATEGORY  E1A */
/* ***TYPE      DOUBLE PRECISION (PCHIC-S, DPCHIC-D) */
/* ***KEYWORDS  CUBIC HERMITE INTERPOLATION, MONOTONE INTERPOLATION, */
/*             PCHIP, PIECEWISE CUBIC INTERPOLATION, */
/*             SHAPE-PRESERVING INTERPOLATION */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */
/* ***DESCRIPTION */

/*         DPCHIC:  Piecewise Cubic Hermite Interpolation Coefficients. */

/*     Sets derivatives needed to determine a piecewise monotone piece- */
/*     wise cubic interpolant to the data given in X and F satisfying the */
/*     boundary conditions specified by IC and VC. */

/*     The treatment of points where monotonicity switches direction is */
/*     controlled by argument SWITCH. */

/*     To facilitate two-dimensional applications, includes an increment */
/*     between successive values of the F- and D-arrays. */

/*     The resulting piecewise cubic Hermite function may be evaluated */
/*     by DPCHFE or DPCHFD. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  IC(2), N, NWK, IERR */
/*        DOUBLE PRECISION  VC(2), SWITCH, X(N), F(INCFD,N), D(INCFD,N), */
/*                          WK(NWK) */

/*        CALL DPCHIC (IC, VC, SWITCH, N, X, F, D, INCFD, WK, NWK, IERR) */

/*   Parameters: */

/*     IC -- (input) integer array of length 2 specifying desired */
/*           boundary conditions: */
/*           IC(1) = IBEG, desired condition at beginning of data. */
/*           IC(2) = IEND, desired condition at end of data. */

/*           IBEG = 0  for the default boundary condition (the same as */
/*                     used by DPCHIM). */
/*           If IBEG.NE.0, then its sign indicates whether the boundary */
/*                     derivative is to be adjusted, if necessary, to be */
/*                     compatible with monotonicity: */
/*              IBEG.GT.0  if no adjustment is to be performed. */
/*              IBEG.LT.0  if the derivative is to be adjusted for */
/*                     monotonicity. */

/*           Allowable values for the magnitude of IBEG are: */
/*           IBEG = 1  if first derivative at X(1) is given in VC(1). */
/*           IBEG = 2  if second derivative at X(1) is given in VC(1). */
/*           IBEG = 3  to use the 3-point difference formula for D(1). */
/*                     (Reverts to the default b.c. if N.LT.3 .) */
/*           IBEG = 4  to use the 4-point difference formula for D(1). */
/*                     (Reverts to the default b.c. if N.LT.4 .) */
/*           IBEG = 5  to set D(1) so that the second derivative is con- */
/*              tinuous at X(2). (Reverts to the default b.c. if N.LT.4.) */
/*              This option is somewhat analogous to the "not a knot" */
/*              boundary condition provided by DPCHSP. */

/*          NOTES (IBEG): */
/*           1. An error return is taken if ABS(IBEG).GT.5 . */
/*           2. Only in case  IBEG.LE.0  is it guaranteed that the */
/*              interpolant will be monotonic in the first interval. */
/*              If the returned value of D(1) lies between zero and */
/*              3*SLOPE(1), the interpolant will be monotonic.  This */
/*              is **NOT** checked if IBEG.GT.0 . */
/*           3. If IBEG.LT.0 and D(1) had to be changed to achieve mono- */
/*              tonicity, a warning error is returned. */

/*           IEND may take on the same values as IBEG, but applied to */
/*           derivative at X(N).  In case IEND = 1 or 2, the value is */
/*           given in VC(2). */

/*          NOTES (IEND): */
/*           1. An error return is taken if ABS(IEND).GT.5 . */
/*           2. Only in case  IEND.LE.0  is it guaranteed that the */
/*              interpolant will be monotonic in the last interval. */
/*              If the returned value of D(1+(N-1)*INCFD) lies between */
/*              zero and 3*SLOPE(N-1), the interpolant will be monotonic. */
/*              This is **NOT** checked if IEND.GT.0 . */
/*           3. If IEND.LT.0 and D(1+(N-1)*INCFD) had to be changed to */
/*              achieve monotonicity, a warning error is returned. */

/*     VC -- (input) real*8 array of length 2 specifying desired boundary */
/*           values, as indicated above. */
/*           VC(1) need be set only if IC(1) = 1 or 2 . */
/*           VC(2) need be set only if IC(2) = 1 or 2 . */

/*     SWITCH -- (input) indicates desired treatment of points where */
/*           direction of monotonicity switches: */
/*           Set SWITCH to zero if interpolant is required to be mono- */
/*           tonic in each interval, regardless of monotonicity of data. */
/*             NOTES: */
/*              1. This will cause D to be set to zero at all switch */
/*                 points, thus forcing extrema there. */
/*              2. The result of using this option with the default boun- */
/*                 dary conditions will be identical to using DPCHIM, but */
/*                 will generally cost more compute time. */
/*                 This option is provided only to facilitate comparison */
/*                 of different switch and/or boundary conditions. */
/*           Set SWITCH nonzero to use a formula based on the 3-point */
/*              difference formula in the vicinity of switch points. */
/*           If SWITCH is positive, the interpolant on each interval */
/*              containing an extremum is controlled to not deviate from */
/*              the data by more than SWITCH*DFLOC, where DFLOC is the */
/*              maximum of the change of F on this interval and its two */
/*              immediate neighbors. */
/*           If SWITCH is negative, no such control is to be imposed. */

/*     N -- (input) number of data points.  (Error return if N.LT.2 .) */

/*     X -- (input) real*8 array of independent variable values.  The */
/*           elements of X must be strictly increasing: */
/*                X(I-1) .LT. X(I),  I = 2(1)N. */
/*           (Error return if not.) */

/*     F -- (input) real*8 array of dependent variable values to be */
/*           interpolated.  F(1+(I-1)*INCFD) is value corresponding to */
/*           X(I). */

/*     D -- (output) real*8 array of derivative values at the data */
/*           points.  These values will determine a monotone cubic */
/*           Hermite function on each subinterval on which the data */
/*           are monotonic, except possibly adjacent to switches in */
/*           monotonicity. The value corresponding to X(I) is stored in */
/*                D(1+(I-1)*INCFD),  I=1(1)N. */
/*           No other entries in D are changed. */

/*     INCFD -- (input) increment between successive values in F and D. */
/*           This argument is provided primarily for 2-D applications. */
/*           (Error return if  INCFD.LT.1 .) */

/*     WK -- (scratch) real*8 array of working storage.  The user may */
/*           wish to know that the returned values are: */
/*              WK(I)     = H(I)     = X(I+1) - X(I) ; */
/*              WK(N-1+I) = SLOPE(I) = (F(1,I+1) - F(1,I)) / H(I) */
/*           for  I = 1(1)N-1. */

/*     NWK -- (input) length of work array. */
/*           (Error return if  NWK.LT.2*(N-1) .) */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           Warning errors: */
/*              IERR = 1  if IBEG.LT.0 and D(1) had to be adjusted for */
/*                        monotonicity. */
/*              IERR = 2  if IEND.LT.0 and D(1+(N-1)*INCFD) had to be */
/*                        adjusted for monotonicity. */
/*              IERR = 3  if both of the above are true. */
/*           "Recoverable" errors: */
/*              IERR = -1  if N.LT.2 . */
/*              IERR = -2  if INCFD.LT.1 . */
/*              IERR = -3  if the X-array is not strictly increasing. */
/*              IERR = -4  if ABS(IBEG).GT.5 . */
/*              IERR = -5  if ABS(IEND).GT.5 . */
/*              IERR = -6  if both of the above are true. */
/*              IERR = -7  if NWK.LT.2*(N-1) . */
/*             (The D-array has not been changed in any of these cases.) */
/*               NOTE:  The above errors are checked in the order listed, */
/*                   and following arguments have **NOT** been validated. */

/* ***REFERENCES  1. F. N. Fritsch, Piecewise Cubic Hermite Interpolation */
/*                 Package, Report UCRL-87285, Lawrence Livermore Natio- */
/*                 nal Laboratory, July 1982.  [Poster presented at the */
/*                 SIAM 30th Anniversary Meeting, 19-23 July 1982.] */
/*               2. F. N. Fritsch and J. Butland, A method for construc- */
/*                 ting local monotone piecewise cubic interpolants, SIAM */
/*                 Journal on Scientific and Statistical Computing 5, 2 */
/*                 (June 1984), pp. 300-304. */
/*               3. F. N. Fritsch and R. E. Carlson, Monotone piecewise */
/*                 cubic interpolation, SIAM Journal on Numerical Ana- */
/*                 lysis 17, 2 (April 1980), pp. 238-246. */
/* ***ROUTINES CALLED  DPCHCE, DPCHCI, DPCHCS, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820218  DATE WRITTEN */
/*   820804  Converted to SLATEC library version. */
/*   870707  Corrected XERROR calls for d.p. name(s). */
/*   870813  Updated Reference 2. */
/*   890206  Corrected XERROR calls. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890703  Corrected category record.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   891006  Cosmetic changes to prologue.  (WRB) */
/*   891006  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   920429  Revised format and order of references.  (WRB,FNF) */
/* ***END PROLOGUE  DPCHIC */
/*  Programming notes: */

/*     To produce a single precision version, simply: */
/*        a. Change DPCHIC to PCHIC wherever it occurs, */
/*        b. Change DPCHCE to PCHCE wherever it occurs, */
/*        c. Change DPCHCI to PCHCI wherever it occurs, */
/*        d. Change DPCHCS to PCHCS wherever it occurs, */
/*        e. Change the double precision declarations to real, and */
/*        f. Change the constant  ZERO  to single precision. */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */

    /* Parameter adjustments */
    --ic;
    --vc;
    --x;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;
    f_dim1 = *incfd;
    f_offset = 1 + f_dim1;
    f -= f_offset;
    --wk;

    /* Function Body */

/*  VALIDITY-CHECK ARGUMENTS. */

/* ***FIRST EXECUTABLE STATEMENT  DPCHIC */
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

    ibeg = ic[1];
    iend = ic[2];
    *ierr = 0;
    if (abs(ibeg) > 5) {
	--(*ierr);
    }
    if (abs(iend) > 5) {
	*ierr += -2;
    }
    if (*ierr < 0) {
	goto L5004;
    }

/*  FUNCTION DEFINITION IS OK -- GO ON. */

    nless1 = *n - 1;
    if (*nwk < nless1 << 1) {
	goto L5007;
    }

/*  SET UP H AND SLOPE ARRAYS. */

    i__1 = nless1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	wk[i__] = x[i__ + 1] - x[i__];
	wk[nless1 + i__] = (f[(i__ + 1) * f_dim1 + 1] - f[i__ * f_dim1 + 1]) /
		 wk[i__];
/* L20: */
    }

/*  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION. */

    if (nless1 > 1) {
	goto L1000;
    }
    d__[d_dim1 + 1] = wk[2];
    d__[*n * d_dim1 + 1] = wk[2];
    goto L3000;

/*  NORMAL CASE  (N .GE. 3) . */

L1000:

/*  SET INTERIOR DERIVATIVES AND DEFAULT END CONDITIONS. */

/*     -------------------------------------- */
    dpchci_(n, &wk[1], &wk[*n], &d__[d_offset], incfd);
/*     -------------------------------------- */

/*  SET DERIVATIVES AT POINTS WHERE MONOTONICITY SWITCHES DIRECTION. */

    if (*switch__ == zero) {
	goto L3000;
    }
/*     ---------------------------------------------------- */
    dpchcs_(switch__, n, &wk[1], &wk[*n], &d__[d_offset], incfd, ierr);
/*     ---------------------------------------------------- */
    if (*ierr != 0) {
	goto L5008;
    }

/*  SET END CONDITIONS. */

L3000:
    if (ibeg == 0 && iend == 0) {
	goto L5000;
    }
/*     ------------------------------------------------------- */
    dpchce_(&ic[1], &vc[1], n, &x[1], &wk[1], &wk[*n], &d__[d_offset], incfd, 
	    ierr);
/*     ------------------------------------------------------- */
    if (*ierr < 0) {
	goto L5009;
    }

/*  NORMAL RETURN. */

L5000:
    return 0;

/*  ERROR RETURNS. */

L5001:
/*     N.LT.2 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "DPCHIC", "NUMBER OF DATA POINTS LESS THAN TWO", ierr, &
	    c__1, (ftnlen)6, (ftnlen)6, (ftnlen)35);
    return 0;

L5002:
/*     INCFD.LT.1 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "DPCHIC", "INCREMENT LESS THAN ONE", ierr, &c__1, (
	    ftnlen)6, (ftnlen)6, (ftnlen)23);
    return 0;

L5003:
/*     X-ARRAY NOT STRICTLY INCREASING. */
    *ierr = -3;
    xermsg_("SLATEC", "DPCHIC", "X-ARRAY NOT STRICTLY INCREASING", ierr, &
	    c__1, (ftnlen)6, (ftnlen)6, (ftnlen)31);
    return 0;

L5004:
/*     IC OUT OF RANGE RETURN. */
    *ierr += -3;
    xermsg_("SLATEC", "DPCHIC", "IC OUT OF RANGE", ierr, &c__1, (ftnlen)6, (
	    ftnlen)6, (ftnlen)15);
    return 0;

L5007:
/*     NWK .LT. 2*(N-1)  RETURN. */
    *ierr = -7;
    xermsg_("SLATEC", "DPCHIC", "WORK ARRAY TOO SMALL", ierr, &c__1, (ftnlen)
	    6, (ftnlen)6, (ftnlen)20);
    return 0;

L5008:
/*     ERROR RETURN FROM DPCHCS. */
    *ierr = -8;
    xermsg_("SLATEC", "DPCHIC", "ERROR RETURN FROM DPCHCS", ierr, &c__1, (
	    ftnlen)6, (ftnlen)6, (ftnlen)24);
    return 0;

L5009:
/*     ERROR RETURN FROM DPCHCE. */
/*   *** THIS CASE SHOULD NEVER OCCUR *** */
    *ierr = -9;
    xermsg_("SLATEC", "DPCHIC", "ERROR RETURN FROM DPCHCE", ierr, &c__1, (
	    ftnlen)6, (ftnlen)6, (ftnlen)24);
    return 0;
/* ------------- LAST LINE OF DPCHIC FOLLOWS ----------------------------- */
} /* dpchic_ */

