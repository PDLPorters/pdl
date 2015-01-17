/* dpchce.f -- translated by f2c (version 20060506).
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

/* DECK DPCHCE */
/* Subroutine */ int dpchce_(integer *ic, doublereal *vc, integer *n, 
	doublereal *x, doublereal *h__, doublereal *slope, doublereal *d__, 
	integer *incfd, integer *ierr)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal half = .5;
    static doublereal two = 2.;
    static doublereal three = 3.;

    /* System generated locals */
    integer d_dim1, d_offset, i__1;
    doublereal d__1, d__2;

    /* Local variables */
    static integer j, k, ibeg, iend, ierf, index;
    static doublereal stemp[3], xtemp[4];
    extern doublereal dpchdf_(integer *, doublereal *, doublereal *, integer *
	    ), dpchst_(doublereal *, doublereal *);
    extern /* Subroutine */ int xermsg_(char *, char *, char *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  DPCHCE */
/* ***SUBSIDIARY */
/* ***PURPOSE  Set boundary conditions for DPCHIC */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***TYPE      DOUBLE PRECISION (PCHCE-S, DPCHCE-D) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/* ***DESCRIPTION */

/*          DPCHCE:  DPCHIC End Derivative Setter. */

/*    Called by DPCHIC to set end derivatives as requested by the user. */
/*    It must be called after interior derivative values have been set. */
/*                      ----- */

/*    To facilitate two-dimensional applications, includes an increment */
/*    between successive values of the D-array. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  IC(2), N, IERR */
/*        DOUBLE PRECISION  VC(2), X(N), H(N), SLOPE(N), D(INCFD,N) */

/*        CALL  DPCHCE (IC, VC, N, X, H, SLOPE, D, INCFD, IERR) */

/*   Parameters: */

/*     IC -- (input) integer array of length 2 specifying desired */
/*           boundary conditions: */
/*           IC(1) = IBEG, desired condition at beginning of data. */
/*           IC(2) = IEND, desired condition at end of data. */
/*           ( see prologue to DPCHIC for details. ) */

/*     VC -- (input) real*8 array of length 2 specifying desired boundary */
/*           values.  VC(1) need be set only if IC(1) = 2 or 3 . */
/*                    VC(2) need be set only if IC(2) = 2 or 3 . */

/*     N -- (input) number of data points.  (assumes N.GE.2) */

/*     X -- (input) real*8 array of independent variable values.  (the */
/*           elements of X are assumed to be strictly increasing.) */

/*     H -- (input) real*8 array of interval lengths. */
/*     SLOPE -- (input) real*8 array of data slopes. */
/*           If the data are (X(I),Y(I)), I=1(1)N, then these inputs are: */
/*                  H(I) =  X(I+1)-X(I), */
/*              SLOPE(I) = (Y(I+1)-Y(I))/H(I),  I=1(1)N-1. */

/*     D -- (input) real*8 array of derivative values at the data points. */
/*           The value corresponding to X(I) must be stored in */
/*                D(1+(I-1)*INCFD),  I=1(1)N. */
/*          (output) the value of D at X(1) and/or X(N) is changed, if */
/*           necessary, to produce the requested boundary conditions. */
/*           no other entries in D are changed. */

/*     INCFD -- (input) increment between successive values in D. */
/*           This argument is provided primarily for 2-D applications. */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           Warning errors: */
/*              IERR = 1  if IBEG.LT.0 and D(1) had to be adjusted for */
/*                        monotonicity. */
/*              IERR = 2  if IEND.LT.0 and D(1+(N-1)*INCFD) had to be */
/*                        adjusted for monotonicity. */
/*              IERR = 3  if both of the above are true. */

/*    ------- */
/*    WARNING:  This routine does no validity-checking of arguments. */
/*    ------- */

/*  Fortran intrinsics used:  ABS. */

/* ***SEE ALSO  DPCHIC */
/* ***ROUTINES CALLED  DPCHDF, DPCHST, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820218  DATE WRITTEN */
/*   820805  Converted to SLATEC library version. */
/*   870707  Corrected XERROR calls for d.p. name(s). */
/*   890206  Corrected XERROR calls. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   900328  Added TYPE section.  (WRB) */
/*   910408  Updated AUTHOR section in prologue.  (WRB) */
/*   930503  Improved purpose.  (FNF) */
/* ***END PROLOGUE  DPCHCE */

/*  Programming notes: */
/*     1. The function DPCHST(ARG1,ARG2)  is assumed to return zero if */
/*        either argument is zero, +1 if they are of the same sign, and */
/*        -1 if they are of opposite sign. */
/*     2. One could reduce the number of arguments and amount of local */
/*        storage, at the expense of reduced code clarity, by passing in */
/*        the array WK (rather than splitting it into H and SLOPE) and */
/*        increasing its length enough to incorporate STEMP and XTEMP. */
/*     3. The two monotonicity checks only use the sufficient conditions. */
/*        Thus, it is possible (but unlikely) for a boundary condition to */
/*        be changed, even though the original interpolant was monotonic. */
/*        (At least the result is a continuous function of the data.) */
/* **End */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*  INITIALIZE. */

    /* Parameter adjustments */
    --ic;
    --vc;
    --x;
    --h__;
    --slope;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;

    /* Function Body */

/* ***FIRST EXECUTABLE STATEMENT  DPCHCE */
    ibeg = ic[1];
    iend = ic[2];
    *ierr = 0;

/*  SET TO DEFAULT BOUNDARY CONDITIONS IF N IS TOO SMALL. */

    if (abs(ibeg) > *n) {
	ibeg = 0;
    }
    if (abs(iend) > *n) {
	iend = 0;
    }

/*  TREAT BEGINNING BOUNDARY CONDITION. */

    if (ibeg == 0) {
	goto L2000;
    }
    k = abs(ibeg);
    if (k == 1) {
/*        BOUNDARY VALUE PROVIDED. */
	d__[d_dim1 + 1] = vc[1];
    } else if (k == 2) {
/*        BOUNDARY SECOND DERIVATIVE PROVIDED. */
	d__[d_dim1 + 1] = half * (three * slope[1] - d__[(d_dim1 << 1) + 1] - 
		half * vc[1] * h__[1]);
    } else if (k < 5) {
/*        USE K-POINT DERIVATIVE FORMULA. */
/*        PICK UP FIRST K POINTS, IN REVERSE ORDER. */
	i__1 = k;
	for (j = 1; j <= i__1; ++j) {
	    index = k - j + 1;
/*           INDEX RUNS FROM K DOWN TO 1. */
	    xtemp[j - 1] = x[index];
	    if (j < k) {
		stemp[j - 1] = slope[index - 1];
	    }
/* L10: */
	}
/*                 ----------------------------- */
	d__[d_dim1 + 1] = dpchdf_(&k, xtemp, stemp, &ierf);
/*                 ----------------------------- */
	if (ierf != 0) {
	    goto L5001;
	}
    } else {
/*        USE 'NOT A KNOT' CONDITION. */
	d__[d_dim1 + 1] = (three * (h__[1] * slope[2] + h__[2] * slope[1]) - 
		two * (h__[1] + h__[2]) * d__[(d_dim1 << 1) + 1] - h__[1] * 
		d__[d_dim1 * 3 + 1]) / h__[2];
    }

    if (ibeg > 0) {
	goto L2000;
    }

/*  CHECK D(1,1) FOR COMPATIBILITY WITH MONOTONICITY. */

    if (slope[1] == zero) {
	if (d__[d_dim1 + 1] != zero) {
	    d__[d_dim1 + 1] = zero;
	    ++(*ierr);
	}
    } else if (dpchst_(&d__[d_dim1 + 1], &slope[1]) < zero) {
	d__[d_dim1 + 1] = zero;
	++(*ierr);
    } else if ((d__1 = d__[d_dim1 + 1], abs(d__1)) > three * abs(slope[1])) {
	d__[d_dim1 + 1] = three * slope[1];
	++(*ierr);
    }

/*  TREAT END BOUNDARY CONDITION. */

L2000:
    if (iend == 0) {
	goto L5000;
    }
    k = abs(iend);
    if (k == 1) {
/*        BOUNDARY VALUE PROVIDED. */
	d__[*n * d_dim1 + 1] = vc[2];
    } else if (k == 2) {
/*        BOUNDARY SECOND DERIVATIVE PROVIDED. */
	d__[*n * d_dim1 + 1] = half * (three * slope[*n - 1] - d__[(*n - 1) * 
		d_dim1 + 1] + half * vc[2] * h__[*n - 1]);
    } else if (k < 5) {
/*        USE K-POINT DERIVATIVE FORMULA. */
/*        PICK UP LAST K POINTS. */
	i__1 = k;
	for (j = 1; j <= i__1; ++j) {
	    index = *n - k + j;
/*           INDEX RUNS FROM N+1-K UP TO N. */
	    xtemp[j - 1] = x[index];
	    if (j < k) {
		stemp[j - 1] = slope[index];
	    }
/* L2010: */
	}
/*                 ----------------------------- */
	d__[*n * d_dim1 + 1] = dpchdf_(&k, xtemp, stemp, &ierf);
/*                 ----------------------------- */
	if (ierf != 0) {
	    goto L5001;
	}
    } else {
/*        USE 'NOT A KNOT' CONDITION. */
	d__[*n * d_dim1 + 1] = (three * (h__[*n - 1] * slope[*n - 2] + h__[*n 
		- 2] * slope[*n - 1]) - two * (h__[*n - 1] + h__[*n - 2]) * 
		d__[(*n - 1) * d_dim1 + 1] - h__[*n - 1] * d__[(*n - 2) * 
		d_dim1 + 1]) / h__[*n - 2];
    }

    if (iend > 0) {
	goto L5000;
    }

/*  CHECK D(1,N) FOR COMPATIBILITY WITH MONOTONICITY. */

    if (slope[*n - 1] == zero) {
	if (d__[*n * d_dim1 + 1] != zero) {
	    d__[*n * d_dim1 + 1] = zero;
	    *ierr += 2;
	}
    } else if (dpchst_(&d__[*n * d_dim1 + 1], &slope[*n - 1]) < zero) {
	d__[*n * d_dim1 + 1] = zero;
	*ierr += 2;
    } else if ((d__2 = d__[*n * d_dim1 + 1], abs(d__2)) > three * (d__1 = 
	    slope[*n - 1], abs(d__1))) {
	d__[*n * d_dim1 + 1] = three * slope[*n - 1];
	*ierr += 2;
    }

/*  NORMAL RETURN. */

L5000:
    return 0;

/*  ERROR RETURN. */

L5001:
/*     ERROR RETURN FROM DPCHDF. */
/*   *** THIS CASE SHOULD NEVER OCCUR *** */
    *ierr = -1;
    xermsg_("SLATEC", "DPCHCE", "ERROR RETURN FROM DPCHDF", ierr, &c__1, (
	    ftnlen)6, (ftnlen)6, (ftnlen)24);
    return 0;
/* ------------- LAST LINE OF DPCHCE FOLLOWS ----------------------------- */
} /* dpchce_ */

