/* dpchci.f -- translated by f2c (version 20060506).
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

/* DECK DPCHCI */
/* Subroutine */ int dpchci_(integer *n, doublereal *h__, doublereal *slope, 
	doublereal *d__, integer *incfd)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal three = 3.;

    /* System generated locals */
    integer d_dim1, d_offset, i__1;
    doublereal d__1, d__2;

    /* Local variables */
    static integer i__;
    static doublereal w1, w2, del1, del2, dmin__, dmax__, hsum, drat1, drat2;
    static integer nless1;
    static doublereal hsumt3;
    extern doublereal dpchst_(doublereal *, doublereal *);

/* ***BEGIN PROLOGUE  DPCHCI */
/* ***SUBSIDIARY */
/* ***PURPOSE  Set interior derivatives for DPCHIC */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***TYPE      DOUBLE PRECISION (PCHCI-S, DPCHCI-D) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/* ***DESCRIPTION */

/*          DPCHCI:  DPCHIC Initial Derivative Setter. */

/*    Called by DPCHIC to set derivatives needed to determine a monotone */
/*    piecewise cubic Hermite interpolant to the data. */

/*    Default boundary conditions are provided which are compatible */
/*    with monotonicity.  If the data are only piecewise monotonic, the */
/*    interpolant will have an extremum at each point where monotonicity */
/*    switches direction. */

/*    To facilitate two-dimensional applications, includes an increment */
/*    between successive values of the D-array. */

/*    The resulting piecewise cubic Hermite function should be identical */
/*    (within roundoff error) to that produced by DPCHIM. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  N */
/*        DOUBLE PRECISION  H(N), SLOPE(N), D(INCFD,N) */

/*        CALL  DPCHCI (N, H, SLOPE, D, INCFD) */

/*   Parameters: */

/*     N -- (input) number of data points. */
/*           If N=2, simply does linear interpolation. */

/*     H -- (input) real*8 array of interval lengths. */
/*     SLOPE -- (input) real*8 array of data slopes. */
/*           If the data are (X(I),Y(I)), I=1(1)N, then these inputs are: */
/*                  H(I) =  X(I+1)-X(I), */
/*              SLOPE(I) = (Y(I+1)-Y(I))/H(I),  I=1(1)N-1. */

/*     D -- (output) real*8 array of derivative values at data points. */
/*           If the data are monotonic, these values will determine a */
/*           a monotone cubic Hermite function. */
/*           The value corresponding to X(I) is stored in */
/*                D(1+(I-1)*INCFD),  I=1(1)N. */
/*           No other entries in D are changed. */

/*     INCFD -- (input) increment between successive values in D. */
/*           This argument is provided primarily for 2-D applications. */

/*    ------- */
/*    WARNING:  This routine does no validity-checking of arguments. */
/*    ------- */

/*  Fortran intrinsics used:  ABS, MAX, MIN. */

/* ***SEE ALSO  DPCHIC */
/* ***ROUTINES CALLED  DPCHST */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820218  DATE WRITTEN */
/*   820601  Modified end conditions to be continuous functions of */
/*           data when monotonicity switches in next interval. */
/*   820602  1. Modified formulas so end conditions are less prone */
/*             to over/underflow problems. */
/*           2. Minor modification to HSUM calculation. */
/*   820805  Converted to SLATEC library version. */
/*   870813  Minor cosmetic changes. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900328  Added TYPE section.  (WRB) */
/*   910408  Updated AUTHOR section in prologue.  (WRB) */
/*   930503  Improved purpose.  (FNF) */
/* ***END PROLOGUE  DPCHCI */

/*  Programming notes: */
/*     1. The function  DPCHST(ARG1,ARG2)  is assumed to return zero if */
/*        either argument is zero, +1 if they are of the same sign, and */
/*        -1 if they are of opposite sign. */
/* **End */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*  INITIALIZE. */

    /* Parameter adjustments */
    --h__;
    --slope;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;

    /* Function Body */
/* ***FIRST EXECUTABLE STATEMENT  DPCHCI */
    nless1 = *n - 1;
    del1 = slope[1];

/*  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION. */

    if (nless1 > 1) {
	goto L10;
    }
    d__[d_dim1 + 1] = del1;
    d__[*n * d_dim1 + 1] = del1;
    goto L5000;

/*  NORMAL CASE  (N .GE. 3). */

L10:
    del2 = slope[2];

/*  SET D(1) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE */
/*     SHAPE-PRESERVING. */

    hsum = h__[1] + h__[2];
    w1 = (h__[1] + hsum) / hsum;
    w2 = -h__[1] / hsum;
    d__[d_dim1 + 1] = w1 * del1 + w2 * del2;
    if (dpchst_(&d__[d_dim1 + 1], &del1) <= zero) {
	d__[d_dim1 + 1] = zero;
    } else if (dpchst_(&del1, &del2) < zero) {
/*        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES. */
	dmax__ = three * del1;
	if ((d__1 = d__[d_dim1 + 1], abs(d__1)) > abs(dmax__)) {
	    d__[d_dim1 + 1] = dmax__;
	}
    }

/*  LOOP THROUGH INTERIOR POINTS. */

    i__1 = nless1;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (i__ == 2) {
	    goto L40;
	}

	hsum = h__[i__ - 1] + h__[i__];
	del1 = del2;
	del2 = slope[i__];
L40:

/*        SET D(I)=0 UNLESS DATA ARE STRICTLY MONOTONIC. */

	d__[i__ * d_dim1 + 1] = zero;
	if (dpchst_(&del1, &del2) <= zero) {
	    goto L50;
	}

/*        USE BRODLIE MODIFICATION OF BUTLAND FORMULA. */

	hsumt3 = hsum + hsum + hsum;
	w1 = (hsum + h__[i__ - 1]) / hsumt3;
	w2 = (hsum + h__[i__]) / hsumt3;
/* Computing MAX */
	d__1 = abs(del1), d__2 = abs(del2);
	dmax__ = max(d__1,d__2);
/* Computing MIN */
	d__1 = abs(del1), d__2 = abs(del2);
	dmin__ = min(d__1,d__2);
	drat1 = del1 / dmax__;
	drat2 = del2 / dmax__;
	d__[i__ * d_dim1 + 1] = dmin__ / (w1 * drat1 + w2 * drat2);

L50:
	;
    }

/*  SET D(N) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE */
/*     SHAPE-PRESERVING. */

    w1 = -h__[*n - 1] / hsum;
    w2 = (h__[*n - 1] + hsum) / hsum;
    d__[*n * d_dim1 + 1] = w1 * del1 + w2 * del2;
    if (dpchst_(&d__[*n * d_dim1 + 1], &del2) <= zero) {
	d__[*n * d_dim1 + 1] = zero;
    } else if (dpchst_(&del1, &del2) < zero) {
/*        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES. */
	dmax__ = three * del2;
	if ((d__1 = d__[*n * d_dim1 + 1], abs(d__1)) > abs(dmax__)) {
	    d__[*n * d_dim1 + 1] = dmax__;
	}
    }

/*  NORMAL RETURN. */

L5000:
    return 0;
/* ------------- LAST LINE OF DPCHCI FOLLOWS ----------------------------- */
} /* dpchci_ */

