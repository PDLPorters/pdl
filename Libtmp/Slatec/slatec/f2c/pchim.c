/* pchim.f -- translated by f2c (version 20060506).
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

/* DECK PCHIM */
/* Subroutine */ int pchim_(integer *n, real *x, real *f, real *d__, integer *
	incfd, integer *ierr)
{
    /* Initialized data */

    static real zero = 0.f;
    static real three = 3.f;

    /* System generated locals */
    integer f_dim1, f_offset, d_dim1, d_offset, i__1;
    real r__1, r__2;

    /* Local variables */
    static integer i__;
    static real h1, h2, w1, w2, del1, del2, dmin__, dmax__, hsum, drat1, 
	    drat2, dsave;
    extern doublereal pchst_(real *, real *);
    static integer nless1;
    static real hsumt3;
    extern /* Subroutine */ int xermsg_(char *, char *, char *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  PCHIM */
/* ***PURPOSE  Set derivatives needed to determine a monotone piecewise */
/*            cubic Hermite interpolant to given data.  Boundary values */
/*            are provided which are compatible with monotonicity.  The */
/*            interpolant will have an extremum at each point where mono- */
/*            tonicity switches direction.  (See PCHIC if user control is */
/*            desired over boundary or switch conditions.) */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***CATEGORY  E1A */
/* ***TYPE      SINGLE PRECISION (PCHIM-S, DPCHIM-D) */
/* ***KEYWORDS  CUBIC HERMITE INTERPOLATION, MONOTONE INTERPOLATION, */
/*             PCHIP, PIECEWISE CUBIC INTERPOLATION */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */
/* ***DESCRIPTION */

/*          PCHIM:  Piecewise Cubic Hermite Interpolation to */
/*                  Monotone data. */

/*     Sets derivatives needed to determine a monotone piecewise cubic */
/*     Hermite interpolant to the data given in X and F. */

/*     Default boundary conditions are provided which are compatible */
/*     with monotonicity.  (See PCHIC if user control of boundary con- */
/*     ditions is desired.) */

/*     If the data are only piecewise monotonic, the interpolant will */
/*     have an extremum at each point where monotonicity switches direc- */
/*     tion.  (See PCHIC if user control is desired in such cases.) */

/*     To facilitate two-dimensional applications, includes an increment */
/*     between successive values of the F- and D-arrays. */

/*     The resulting piecewise cubic Hermite function may be evaluated */
/*     by PCHFE or PCHFD. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  N, IERR */
/*        REAL  X(N), F(INCFD,N), D(INCFD,N) */

/*        CALL  PCHIM (N, X, F, D, INCFD, IERR) */

/*   Parameters: */

/*     N -- (input) number of data points.  (Error return if N.LT.2 .) */
/*           If N=2, simply does linear interpolation. */

/*     X -- (input) real array of independent variable values.  The */
/*           elements of X must be strictly increasing: */
/*                X(I-1) .LT. X(I),  I = 2(1)N. */
/*           (Error return if not.) */

/*     F -- (input) real array of dependent variable values to be inter- */
/*           polated.  F(1+(I-1)*INCFD) is value corresponding to X(I). */
/*           PCHIM is designed for monotonic data, but it will work for */
/*           any F-array.  It will force extrema at points where mono- */
/*           tonicity switches direction.  If some other treatment of */
/*           switch points is desired, PCHIC should be used instead. */
/*                                     ----- */
/*     D -- (output) real array of derivative values at the data points. */
/*           If the data are monotonic, these values will determine a */
/*           a monotone cubic Hermite function. */
/*           The value corresponding to X(I) is stored in */
/*                D(1+(I-1)*INCFD),  I=1(1)N. */
/*           No other entries in D are changed. */

/*     INCFD -- (input) increment between successive values in F and D. */
/*           This argument is provided primarily for 2-D applications. */
/*           (Error return if  INCFD.LT.1 .) */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           Warning error: */
/*              IERR.GT.0  means that IERR switches in the direction */
/*                 of monotonicity were detected. */
/*           "Recoverable" errors: */
/*              IERR = -1  if N.LT.2 . */
/*              IERR = -2  if INCFD.LT.1 . */
/*              IERR = -3  if the X-array is not strictly increasing. */
/*             (The D-array has not been changed in any of these cases.) */
/*               NOTE:  The above errors are checked in the order listed, */
/*                   and following arguments have **NOT** been validated. */

/* ***REFERENCES  1. F. N. Fritsch and J. Butland, A method for construc- */
/*                 ting local monotone piecewise cubic interpolants, SIAM */
/*                 Journal on Scientific and Statistical Computing 5, 2 */
/*                 (June 1984), pp. 300-304. */
/*               2. F. N. Fritsch and R. E. Carlson, Monotone piecewise */
/*                 cubic interpolation, SIAM Journal on Numerical Ana- */
/*                 lysis 17, 2 (April 1980), pp. 238-246. */
/* ***ROUTINES CALLED  PCHST, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   811103  DATE WRITTEN */
/*   820201  1. Introduced  PCHST  to reduce possible over/under- */
/*             flow problems. */
/*           2. Rearranged derivative formula for same reason. */
/*   820602  1. Modified end conditions to be continuous functions */
/*             of data when monotonicity switches in next interval. */
/*           2. Modified formulas so end conditions are less prone */
/*             of over/underflow problems. */
/*   820803  Minor cosmetic changes for release 1. */
/*   870813  Updated Reference 1. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890703  Corrected category record.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   920429  Revised format and order of references.  (WRB,FNF) */
/* ***END PROLOGUE  PCHIM */
/*  Programming notes: */

/*     1. The function  PCHST(ARG1,ARG2)  is assumed to return zero if */
/*        either argument is zero, +1 if they are of the same sign, and */
/*        -1 if they are of opposite sign. */
/*     2. To produce a double precision version, simply: */
/*        a. Change PCHIM to DPCHIM wherever it occurs, */
/*        b. Change PCHST to DPCHST wherever it occurs, */
/*        c. Change all references to the Fortran intrinsics to their */
/*           double precision equivalents, */
/*        d. Change the real declarations to double precision, and */
/*        e. Change the constants ZERO and THREE to double precision. */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */

    /* Parameter adjustments */
    --x;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;
    f_dim1 = *incfd;
    f_offset = 1 + f_dim1;
    f -= f_offset;

    /* Function Body */

/*  VALIDITY-CHECK ARGUMENTS. */

/* ***FIRST EXECUTABLE STATEMENT  PCHIM */
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

    *ierr = 0;
    nless1 = *n - 1;
    h1 = x[2] - x[1];
    del1 = (f[(f_dim1 << 1) + 1] - f[f_dim1 + 1]) / h1;
    dsave = del1;

/*  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION. */

    if (nless1 > 1) {
	goto L10;
    }
    d__[d_dim1 + 1] = del1;
    d__[*n * d_dim1 + 1] = del1;
    goto L5000;

/*  NORMAL CASE  (N .GE. 3). */

L10:
    h2 = x[3] - x[2];
    del2 = (f[f_dim1 * 3 + 1] - f[(f_dim1 << 1) + 1]) / h2;

/*  SET D(1) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE */
/*     SHAPE-PRESERVING. */

    hsum = h1 + h2;
    w1 = (h1 + hsum) / hsum;
    w2 = -h1 / hsum;
    d__[d_dim1 + 1] = w1 * del1 + w2 * del2;
    if (pchst_(&d__[d_dim1 + 1], &del1) <= zero) {
	d__[d_dim1 + 1] = zero;
    } else if (pchst_(&del1, &del2) < zero) {
/*        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES. */
	dmax__ = three * del1;
	if ((r__1 = d__[d_dim1 + 1], dabs(r__1)) > dabs(dmax__)) {
	    d__[d_dim1 + 1] = dmax__;
	}
    }

/*  LOOP THROUGH INTERIOR POINTS. */

    i__1 = nless1;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (i__ == 2) {
	    goto L40;
	}

	h1 = h2;
	h2 = x[i__ + 1] - x[i__];
	hsum = h1 + h2;
	del1 = del2;
	del2 = (f[(i__ + 1) * f_dim1 + 1] - f[i__ * f_dim1 + 1]) / h2;
L40:

/*        SET D(I)=0 UNLESS DATA ARE STRICTLY MONOTONIC. */

	d__[i__ * d_dim1 + 1] = zero;
	if ((r__1 = pchst_(&del1, &del2)) < 0.f) {
	    goto L42;
	} else if (r__1 == 0) {
	    goto L41;
	} else {
	    goto L45;
	}

/*        COUNT NUMBER OF CHANGES IN DIRECTION OF MONOTONICITY. */

L41:
	if (del2 == zero) {
	    goto L50;
	}
	if (pchst_(&dsave, &del2) < zero) {
	    ++(*ierr);
	}
	dsave = del2;
	goto L50;

L42:
	++(*ierr);
	dsave = del2;
	goto L50;

/*        USE BRODLIE MODIFICATION OF BUTLAND FORMULA. */

L45:
	hsumt3 = hsum + hsum + hsum;
	w1 = (hsum + h1) / hsumt3;
	w2 = (hsum + h2) / hsumt3;
/* Computing MAX */
	r__1 = dabs(del1), r__2 = dabs(del2);
	dmax__ = dmax(r__1,r__2);
/* Computing MIN */
	r__1 = dabs(del1), r__2 = dabs(del2);
	dmin__ = dmin(r__1,r__2);
	drat1 = del1 / dmax__;
	drat2 = del2 / dmax__;
	d__[i__ * d_dim1 + 1] = dmin__ / (w1 * drat1 + w2 * drat2);

L50:
	;
    }

/*  SET D(N) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE */
/*     SHAPE-PRESERVING. */

    w1 = -h2 / hsum;
    w2 = (h2 + hsum) / hsum;
    d__[*n * d_dim1 + 1] = w1 * del1 + w2 * del2;
    if (pchst_(&d__[*n * d_dim1 + 1], &del2) <= zero) {
	d__[*n * d_dim1 + 1] = zero;
    } else if (pchst_(&del1, &del2) < zero) {
/*        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES. */
	dmax__ = three * del2;
	if ((r__1 = d__[*n * d_dim1 + 1], dabs(r__1)) > dabs(dmax__)) {
	    d__[*n * d_dim1 + 1] = dmax__;
	}
    }

/*  NORMAL RETURN. */

L5000:
    return 0;

/*  ERROR RETURNS. */

L5001:
/*     N.LT.2 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "PCHIM", "NUMBER OF DATA POINTS LESS THAN TWO", ierr, &
	    c__1, (ftnlen)6, (ftnlen)5, (ftnlen)35);
    return 0;

L5002:
/*     INCFD.LT.1 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "PCHIM", "INCREMENT LESS THAN ONE", ierr, &c__1, (
	    ftnlen)6, (ftnlen)5, (ftnlen)23);
    return 0;

L5003:
/*     X-ARRAY NOT STRICTLY INCREASING. */
    *ierr = -3;
    xermsg_("SLATEC", "PCHIM", "X-ARRAY NOT STRICTLY INCREASING", ierr, &c__1,
	     (ftnlen)6, (ftnlen)5, (ftnlen)31);
    return 0;
/* ------------- LAST LINE OF PCHIM FOLLOWS ------------------------------ */
} /* pchim_ */

