/* pchsp.f -- translated by f2c (version 20060506).
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

/* DECK PCHSP */
/* Subroutine */ int pchsp_(integer *ic, real *vc, integer *n, real *x, real *
	f, real *d__, integer *incfd, real *wk, integer *nwk, integer *ierr)
{
    /* Initialized data */

    static real zero = 0.f;
    static real half = .5f;
    static real one = 1.f;
    static real two = 2.f;
    static real three = 3.f;

    /* System generated locals */
    integer f_dim1, f_offset, d_dim1, d_offset, i__1;
    real r__1;

    /* Local variables */
    static real g;
    static integer j, nm1, ibeg, iend;
    extern doublereal pchdf_(integer *, real *, real *, integer *);
    static integer index;
    static real stemp[3], xtemp[4];
    extern /* Subroutine */ int xermsg_(char *, char *, char *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  PCHSP */
/* ***PURPOSE  Set derivatives needed to determine the Hermite represen- */
/*            tation of the cubic spline interpolant to given data, with */
/*            specified boundary conditions. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***CATEGORY  E1A */
/* ***TYPE      SINGLE PRECISION (PCHSP-S, DPCHSP-D) */
/* ***KEYWORDS  CUBIC HERMITE INTERPOLATION, PCHIP, */
/*             PIECEWISE CUBIC INTERPOLATION, SPLINE INTERPOLATION */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */
/* ***DESCRIPTION */

/*          PCHSP:   Piecewise Cubic Hermite Spline */

/*     Computes the Hermite representation of the cubic spline inter- */
/*     polant to the data given in X and F satisfying the boundary */
/*     conditions specified by IC and VC. */

/*     To facilitate two-dimensional applications, includes an increment */
/*     between successive values of the F- and D-arrays. */

/*     The resulting piecewise cubic Hermite function may be evaluated */
/*     by PCHFE or PCHFD. */

/*     NOTE:  This is a modified version of C. de Boor's cubic spline */
/*            routine CUBSPL. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  IC(2), N, NWK, IERR */
/*        REAL  VC(2), X(N), F(INCFD,N), D(INCFD,N), WK(NWK) */

/*        CALL  PCHSP (IC, VC, N, X, F, D, INCFD, WK, NWK, IERR) */

/*   Parameters: */

/*     IC -- (input) integer array of length 2 specifying desired */
/*           boundary conditions: */
/*           IC(1) = IBEG, desired condition at beginning of data. */
/*           IC(2) = IEND, desired condition at end of data. */

/*           IBEG = 0  to set D(1) so that the third derivative is con- */
/*              tinuous at X(2).  This is the "not a knot" condition */
/*              provided by de Boor's cubic spline routine CUBSPL. */
/*              < This is the default boundary condition. > */
/*           IBEG = 1  if first derivative at X(1) is given in VC(1). */
/*           IBEG = 2  if second derivative at X(1) is given in VC(1). */
/*           IBEG = 3  to use the 3-point difference formula for D(1). */
/*                     (Reverts to the default b.c. if N.LT.3 .) */
/*           IBEG = 4  to use the 4-point difference formula for D(1). */
/*                     (Reverts to the default b.c. if N.LT.4 .) */
/*          NOTES: */
/*           1. An error return is taken if IBEG is out of range. */
/*           2. For the "natural" boundary condition, use IBEG=2 and */
/*              VC(1)=0. */

/*           IEND may take on the same values as IBEG, but applied to */
/*           derivative at X(N).  In case IEND = 1 or 2, the value is */
/*           given in VC(2). */

/*          NOTES: */
/*           1. An error return is taken if IEND is out of range. */
/*           2. For the "natural" boundary condition, use IEND=2 and */
/*              VC(2)=0. */

/*     VC -- (input) real array of length 2 specifying desired boundary */
/*           values, as indicated above. */
/*           VC(1) need be set only if IC(1) = 1 or 2 . */
/*           VC(2) need be set only if IC(2) = 1 or 2 . */

/*     N -- (input) number of data points.  (Error return if N.LT.2 .) */

/*     X -- (input) real array of independent variable values.  The */
/*           elements of X must be strictly increasing: */
/*                X(I-1) .LT. X(I),  I = 2(1)N. */
/*           (Error return if not.) */

/*     F -- (input) real array of dependent variable values to be inter- */
/*           polated.  F(1+(I-1)*INCFD) is value corresponding to X(I). */

/*     D -- (output) real array of derivative values at the data points. */
/*           These values will determine the cubic spline interpolant */
/*           with the requested boundary conditions. */
/*           The value corresponding to X(I) is stored in */
/*                D(1+(I-1)*INCFD),  I=1(1)N. */
/*           No other entries in D are changed. */

/*     INCFD -- (input) increment between successive values in F and D. */
/*           This argument is provided primarily for 2-D applications. */
/*           (Error return if  INCFD.LT.1 .) */

/*     WK -- (scratch) real array of working storage. */

/*     NWK -- (input) length of work array. */
/*           (Error return if NWK.LT.2*N .) */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           "Recoverable" errors: */
/*              IERR = -1  if N.LT.2 . */
/*              IERR = -2  if INCFD.LT.1 . */
/*              IERR = -3  if the X-array is not strictly increasing. */
/*              IERR = -4  if IBEG.LT.0 or IBEG.GT.4 . */
/*              IERR = -5  if IEND.LT.0 of IEND.GT.4 . */
/*              IERR = -6  if both of the above are true. */
/*              IERR = -7  if NWK is too small. */
/*               NOTE:  The above errors are checked in the order listed, */
/*                   and following arguments have **NOT** been validated. */
/*             (The D-array has not been changed in any of these cases.) */
/*              IERR = -8  in case of trouble solving the linear system */
/*                         for the interior derivative values. */
/*             (The D-array may have been changed in this case.) */
/*             (             Do **NOT** use it!                ) */

/* ***REFERENCES  Carl de Boor, A Practical Guide to Splines, Springer- */
/*                 Verlag, New York, 1978, pp. 53-59. */
/* ***ROUTINES CALLED  PCHDF, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820503  DATE WRITTEN */
/*   820804  Converted to SLATEC library version. */
/*   870707  Minor cosmetic changes to prologue. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890703  Corrected category record.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   920429  Revised format and order of references.  (WRB,FNF) */
/* ***END PROLOGUE  PCHSP */
/*  Programming notes: */

/*     To produce a double precision version, simply: */
/*        a. Change PCHSP to DPCHSP wherever it occurs, */
/*        b. Change the real declarations to double precision, and */
/*        c. Change the constants ZERO, HALF, ... to double precision. */

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
    wk -= 3;

    /* Function Body */

/*  VALIDITY-CHECK ARGUMENTS. */

/* ***FIRST EXECUTABLE STATEMENT  PCHSP */
    if (*n < 2) {
	goto L5001;
    }
    if (*incfd < 1) {
	goto L5002;
    }
    i__1 = *n;
    for (j = 2; j <= i__1; ++j) {
	if (x[j] <= x[j - 1]) {
	    goto L5003;
	}
/* L1: */
    }

    ibeg = ic[1];
    iend = ic[2];
    *ierr = 0;
    if (ibeg < 0 || ibeg > 4) {
	--(*ierr);
    }
    if (iend < 0 || iend > 4) {
	*ierr += -2;
    }
    if (*ierr < 0) {
	goto L5004;
    }

/*  FUNCTION DEFINITION IS OK -- GO ON. */

    if (*nwk < *n << 1) {
	goto L5007;
    }

/*  COMPUTE FIRST DIFFERENCES OF X SEQUENCE AND STORE IN WK(1,.). ALSO, */
/*  COMPUTE FIRST DIVIDED DIFFERENCE OF DATA AND STORE IN WK(2,.). */
    i__1 = *n;
    for (j = 2; j <= i__1; ++j) {
	wk[(j << 1) + 1] = x[j] - x[j - 1];
	wk[(j << 1) + 2] = (f[j * f_dim1 + 1] - f[(j - 1) * f_dim1 + 1]) / wk[
		(j << 1) + 1];
/* L5: */
    }

/*  SET TO DEFAULT BOUNDARY CONDITIONS IF N IS TOO SMALL. */

    if (ibeg > *n) {
	ibeg = 0;
    }
    if (iend > *n) {
	iend = 0;
    }

/*  SET UP FOR BOUNDARY CONDITIONS. */

    if (ibeg == 1 || ibeg == 2) {
	d__[d_dim1 + 1] = vc[1];
    } else if (ibeg > 2) {
/*        PICK UP FIRST IBEG POINTS, IN REVERSE ORDER. */
	i__1 = ibeg;
	for (j = 1; j <= i__1; ++j) {
	    index = ibeg - j + 1;
/*           INDEX RUNS FROM IBEG DOWN TO 1. */
	    xtemp[j - 1] = x[index];
	    if (j < ibeg) {
		stemp[j - 1] = wk[(index << 1) + 2];
	    }
/* L10: */
	}
/*                 -------------------------------- */
	d__[d_dim1 + 1] = pchdf_(&ibeg, xtemp, stemp, ierr);
/*                 -------------------------------- */
	if (*ierr != 0) {
	    goto L5009;
	}
	ibeg = 1;
    }

    if (iend == 1 || iend == 2) {
	d__[*n * d_dim1 + 1] = vc[2];
    } else if (iend > 2) {
/*        PICK UP LAST IEND POINTS. */
	i__1 = iend;
	for (j = 1; j <= i__1; ++j) {
	    index = *n - iend + j;
/*           INDEX RUNS FROM N+1-IEND UP TO N. */
	    xtemp[j - 1] = x[index];
	    if (j < iend) {
		stemp[j - 1] = wk[(index + 1 << 1) + 2];
	    }
/* L15: */
	}
/*                 -------------------------------- */
	d__[*n * d_dim1 + 1] = pchdf_(&iend, xtemp, stemp, ierr);
/*                 -------------------------------- */
	if (*ierr != 0) {
	    goto L5009;
	}
	iend = 1;
    }

/* --------------------( BEGIN CODING FROM CUBSPL )-------------------- */

/*  **** A TRIDIAGONAL LINEAR SYSTEM FOR THE UNKNOWN SLOPES S(J) OF */
/*  F  AT X(J), J=1,...,N, IS GENERATED AND THEN SOLVED BY GAUSS ELIM- */
/*  INATION, WITH S(J) ENDING UP IN D(1,J), ALL J. */
/*     WK(1,.) AND WK(2,.) ARE USED FOR TEMPORARY STORAGE. */

/*  CONSTRUCT FIRST EQUATION FROM FIRST BOUNDARY CONDITION, OF THE FORM */
/*             WK(2,1)*S(1) + WK(1,1)*S(2) = D(1,1) */

    if (ibeg == 0) {
	if (*n == 2) {
/*           NO CONDITION AT LEFT END AND N = 2. */
	    wk[4] = one;
	    wk[3] = one;
	    d__[d_dim1 + 1] = two * wk[6];
	} else {
/*           NOT-A-KNOT CONDITION AT LEFT END AND N .GT. 2. */
	    wk[4] = wk[7];
	    wk[3] = wk[5] + wk[7];
/* Computing 2nd power */
	    r__1 = wk[5];
	    d__[d_dim1 + 1] = ((wk[5] + two * wk[3]) * wk[6] * wk[7] + r__1 * 
		    r__1 * wk[8]) / wk[3];
	}
    } else if (ibeg == 1) {
/*        SLOPE PRESCRIBED AT LEFT END. */
	wk[4] = one;
	wk[3] = zero;
    } else {
/*        SECOND DERIVATIVE PRESCRIBED AT LEFT END. */
	wk[4] = two;
	wk[3] = one;
	d__[d_dim1 + 1] = three * wk[6] - half * wk[5] * d__[d_dim1 + 1];
    }

/*  IF THERE ARE INTERIOR KNOTS, GENERATE THE CORRESPONDING EQUATIONS AND */
/*  CARRY OUT THE FORWARD PASS OF GAUSS ELIMINATION, AFTER WHICH THE J-TH */
/*  EQUATION READS    WK(2,J)*S(J) + WK(1,J)*S(J+1) = D(1,J). */

    nm1 = *n - 1;
    if (nm1 > 1) {
	i__1 = nm1;
	for (j = 2; j <= i__1; ++j) {
	    if (wk[(j - 1 << 1) + 2] == zero) {
		goto L5008;
	    }
	    g = -wk[(j + 1 << 1) + 1] / wk[(j - 1 << 1) + 2];
	    d__[j * d_dim1 + 1] = g * d__[(j - 1) * d_dim1 + 1] + three * (wk[
		    (j << 1) + 1] * wk[(j + 1 << 1) + 2] + wk[(j + 1 << 1) + 
		    1] * wk[(j << 1) + 2]);
	    wk[(j << 1) + 2] = g * wk[(j - 1 << 1) + 1] + two * (wk[(j << 1) 
		    + 1] + wk[(j + 1 << 1) + 1]);
/* L20: */
	}
    }

/*  CONSTRUCT LAST EQUATION FROM SECOND BOUNDARY CONDITION, OF THE FORM */
/*           (-G*WK(2,N-1))*S(N-1) + WK(2,N)*S(N) = D(1,N) */

/*     IF SLOPE IS PRESCRIBED AT RIGHT END, ONE CAN GO DIRECTLY TO BACK- */
/*     SUBSTITUTION, SINCE ARRAYS HAPPEN TO BE SET UP JUST RIGHT FOR IT */
/*     AT THIS POINT. */
    if (iend == 1) {
	goto L30;
    }

    if (iend == 0) {
	if (*n == 2 && ibeg == 0) {
/*           NOT-A-KNOT AT RIGHT ENDPOINT AND AT LEFT ENDPOINT AND N = 2. */
	    d__[(d_dim1 << 1) + 1] = wk[6];
	    goto L30;
	} else if (*n == 2 || *n == 3 && ibeg == 0) {
/*           EITHER (N=3 AND NOT-A-KNOT ALSO AT LEFT) OR (N=2 AND *NOT* */
/*           NOT-A-KNOT AT LEFT END POINT). */
	    d__[*n * d_dim1 + 1] = two * wk[(*n << 1) + 2];
	    wk[(*n << 1) + 2] = one;
	    if (wk[(*n - 1 << 1) + 2] == zero) {
		goto L5008;
	    }
	    g = -one / wk[(*n - 1 << 1) + 2];
	} else {
/*           NOT-A-KNOT AND N .GE. 3, AND EITHER N.GT.3 OR  ALSO NOT-A- */
/*           KNOT AT LEFT END POINT. */
	    g = wk[(*n - 1 << 1) + 1] + wk[(*n << 1) + 1];
/*           DO NOT NEED TO CHECK FOLLOWING DENOMINATORS (X-DIFFERENCES). */
/* Computing 2nd power */
	    r__1 = wk[(*n << 1) + 1];
	    d__[*n * d_dim1 + 1] = ((wk[(*n << 1) + 1] + two * g) * wk[(*n << 
		    1) + 2] * wk[(*n - 1 << 1) + 1] + r__1 * r__1 * (f[(*n - 
		    1) * f_dim1 + 1] - f[(*n - 2) * f_dim1 + 1]) / wk[(*n - 1 
		    << 1) + 1]) / g;
	    if (wk[(*n - 1 << 1) + 2] == zero) {
		goto L5008;
	    }
	    g = -g / wk[(*n - 1 << 1) + 2];
	    wk[(*n << 1) + 2] = wk[(*n - 1 << 1) + 1];
	}
    } else {
/*        SECOND DERIVATIVE PRESCRIBED AT RIGHT ENDPOINT. */
	d__[*n * d_dim1 + 1] = three * wk[(*n << 1) + 2] + half * wk[(*n << 1)
		 + 1] * d__[*n * d_dim1 + 1];
	wk[(*n << 1) + 2] = two;
	if (wk[(*n - 1 << 1) + 2] == zero) {
	    goto L5008;
	}
	g = -one / wk[(*n - 1 << 1) + 2];
    }

/*  COMPLETE FORWARD PASS OF GAUSS ELIMINATION. */

    wk[(*n << 1) + 2] = g * wk[(*n - 1 << 1) + 1] + wk[(*n << 1) + 2];
    if (wk[(*n << 1) + 2] == zero) {
	goto L5008;
    }
    d__[*n * d_dim1 + 1] = (g * d__[(*n - 1) * d_dim1 + 1] + d__[*n * d_dim1 
	    + 1]) / wk[(*n << 1) + 2];

/*  CARRY OUT BACK SUBSTITUTION */

L30:
    for (j = nm1; j >= 1; --j) {
	if (wk[(j << 1) + 2] == zero) {
	    goto L5008;
	}
	d__[j * d_dim1 + 1] = (d__[j * d_dim1 + 1] - wk[(j << 1) + 1] * d__[(
		j + 1) * d_dim1 + 1]) / wk[(j << 1) + 2];
/* L40: */
    }
/* --------------------(  END  CODING FROM CUBSPL )-------------------- */

/*  NORMAL RETURN. */

    return 0;

/*  ERROR RETURNS. */

L5001:
/*     N.LT.2 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "PCHSP", "NUMBER OF DATA POINTS LESS THAN TWO", ierr, &
	    c__1, (ftnlen)6, (ftnlen)5, (ftnlen)35);
    return 0;

L5002:
/*     INCFD.LT.1 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "PCHSP", "INCREMENT LESS THAN ONE", ierr, &c__1, (
	    ftnlen)6, (ftnlen)5, (ftnlen)23);
    return 0;

L5003:
/*     X-ARRAY NOT STRICTLY INCREASING. */
    *ierr = -3;
    xermsg_("SLATEC", "PCHSP", "X-ARRAY NOT STRICTLY INCREASING", ierr, &c__1,
	     (ftnlen)6, (ftnlen)5, (ftnlen)31);
    return 0;

L5004:
/*     IC OUT OF RANGE RETURN. */
    *ierr += -3;
    xermsg_("SLATEC", "PCHSP", "IC OUT OF RANGE", ierr, &c__1, (ftnlen)6, (
	    ftnlen)5, (ftnlen)15);
    return 0;

L5007:
/*     NWK TOO SMALL RETURN. */
    *ierr = -7;
    xermsg_("SLATEC", "PCHSP", "WORK ARRAY TOO SMALL", ierr, &c__1, (ftnlen)6,
	     (ftnlen)5, (ftnlen)20);
    return 0;

L5008:
/*     SINGULAR SYSTEM. */
/*   *** THEORETICALLY, THIS CAN ONLY OCCUR IF SUCCESSIVE X-VALUES   *** */
/*   *** ARE EQUAL, WHICH SHOULD ALREADY HAVE BEEN CAUGHT (IERR=-3). *** */
    *ierr = -8;
    xermsg_("SLATEC", "PCHSP", "SINGULAR LINEAR SYSTEM", ierr, &c__1, (ftnlen)
	    6, (ftnlen)5, (ftnlen)22);
    return 0;

L5009:
/*     ERROR RETURN FROM PCHDF. */
/*   *** THIS CASE SHOULD NEVER OCCUR *** */
    *ierr = -9;
    xermsg_("SLATEC", "PCHSP", "ERROR RETURN FROM PCHDF", ierr, &c__1, (
	    ftnlen)6, (ftnlen)5, (ftnlen)23);
    return 0;
/* ------------- LAST LINE OF PCHSP FOLLOWS ------------------------------ */
} /* pchsp_ */

