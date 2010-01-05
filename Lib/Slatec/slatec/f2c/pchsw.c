/* pchsw.f -- translated by f2c (version 20060506).
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

static integer c__4 = 4;
static integer c__1 = 1;

/* DECK PCHSW */
/* Subroutine */ int pchsw_(real *dfmax, integer *iextrm, real *d1, real *d2, 
	real *h__, real *slope, integer *ierr)
{
    /* Initialized data */

    static real zero = 0.f;
    static real one = 1.f;
    static real two = 2.f;
    static real three = 3.f;
    static real fact = 100.f;
    static real third = .33333f;

    /* System generated locals */
    real r__1;

    /* Builtin functions */
    double r_sign(real *, real *), sqrt(doublereal);

    /* Local variables */
    static real cp, nu, phi, rho, hphi, that, sigma, small;
    extern doublereal r1mach_(integer *);
    static real lambda, radcal;
    extern /* Subroutine */ int xermsg_(char *, char *, char *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  PCHSW */
/* ***SUBSIDIARY */
/* ***PURPOSE  Limits excursion from data for PCHCS */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***TYPE      SINGLE PRECISION (PCHSW-S, DPCHSW-D) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/* ***DESCRIPTION */

/*         PCHSW:  PCHCS Switch Excursion Limiter. */

/*     Called by  PCHCS  to adjust D1 and D2 if necessary to insure that */
/*     the extremum on this interval is not further than DFMAX from the */
/*     extreme data value. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        INTEGER  IEXTRM, IERR */
/*        REAL  DFMAX, D1, D2, H, SLOPE */

/*        CALL  PCHSW (DFMAX, IEXTRM, D1, D2, H, SLOPE, IERR) */

/*   Parameters: */

/*     DFMAX -- (input) maximum allowed difference between F(IEXTRM) and */
/*           the cubic determined by derivative values D1,D2.  (assumes */
/*           DFMAX.GT.0.) */

/*     IEXTRM -- (input) index of the extreme data value.  (assumes */
/*           IEXTRM = 1 or 2 .  Any value .NE.1 is treated as 2.) */

/*     D1,D2 -- (input) derivative values at the ends of the interval. */
/*           (Assumes D1*D2 .LE. 0.) */
/*          (output) may be modified if necessary to meet the restriction */
/*           imposed by DFMAX. */

/*     H -- (input) interval length.  (Assumes  H.GT.0.) */

/*     SLOPE -- (input) data slope on the interval. */

/*     IERR -- (output) error flag.  should be zero. */
/*           If IERR=-1, assumption on D1 and D2 is not satisfied. */
/*           If IERR=-2, quadratic equation locating extremum has */
/*                       negative discriminant (should never occur). */

/*    ------- */
/*    WARNING:  This routine does no validity-checking of arguments. */
/*    ------- */

/*  Fortran intrinsics used:  ABS, SIGN, SQRT. */

/* ***SEE ALSO  PCHCS */
/* ***ROUTINES CALLED  R1MACH, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820218  DATE WRITTEN */
/*   820805  Converted to SLATEC library version. */
/*   870707  Replaced DATA statement for SMALL with a use of R1MACH. */
/*   890411  1. Added SAVE statements (Vers. 3.2). */
/*           2. Added REAL R1MACH for consistency with D.P. version. */
/*   890411  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   900328  Added TYPE section.  (WRB) */
/*   910408  Updated AUTHOR and DATE WRITTEN sections in prologue.  (WRB) */
/*   920526  Eliminated possible divide by zero problem.  (FNF) */
/*   930503  Improved purpose.  (FNF) */
/* ***END PROLOGUE  PCHSW */

/* **End */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*        THIRD SHOULD BE SLIGHTLY LESS THAN 1/3. */

/*  NOTATION AND GENERAL REMARKS. */

/*     RHO IS THE RATIO OF THE DATA SLOPE TO THE DERIVATIVE BEING TESTED. */
/*     LAMBDA IS THE RATIO OF D2 TO D1. */
/*     THAT = T-HAT(RHO) IS THE NORMALIZED LOCATION OF THE EXTREMUM. */
/*     PHI IS THE NORMALIZED VALUE OF P(X)-F1 AT X = XHAT = X-HAT(RHO), */
/*           WHERE  THAT = (XHAT - X1)/H . */
/*        THAT IS, P(XHAT)-F1 = D*H*PHI,  WHERE D=D1 OR D2. */
/*     SIMILARLY,  P(XHAT)-F2 = D*H*(PHI-RHO) . */

/*      SMALL SHOULD BE A FEW ORDERS OF MAGNITUDE GREATER THAN MACHEPS. */
/* ***FIRST EXECUTABLE STATEMENT  PCHSW */
    small = fact * r1mach_(&c__4);

/*  DO MAIN CALCULATION. */

    if (*d1 == zero) {

/*        SPECIAL CASE -- D1.EQ.ZERO . */

/*          IF D2 IS ALSO ZERO, THIS ROUTINE SHOULD NOT HAVE BEEN CALLED. */
	if (*d2 == zero) {
	    goto L5001;
	}

	rho = *slope / *d2;
/*          EXTREMUM IS OUTSIDE INTERVAL WHEN RHO .GE. 1/3 . */
	if (rho >= third) {
	    goto L5000;
	}
	that = two * (three * rho - one) / (three * (two * rho - one));
/* Computing 2nd power */
	r__1 = that;
	phi = r__1 * r__1 * ((three * rho - one) / three);

/*          CONVERT TO DISTANCE FROM F2 IF IEXTRM.NE.1 . */
	if (*iextrm != 1) {
	    phi -= rho;
	}

/*          TEST FOR EXCEEDING LIMIT, AND ADJUST ACCORDINGLY. */
	hphi = *h__ * dabs(phi);
	if (hphi * dabs(*d2) > *dfmax) {
/*           AT THIS POINT, HPHI.GT.0, SO DIVIDE IS OK. */
	    r__1 = *dfmax / hphi;
	    *d2 = r_sign(&r__1, d2);
	}
    } else {

	rho = *slope / *d1;
	lambda = -(*d2) / *d1;
	if (*d2 == zero) {

/*           SPECIAL CASE -- D2.EQ.ZERO . */

/*             EXTREMUM IS OUTSIDE INTERVAL WHEN RHO .GE. 1/3 . */
	    if (rho >= third) {
		goto L5000;
	    }
	    cp = two - three * rho;
	    nu = one - two * rho;
	    that = one / (three * nu);
	} else {
	    if (lambda <= zero) {
		goto L5001;
	    }

/*           NORMAL CASE -- D1 AND D2 BOTH NONZERO, OPPOSITE SIGNS. */

	    nu = one - lambda - two * rho;
	    sigma = one - rho;
	    cp = nu + sigma;
	    if (dabs(nu) > small) {
/* Computing 2nd power */
		r__1 = sigma;
		radcal = (nu - (two * rho + one)) * nu + r__1 * r__1;
		if (radcal < zero) {
		    goto L5002;
		}
		that = (cp - sqrt(radcal)) / (three * nu);
	    } else {
		that = one / (two * sigma);
	    }
	}
	phi = that * ((nu * that - cp) * that + one);

/*          CONVERT TO DISTANCE FROM F2 IF IEXTRM.NE.1 . */
	if (*iextrm != 1) {
	    phi -= rho;
	}

/*          TEST FOR EXCEEDING LIMIT, AND ADJUST ACCORDINGLY. */
	hphi = *h__ * dabs(phi);
	if (hphi * dabs(*d1) > *dfmax) {
/*           AT THIS POINT, HPHI.GT.0, SO DIVIDE IS OK. */
	    r__1 = *dfmax / hphi;
	    *d1 = r_sign(&r__1, d1);
	    *d2 = -lambda * *d1;
	}
    }

/*  NORMAL RETURN. */

L5000:
    *ierr = 0;
    return 0;

/*  ERROR RETURNS. */

L5001:
/*     D1 AND D2 BOTH ZERO, OR BOTH NONZERO AND SAME SIGN. */
    *ierr = -1;
    xermsg_("SLATEC", "PCHSW", "D1 AND/OR D2 INVALID", ierr, &c__1, (ftnlen)6,
	     (ftnlen)5, (ftnlen)20);
    return 0;

L5002:
/*     NEGATIVE VALUE OF RADICAL (SHOULD NEVER OCCUR). */
    *ierr = -2;
    xermsg_("SLATEC", "PCHSW", "NEGATIVE RADICAL", ierr, &c__1, (ftnlen)6, (
	    ftnlen)5, (ftnlen)16);
    return 0;
/* ------------- LAST LINE OF PCHSW FOLLOWS ------------------------------ */
} /* pchsw_ */

