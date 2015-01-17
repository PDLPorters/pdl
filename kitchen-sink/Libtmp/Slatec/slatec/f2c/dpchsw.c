/* dpchsw.f -- translated by f2c (version 20060506).
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

/* DECK DPCHSW */
/* Subroutine */ int dpchsw_(doublereal *dfmax, integer *iextrm, doublereal *
	d1, doublereal *d2, doublereal *h__, doublereal *slope, integer *ierr)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal one = 1.;
    static doublereal two = 2.;
    static doublereal three = 3.;
    static doublereal fact = 100.;
    static doublereal third = .33333;

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *), sqrt(doublereal);

    /* Local variables */
    static doublereal cp, nu, phi, rho, hphi, that, sigma, small;
    extern doublereal d1mach_(integer *);
    static doublereal lambda, radcal;
    extern /* Subroutine */ int xermsg_(char *, char *, char *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  DPCHSW */
/* ***SUBSIDIARY */
/* ***PURPOSE  Limits excursion from data for DPCHCS */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***TYPE      DOUBLE PRECISION (PCHSW-S, DPCHSW-D) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/* ***DESCRIPTION */

/*         DPCHSW:  DPCHCS Switch Excursion Limiter. */

/*     Called by  DPCHCS  to adjust D1 and D2 if necessary to insure that */
/*     the extremum on this interval is not further than DFMAX from the */
/*     extreme data value. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        INTEGER  IEXTRM, IERR */
/*        DOUBLE PRECISION  DFMAX, D1, D2, H, SLOPE */

/*        CALL  DPCHSW (DFMAX, IEXTRM, D1, D2, H, SLOPE, IERR) */

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

/* ***SEE ALSO  DPCHCS */
/* ***ROUTINES CALLED  D1MACH, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820218  DATE WRITTEN */
/*   820805  Converted to SLATEC library version. */
/*   870707  Corrected XERROR calls for d.p. name(s). */
/*   870707  Replaced DATA statement for SMALL with a use of D1MACH. */
/*   870813  Minor cosmetic changes. */
/*   890206  Corrected XERROR calls. */
/*   890411  1. Added SAVE statements (Vers. 3.2). */
/*           2. Added DOUBLE PRECISION declaration for D1MACH. */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   900328  Added TYPE section.  (WRB) */
/*   910408  Updated AUTHOR and DATE WRITTEN sections in prologue.  (WRB) */
/*   920526  Eliminated possible divide by zero problem.  (FNF) */
/*   930503  Improved purpose.  (FNF) */
/* ***END PROLOGUE  DPCHSW */

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
/* ***FIRST EXECUTABLE STATEMENT  DPCHSW */
    small = fact * d1mach_(&c__4);

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
	d__1 = that;
	phi = d__1 * d__1 * ((three * rho - one) / three);

/*          CONVERT TO DISTANCE FROM F2 IF IEXTRM.NE.1 . */
	if (*iextrm != 1) {
	    phi -= rho;
	}

/*          TEST FOR EXCEEDING LIMIT, AND ADJUST ACCORDINGLY. */
	hphi = *h__ * abs(phi);
	if (hphi * abs(*d2) > *dfmax) {
/*           AT THIS POINT, HPHI.GT.0, SO DIVIDE IS OK. */
	    d__1 = *dfmax / hphi;
	    *d2 = d_sign(&d__1, d2);
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
	    if (abs(nu) > small) {
/* Computing 2nd power */
		d__1 = sigma;
		radcal = (nu - (two * rho + one)) * nu + d__1 * d__1;
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
	hphi = *h__ * abs(phi);
	if (hphi * abs(*d1) > *dfmax) {
/*           AT THIS POINT, HPHI.GT.0, SO DIVIDE IS OK. */
	    d__1 = *dfmax / hphi;
	    *d1 = d_sign(&d__1, d1);
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
    xermsg_("SLATEC", "DPCHSW", "D1 AND/OR D2 INVALID", ierr, &c__1, (ftnlen)
	    6, (ftnlen)6, (ftnlen)20);
    return 0;

L5002:
/*     NEGATIVE VALUE OF RADICAL (SHOULD NEVER OCCUR). */
    *ierr = -2;
    xermsg_("SLATEC", "DPCHSW", "NEGATIVE RADICAL", ierr, &c__1, (ftnlen)6, (
	    ftnlen)6, (ftnlen)16);
    return 0;
/* ------------- LAST LINE OF DPCHSW FOLLOWS ----------------------------- */
} /* dpchsw_ */

