/* dchfcm.f -- translated by f2c (version 20060506).
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

/* DECK DCHFCM */
integer dchfcm_(doublereal *d1, doublereal *d2, doublereal *delta)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal one = 1.;
    static doublereal two = 2.;
    static doublereal three = 3.;
    static doublereal four = 4.;
    static doublereal ten = 10.;

    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal a, b, phi, eps;
    static integer ismon, itrue;
    extern doublereal d1mach_(integer *);

/* ***BEGIN PROLOGUE  DCHFCM */
/* ***SUBSIDIARY */
/* ***PURPOSE  Check a single cubic for monotonicity. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***TYPE      DOUBLE PRECISION (CHFCM-S, DCHFCM-D) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/* ***DESCRIPTION */

/* *Usage: */

/*        DOUBLE PRECISION  D1, D2, DELTA */
/*        INTEGER  ISMON, DCHFCM */

/*        ISMON = DCHFCM (D1, D2, DELTA) */

/* *Arguments: */

/*     D1,D2:IN  are the derivative values at the ends of an interval. */

/*     DELTA:IN  is the data slope over that interval. */

/* *Function Return Values: */
/*     ISMON : indicates the monotonicity of the cubic segment: */
/*             ISMON = -3  if function is probably decreasing; */
/*             ISMON = -1  if function is strictly decreasing; */
/*             ISMON =  0  if function is constant; */
/*             ISMON =  1  if function is strictly increasing; */
/*             ISMON =  2  if function is non-monotonic; */
/*             ISMON =  3  if function is probably increasing. */
/*           If ABS(ISMON)=3, the derivative values are too close to the */
/*           boundary of the monotonicity region to declare monotonicity */
/*           in the presence of roundoff error. */

/* *Description: */

/*          DCHFCM:  Cubic Hermite Function -- Check Monotonicity. */

/*    Called by  DPCHCM  to determine the monotonicity properties of the */
/*    cubic with boundary derivative values D1,D2 and chord slope DELTA. */

/* *Cautions: */
/*     This is essentially the same as old DCHFMC, except that a */
/*     new output value, -3, was added February 1989.  (Formerly, -3 */
/*     and +3 were lumped together in the single value 3.)  Codes that */
/*     flag nonmonotonicity by "IF (ISMON.EQ.2)" need not be changed. */
/*     Codes that check via "IF (ISMON.GE.3)" should change the test to */
/*     "IF (IABS(ISMON).GE.3)".  Codes that declare monotonicity via */
/*     "IF (ISMON.LE.1)" should change to "IF (IABS(ISMON).LE.1)". */

/*   REFER TO  DPCHCM */

/* ***ROUTINES CALLED  D1MACH */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820518  DATE WRITTEN */
/*   820805  Converted to SLATEC library version. */
/*   831201  Changed from  ISIGN  to SIGN  to correct bug that */
/*           produced wrong sign when -1 .LT. DELTA .LT. 0 . */
/*   890206  Added SAVE statements. */
/*   890209  Added sign to returned value ISMON=3 and corrected */
/*           argument description accordingly. */
/*   890306  Added caution about changed output. */
/*   890407  Changed name from DCHFMC to DCHFCM, as requested at the */
/*           March 1989 SLATEC CML meeting, and made a few other */
/*           minor modifications necessitated by this change. */
/*   890407  Converted to new SLATEC format. */
/*   890407  Modified DESCRIPTION to LDOC format. */
/*   891214  Moved SAVE statements.  (WRB) */
/* ***END PROLOGUE  DCHFCM */

/*  Fortran intrinsics used:  DSIGN. */
/*  Other routines used:  D1MACH. */

/* ---------------------------------------------------------------------- */

/*  Programming notes: */

/*     TEN is actually a tuning parameter, which determines the width of */
/*     the fuzz around the elliptical boundary. */

/*     To produce a single precision version, simply: */
/*        a. Change DCHFCM to CHFCM wherever it occurs, */
/*        b. Change the double precision declarations to real, and */
/*        c. Change the constants ZERO, ONE, ... to single precision. */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*  INITIALIZE. */


/*        MACHINE-DEPENDENT PARAMETER -- SHOULD BE ABOUT 10*UROUND. */
/* ***FIRST EXECUTABLE STATEMENT  DCHFCM */
    eps = ten * d1mach_(&c__4);

/*  MAKE THE CHECK. */

    if (*delta == zero) {
/*        CASE OF CONSTANT DATA. */
	if (*d1 == zero && *d2 == zero) {
	    ismon = 0;
	} else {
	    ismon = 2;
	}
    } else {
/*        DATA IS NOT CONSTANT -- PICK UP SIGN. */
	itrue = (integer) d_sign(&one, delta);
	a = *d1 / *delta;
	b = *d2 / *delta;
	if (a < zero || b < zero) {
	    ismon = 2;
	} else if (a <= three - eps && b <= three - eps) {
/*           INSIDE SQUARE (0,3)X(0,3)  IMPLIES   OK. */
	    ismon = itrue;
	} else if (a > four + eps && b > four + eps) {
/*           OUTSIDE SQUARE (0,4)X(0,4)  IMPLIES   NONMONOTONIC. */
	    ismon = 2;
	} else {
/*           MUST CHECK AGAINST BOUNDARY OF ELLIPSE. */
	    a -= two;
	    b -= two;
	    phi = a * a + b * b + a * b - three;
	    if (phi < -eps) {
		ismon = itrue;
	    } else if (phi > eps) {
		ismon = 2;
	    } else {
/*              TO CLOSE TO BOUNDARY TO TELL, */
/*                  IN THE PRESENCE OF ROUND-OFF ERRORS. */
		ismon = itrue * 3;
	    }
	}
    }

/*  RETURN VALUE. */

    ret_val = ismon;
    return ret_val;
/* ------------- LAST LINE OF DCHFCM FOLLOWS ----------------------------- */
} /* dchfcm_ */

