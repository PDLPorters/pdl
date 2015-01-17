/* dchfie.f -- translated by f2c (version 20060506).
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

/* DECK DCHFIE */
doublereal dchfie_(doublereal *x1, doublereal *x2, doublereal *f1, doublereal 
	*f2, doublereal *d1, doublereal *d2, doublereal *a, doublereal *b)
{
    /* Initialized data */

    static doublereal half = .5;
    static doublereal two = 2.;
    static doublereal three = 3.;
    static doublereal four = 4.;
    static doublereal six = 6.;

    /* System generated locals */
    doublereal ret_val, d__1;

    /* Local variables */
    static doublereal h__, ta1, ta2, tb1, tb2, ua1, ua2, ub1, ub2, phia1, 
	    phia2, phib1, phib2, psia1, psia2, psib1, psib2, dterm, fterm;

/* ***BEGIN PROLOGUE  DCHFIE */
/* ***SUBSIDIARY */
/* ***PURPOSE  Evaluates integral of a single cubic for DPCHIA */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***TYPE      DOUBLE PRECISION (CHFIE-S, DCHFIE-D) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/* ***DESCRIPTION */

/*          DCHFIE:  Cubic Hermite Function Integral Evaluator. */

/*     Called by  DPCHIA  to evaluate the integral of a single cubic (in */
/*     Hermite form) over an arbitrary interval (A,B). */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        DOUBLE PRECISION  X1, X2, F1, F2, D1, D2, A, B */
/*        DOUBLE PRECISION  VALUE, DCHFIE */

/*        VALUE = DCHFIE (X1, X2, F1, F2, D1, D2, A, B) */

/*   Parameters: */

/*     VALUE -- (output) value of the requested integral. */

/*     X1,X2 -- (input) endpoints if interval of definition of cubic. */

/*     F1,F2 -- (input) function values at the ends of the interval. */

/*     D1,D2 -- (input) derivative values at the ends of the interval. */

/*     A,B -- (input) endpoints of interval of integration. */

/* ***SEE ALSO  DPCHIA */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820730  DATE WRITTEN */
/*   820805  Converted to SLATEC library version. */
/*   870707  Corrected subroutine name from DCHIV to DCHFIV. */
/*   870813  Minor cosmetic changes. */
/*   890411  1. Added SAVE statements (Vers. 3.2). */
/*           2. Added SIX to DOUBLE PRECISION declaration. */
/*   890411  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   900328  Added TYPE section.  (WRB) */
/*   910408  Updated AUTHOR section in prologue.  (WRB) */
/*   930503  Corrected to set VALUE=0 when IERR.ne.0.  (FNF) */
/*   930504  Eliminated IERR and changed name DCHFIV to DCHFIE.  (FNF) */
/* ***END PROLOGUE  DCHFIE */

/*  Programming notes: */
/*  1. There is no error return from this routine because zero is */
/*     indeed the mathematically correct answer when X1.EQ.X2 . */
/* **End */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*  INITIALIZE. */


/*  VALIDITY CHECK INPUT. */

/* ***FIRST EXECUTABLE STATEMENT  DCHFIE */
    if (*x1 == *x2) {
	ret_val = 0.;
    } else {
	h__ = *x2 - *x1;
	ta1 = (*a - *x1) / h__;
	ta2 = (*x2 - *a) / h__;
	tb1 = (*b - *x1) / h__;
	tb2 = (*x2 - *b) / h__;

/* Computing 3rd power */
	d__1 = ta1;
	ua1 = d__1 * (d__1 * d__1);
	phia1 = ua1 * (two - ta1);
	psia1 = ua1 * (three * ta1 - four);
/* Computing 3rd power */
	d__1 = ta2;
	ua2 = d__1 * (d__1 * d__1);
	phia2 = ua2 * (two - ta2);
	psia2 = -ua2 * (three * ta2 - four);

/* Computing 3rd power */
	d__1 = tb1;
	ub1 = d__1 * (d__1 * d__1);
	phib1 = ub1 * (two - tb1);
	psib1 = ub1 * (three * tb1 - four);
/* Computing 3rd power */
	d__1 = tb2;
	ub2 = d__1 * (d__1 * d__1);
	phib2 = ub2 * (two - tb2);
	psib2 = -ub2 * (three * tb2 - four);

	fterm = *f1 * (phia2 - phib2) + *f2 * (phib1 - phia1);
	dterm = (*d1 * (psia2 - psib2) + *d2 * (psib1 - psia1)) * (h__ / six);

	ret_val = half * h__ * (fterm + dterm);
    }

    return ret_val;
/* ------------- LAST LINE OF DCHFIE FOLLOWS ----------------------------- */
} /* dchfie_ */

