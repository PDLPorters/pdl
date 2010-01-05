/* pcoef.f -- translated by f2c (version 20060506).
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

/* DECK PCOEF */
/* Subroutine */ int pcoef_(integer *l, real *c__, real *tc, real *a)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, ll, nr;
    static real fac;
    static integer new__, llp1, llp2;
    static real save;
    extern /* Subroutine */ int pvalue_(integer *, integer *, real *, real *, 
	    real *, real *);

/* ***BEGIN PROLOGUE  PCOEF */
/* ***PURPOSE  Convert the POLFIT coefficients to Taylor series form. */
/* ***LIBRARY   SLATEC */
/* ***CATEGORY  K1A1A2 */
/* ***TYPE      SINGLE PRECISION (PCOEF-S, DPCOEF-D) */
/* ***KEYWORDS  CURVE FITTING, DATA FITTING, LEAST SQUARES, POLYNOMIAL FIT */
/* ***AUTHOR  Shampine, L. F., (SNLA) */
/*           Davenport, S. M., (SNLA) */
/* ***DESCRIPTION */

/*     Written BY L. F. Shampine and S. M. Davenport. */

/*     Abstract */

/*     POLFIT  computes the least squares polynomial fit of degree  L  as */
/*     a sum of orthogonal polynomials.  PCOEF  changes this fit to its */
/*     Taylor expansion about any point  C , i.e. writes the polynomial */
/*     as a sum of powers of (X-C).  Taking  C=0.  gives the polynomial */
/*     in powers of X, but a suitable non-zero  C  often leads to */
/*     polynomials which are better scaled and more accurately evaluated. */

/*     The parameters for  PCOEF  are */

/*     INPUT -- */
/*         L -      Indicates the degree of polynomial to be changed to */
/*                  its Taylor expansion.  To obtain the Taylor */
/*                  coefficients in reverse order, input  L  as the */
/*                  negative of the degree desired.  The absolute value */
/*                  of L  must be less than or equal to NDEG, the highest */
/*                  degree polynomial fitted by  POLFIT . */
/*         C -      The point about which the Taylor expansion is to be */
/*                  made. */
/*         A -      Work and output array containing values from last */
/*                  call to  POLFIT . */

/*     OUTPUT -- */
/*         TC -     Vector containing the first LL+1 Taylor coefficients */
/*                  where LL=ABS(L).  If  L.GT.0 , the coefficients are */
/*                  in the usual Taylor series order, i.e. */
/*                    P(X) = TC(1) + TC(2)*(X-C) + ... + TC(N+1)*(X-C)**N */
/*                  If L .LT. 0, the coefficients are in reverse order, */
/*                  i.e. */
/*                    P(X) = TC(1)*(X-C)**N + ... + TC(N)*(X-C) + TC(N+1) */

/* ***REFERENCES  L. F. Shampine, S. M. Davenport and R. E. Huddleston, */
/*                 Curve fitting by polynomials in one variable, Report */
/*                 SLA-74-0270, Sandia Laboratories, June 1974. */
/* ***ROUTINES CALLED  PVALUE */
/* ***REVISION HISTORY  (YYMMDD) */
/*   740601  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  PCOEF */

/* ***FIRST EXECUTABLE STATEMENT  PCOEF */
    /* Parameter adjustments */
    --a;
    --tc;

    /* Function Body */
    ll = abs(*l);
    llp1 = ll + 1;
    pvalue_(&ll, &ll, c__, &tc[1], &tc[2], &a[1]);
    if (ll < 2) {
	goto L2;
    }
    fac = 1.f;
    i__1 = llp1;
    for (i__ = 3; i__ <= i__1; ++i__) {
	fac *= i__ - 1;
/* L1: */
	tc[i__] /= fac;
    }
L2:
    if (*l >= 0) {
	goto L4;
    }
    nr = llp1 / 2;
    llp2 = ll + 2;
    i__1 = nr;
    for (i__ = 1; i__ <= i__1; ++i__) {
	save = tc[i__];
	new__ = llp2 - i__;
	tc[i__] = tc[new__];
/* L3: */
	tc[new__] = save;
    }
L4:
    return 0;
} /* pcoef_ */

