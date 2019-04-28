/* ezfftf.f -- translated by f2c (version 20060506).
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

/* DECK EZFFTF */
/* Subroutine */ int ezfftf_(integer *n, real *r__, real *azero, real *a, 
	real *b, real *wsave)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;
    static real cf;
    static integer ns2;
    static real cfm;
    static integer ns2m;
    extern /* Subroutine */ int rfftf_(integer *, real *, real *);

/* ***BEGIN PROLOGUE  EZFFTF */
/* ***PURPOSE  Compute a simplified real, periodic, fast Fourier forward */
/*            transform. */
/* ***LIBRARY   SLATEC (FFTPACK) */
/* ***CATEGORY  J1A1 */
/* ***TYPE      SINGLE PRECISION (EZFFTF-S) */
/* ***KEYWORDS  FFTPACK, FOURIER TRANSFORM */
/* ***AUTHOR  Swarztrauber, P. N., (NCAR) */
/* ***DESCRIPTION */

/*  Subroutine EZFFTF computes the Fourier coefficients of a real */
/*  periodic sequence (Fourier analysis).  The transform is defined */
/*  below at Output Parameters AZERO, A and B.  EZFFTF is a simplified */
/*  but slower version of RFFTF. */

/*  Input Parameters */

/*  N       the length of the array R to be transformed.  The method */
/*          is most efficient when N is the product of small primes. */

/*  R       a real array of length N which contains the sequence */
/*          to be transformed.  R is not destroyed. */


/*  WSAVE   a work array which must be dimensioned at least 3*N+15 */
/*          in the program that calls EZFFTF.  The WSAVE array must be */
/*          initialized by calling subroutine EZFFTI(N,WSAVE), and a */
/*          different WSAVE array must be used for each different */
/*          value of N.  This initialization does not have to be */
/*          repeated so long as N remains unchanged.  Thus subsequent */
/*          transforms can be obtained faster than the first. */
/*          The same WSAVE array can be used by EZFFTF and EZFFTB. */

/*  Output Parameters */

/*  AZERO   the sum from I=1 to I=N of R(I)/N */

/*  A,B     for N even B(N/2)=0. and A(N/2) is the sum from I=1 to */
/*          I=N of (-1)**(I-1)*R(I)/N */

/*          for N even define KMAX=N/2-1 */
/*          for N odd  define KMAX=(N-1)/2 */

/*          then for  K=1,...,KMAX */

/*               A(K) equals the sum from I=1 to I=N of */

/*                    2./N*R(I)*COS(K*(I-1)*2*PI/N) */

/*               B(K) equals the sum from I=1 to I=N of */

/*                    2./N*R(I)*SIN(K*(I-1)*2*PI/N) */

/* ***REFERENCES  P. N. Swarztrauber, Vectorizing the FFTs, in Parallel */
/*                 Computations (G. Rodrigue, ed.), Academic Press, */
/*                 1982, pp. 51-83. */
/* ***ROUTINES CALLED  RFFTF */
/* ***REVISION HISTORY  (YYMMDD) */
/*   790601  DATE WRITTEN */
/*   830401  Modified to use SLATEC library source file format. */
/*   860115  Modified by Ron Boisvert to adhere to Fortran 77 by */
/*           (a) changing dummy array size declarations (1) to (*), */
/*           (b) changing references to intrinsic function FLOAT */
/*               to REAL. */
/*   881128  Modified by Dick Valent to meet prologue standards. */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  EZFFTF */
/* ***FIRST EXECUTABLE STATEMENT  EZFFTF */
    /* Parameter adjustments */
    --wsave;
    --b;
    --a;
    --r__;

    /* Function Body */
    if ((i__1 = *n - 2) < 0) {
	goto L101;
    } else if (i__1 == 0) {
	goto L102;
    } else {
	goto L103;
    }
L101:
    *azero = r__[1];
    return 0;
L102:
    *azero = (r__[1] + r__[2]) * .5f;
    a[1] = (r__[1] - r__[2]) * .5f;
    return 0;
L103:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	wsave[i__] = r__[i__];
/* L104: */
    }
    rfftf_(n, &wsave[1], &wsave[*n + 1]);
    cf = 2.f / *n;
    cfm = -cf;
    *azero = cf * .5f * wsave[1];
    ns2 = (*n + 1) / 2;
    ns2m = ns2 - 1;
    i__1 = ns2m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	a[i__] = cf * wsave[i__ * 2];
	b[i__] = cfm * wsave[(i__ << 1) + 1];
/* L105: */
    }
    if (*n % 2 == 0) {
	a[ns2] = cf * .5f * wsave[*n];
    }
    return 0;
} /* ezfftf_ */

