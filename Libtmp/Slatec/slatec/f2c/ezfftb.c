/* ezfftb.f -- translated by f2c (version 20060506).
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

/* DECK EZFFTB */
/* Subroutine */ int ezfftb_(integer *n, real *r__, real *azero, real *a, 
	real *b, real *wsave)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, ns2;
    extern /* Subroutine */ int rfftb_(integer *, real *, real *);

/* ***BEGIN PROLOGUE  EZFFTB */
/* ***PURPOSE  A simplified real, periodic, backward fast Fourier */
/*            transform. */
/* ***LIBRARY   SLATEC (FFTPACK) */
/* ***CATEGORY  J1A1 */
/* ***TYPE      SINGLE PRECISION (EZFFTB-S) */
/* ***KEYWORDS  FFTPACK, FOURIER TRANSFORM */
/* ***AUTHOR  Swarztrauber, P. N., (NCAR) */
/* ***DESCRIPTION */

/*  Subroutine EZFFTB computes a real periodic sequence from its */
/*  Fourier coefficients (Fourier synthesis).  The transform is */
/*  defined below at Output Parameter R.  EZFFTB is a simplified */
/*  but slower version of RFFTB. */

/*  Input Parameters */

/*  N       the length of the output array R.  The method is most */
/*          efficient when N is the product of small primes. */

/*  AZERO   the constant Fourier coefficient */

/*  A,B     arrays which contain the remaining Fourier coefficients. */
/*          These arrays are not destroyed. */

/*          The length of these arrays depends on whether N is even or */
/*          odd. */

/*          If N is even, N/2    locations are required. */
/*          If N is odd, (N-1)/2 locations are required */

/*  WSAVE   a work array which must be dimensioned at least 3*N+15 */
/*          in the program that calls EZFFTB.  The WSAVE array must be */
/*          initialized by calling subroutine EZFFTI(N,WSAVE), and a */
/*          different WSAVE array must be used for each different */
/*          value of N.  This initialization does not have to be */
/*          repeated so long as N remains unchanged.  Thus subsequent */
/*          transforms can be obtained faster than the first. */
/*          The same WSAVE array can be used by EZFFTF and EZFFTB. */

/*  Output Parameters */

/*  R       if N is even, define KMAX=N/2 */
/*          if N is odd,  define KMAX=(N-1)/2 */

/*          Then for I=1,...,N */

/*               R(I)=AZERO plus the sum from K=1 to K=KMAX of */

/*               A(K)*COS(K*(I-1)*2*PI/N)+B(K)*SIN(K*(I-1)*2*PI/N) */

/*  ********************* Complex Notation ************************** */

/*          For J=1,...,N */

/*          R(J) equals the sum from K=-KMAX to K=KMAX of */

/*               C(K)*EXP(I*K*(J-1)*2*PI/N) */

/*          where */

/*               C(K) = .5*CMPLX(A(K),-B(K))   for K=1,...,KMAX */

/*               C(-K) = CONJG(C(K)) */

/*               C(0) = AZERO */

/*                    and I=SQRT(-1) */

/*  *************** Amplitude - Phase Notation *********************** */

/*          For I=1,...,N */

/*          R(I) equals AZERO plus the sum from K=1 to K=KMAX of */

/*               ALPHA(K)*COS(K*(I-1)*2*PI/N+BETA(K)) */

/*          where */

/*               ALPHA(K) = SQRT(A(K)*A(K)+B(K)*B(K)) */

/*               COS(BETA(K))=A(K)/ALPHA(K) */

/*               SIN(BETA(K))=-B(K)/ALPHA(K) */

/* ***REFERENCES  P. N. Swarztrauber, Vectorizing the FFTs, in Parallel */
/*                 Computations (G. Rodrigue, ed.), Academic Press, */
/*                 1982, pp. 51-83. */
/* ***ROUTINES CALLED  RFFTB */
/* ***REVISION HISTORY  (YYMMDD) */
/*   790601  DATE WRITTEN */
/*   830401  Modified to use SLATEC library source file format. */
/*   860115  Modified by Ron Boisvert to adhere to Fortran 77 by */
/*           changing dummy array size declarations (1) to (*) */
/*   861211  REVISION DATE from Version 3.2 */
/*   881128  Modified by Dick Valent to meet prologue standards. */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  EZFFTB */
/* ***FIRST EXECUTABLE STATEMENT  EZFFTB */
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
    r__[1] = *azero;
    return 0;
L102:
    r__[1] = *azero + a[1];
    r__[2] = *azero - a[1];
    return 0;
L103:
    ns2 = (*n - 1) / 2;
    i__1 = ns2;
    for (i__ = 1; i__ <= i__1; ++i__) {
	r__[i__ * 2] = a[i__] * .5f;
	r__[(i__ << 1) + 1] = b[i__] * -.5f;
/* L104: */
    }
    r__[1] = *azero;
    if (*n % 2 == 0) {
	r__[*n] = a[ns2 + 1];
    }
    rfftb_(n, &r__[1], &wsave[*n + 1]);
    return 0;
} /* ezfftb_ */

