/* rfftb1.f -- translated by f2c (version 20060506).
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

/* DECK RFFTB1 */
/* Subroutine */ int rfftb1_(integer *n, real *c__, real *ch, real *wa, 
	integer *ifac)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, k1, l1, l2, na, nf, ip, iw, ix2, ix3, ix4, ido, idl1;
    extern /* Subroutine */ int radb2_(integer *, integer *, real *, real *, 
	    real *), radb3_(integer *, integer *, real *, real *, real *, 
	    real *), radb4_(integer *, integer *, real *, real *, real *, 
	    real *, real *), radb5_(integer *, integer *, real *, real *, 
	    real *, real *, real *, real *), radbg_(integer *, integer *, 
	    integer *, integer *, real *, real *, real *, real *, real *, 
	    real *);

/* ***BEGIN PROLOGUE  RFFTB1 */
/* ***PURPOSE  Compute the backward fast Fourier transform of a real */
/*            coefficient array. */
/* ***LIBRARY   SLATEC (FFTPACK) */
/* ***CATEGORY  J1A1 */
/* ***TYPE      SINGLE PRECISION (RFFTB1-S, CFFTB1-C) */
/* ***KEYWORDS  FFTPACK, FOURIER TRANSFORM */
/* ***AUTHOR  Swarztrauber, P. N., (NCAR) */
/* ***DESCRIPTION */

/*   Subroutine RFFTB1 computes the real periodic sequence from its */
/*   Fourier coefficients (Fourier synthesis).  The transform is defined */
/*   below at output parameter C. */

/*   The arrays WA and IFAC which are used by subroutine RFFTB1 must be */
/*   initialized by calling subroutine RFFTI1. */

/*   Input Arguments */

/*   N       the length of the array R to be transformed.  The method */
/*           is most efficient when N is a product of small primes. */
/*           N may change so long as different work arrays are provided. */

/*   C       a real array of length N which contains the sequence */
/*           to be transformed. */

/*   CH      a real work array of length at least N. */

/*   WA      a real work array which must be dimensioned at least N. */

/*   IFAC    an integer work array which must be dimensioned at least 15. */

/*           The WA and IFAC arrays must be initialized by calling */
/*           subroutine RFFTI1, and different WA and IFAC arrays must be */
/*           used for each different value of N.  This initialization */
/*           does not have to be repeated so long as N remains unchanged. */
/*           Thus subsequent transforms can be obtained faster than the */
/*           first.  The same WA and IFAC arrays can be used by RFFTF1 */
/*           and RFFTB1. */

/*   Output Argument */

/*   C       For N even and for I = 1,...,N */

/*                C(I) = C(1)+(-1)**(I-1)*C(N) */

/*                     plus the sum from K=2 to K=N/2 of */

/*                      2.*C(2*K-2)*COS((K-1)*(I-1)*2*PI/N) */

/*                     -2.*C(2*K-1)*SIN((K-1)*(I-1)*2*PI/N) */

/*           For N odd and for I = 1,...,N */

/*                C(I) = C(1) plus the sum from K=2 to K=(N+1)/2 of */

/*                     2.*C(2*K-2)*COS((K-1)*(I-1)*2*PI/N) */

/*                    -2.*C(2*K-1)*SIN((K-1)*(I-1)*2*PI/N) */

/*   Notes:  This transform is unnormalized since a call of RFFTF1 */
/*           followed by a call of RFFTB1 will multiply the input */
/*           sequence by N. */

/*           WA and IFAC contain initialization calculations which must */
/*           not be destroyed between calls of subroutine RFFTF1 or */
/*           RFFTB1. */

/* ***REFERENCES  P. N. Swarztrauber, Vectorizing the FFTs, in Parallel */
/*                 Computations (G. Rodrigue, ed.), Academic Press, */
/*                 1982, pp. 51-83. */
/* ***ROUTINES CALLED  RADB2, RADB3, RADB4, RADB5, RADBG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   790601  DATE WRITTEN */
/*   830401  Modified to use SLATEC library source file format. */
/*   860115  Modified by Ron Boisvert to adhere to Fortran 77 by */
/*           changing dummy array size declarations (1) to (*). */
/*   881128  Modified by Dick Valent to meet prologue standards. */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900131  Routine changed from subsidiary to user-callable.  (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  RFFTB1 */
/* ***FIRST EXECUTABLE STATEMENT  RFFTB1 */
    /* Parameter adjustments */
    --ifac;
    --wa;
    --ch;
    --c__;

    /* Function Body */
    nf = ifac[2];
    na = 0;
    l1 = 1;
    iw = 1;
    i__1 = nf;
    for (k1 = 1; k1 <= i__1; ++k1) {
	ip = ifac[k1 + 2];
	l2 = ip * l1;
	ido = *n / l2;
	idl1 = ido * l1;
	if (ip != 4) {
	    goto L103;
	}
	ix2 = iw + ido;
	ix3 = ix2 + ido;
	if (na != 0) {
	    goto L101;
	}
	radb4_(&ido, &l1, &c__[1], &ch[1], &wa[iw], &wa[ix2], &wa[ix3]);
	goto L102;
L101:
	radb4_(&ido, &l1, &ch[1], &c__[1], &wa[iw], &wa[ix2], &wa[ix3]);
L102:
	na = 1 - na;
	goto L115;
L103:
	if (ip != 2) {
	    goto L106;
	}
	if (na != 0) {
	    goto L104;
	}
	radb2_(&ido, &l1, &c__[1], &ch[1], &wa[iw]);
	goto L105;
L104:
	radb2_(&ido, &l1, &ch[1], &c__[1], &wa[iw]);
L105:
	na = 1 - na;
	goto L115;
L106:
	if (ip != 3) {
	    goto L109;
	}
	ix2 = iw + ido;
	if (na != 0) {
	    goto L107;
	}
	radb3_(&ido, &l1, &c__[1], &ch[1], &wa[iw], &wa[ix2]);
	goto L108;
L107:
	radb3_(&ido, &l1, &ch[1], &c__[1], &wa[iw], &wa[ix2]);
L108:
	na = 1 - na;
	goto L115;
L109:
	if (ip != 5) {
	    goto L112;
	}
	ix2 = iw + ido;
	ix3 = ix2 + ido;
	ix4 = ix3 + ido;
	if (na != 0) {
	    goto L110;
	}
	radb5_(&ido, &l1, &c__[1], &ch[1], &wa[iw], &wa[ix2], &wa[ix3], &wa[
		ix4]);
	goto L111;
L110:
	radb5_(&ido, &l1, &ch[1], &c__[1], &wa[iw], &wa[ix2], &wa[ix3], &wa[
		ix4]);
L111:
	na = 1 - na;
	goto L115;
L112:
	if (na != 0) {
	    goto L113;
	}
	radbg_(&ido, &ip, &l1, &idl1, &c__[1], &c__[1], &c__[1], &ch[1], &ch[
		1], &wa[iw]);
	goto L114;
L113:
	radbg_(&ido, &ip, &l1, &idl1, &ch[1], &ch[1], &ch[1], &c__[1], &c__[1]
		, &wa[iw]);
L114:
	if (ido == 1) {
	    na = 1 - na;
	}
L115:
	l1 = l2;
	iw += (ip - 1) * ido;
/* L116: */
    }
    if (na == 0) {
	return 0;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	c__[i__] = ch[i__];
/* L117: */
    }
    return 0;
} /* rfftb1_ */

