/* rfftf1.f -- translated by f2c (version 20060506).
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

/* DECK RFFTF1 */
/* Subroutine */ int rfftf1_(integer *n, real *c__, real *ch, real *wa, 
	integer *ifac)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, k1, l1, l2, na, kh, nf, ip, iw, ix2, ix3, ix4, ido, 
	    idl1;
    extern /* Subroutine */ int radf2_(integer *, integer *, real *, real *, 
	    real *), radf3_(integer *, integer *, real *, real *, real *, 
	    real *), radf4_(integer *, integer *, real *, real *, real *, 
	    real *, real *), radf5_(integer *, integer *, real *, real *, 
	    real *, real *, real *, real *), radfg_(integer *, integer *, 
	    integer *, integer *, real *, real *, real *, real *, real *, 
	    real *);

/* ***BEGIN PROLOGUE  RFFTF1 */
/* ***PURPOSE  Compute the forward transform of a real, periodic sequence. */
/* ***LIBRARY   SLATEC (FFTPACK) */
/* ***CATEGORY  J1A1 */
/* ***TYPE      SINGLE PRECISION (RFFTF1-S, CFFTF1-C) */
/* ***KEYWORDS  FFTPACK, FOURIER TRANSFORM */
/* ***AUTHOR  Swarztrauber, P. N., (NCAR) */
/* ***DESCRIPTION */

/*   Subroutine RFFTF1 computes the Fourier coefficients of a real */
/*   periodic sequence (Fourier analysis).  The transform is defined */
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

/*   C       C(1) = the sum from I=1 to I=N of R(I) */

/*           If N is even set L = N/2; if N is odd set L = (N+1)/2 */

/*             then for K = 2,...,L */

/*                C(2*K-2) = the sum from I = 1 to I = N of */

/*                     C(I)*COS((K-1)*(I-1)*2*PI/N) */

/*                C(2*K-1) = the sum from I = 1 to I = N of */

/*                    -C(I)*SIN((K-1)*(I-1)*2*PI/N) */

/*           If N is even */

/*                C(N) = the sum from I = 1 to I = N of */

/*                     (-1)**(I-1)*C(I) */

/*   Notes:  This transform is unnormalized since a call of RFFTF1 */
/*           followed by a call of RFFTB1 will multiply the input */
/*           sequence by N. */

/*           WA and IFAC contain initialization calculations which must */
/*           not be destroyed between calls of subroutine RFFTF1 or */
/*           RFFTB1. */

/* ***REFERENCES  P. N. Swarztrauber, Vectorizing the FFTs, in Parallel */
/*                 Computations (G. Rodrigue, ed.), Academic Press, */
/*                 1982, pp. 51-83. */
/* ***ROUTINES CALLED  RADF2, RADF3, RADF4, RADF5, RADFG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   790601  DATE WRITTEN */
/*   830401  Modified to use SLATEC library source file format. */
/*   860115  Modified by Ron Boisvert to adhere to Fortran 77 by */
/*           changing dummy array size declarations (1) to (*). */
/*   881128  Modified by Dick Valent to meet prologue standards. */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900131  Routine changed from subsidiary to user-callable.  (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  RFFTF1 */
/* ***FIRST EXECUTABLE STATEMENT  RFFTF1 */
    /* Parameter adjustments */
    --ifac;
    --wa;
    --ch;
    --c__;

    /* Function Body */
    nf = ifac[2];
    na = 1;
    l2 = *n;
    iw = *n;
    i__1 = nf;
    for (k1 = 1; k1 <= i__1; ++k1) {
	kh = nf - k1;
	ip = ifac[kh + 3];
	l1 = l2 / ip;
	ido = *n / l2;
	idl1 = ido * l1;
	iw -= (ip - 1) * ido;
	na = 1 - na;
	if (ip != 4) {
	    goto L102;
	}
	ix2 = iw + ido;
	ix3 = ix2 + ido;
	if (na != 0) {
	    goto L101;
	}
	radf4_(&ido, &l1, &c__[1], &ch[1], &wa[iw], &wa[ix2], &wa[ix3]);
	goto L110;
L101:
	radf4_(&ido, &l1, &ch[1], &c__[1], &wa[iw], &wa[ix2], &wa[ix3]);
	goto L110;
L102:
	if (ip != 2) {
	    goto L104;
	}
	if (na != 0) {
	    goto L103;
	}
	radf2_(&ido, &l1, &c__[1], &ch[1], &wa[iw]);
	goto L110;
L103:
	radf2_(&ido, &l1, &ch[1], &c__[1], &wa[iw]);
	goto L110;
L104:
	if (ip != 3) {
	    goto L106;
	}
	ix2 = iw + ido;
	if (na != 0) {
	    goto L105;
	}
	radf3_(&ido, &l1, &c__[1], &ch[1], &wa[iw], &wa[ix2]);
	goto L110;
L105:
	radf3_(&ido, &l1, &ch[1], &c__[1], &wa[iw], &wa[ix2]);
	goto L110;
L106:
	if (ip != 5) {
	    goto L108;
	}
	ix2 = iw + ido;
	ix3 = ix2 + ido;
	ix4 = ix3 + ido;
	if (na != 0) {
	    goto L107;
	}
	radf5_(&ido, &l1, &c__[1], &ch[1], &wa[iw], &wa[ix2], &wa[ix3], &wa[
		ix4]);
	goto L110;
L107:
	radf5_(&ido, &l1, &ch[1], &c__[1], &wa[iw], &wa[ix2], &wa[ix3], &wa[
		ix4]);
	goto L110;
L108:
	if (ido == 1) {
	    na = 1 - na;
	}
	if (na != 0) {
	    goto L109;
	}
	radfg_(&ido, &ip, &l1, &idl1, &c__[1], &c__[1], &c__[1], &ch[1], &ch[
		1], &wa[iw]);
	na = 1;
	goto L110;
L109:
	radfg_(&ido, &ip, &l1, &idl1, &ch[1], &ch[1], &ch[1], &c__[1], &c__[1]
		, &wa[iw]);
	na = 0;
L110:
	l2 = l1;
/* L111: */
    }
    if (na == 1) {
	return 0;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	c__[i__] = ch[i__];
/* L112: */
    }
    return 0;
} /* rfftf1_ */

