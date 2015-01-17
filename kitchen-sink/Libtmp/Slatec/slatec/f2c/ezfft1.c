/* ezfft1.f -- translated by f2c (version 20060506).
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

/* DECK EZFFT1 */
/* Subroutine */ int ezfft1_(integer *n, real *wa, integer *ifac)
{
    /* Initialized data */

    static integer ntryh[4] = { 4,2,3,5 };

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    double atan(doublereal), cos(doublereal), sin(doublereal);

    /* Local variables */
    static integer i__, j, k1, l1, l2, ib, ii, nf, ip, nl, is, nq, nr;
    static real ch1, sh1;
    static integer ido, ipm;
    static real tpi, dch1, ch1h, arg1, dsh1;
    static integer nfm1;
    static real argh;
    static integer ntry;

/* ***BEGIN PROLOGUE  EZFFT1 */
/* ***SUBSIDIARY */
/* ***PURPOSE  EZFFTI calls EZFFT1 with appropriate work array */
/*            partitioning. */
/* ***LIBRARY   SLATEC (FFTPACK) */
/* ***TYPE      SINGLE PRECISION (EZFFT1-S) */
/* ***AUTHOR  Swarztrauber, P. N., (NCAR) */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   790601  DATE WRITTEN */
/*   830401  Modified to use SLATEC library source file format. */
/*   860115  Modified by Ron Boisvert to adhere to Fortran 77 by */
/*           (a) changing dummy array size declarations (1) to (*), */
/*           (b) changing references to intrinsic function FLOAT */
/*               to REAL, and */
/*           (c) changing definition of variable TPI by using */
/*               FORTRAN intrinsic function ATAN instead of a DATA */
/*               statement. */
/*   881128  Modified by Dick Valent to meet prologue standards. */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900402  Added TYPE section.  (WRB) */
/* ***END PROLOGUE  EZFFT1 */
    /* Parameter adjustments */
    --ifac;
    --wa;

    /* Function Body */
/* ***FIRST EXECUTABLE STATEMENT  EZFFT1 */
    tpi = 8.f * atan(1.f);
    nl = *n;
    nf = 0;
    j = 0;
L101:
    ++j;
    if (j - 4 <= 0) {
	goto L102;
    } else {
	goto L103;
    }
L102:
    ntry = ntryh[j - 1];
    goto L104;
L103:
    ntry += 2;
L104:
    nq = nl / ntry;
    nr = nl - ntry * nq;
    if (nr != 0) {
	goto L101;
    } else {
	goto L105;
    }
L105:
    ++nf;
    ifac[nf + 2] = ntry;
    nl = nq;
    if (ntry != 2) {
	goto L107;
    }
    if (nf == 1) {
	goto L107;
    }
    i__1 = nf;
    for (i__ = 2; i__ <= i__1; ++i__) {
	ib = nf - i__ + 2;
	ifac[ib + 2] = ifac[ib + 1];
/* L106: */
    }
    ifac[3] = 2;
L107:
    if (nl != 1) {
	goto L104;
    }
    ifac[1] = *n;
    ifac[2] = nf;
    argh = tpi / *n;
    is = 0;
    nfm1 = nf - 1;
    l1 = 1;
    if (nfm1 == 0) {
	return 0;
    }
    i__1 = nfm1;
    for (k1 = 1; k1 <= i__1; ++k1) {
	ip = ifac[k1 + 2];
	l2 = l1 * ip;
	ido = *n / l2;
	ipm = ip - 1;
	arg1 = l1 * argh;
	ch1 = 1.f;
	sh1 = 0.f;
	dch1 = cos(arg1);
	dsh1 = sin(arg1);
	i__2 = ipm;
	for (j = 1; j <= i__2; ++j) {
	    ch1h = dch1 * ch1 - dsh1 * sh1;
	    sh1 = dch1 * sh1 + dsh1 * ch1;
	    ch1 = ch1h;
	    i__ = is + 2;
	    wa[i__ - 1] = ch1;
	    wa[i__] = sh1;
	    if (ido < 5) {
		goto L109;
	    }
	    i__3 = ido;
	    for (ii = 5; ii <= i__3; ii += 2) {
		i__ += 2;
		wa[i__ - 1] = ch1 * wa[i__ - 3] - sh1 * wa[i__ - 2];
		wa[i__] = ch1 * wa[i__ - 2] + sh1 * wa[i__ - 3];
/* L108: */
	    }
L109:
	    is += ido;
/* L110: */
	}
	l1 = l2;
/* L111: */
    }
    return 0;
} /* ezfft1_ */

