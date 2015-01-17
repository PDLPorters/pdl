/* radb3.f -- translated by f2c (version 20060506).
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

/* DECK RADB3 */
/* Subroutine */ int radb3_(integer *ido, integer *l1, real *cc, real *ch, 
	real *wa1, real *wa2)
{
    /* System generated locals */
    integer cc_dim1, cc_offset, ch_dim1, ch_dim2, ch_offset, i__1, i__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer i__, k, ic;
    static real ci2, ci3, di2, di3, cr2, cr3, dr2, dr3, ti2, tr2;
    static integer idp2;
    static real taui, taur;

/* ***BEGIN PROLOGUE  RADB3 */
/* ***SUBSIDIARY */
/* ***PURPOSE  Calculate the fast Fourier transform of subvectors of */
/*            length three. */
/* ***LIBRARY   SLATEC (FFTPACK) */
/* ***TYPE      SINGLE PRECISION (RADB3-S) */
/* ***AUTHOR  Swarztrauber, P. N., (NCAR) */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   790601  DATE WRITTEN */
/*   830401  Modified to use SLATEC library source file format. */
/*   860115  Modified by Ron Boisvert to adhere to Fortran 77 by */
/*           (a) changing dummy array size declarations (1) to (*), */
/*           (b) changing definition of variable TAUI by using */
/*               FORTRAN intrinsic function SQRT instead of a DATA */
/*               statement. */
/*   881128  Modified by Dick Valent to meet prologue standards. */
/*   890831  Modified array declarations.  (WRB) */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900402  Added TYPE section.  (WRB) */
/* ***END PROLOGUE  RADB3 */
/* ***FIRST EXECUTABLE STATEMENT  RADB3 */
    /* Parameter adjustments */
    cc_dim1 = *ido;
    cc_offset = 1 + (cc_dim1 << 2);
    cc -= cc_offset;
    ch_dim1 = *ido;
    ch_dim2 = *l1;
    ch_offset = 1 + ch_dim1 * (1 + ch_dim2);
    ch -= ch_offset;
    --wa1;
    --wa2;

    /* Function Body */
    taur = -.5f;
    taui = sqrt(3.f) * .5f;
    i__1 = *l1;
    for (k = 1; k <= i__1; ++k) {
	tr2 = cc[*ido + (k * 3 + 2) * cc_dim1] + cc[*ido + (k * 3 + 2) * 
		cc_dim1];
	cr2 = cc[(k * 3 + 1) * cc_dim1 + 1] + taur * tr2;
	ch[(k + ch_dim2) * ch_dim1 + 1] = cc[(k * 3 + 1) * cc_dim1 + 1] + tr2;
	ci3 = taui * (cc[(k * 3 + 3) * cc_dim1 + 1] + cc[(k * 3 + 3) * 
		cc_dim1 + 1]);
	ch[(k + (ch_dim2 << 1)) * ch_dim1 + 1] = cr2 - ci3;
	ch[(k + ch_dim2 * 3) * ch_dim1 + 1] = cr2 + ci3;
/* L101: */
    }
    if (*ido == 1) {
	return 0;
    }
    idp2 = *ido + 2;
    if ((*ido - 1) / 2 < *l1) {
	goto L104;
    }
    i__1 = *l1;
    for (k = 1; k <= i__1; ++k) {
/* DIR$ IVDEP */
	i__2 = *ido;
	for (i__ = 3; i__ <= i__2; i__ += 2) {
	    ic = idp2 - i__;
	    tr2 = cc[i__ - 1 + (k * 3 + 3) * cc_dim1] + cc[ic - 1 + (k * 3 + 
		    2) * cc_dim1];
	    cr2 = cc[i__ - 1 + (k * 3 + 1) * cc_dim1] + taur * tr2;
	    ch[i__ - 1 + (k + ch_dim2) * ch_dim1] = cc[i__ - 1 + (k * 3 + 1) *
		     cc_dim1] + tr2;
	    ti2 = cc[i__ + (k * 3 + 3) * cc_dim1] - cc[ic + (k * 3 + 2) * 
		    cc_dim1];
	    ci2 = cc[i__ + (k * 3 + 1) * cc_dim1] + taur * ti2;
	    ch[i__ + (k + ch_dim2) * ch_dim1] = cc[i__ + (k * 3 + 1) * 
		    cc_dim1] + ti2;
	    cr3 = taui * (cc[i__ - 1 + (k * 3 + 3) * cc_dim1] - cc[ic - 1 + (
		    k * 3 + 2) * cc_dim1]);
	    ci3 = taui * (cc[i__ + (k * 3 + 3) * cc_dim1] + cc[ic + (k * 3 + 
		    2) * cc_dim1]);
	    dr2 = cr2 - ci3;
	    dr3 = cr2 + ci3;
	    di2 = ci2 + cr3;
	    di3 = ci2 - cr3;
	    ch[i__ - 1 + (k + (ch_dim2 << 1)) * ch_dim1] = wa1[i__ - 2] * dr2 
		    - wa1[i__ - 1] * di2;
	    ch[i__ + (k + (ch_dim2 << 1)) * ch_dim1] = wa1[i__ - 2] * di2 + 
		    wa1[i__ - 1] * dr2;
	    ch[i__ - 1 + (k + ch_dim2 * 3) * ch_dim1] = wa2[i__ - 2] * dr3 - 
		    wa2[i__ - 1] * di3;
	    ch[i__ + (k + ch_dim2 * 3) * ch_dim1] = wa2[i__ - 2] * di3 + wa2[
		    i__ - 1] * dr3;
/* L102: */
	}
/* L103: */
    }
    return 0;
L104:
    i__1 = *ido;
    for (i__ = 3; i__ <= i__1; i__ += 2) {
	ic = idp2 - i__;
/* DIR$ IVDEP */
	i__2 = *l1;
	for (k = 1; k <= i__2; ++k) {
	    tr2 = cc[i__ - 1 + (k * 3 + 3) * cc_dim1] + cc[ic - 1 + (k * 3 + 
		    2) * cc_dim1];
	    cr2 = cc[i__ - 1 + (k * 3 + 1) * cc_dim1] + taur * tr2;
	    ch[i__ - 1 + (k + ch_dim2) * ch_dim1] = cc[i__ - 1 + (k * 3 + 1) *
		     cc_dim1] + tr2;
	    ti2 = cc[i__ + (k * 3 + 3) * cc_dim1] - cc[ic + (k * 3 + 2) * 
		    cc_dim1];
	    ci2 = cc[i__ + (k * 3 + 1) * cc_dim1] + taur * ti2;
	    ch[i__ + (k + ch_dim2) * ch_dim1] = cc[i__ + (k * 3 + 1) * 
		    cc_dim1] + ti2;
	    cr3 = taui * (cc[i__ - 1 + (k * 3 + 3) * cc_dim1] - cc[ic - 1 + (
		    k * 3 + 2) * cc_dim1]);
	    ci3 = taui * (cc[i__ + (k * 3 + 3) * cc_dim1] + cc[ic + (k * 3 + 
		    2) * cc_dim1]);
	    dr2 = cr2 - ci3;
	    dr3 = cr2 + ci3;
	    di2 = ci2 + cr3;
	    di3 = ci2 - cr3;
	    ch[i__ - 1 + (k + (ch_dim2 << 1)) * ch_dim1] = wa1[i__ - 2] * dr2 
		    - wa1[i__ - 1] * di2;
	    ch[i__ + (k + (ch_dim2 << 1)) * ch_dim1] = wa1[i__ - 2] * di2 + 
		    wa1[i__ - 1] * dr2;
	    ch[i__ - 1 + (k + ch_dim2 * 3) * ch_dim1] = wa2[i__ - 2] * dr3 - 
		    wa2[i__ - 1] * di3;
	    ch[i__ + (k + ch_dim2 * 3) * ch_dim1] = wa2[i__ - 2] * di3 + wa2[
		    i__ - 1] * dr3;
/* L105: */
	}
/* L106: */
    }
    return 0;
} /* radb3_ */

