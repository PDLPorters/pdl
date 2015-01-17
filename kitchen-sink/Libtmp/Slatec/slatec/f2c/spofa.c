/* spofa.f -- translated by f2c (version 20060506).
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

static integer c__1 = 1;

/* DECK SPOFA */
/* Subroutine */ int spofa_(real *a, integer *lda, integer *n, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer j, k;
    static real s, t;
    static integer jm1;
    extern doublereal sdot_(integer *, real *, integer *, real *, integer *);

/* ***BEGIN PROLOGUE  SPOFA */
/* ***PURPOSE  Factor a real symmetric positive definite matrix. */
/* ***LIBRARY   SLATEC (LINPACK) */
/* ***CATEGORY  D2B1B */
/* ***TYPE      SINGLE PRECISION (SPOFA-S, DPOFA-D, CPOFA-C) */
/* ***KEYWORDS  LINEAR ALGEBRA, LINPACK, MATRIX FACTORIZATION, */
/*             POSITIVE DEFINITE */
/* ***AUTHOR  Moler, C. B., (U. of New Mexico) */
/* ***DESCRIPTION */

/*     SPOFA factors a real symmetric positive definite matrix. */

/*     SPOFA is usually called by SPOCO, but it can be called */
/*     directly with a saving in time if  RCOND  is not needed. */
/*     (Time for SPOCO) = (1 + 18/N)*(Time for SPOFA) . */

/*     On Entry */

/*        A       REAL(LDA, N) */
/*                the symmetric matrix to be factored.  Only the */
/*                diagonal and upper triangle are used. */

/*        LDA     INTEGER */
/*                the leading dimension of the array  A . */

/*        N       INTEGER */
/*                the order of the matrix  A . */

/*     On Return */

/*        A       an upper triangular matrix  R  so that  A = TRANS(R)*R */
/*                where  TRANS(R)  is the transpose. */
/*                The strict lower triangle is unaltered. */
/*                If  INFO .NE. 0 , the factorization is not complete. */

/*        INFO    INTEGER */
/*                = 0  for normal return. */
/*                = K  signals an error condition.  The leading minor */
/*                     of order  K  is not positive definite. */

/* ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W. */
/*                 Stewart, LINPACK Users' Guide, SIAM, 1979. */
/* ***ROUTINES CALLED  SDOT */
/* ***REVISION HISTORY  (YYMMDD) */
/*   780814  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900326  Removed duplicate information from DESCRIPTION section. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  SPOFA */

/* ***FIRST EXECUTABLE STATEMENT  SPOFA */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	*info = j;
	s = 0.f;
	jm1 = j - 1;
	if (jm1 < 1) {
	    goto L20;
	}
	i__2 = jm1;
	for (k = 1; k <= i__2; ++k) {
	    i__3 = k - 1;
	    t = a[k + j * a_dim1] - sdot_(&i__3, &a[k * a_dim1 + 1], &c__1, &
		    a[j * a_dim1 + 1], &c__1);
	    t /= a[k + k * a_dim1];
	    a[k + j * a_dim1] = t;
	    s += t * t;
/* L10: */
	}
L20:
	s = a[j + j * a_dim1] - s;
	if (s <= 0.f) {
	    goto L40;
	}
	a[j + j * a_dim1] = sqrt(s);
/* L30: */
    }
    *info = 0;
L40:
    return 0;
} /* spofa_ */

