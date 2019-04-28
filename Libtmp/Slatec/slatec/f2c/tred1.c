/* tred1.f -- translated by f2c (version 20060506).
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

/* DECK TRED1 */
/* Subroutine */ int tred1_(integer *nm, integer *n, real *a, real *d__, real 
	*e, real *e2)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    real r__1;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static real f, g, h__;
    static integer i__, j, k, l, ii, jp1;
    static real scale;

/* ***BEGIN PROLOGUE  TRED1 */
/* ***PURPOSE  Reduce a real symmetric matrix to symmetric tridiagonal */
/*            matrix using orthogonal similarity transformations. */
/* ***LIBRARY   SLATEC (EISPACK) */
/* ***CATEGORY  D4C1B1 */
/* ***TYPE      SINGLE PRECISION (TRED1-S) */
/* ***KEYWORDS  EIGENVALUES, EIGENVECTORS, EISPACK */
/* ***AUTHOR  Smith, B. T., et al. */
/* ***DESCRIPTION */

/*     This subroutine is a translation of the ALGOL procedure TRED1, */
/*     NUM. MATH. 11, 181-195(1968) by Martin, Reinsch, and Wilkinson. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971). */

/*     This subroutine reduces a REAL SYMMETRIC matrix */
/*     to a symmetric tridiagonal matrix using */
/*     orthogonal similarity transformations. */

/*     On Input */

/*        NM must be set to the row dimension of the two-dimensional */
/*          array parameter, A, as declared in the calling program */
/*          dimension statement.  NM is an INTEGER variable. */

/*        N is the order of the matrix A.  N is an INTEGER variable. */
/*          N must be less than or equal to NM. */

/*        A contains the real symmetric input matrix.  Only the lower */
/*          triangle of the matrix need be supplied.  A is a two- */
/*          dimensional REAL array, dimensioned A(NM,N). */

/*     On Output */

/*        A contains information about the orthogonal transformations */
/*          used in the reduction in its strict lower triangle.  The */
/*          full upper triangle of A is unaltered. */

/*        D contains the diagonal elements of the symmetric tridiagonal */
/*          matrix.  D is a one-dimensional REAL array, dimensioned D(N). */

/*        E contains the subdiagonal elements of the symmetric */
/*          tridiagonal matrix in its last N-1 positions.  E(1) is set */
/*          to zero.  E is a one-dimensional REAL array, dimensioned */
/*          E(N). */

/*        E2 contains the squares of the corresponding elements of E. */
/*          E2 may coincide with E if the squares are not needed. */
/*          E2 is a one-dimensional REAL array, dimensioned E2(N). */

/*     Questions and comments should be directed to B. S. Garbow, */
/*     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY */
/*     ------------------------------------------------------------------ */

/* ***REFERENCES  B. T. Smith, J. M. Boyle, J. J. Dongarra, B. S. Garbow, */
/*                 Y. Ikebe, V. C. Klema and C. B. Moler, Matrix Eigen- */
/*                 system Routines - EISPACK Guide, Springer-Verlag, */
/*                 1976. */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   760101  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  TRED1 */


/* ***FIRST EXECUTABLE STATEMENT  TRED1 */
    /* Parameter adjustments */
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --d__;
    --e;
    --e2;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L100: */
	d__[i__] = a[i__ + i__ * a_dim1];
    }
/*     .......... FOR I=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = *n + 1 - ii;
	l = i__ - 1;
	h__ = 0.f;
	scale = 0.f;
	if (l < 1) {
	    goto L130;
	}
/*     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) .......... */
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L120: */
	    scale += (r__1 = a[i__ + k * a_dim1], dabs(r__1));
	}

	if (scale != 0.f) {
	    goto L140;
	}
L130:
	e[i__] = 0.f;
	e2[i__] = 0.f;
	goto L290;

L140:
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    a[i__ + k * a_dim1] /= scale;
	    h__ += a[i__ + k * a_dim1] * a[i__ + k * a_dim1];
/* L150: */
	}

	e2[i__] = scale * scale * h__;
	f = a[i__ + l * a_dim1];
	r__1 = sqrt(h__);
	g = -r_sign(&r__1, &f);
	e[i__] = scale * g;
	h__ -= f * g;
	a[i__ + l * a_dim1] = f - g;
	if (l == 1) {
	    goto L270;
	}
	f = 0.f;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    g = 0.f;
/*     .......... FORM ELEMENT OF A*U .......... */
	    i__3 = j;
	    for (k = 1; k <= i__3; ++k) {
/* L180: */
		g += a[j + k * a_dim1] * a[i__ + k * a_dim1];
	    }

	    jp1 = j + 1;
	    if (l < jp1) {
		goto L220;
	    }

	    i__3 = l;
	    for (k = jp1; k <= i__3; ++k) {
/* L200: */
		g += a[k + j * a_dim1] * a[i__ + k * a_dim1];
	    }
/*     .......... FORM ELEMENT OF P .......... */
L220:
	    e[j] = g / h__;
	    f += e[j] * a[i__ + j * a_dim1];
/* L240: */
	}

	h__ = f / (h__ + h__);
/*     .......... FORM REDUCED A .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = a[i__ + j * a_dim1];
	    g = e[j] - h__ * f;
	    e[j] = g;

	    i__3 = j;
	    for (k = 1; k <= i__3; ++k) {
		a[j + k * a_dim1] = a[j + k * a_dim1] - f * e[k] - g * a[i__ 
			+ k * a_dim1];
/* L260: */
	    }
	}

L270:
	i__3 = l;
	for (k = 1; k <= i__3; ++k) {
/* L280: */
	    a[i__ + k * a_dim1] = scale * a[i__ + k * a_dim1];
	}

L290:
	h__ = d__[i__];
	d__[i__] = a[i__ + i__ * a_dim1];
	a[i__ + i__ * a_dim1] = h__;
/* L300: */
    }

    return 0;
} /* tred1_ */

