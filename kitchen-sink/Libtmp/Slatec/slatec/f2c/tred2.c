/* tred2.f -- translated by f2c (version 20060506).
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

/* DECK TRED2 */
/* Subroutine */ int tred2_(integer *nm, integer *n, real *a, real *d__, real 
	*e, real *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3;
    real r__1;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static real f, g, h__;
    static integer i__, j, k, l;
    static real hh;
    static integer ii, jp1;
    static real scale;

/* ***BEGIN PROLOGUE  TRED2 */
/* ***PURPOSE  Reduce a real symmetric matrix to a symmetric tridiagonal */
/*            matrix using and accumulating orthogonal transformations. */
/* ***LIBRARY   SLATEC (EISPACK) */
/* ***CATEGORY  D4C1B1 */
/* ***TYPE      SINGLE PRECISION (TRED2-S) */
/* ***KEYWORDS  EIGENVALUES, EIGENVECTORS, EISPACK */
/* ***AUTHOR  Smith, B. T., et al. */
/* ***DESCRIPTION */

/*     This subroutine is a translation of the ALGOL procedure TRED2, */
/*     NUM. MATH. 11, 181-195(1968) by Martin, Reinsch, and Wilkinson. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971). */

/*     This subroutine reduces a REAL SYMMETRIC matrix to a */
/*     symmetric tridiagonal matrix using and accumulating */
/*     orthogonal similarity transformations. */

/*     On Input */

/*        NM must be set to the row dimension of the two-dimensional */
/*          array parameters, A and Z, as declared in the calling */
/*          program dimension statement.  NM is an INTEGER variable. */

/*        N is the order of the matrix A.  N is an INTEGER variable. */
/*          N must be less than or equal to NM. */

/*        A contains the real symmetric input matrix.  Only the lower */
/*          triangle of the matrix need be supplied.  A is a two- */
/*          dimensional REAL array, dimensioned A(NM,N). */

/*     On Output */

/*        D contains the diagonal elements of the symmetric tridiagonal */
/*          matrix.  D is a one-dimensional REAL array, dimensioned D(N). */

/*        E contains the subdiagonal elements of the symmetric */
/*          tridiagonal matrix in its last N-1 positions.  E(1) is set */
/*          to zero.  E is a one-dimensional REAL array, dimensioned */
/*          E(N). */

/*        Z contains the orthogonal transformation matrix produced in */
/*          the reduction.  Z is a two-dimensional REAL array, */
/*          dimensioned Z(NM,N). */

/*        A and Z may coincide.  If distinct, A is unaltered. */

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
/* ***END PROLOGUE  TRED2 */


/* ***FIRST EXECUTABLE STATEMENT  TRED2 */
    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --d__;
    --e;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = i__;
	for (j = 1; j <= i__2; ++j) {
	    z__[i__ + j * z_dim1] = a[i__ + j * a_dim1];
/* L100: */
	}
    }

    if (*n == 1) {
	goto L320;
    }
/*     .......... FOR I=N STEP -1 UNTIL 2 DO -- .......... */
    i__2 = *n;
    for (ii = 2; ii <= i__2; ++ii) {
	i__ = *n + 2 - ii;
	l = i__ - 1;
	h__ = 0.f;
	scale = 0.f;
	if (l < 2) {
	    goto L130;
	}
/*     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) .......... */
	i__1 = l;
	for (k = 1; k <= i__1; ++k) {
/* L120: */
	    scale += (r__1 = z__[i__ + k * z_dim1], dabs(r__1));
	}

	if (scale != 0.f) {
	    goto L140;
	}
L130:
	e[i__] = z__[i__ + l * z_dim1];
	goto L290;

L140:
	i__1 = l;
	for (k = 1; k <= i__1; ++k) {
	    z__[i__ + k * z_dim1] /= scale;
	    h__ += z__[i__ + k * z_dim1] * z__[i__ + k * z_dim1];
/* L150: */
	}

	f = z__[i__ + l * z_dim1];
	r__1 = sqrt(h__);
	g = -r_sign(&r__1, &f);
	e[i__] = scale * g;
	h__ -= f * g;
	z__[i__ + l * z_dim1] = f - g;
	f = 0.f;

	i__1 = l;
	for (j = 1; j <= i__1; ++j) {
	    z__[j + i__ * z_dim1] = z__[i__ + j * z_dim1] / h__;
	    g = 0.f;
/*     .......... FORM ELEMENT OF A*U .......... */
	    i__3 = j;
	    for (k = 1; k <= i__3; ++k) {
/* L180: */
		g += z__[j + k * z_dim1] * z__[i__ + k * z_dim1];
	    }

	    jp1 = j + 1;
	    if (l < jp1) {
		goto L220;
	    }

	    i__3 = l;
	    for (k = jp1; k <= i__3; ++k) {
/* L200: */
		g += z__[k + j * z_dim1] * z__[i__ + k * z_dim1];
	    }
/*     .......... FORM ELEMENT OF P .......... */
L220:
	    e[j] = g / h__;
	    f += e[j] * z__[i__ + j * z_dim1];
/* L240: */
	}

	hh = f / (h__ + h__);
/*     .......... FORM REDUCED A .......... */
	i__1 = l;
	for (j = 1; j <= i__1; ++j) {
	    f = z__[i__ + j * z_dim1];
	    g = e[j] - hh * f;
	    e[j] = g;

	    i__3 = j;
	    for (k = 1; k <= i__3; ++k) {
		z__[j + k * z_dim1] = z__[j + k * z_dim1] - f * e[k] - g * 
			z__[i__ + k * z_dim1];
/* L260: */
	    }
	}

L290:
	d__[i__] = h__;
/* L300: */
    }

L320:
    d__[1] = 0.f;
    e[1] = 0.f;
/*     .......... ACCUMULATION OF TRANSFORMATION MATRICES .......... */
    i__2 = *n;
    for (i__ = 1; i__ <= i__2; ++i__) {
	l = i__ - 1;
	if (d__[i__] == 0.f) {
	    goto L380;
	}

	i__3 = l;
	for (j = 1; j <= i__3; ++j) {
	    g = 0.f;

	    i__1 = l;
	    for (k = 1; k <= i__1; ++k) {
/* L340: */
		g += z__[i__ + k * z_dim1] * z__[k + j * z_dim1];
	    }

	    i__1 = l;
	    for (k = 1; k <= i__1; ++k) {
		z__[k + j * z_dim1] -= g * z__[k + i__ * z_dim1];
/* L360: */
	    }
	}

L380:
	d__[i__] = z__[i__ + i__ * z_dim1];
	z__[i__ + i__ * z_dim1] = 1.f;
	if (l < 1) {
	    goto L500;
	}

	i__1 = l;
	for (j = 1; j <= i__1; ++j) {
	    z__[i__ + j * z_dim1] = 0.f;
	    z__[j + i__ * z_dim1] = 0.f;
/* L400: */
	}

L500:
	;
    }

    return 0;
} /* tred2_ */

