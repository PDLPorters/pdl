/* tql2.f -- translated by f2c (version 20060506).
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

static real c_b10 = 1.f;

/* DECK TQL2 */
/* Subroutine */ int tql2_(integer *nm, integer *n, real *d__, real *e, real *
	z__, integer *ierr)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3;
    real r__1, r__2;

    /* Builtin functions */
    double r_sign(real *, real *), sqrt(doublereal);

    /* Local variables */
    static real b, c__, f, g, h__;
    static integer i__, j, k, l, m;
    static real p, r__, s, c2, c3;
    static integer l1, l2;
    static real s2;
    static integer ii;
    static real dl1, el1;
    static integer mml;
    extern doublereal pythag_(real *, real *);

/* ***BEGIN PROLOGUE  TQL2 */
/* ***PURPOSE  Compute the eigenvalues and eigenvectors of symmetric */
/*            tridiagonal matrix. */
/* ***LIBRARY   SLATEC (EISPACK) */
/* ***CATEGORY  D4A5, D4C2A */
/* ***TYPE      SINGLE PRECISION (TQL2-S) */
/* ***KEYWORDS  EIGENVALUES, EIGENVECTORS, EISPACK */
/* ***AUTHOR  Smith, B. T., et al. */
/* ***DESCRIPTION */

/*     This subroutine is a translation of the ALGOL procedure TQL2, */
/*     NUM. MATH. 11, 293-306(1968) by Bowdler, Martin, Reinsch, and */
/*     Wilkinson. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 227-240(1971). */

/*     This subroutine finds the eigenvalues and eigenvectors */
/*     of a SYMMETRIC TRIDIAGONAL matrix by the QL method. */
/*     The eigenvectors of a FULL SYMMETRIC matrix can also */
/*     be found if  TRED2  has been used to reduce this */
/*     full matrix to tridiagonal form. */

/*     On Input */

/*        NM must be set to the row dimension of the two-dimensional */
/*          array parameter, Z, as declared in the calling program */
/*          dimension statement.  NM is an INTEGER variable. */

/*        N is the order of the matrix.  N is an INTEGER variable. */
/*          N must be less than or equal to NM. */

/*        D contains the diagonal elements of the symmetric tridiagonal */
/*          matrix.  D is a one-dimensional REAL array, dimensioned D(N). */

/*        E contains the subdiagonal elements of the symmetric */
/*          tridiagonal matrix in its last N-1 positions.  E(1) is */
/*          arbitrary.  E is a one-dimensional REAL array, dimensioned */
/*          E(N). */

/*        Z contains the transformation matrix produced in the */
/*          reduction by  TRED2, if performed.  If the eigenvectors */
/*          of the tridiagonal matrix are desired, Z must contain */
/*          the identity matrix.  Z is a two-dimensional REAL array, */
/*          dimensioned Z(NM,N). */

/*      On Output */

/*        D contains the eigenvalues in ascending order.  If an */
/*          error exit is made, the eigenvalues are correct but */
/*          unordered for indices 1, 2, ..., IERR-1. */

/*        E has been destroyed. */

/*        Z contains orthonormal eigenvectors of the symmetric */
/*          tridiagonal (or full) matrix.  If an error exit is made, */
/*          Z contains the eigenvectors associated with the stored */
/*          eigenvalues. */

/*        IERR is an INTEGER flag set to */
/*          Zero       for normal return, */
/*          J          if the J-th eigenvalue has not been */
/*                     determined after 30 iterations. */

/*     Calls PYTHAG(A,B) for sqrt(A**2 + B**2). */

/*     Questions and comments should be directed to B. S. Garbow, */
/*     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY */
/*     ------------------------------------------------------------------ */

/* ***REFERENCES  B. T. Smith, J. M. Boyle, J. J. Dongarra, B. S. Garbow, */
/*                 Y. Ikebe, V. C. Klema and C. B. Moler, Matrix Eigen- */
/*                 system Routines - EISPACK Guide, Springer-Verlag, */
/*                 1976. */
/* ***ROUTINES CALLED  PYTHAG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   760101  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  TQL2 */


/* ***FIRST EXECUTABLE STATEMENT  TQL2 */
    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --d__;
    --e;

    /* Function Body */
    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/* L100: */
	e[i__ - 1] = e[i__];
    }

    f = 0.f;
    b = 0.f;
    e[*n] = 0.f;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
	h__ = (r__1 = d__[l], dabs(r__1)) + (r__2 = e[l], dabs(r__2));
	if (b < h__) {
	    b = h__;
	}
/*     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT .......... */
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    if (b + (r__1 = e[m], dabs(r__1)) == b) {
		goto L120;
	    }
/*     .......... E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT */
/*                THROUGH THE BOTTOM OF THE LOOP .......... */
/* L110: */
	}

L120:
	if (m == l) {
	    goto L220;
	}
L130:
	if (j == 30) {
	    goto L1000;
	}
	++j;
/*     .......... FORM SHIFT .......... */
	l1 = l + 1;
	l2 = l1 + 1;
	g = d__[l];
	p = (d__[l1] - g) / (e[l] * 2.f);
	r__ = pythag_(&p, &c_b10);
	d__[l] = e[l] / (p + r_sign(&r__, &p));
	d__[l1] = e[l] * (p + r_sign(&r__, &p));
	dl1 = d__[l1];
	h__ = g - d__[l];
	if (l2 > *n) {
	    goto L145;
	}

	i__2 = *n;
	for (i__ = l2; i__ <= i__2; ++i__) {
/* L140: */
	    d__[i__] -= h__;
	}

L145:
	f += h__;
/*     .......... QL TRANSFORMATION .......... */
	p = d__[m];
	c__ = 1.f;
	c2 = c__;
	el1 = e[l1];
	s = 0.f;
	mml = m - l;
/*     .......... FOR I=M-1 STEP -1 UNTIL L DO -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    c3 = c2;
	    c2 = c__;
	    s2 = s;
	    i__ = m - ii;
	    g = c__ * e[i__];
	    h__ = c__ * p;
	    if (dabs(p) < (r__1 = e[i__], dabs(r__1))) {
		goto L150;
	    }
	    c__ = e[i__] / p;
	    r__ = sqrt(c__ * c__ + 1.f);
	    e[i__ + 1] = s * p * r__;
	    s = c__ / r__;
	    c__ = 1.f / r__;
	    goto L160;
L150:
	    c__ = p / e[i__];
	    r__ = sqrt(c__ * c__ + 1.f);
	    e[i__ + 1] = s * e[i__] * r__;
	    s = 1.f / r__;
	    c__ *= s;
L160:
	    p = c__ * d__[i__] - s * g;
	    d__[i__ + 1] = h__ + s * (c__ * g + s * d__[i__]);
/*     .......... FORM VECTOR .......... */
	    i__3 = *n;
	    for (k = 1; k <= i__3; ++k) {
		h__ = z__[k + (i__ + 1) * z_dim1];
		z__[k + (i__ + 1) * z_dim1] = s * z__[k + i__ * z_dim1] + c__ 
			* h__;
		z__[k + i__ * z_dim1] = c__ * z__[k + i__ * z_dim1] - s * h__;
/* L180: */
	    }

/* L200: */
	}

	p = -s * s2 * c3 * el1 * e[l] / dl1;
	e[l] = s * p;
	d__[l] = c__ * p;
	if (b + (r__1 = e[l], dabs(r__1)) > b) {
	    goto L130;
	}
L220:
	d__[l] += f;
/* L240: */
    }
/*     .......... ORDER EIGENVALUES AND EIGENVECTORS .......... */
    i__1 = *n;
    for (ii = 2; ii <= i__1; ++ii) {
	i__ = ii - 1;
	k = i__;
	p = d__[i__];

	i__2 = *n;
	for (j = ii; j <= i__2; ++j) {
	    if (d__[j] >= p) {
		goto L260;
	    }
	    k = j;
	    p = d__[j];
L260:
	    ;
	}

	if (k == i__) {
	    goto L300;
	}
	d__[k] = d__[i__];
	d__[i__] = p;

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    p = z__[j + i__ * z_dim1];
	    z__[j + i__ * z_dim1] = z__[j + k * z_dim1];
	    z__[j + k * z_dim1] = p;
/* L280: */
	}

L300:
	;
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO AN */
/*                EIGENVALUE AFTER 30 ITERATIONS .......... */
L1000:
    *ierr = l;
L1001:
    return 0;
} /* tql2_ */

