/* rs.f -- translated by f2c (version 20060506).
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

/* DECK RSFOO */
/* Subroutine */ int rsfoo_(integer *nm, integer *n, real *a, real *w, 
	integer *matz, real *z__, real *fv1, real *fv2, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset;

    /* Local variables */
    extern /* Subroutine */ int tql2_(integer *, integer *, real *, real *, 
	    real *, integer *), tred1_(integer *, integer *, real *, real *, 
	    real *, real *), tred2_(integer *, integer *, real *, real *, 
	    real *, real *), tqlrat_(integer *, real *, real *, integer *);

/* ***BEGIN PROLOGUE  RS */
/* ***PURPOSE  Compute the eigenvalues and, optionally, the eigenvectors */
/*            of a real symmetric matrix. */
/* ***LIBRARY   SLATEC (EISPACK) */
/* ***CATEGORY  D4A1 */
/* ***TYPE      SINGLE PRECISION (RS-S, CH-C) */
/* ***KEYWORDS  EIGENVALUES, EIGENVECTORS, EISPACK */
/* ***AUTHOR  Smith, B. T., et al. */
/* ***DESCRIPTION */

/*     This subroutine calls the recommended sequence of */
/*     subroutines from the eigensystem subroutine package (EISPACK) */
/*     to find the eigenvalues and eigenvectors (if desired) */
/*     of a REAL SYMMETRIC matrix. */

/*     On Input */

/*        NM must be set to the row dimension of the two-dimensional */
/*          array parameters, A and Z, as declared in the calling */
/*          program dimension statement.  NM is an INTEGER variable. */

/*        N is the order of the matrix A.  N is an INTEGER variable. */
/*          N must be less than or equal to NM. */

/*        A contains the real symmetric matrix.  A is a two-dimensional */
/*          REAL array, dimensioned A(NM,N). */

/*        MATZ is an INTEGER variable set equal to zero if only */
/*          eigenvalues are desired.  Otherwise, it is set to any */
/*          non-zero integer for both eigenvalues and eigenvectors. */

/*     On Output */

/*        A is unaltered. */

/*        W contains the eigenvalues in ascending order.  W is a one- */
/*          dimensional REAL array, dimensioned W(N). */

/*        Z contains the eigenvectors if MATZ is not zero.  The */
/*          eigenvectors are orthonormal.  Z is a two-dimensional */
/*          REAL array, dimensioned Z(NM,N). */

/*        IERR is an INTEGER flag set to */
/*          Zero       for normal return, */
/*          10*N       if N is greater than NM, */
/*          J          if the J-th eigenvalue has not been */
/*                     determined after 30 iterations. */
/*                     The eigenvalues, and eigenvectors if requested, */
/*                     should be correct for indices 1, 2, ..., IERR-1. */

/*        FV1 and FV2 are one-dimensional REAL arrays used for temporary */
/*          storage, dimensioned FV1(N) and FV2(N). */

/*     Questions and comments should be directed to B. S. Garbow, */
/*     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY */
/*     ------------------------------------------------------------------ */

/* ***REFERENCES  B. T. Smith, J. M. Boyle, J. J. Dongarra, B. S. Garbow, */
/*                 Y. Ikebe, V. C. Klema and C. B. Moler, Matrix Eigen- */
/*                 system Routines - EISPACK Guide, Springer-Verlag, */
/*                 1976. */
/* ***ROUTINES CALLED  TQL2, TQLRAT, TRED1, TRED2 */
/* ***REVISION HISTORY  (YYMMDD) */
/*   760101  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  RS */


/* ***FIRST EXECUTABLE STATEMENT  RS */
    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --w;
    --fv1;
    --fv2;

    /* Function Body */
    if (*n <= *nm) {
	goto L10;
    }
    *ierr = *n * 10;
    goto L50;

L10:
    if (*matz != 0) {
	goto L20;
    }
/*     .......... FIND EIGENVALUES ONLY .......... */
    tred1_(nm, n, &a[a_offset], &w[1], &fv1[1], &fv2[1]);
    tqlrat_(n, &w[1], &fv2[1], ierr);
    goto L50;
/*     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS .......... */
L20:
    tred2_(nm, n, &a[a_offset], &w[1], &fv1[1], &z__[z_offset]);
    tql2_(nm, n, &w[1], &fv1[1], &z__[z_offset], ierr);
L50:
    return 0;
} /* rsfoo_ */

