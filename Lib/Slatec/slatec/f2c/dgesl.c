/* dgesl.f -- translated by f2c (version 20060506).
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

/* ====================================================================== */
/* NIST Guide to Available Math Software. */
/* Source for module DGESL from package SLATEC. */
/* Retrieved from CAMSUN on Sat Sep 25 04:27:36 1999. */
/* ====================================================================== */
/* DECK DGESL */
/* Subroutine */ int dgesl_(doublereal *a, integer *lda, integer *n, integer *
	ipvt, doublereal *b, integer *job)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    static integer k, l;
    static doublereal t;
    static integer kb, nm1;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, integer *);

/* ***BEGIN PROLOGUE  DGESL */
/* ***PURPOSE  Solve the real system A*X=B or TRANS(A)*X=B using the */
/*            factors computed by DGECO or DGEFA. */
/* ***LIBRARY   SLATEC (LINPACK) */
/* ***CATEGORY  D2A1 */
/* ***TYPE      DOUBLE PRECISION (SGESL-S, DGESL-D, CGESL-C) */
/* ***KEYWORDS  LINEAR ALGEBRA, LINPACK, MATRIX, SOLVE */
/* ***AUTHOR  Moler, C. B., (U. of New Mexico) */
/* ***DESCRIPTION */

/*     DGESL solves the double precision system */
/*     A * X = B  or  TRANS(A) * X = B */
/*     using the factors computed by DGECO or DGEFA. */

/*     On Entry */

/*        A       DOUBLE PRECISION(LDA, N) */
/*                the output from DGECO or DGEFA. */

/*        LDA     INTEGER */
/*                the leading dimension of the array  A . */

/*        N       INTEGER */
/*                the order of the matrix  A . */

/*        IPVT    INTEGER(N) */
/*                the pivot vector from DGECO or DGEFA. */

/*        B       DOUBLE PRECISION(N) */
/*                the right hand side vector. */

/*        JOB     INTEGER */
/*                = 0         to solve  A*X = B , */
/*                = nonzero   to solve  TRANS(A)*X = B  where */
/*                            TRANS(A)  is the transpose. */

/*     On Return */

/*        B       the solution vector  X . */

/*     Error Condition */

/*        A division by zero will occur if the input factor contains a */
/*        zero on the diagonal.  Technically this indicates singularity */
/*        but it is often caused by improper arguments or improper */
/*        setting of LDA .  It will not occur if the subroutines are */
/*        called correctly and if DGECO has set RCOND .GT. 0.0 */
/*        or DGEFA has set INFO .EQ. 0 . */

/*     To compute  INVERSE(A) * C  where  C  is a matrix */
/*     with  P  columns */
/*           CALL DGECO(A,LDA,N,IPVT,RCOND,Z) */
/*           IF (RCOND is too small) GO TO ... */
/*           DO 10 J = 1, P */
/*              CALL DGESL(A,LDA,N,IPVT,C(1,J),0) */
/*        10 CONTINUE */

/* ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W. */
/*                 Stewart, LINPACK Users' Guide, SIAM, 1979. */
/* ***ROUTINES CALLED  DAXPY, DDOT */
/* ***REVISION HISTORY  (YYMMDD) */
/*   780814  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900326  Removed duplicate information from DESCRIPTION section. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  DGESL */

/* ***FIRST EXECUTABLE STATEMENT  DGESL */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --ipvt;
    --b;

    /* Function Body */
    nm1 = *n - 1;
    if (*job != 0) {
	goto L50;
    }

/*        JOB = 0 , SOLVE  A * X = B */
/*        FIRST SOLVE  L*Y = B */

    if (nm1 < 1) {
	goto L30;
    }
    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
	l = ipvt[k];
	t = b[l];
	if (l == k) {
	    goto L10;
	}
	b[l] = b[k];
	b[k] = t;
L10:
	i__2 = *n - k;
	daxpy_(&i__2, &t, &a[k + 1 + k * a_dim1], &c__1, &b[k + 1], &c__1);
/* L20: */
    }
L30:

/*        NOW SOLVE  U*X = Y */

    i__1 = *n;
    for (kb = 1; kb <= i__1; ++kb) {
	k = *n + 1 - kb;
	b[k] /= a[k + k * a_dim1];
	t = -b[k];
	i__2 = k - 1;
	daxpy_(&i__2, &t, &a[k * a_dim1 + 1], &c__1, &b[1], &c__1);
/* L40: */
    }
    goto L100;
L50:

/*        JOB = NONZERO, SOLVE  TRANS(A) * X = B */
/*        FIRST SOLVE  TRANS(U)*Y = B */

    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	i__2 = k - 1;
	t = ddot_(&i__2, &a[k * a_dim1 + 1], &c__1, &b[1], &c__1);
	b[k] = (b[k] - t) / a[k + k * a_dim1];
/* L60: */
    }

/*        NOW SOLVE TRANS(L)*X = Y */

    if (nm1 < 1) {
	goto L90;
    }
    i__1 = nm1;
    for (kb = 1; kb <= i__1; ++kb) {
	k = *n - kb;
	i__2 = *n - k;
	b[k] += ddot_(&i__2, &a[k + 1 + k * a_dim1], &c__1, &b[k + 1], &c__1);
	l = ipvt[k];
	if (l == k) {
	    goto L70;
	}
	t = b[l];
	b[l] = b[k];
	b[k] = t;
L70:
/* L80: */
	;
    }
L90:
L100:
    return 0;
} /* dgesl_ */

