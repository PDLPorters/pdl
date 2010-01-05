/* spoco.f -- translated by f2c (version 20060506).
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

/* DECK SPOCO */
/* Subroutine */ int spoco_(real *a, integer *lda, integer *n, real *rcond, 
	real *z__, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    real r__1, r__2;

    /* Builtin functions */
    double r_sign(real *, real *);

    /* Local variables */
    static integer i__, j, k;
    static real s, t;
    static integer kb;
    static real ek, sm, wk;
    static integer jm1, kp1;
    static real wkm;
    extern doublereal sdot_(integer *, real *, integer *, real *, integer *);
    extern /* Subroutine */ int sscal_(integer *, real *, real *, integer *), 
	    spofa_(real *, integer *, integer *, integer *);
    static real anorm;
    extern doublereal sasum_(integer *, real *, integer *);
    static real ynorm;
    extern /* Subroutine */ int saxpy_(integer *, real *, real *, integer *, 
	    real *, integer *);

/* ***BEGIN PROLOGUE  SPOCO */
/* ***PURPOSE  Factor a real symmetric positive definite matrix */
/*            and estimate the condition number of the matrix. */
/* ***LIBRARY   SLATEC (LINPACK) */
/* ***CATEGORY  D2B1B */
/* ***TYPE      SINGLE PRECISION (SPOCO-S, DPOCO-D, CPOCO-C) */
/* ***KEYWORDS  CONDITION NUMBER, LINEAR ALGEBRA, LINPACK, */
/*             MATRIX FACTORIZATION, POSITIVE DEFINITE */
/* ***AUTHOR  Moler, C. B., (U. of New Mexico) */
/* ***DESCRIPTION */

/*     SPOCO factors a real symmetric positive definite matrix */
/*     and estimates the condition of the matrix. */

/*     If  RCOND  is not needed, SPOFA is slightly faster. */
/*     To solve  A*X = B , follow SPOCO by SPOSL. */
/*     To compute  INVERSE(A)*C , follow SPOCO by SPOSL. */
/*     To compute  DETERMINANT(A) , follow SPOCO by SPODI. */
/*     To compute  INVERSE(A) , follow SPOCO by SPODI. */

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

/*        RCOND   REAL */
/*                an estimate of the reciprocal condition of  A . */
/*                For the system  A*X = B , relative perturbations */
/*                in  A  and  B  of size  EPSILON  may cause */
/*                relative perturbations in  X  of size  EPSILON/RCOND . */
/*                If  RCOND  is so small that the logical expression */
/*                           1.0 + RCOND .EQ. 1.0 */
/*                is true, then  A  may be singular to working */
/*                precision.  In particular,  RCOND  is zero  if */
/*                exact singularity is detected or the estimate */
/*                underflows.  If INFO .NE. 0 , RCOND is unchanged. */

/*        Z       REAL(N) */
/*                a work vector whose contents are usually unimportant. */
/*                If  A  is close to a singular matrix, then  Z  is */
/*                an approximate null vector in the sense that */
/*                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) . */
/*                If  INFO .NE. 0 , Z  is unchanged. */

/*        INFO    INTEGER */
/*                = 0  for normal return. */
/*                = K  signals an error condition.  The leading minor */
/*                     of order  K  is not positive definite. */

/* ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W. */
/*                 Stewart, LINPACK Users' Guide, SIAM, 1979. */
/* ***ROUTINES CALLED  SASUM, SAXPY, SDOT, SPOFA, SSCAL */
/* ***REVISION HISTORY  (YYMMDD) */
/*   780814  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900326  Removed duplicate information from DESCRIPTION section. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  SPOCO */


/*     FIND NORM OF A USING ONLY UPPER HALF */

/* ***FIRST EXECUTABLE STATEMENT  SPOCO */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --z__;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	z__[j] = sasum_(&j, &a[j * a_dim1 + 1], &c__1);
	jm1 = j - 1;
	if (jm1 < 1) {
	    goto L20;
	}
	i__2 = jm1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    z__[i__] += (r__1 = a[i__ + j * a_dim1], dabs(r__1));
/* L10: */
	}
L20:
/* L30: */
	;
    }
    anorm = 0.f;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/* Computing MAX */
	r__1 = anorm, r__2 = z__[j];
	anorm = dmax(r__1,r__2);
/* L40: */
    }

/*     FACTOR */

    spofa_(&a[a_offset], lda, n, info);
    if (*info != 0) {
	goto L180;
    }

/*        RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) . */
/*        ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  A*Y = E . */
/*        THE COMPONENTS OF  E  ARE CHOSEN TO CAUSE MAXIMUM LOCAL */
/*        GROWTH IN THE ELEMENTS OF W  WHERE  TRANS(R)*W = E . */
/*        THE VECTORS ARE FREQUENTLY RESCALED TO AVOID OVERFLOW. */

/*        SOLVE TRANS(R)*W = E */

    ek = 1.f;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	z__[j] = 0.f;
/* L50: */
    }
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	if (z__[k] != 0.f) {
	    r__1 = -z__[k];
	    ek = r_sign(&ek, &r__1);
	}
	if ((r__1 = ek - z__[k], dabs(r__1)) <= a[k + k * a_dim1]) {
	    goto L60;
	}
	s = a[k + k * a_dim1] / (r__1 = ek - z__[k], dabs(r__1));
	sscal_(n, &s, &z__[1], &c__1);
	ek = s * ek;
L60:
	wk = ek - z__[k];
	wkm = -ek - z__[k];
	s = dabs(wk);
	sm = dabs(wkm);
	wk /= a[k + k * a_dim1];
	wkm /= a[k + k * a_dim1];
	kp1 = k + 1;
	if (kp1 > *n) {
	    goto L100;
	}
	i__2 = *n;
	for (j = kp1; j <= i__2; ++j) {
	    sm += (r__1 = z__[j] + wkm * a[k + j * a_dim1], dabs(r__1));
	    z__[j] += wk * a[k + j * a_dim1];
	    s += (r__1 = z__[j], dabs(r__1));
/* L70: */
	}
	if (s >= sm) {
	    goto L90;
	}
	t = wkm - wk;
	wk = wkm;
	i__2 = *n;
	for (j = kp1; j <= i__2; ++j) {
	    z__[j] += t * a[k + j * a_dim1];
/* L80: */
	}
L90:
L100:
	z__[k] = wk;
/* L110: */
    }
    s = 1.f / sasum_(n, &z__[1], &c__1);
    sscal_(n, &s, &z__[1], &c__1);

/*        SOLVE R*Y = W */

    i__1 = *n;
    for (kb = 1; kb <= i__1; ++kb) {
	k = *n + 1 - kb;
	if ((r__1 = z__[k], dabs(r__1)) <= a[k + k * a_dim1]) {
	    goto L120;
	}
	s = a[k + k * a_dim1] / (r__1 = z__[k], dabs(r__1));
	sscal_(n, &s, &z__[1], &c__1);
L120:
	z__[k] /= a[k + k * a_dim1];
	t = -z__[k];
	i__2 = k - 1;
	saxpy_(&i__2, &t, &a[k * a_dim1 + 1], &c__1, &z__[1], &c__1);
/* L130: */
    }
    s = 1.f / sasum_(n, &z__[1], &c__1);
    sscal_(n, &s, &z__[1], &c__1);

    ynorm = 1.f;

/*        SOLVE TRANS(R)*V = Y */

    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	i__2 = k - 1;
	z__[k] -= sdot_(&i__2, &a[k * a_dim1 + 1], &c__1, &z__[1], &c__1);
	if ((r__1 = z__[k], dabs(r__1)) <= a[k + k * a_dim1]) {
	    goto L140;
	}
	s = a[k + k * a_dim1] / (r__1 = z__[k], dabs(r__1));
	sscal_(n, &s, &z__[1], &c__1);
	ynorm = s * ynorm;
L140:
	z__[k] /= a[k + k * a_dim1];
/* L150: */
    }
    s = 1.f / sasum_(n, &z__[1], &c__1);
    sscal_(n, &s, &z__[1], &c__1);
    ynorm = s * ynorm;

/*        SOLVE R*Z = V */

    i__1 = *n;
    for (kb = 1; kb <= i__1; ++kb) {
	k = *n + 1 - kb;
	if ((r__1 = z__[k], dabs(r__1)) <= a[k + k * a_dim1]) {
	    goto L160;
	}
	s = a[k + k * a_dim1] / (r__1 = z__[k], dabs(r__1));
	sscal_(n, &s, &z__[1], &c__1);
	ynorm = s * ynorm;
L160:
	z__[k] /= a[k + k * a_dim1];
	t = -z__[k];
	i__2 = k - 1;
	saxpy_(&i__2, &t, &a[k * a_dim1 + 1], &c__1, &z__[1], &c__1);
/* L170: */
    }
/*        MAKE ZNORM = 1.0 */
    s = 1.f / sasum_(n, &z__[1], &c__1);
    sscal_(n, &s, &z__[1], &c__1);
    ynorm = s * ynorm;

    if (anorm != 0.f) {
	*rcond = ynorm / anorm;
    }
    if (anorm == 0.f) {
	*rcond = 0.f;
    }
L180:
    return 0;
} /* spoco_ */

