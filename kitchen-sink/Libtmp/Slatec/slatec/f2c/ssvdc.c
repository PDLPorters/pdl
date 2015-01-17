/* ssvdc.f -- translated by f2c (version 20060506).
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
static real c_b44 = -1.f;

/* DECK SSVDC */
/* Subroutine */ int ssvdc_(real *x, integer *ldx, integer *n, integer *p, 
	real *s, real *e, real *u, integer *ldu, real *v, integer *ldv, real *
	work, integer *job, integer *info)
{
    /* System generated locals */
    integer x_dim1, x_offset, u_dim1, u_offset, v_dim1, v_offset, i__1, i__2, 
	    i__3;
    real r__1, r__2, r__3, r__4, r__5, r__6, r__7;

    /* Builtin functions */
    double r_sign(real *, real *), sqrt(doublereal);

    /* Local variables */
    static real b, c__, f, g;
    static integer i__, j, k, l, m;
    static real t, t1, el;
    static integer kk;
    static real cs;
    static integer ll, mm, ls;
    static real sl;
    static integer lu;
    static real sm, sn;
    static integer lm1, mm1, lp1, mp1, nct, ncu, lls, nrt;
    static real emm1, smm1;
    static integer kase, jobu, iter;
    extern doublereal sdot_(integer *, real *, integer *, real *, integer *);
    static real test;
    extern /* Subroutine */ int srot_(integer *, real *, integer *, real *, 
	    integer *, real *, real *);
    static integer nctp1;
    extern doublereal snrm2_(integer *, real *, integer *);
    static integer nrtp1;
    static real scale;
    extern /* Subroutine */ int sscal_(integer *, real *, real *, integer *);
    static real shift;
    static integer maxit;
    extern /* Subroutine */ int sswap_(integer *, real *, integer *, real *, 
	    integer *);
    static logical wantu, wantv;
    extern /* Subroutine */ int srotg_(real *, real *, real *, real *), 
	    saxpy_(integer *, real *, real *, integer *, real *, integer *);
    static real ztest;

/* ***BEGIN PROLOGUE  SSVDC */
/* ***PURPOSE  Perform the singular value decomposition of a rectangular */
/*            matrix. */
/* ***LIBRARY   SLATEC (LINPACK) */
/* ***CATEGORY  D6 */
/* ***TYPE      SINGLE PRECISION (SSVDC-S, DSVDC-D, CSVDC-C) */
/* ***KEYWORDS  LINEAR ALGEBRA, LINPACK, MATRIX, */
/*             SINGULAR VALUE DECOMPOSITION */
/* ***AUTHOR  Stewart, G. W., (U. of Maryland) */
/* ***DESCRIPTION */

/*     SSVDC is a subroutine to reduce a real NxP matrix X by orthogonal */
/*     transformations U and V to diagonal form.  The elements S(I) are */
/*     the singular values of X.  The columns of U are the corresponding */
/*     left singular vectors, and the columns of V the right singular */
/*     vectors. */

/*     On Entry */

/*         X         REAL(LDX,P), where LDX .GE. N. */
/*                   X contains the matrix whose singular value */
/*                   decomposition is to be computed.  X is */
/*                   destroyed by SSVDC. */

/*         LDX       INTEGER */
/*                   LDX is the leading dimension of the array X. */

/*         N         INTEGER */
/*                   N is the number of rows of the matrix X. */

/*         P         INTEGER */
/*                   P is the number of columns of the matrix X. */

/*         LDU       INTEGER */
/*                   LDU is the leading dimension of the array U. */
/*                   (See below). */

/*         LDV       INTEGER */
/*                   LDV is the leading dimension of the array V. */
/*                   (See below). */

/*         WORK      REAL(N) */
/*                   work is a scratch array. */

/*         JOB       INTEGER */
/*                   JOB controls the computation of the singular */
/*                   vectors.  It has the decimal expansion AB */
/*                   with the following meaning */

/*                        A .EQ. 0  Do not compute the left singular */
/*                                  vectors. */
/*                        A .EQ. 1  Return the N left singular vectors */
/*                                  in U. */
/*                        A .GE. 2  Return the first MIN(N,P) singular */
/*                                  vectors in U. */
/*                        B .EQ. 0  Do not compute the right singular */
/*                                  vectors. */
/*                        B .EQ. 1  Return the right singular vectors */
/*                                  in V. */

/*     On Return */

/*         S         REAL(MM), where MM=MIN(N+1,P). */
/*                   The first MIN(N,P) entries of S contain the */
/*                   singular values of X arranged in descending */
/*                   order of magnitude. */

/*         E         REAL(P). */
/*                   E ordinarily contains zeros.  However, see the */
/*                   discussion of INFO for exceptions. */

/*         U         REAL(LDU,K), where LDU .GE. N.  If JOBA .EQ. 1, then */
/*                                   K .EQ. N.  If JOBA .GE. 2 , then */
/*                                   K .EQ. MIN(N,P). */
/*                   U contains the matrix of right singular vectors. */
/*                   U is not referenced if JOBA .EQ. 0.  If N .LE. P */
/*                   or if JOBA .EQ. 2, then U may be identified with X */
/*                   in the subroutine call. */

/*         V         REAL(LDV,P), where LDV .GE. P. */
/*                   V contains the matrix of right singular vectors. */
/*                   V is not referenced if JOB .EQ. 0.  If P .LE. N, */
/*                   then V may be identified with X in the */
/*                   subroutine call. */

/*         INFO      INTEGER. */
/*                   the singular values (and their corresponding */
/*                   singular vectors) S(INFO+1),S(INFO+2),...,S(M) */
/*                   are correct (here M=MIN(N,P)).  Thus if */
/*                   INFO .EQ. 0, all the singular values and their */
/*                   vectors are correct.  In any event, the matrix */
/*                   B = TRANS(U)*X*V is the bidiagonal matrix */
/*                   with the elements of S on its diagonal and the */
/*                   elements of E on its super-diagonal (TRANS(U) */
/*                   is the transpose of U).  Thus the singular */
/*                   values of X and B are the same. */

/* ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W. */
/*                 Stewart, LINPACK Users' Guide, SIAM, 1979. */
/* ***ROUTINES CALLED  SAXPY, SDOT, SNRM2, SROT, SROTG, SSCAL, SSWAP */
/* ***REVISION HISTORY  (YYMMDD) */
/*   790319  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900326  Removed duplicate information from DESCRIPTION section. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  SSVDC */


/* ***FIRST EXECUTABLE STATEMENT  SSVDC */

/*     SET THE MAXIMUM NUMBER OF ITERATIONS. */

    /* Parameter adjustments */
    x_dim1 = *ldx;
    x_offset = 1 + x_dim1;
    x -= x_offset;
    --s;
    --e;
    u_dim1 = *ldu;
    u_offset = 1 + u_dim1;
    u -= u_offset;
    v_dim1 = *ldv;
    v_offset = 1 + v_dim1;
    v -= v_offset;
    --work;

    /* Function Body */
    maxit = 30;

/*     DETERMINE WHAT IS TO BE COMPUTED. */

    wantu = FALSE_;
    wantv = FALSE_;
    jobu = *job % 100 / 10;
    ncu = *n;
    if (jobu > 1) {
	ncu = min(*n,*p);
    }
    if (jobu != 0) {
	wantu = TRUE_;
    }
    if (*job % 10 != 0) {
	wantv = TRUE_;
    }

/*     REDUCE X TO BIDIAGONAL FORM, STORING THE DIAGONAL ELEMENTS */
/*     IN S AND THE SUPER-DIAGONAL ELEMENTS IN E. */

    *info = 0;
/* Computing MIN */
    i__1 = *n - 1;
    nct = min(i__1,*p);
/* Computing MAX */
/* Computing MIN */
    i__3 = *p - 2;
    i__1 = 0, i__2 = min(i__3,*n);
    nrt = max(i__1,i__2);
    lu = max(nct,nrt);
    if (lu < 1) {
	goto L170;
    }
    i__1 = lu;
    for (l = 1; l <= i__1; ++l) {
	lp1 = l + 1;
	if (l > nct) {
	    goto L20;
	}

/*           COMPUTE THE TRANSFORMATION FOR THE L-TH COLUMN AND */
/*           PLACE THE L-TH DIAGONAL IN S(L). */

	i__2 = *n - l + 1;
	s[l] = snrm2_(&i__2, &x[l + l * x_dim1], &c__1);
	if (s[l] == 0.f) {
	    goto L10;
	}
	if (x[l + l * x_dim1] != 0.f) {
	    s[l] = r_sign(&s[l], &x[l + l * x_dim1]);
	}
	i__2 = *n - l + 1;
	r__1 = 1.f / s[l];
	sscal_(&i__2, &r__1, &x[l + l * x_dim1], &c__1);
	x[l + l * x_dim1] += 1.f;
L10:
	s[l] = -s[l];
L20:
	if (*p < lp1) {
	    goto L50;
	}
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    if (l > nct) {
		goto L30;
	    }
	    if (s[l] == 0.f) {
		goto L30;
	    }

/*              APPLY THE TRANSFORMATION. */

	    i__3 = *n - l + 1;
	    t = -sdot_(&i__3, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1], &
		    c__1) / x[l + l * x_dim1];
	    i__3 = *n - l + 1;
	    saxpy_(&i__3, &t, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1], &
		    c__1);
L30:

/*           PLACE THE L-TH ROW OF X INTO  E FOR THE */
/*           SUBSEQUENT CALCULATION OF THE ROW TRANSFORMATION. */

	    e[j] = x[l + j * x_dim1];
/* L40: */
	}
L50:
	if (! wantu || l > nct) {
	    goto L70;
	}

/*           PLACE THE TRANSFORMATION IN U FOR SUBSEQUENT BACK */
/*           MULTIPLICATION. */

	i__2 = *n;
	for (i__ = l; i__ <= i__2; ++i__) {
	    u[i__ + l * u_dim1] = x[i__ + l * x_dim1];
/* L60: */
	}
L70:
	if (l > nrt) {
	    goto L150;
	}

/*           COMPUTE THE L-TH ROW TRANSFORMATION AND PLACE THE */
/*           L-TH SUPER-DIAGONAL IN E(L). */

	i__2 = *p - l;
	e[l] = snrm2_(&i__2, &e[lp1], &c__1);
	if (e[l] == 0.f) {
	    goto L80;
	}
	if (e[lp1] != 0.f) {
	    e[l] = r_sign(&e[l], &e[lp1]);
	}
	i__2 = *p - l;
	r__1 = 1.f / e[l];
	sscal_(&i__2, &r__1, &e[lp1], &c__1);
	e[lp1] += 1.f;
L80:
	e[l] = -e[l];
	if (lp1 > *n || e[l] == 0.f) {
	    goto L120;
	}

/*              APPLY THE TRANSFORMATION. */

	i__2 = *n;
	for (i__ = lp1; i__ <= i__2; ++i__) {
	    work[i__] = 0.f;
/* L90: */
	}
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l;
	    saxpy_(&i__3, &e[j], &x[lp1 + j * x_dim1], &c__1, &work[lp1], &
		    c__1);
/* L100: */
	}
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l;
	    r__1 = -e[j] / e[lp1];
	    saxpy_(&i__3, &r__1, &work[lp1], &c__1, &x[lp1 + j * x_dim1], &
		    c__1);
/* L110: */
	}
L120:
	if (! wantv) {
	    goto L140;
	}

/*              PLACE THE TRANSFORMATION IN V FOR SUBSEQUENT */
/*              BACK MULTIPLICATION. */

	i__2 = *p;
	for (i__ = lp1; i__ <= i__2; ++i__) {
	    v[i__ + l * v_dim1] = e[i__];
/* L130: */
	}
L140:
L150:
/* L160: */
	;
    }
L170:

/*     SET UP THE FINAL BIDIAGONAL MATRIX OR ORDER M. */

/* Computing MIN */
    i__1 = *p, i__2 = *n + 1;
    m = min(i__1,i__2);
    nctp1 = nct + 1;
    nrtp1 = nrt + 1;
    if (nct < *p) {
	s[nctp1] = x[nctp1 + nctp1 * x_dim1];
    }
    if (*n < m) {
	s[m] = 0.f;
    }
    if (nrtp1 < m) {
	e[nrtp1] = x[nrtp1 + m * x_dim1];
    }
    e[m] = 0.f;

/*     IF REQUIRED, GENERATE U. */

    if (! wantu) {
	goto L300;
    }
    if (ncu < nctp1) {
	goto L200;
    }
    i__1 = ncu;
    for (j = nctp1; j <= i__1; ++j) {
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    u[i__ + j * u_dim1] = 0.f;
/* L180: */
	}
	u[j + j * u_dim1] = 1.f;
/* L190: */
    }
L200:
    if (nct < 1) {
	goto L290;
    }
    i__1 = nct;
    for (ll = 1; ll <= i__1; ++ll) {
	l = nct - ll + 1;
	if (s[l] == 0.f) {
	    goto L250;
	}
	lp1 = l + 1;
	if (ncu < lp1) {
	    goto L220;
	}
	i__2 = ncu;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l + 1;
	    t = -sdot_(&i__3, &u[l + l * u_dim1], &c__1, &u[l + j * u_dim1], &
		    c__1) / u[l + l * u_dim1];
	    i__3 = *n - l + 1;
	    saxpy_(&i__3, &t, &u[l + l * u_dim1], &c__1, &u[l + j * u_dim1], &
		    c__1);
/* L210: */
	}
L220:
	i__2 = *n - l + 1;
	sscal_(&i__2, &c_b44, &u[l + l * u_dim1], &c__1);
	u[l + l * u_dim1] += 1.f;
	lm1 = l - 1;
	if (lm1 < 1) {
	    goto L240;
	}
	i__2 = lm1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    u[i__ + l * u_dim1] = 0.f;
/* L230: */
	}
L240:
	goto L270;
L250:
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    u[i__ + l * u_dim1] = 0.f;
/* L260: */
	}
	u[l + l * u_dim1] = 1.f;
L270:
/* L280: */
	;
    }
L290:
L300:

/*     IF IT IS REQUIRED, GENERATE V. */

    if (! wantv) {
	goto L350;
    }
    i__1 = *p;
    for (ll = 1; ll <= i__1; ++ll) {
	l = *p - ll + 1;
	lp1 = l + 1;
	if (l > nrt) {
	    goto L320;
	}
	if (e[l] == 0.f) {
	    goto L320;
	}
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *p - l;
	    t = -sdot_(&i__3, &v[lp1 + l * v_dim1], &c__1, &v[lp1 + j * 
		    v_dim1], &c__1) / v[lp1 + l * v_dim1];
	    i__3 = *p - l;
	    saxpy_(&i__3, &t, &v[lp1 + l * v_dim1], &c__1, &v[lp1 + j * 
		    v_dim1], &c__1);
/* L310: */
	}
L320:
	i__2 = *p;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    v[i__ + l * v_dim1] = 0.f;
/* L330: */
	}
	v[l + l * v_dim1] = 1.f;
/* L340: */
    }
L350:

/*     MAIN ITERATION LOOP FOR THE SINGULAR VALUES. */

    mm = m;
    iter = 0;
L360:

/*        QUIT IF ALL THE SINGULAR VALUES HAVE BEEN FOUND. */

    if (m == 0) {
	goto L620;
    }

/*        IF TOO MANY ITERATIONS HAVE BEEN PERFORMED, SET */
/*        FLAG AND RETURN. */

    if (iter < maxit) {
	goto L370;
    }
    *info = m;
    goto L620;
L370:

/*        THIS SECTION OF THE PROGRAM INSPECTS FOR */
/*        NEGLIGIBLE ELEMENTS IN THE S AND E ARRAYS.  ON */
/*        COMPLETION THE VARIABLES KASE AND L ARE SET AS FOLLOWS. */

/*           KASE = 1     IF S(M) AND E(L-1) ARE NEGLIGIBLE AND L.LT.M */
/*           KASE = 2     IF S(L) IS NEGLIGIBLE AND L.LT.M */
/*           KASE = 3     IF E(L-1) IS NEGLIGIBLE, L.LT.M, AND */
/*                        S(L), ..., S(M) ARE NOT NEGLIGIBLE (QR STEP). */
/*           KASE = 4     IF E(M-1) IS NEGLIGIBLE (CONVERGENCE). */

    i__1 = m;
    for (ll = 1; ll <= i__1; ++ll) {
	l = m - ll;
	if (l == 0) {
	    goto L400;
	}
	test = (r__1 = s[l], dabs(r__1)) + (r__2 = s[l + 1], dabs(r__2));
	ztest = test + (r__1 = e[l], dabs(r__1));
	if (ztest != test) {
	    goto L380;
	}
	e[l] = 0.f;
	goto L400;
L380:
/* L390: */
	;
    }
L400:
    if (l != m - 1) {
	goto L410;
    }
    kase = 4;
    goto L480;
L410:
    lp1 = l + 1;
    mp1 = m + 1;
    i__1 = mp1;
    for (lls = lp1; lls <= i__1; ++lls) {
	ls = m - lls + lp1;
	if (ls == l) {
	    goto L440;
	}
	test = 0.f;
	if (ls != m) {
	    test += (r__1 = e[ls], dabs(r__1));
	}
	if (ls != l + 1) {
	    test += (r__1 = e[ls - 1], dabs(r__1));
	}
	ztest = test + (r__1 = s[ls], dabs(r__1));
	if (ztest != test) {
	    goto L420;
	}
	s[ls] = 0.f;
	goto L440;
L420:
/* L430: */
	;
    }
L440:
    if (ls != l) {
	goto L450;
    }
    kase = 3;
    goto L470;
L450:
    if (ls != m) {
	goto L460;
    }
    kase = 1;
    goto L470;
L460:
    kase = 2;
    l = ls;
L470:
L480:
    ++l;

/*        PERFORM THE TASK INDICATED BY KASE. */

    switch (kase) {
	case 1:  goto L490;
	case 2:  goto L520;
	case 3:  goto L540;
	case 4:  goto L570;
    }

/*        DEFLATE NEGLIGIBLE S(M). */

L490:
    mm1 = m - 1;
    f = e[m - 1];
    e[m - 1] = 0.f;
    i__1 = mm1;
    for (kk = l; kk <= i__1; ++kk) {
	k = mm1 - kk + l;
	t1 = s[k];
	srotg_(&t1, &f, &cs, &sn);
	s[k] = t1;
	if (k == l) {
	    goto L500;
	}
	f = -sn * e[k - 1];
	e[k - 1] = cs * e[k - 1];
L500:
	if (wantv) {
	    srot_(p, &v[k * v_dim1 + 1], &c__1, &v[m * v_dim1 + 1], &c__1, &
		    cs, &sn);
	}
/* L510: */
    }
    goto L610;

/*        SPLIT AT NEGLIGIBLE S(L). */

L520:
    f = e[l - 1];
    e[l - 1] = 0.f;
    i__1 = m;
    for (k = l; k <= i__1; ++k) {
	t1 = s[k];
	srotg_(&t1, &f, &cs, &sn);
	s[k] = t1;
	f = -sn * e[k];
	e[k] = cs * e[k];
	if (wantu) {
	    srot_(n, &u[k * u_dim1 + 1], &c__1, &u[(l - 1) * u_dim1 + 1], &
		    c__1, &cs, &sn);
	}
/* L530: */
    }
    goto L610;

/*        PERFORM ONE QR STEP. */

L540:

/*           CALCULATE THE SHIFT. */

/* Computing MAX */
    r__6 = (r__1 = s[m], dabs(r__1)), r__7 = (r__2 = s[m - 1], dabs(r__2)), 
	    r__6 = max(r__6,r__7), r__7 = (r__3 = e[m - 1], dabs(r__3)), r__6 
	    = max(r__6,r__7), r__7 = (r__4 = s[l], dabs(r__4)), r__6 = max(
	    r__6,r__7), r__7 = (r__5 = e[l], dabs(r__5));
    scale = dmax(r__6,r__7);
    sm = s[m] / scale;
    smm1 = s[m - 1] / scale;
    emm1 = e[m - 1] / scale;
    sl = s[l] / scale;
    el = e[l] / scale;
/* Computing 2nd power */
    r__1 = emm1;
    b = ((smm1 + sm) * (smm1 - sm) + r__1 * r__1) / 2.f;
/* Computing 2nd power */
    r__1 = sm * emm1;
    c__ = r__1 * r__1;
    shift = 0.f;
    if (b == 0.f && c__ == 0.f) {
	goto L550;
    }
/* Computing 2nd power */
    r__1 = b;
    shift = sqrt(r__1 * r__1 + c__);
    if (b < 0.f) {
	shift = -shift;
    }
    shift = c__ / (b + shift);
L550:
    f = (sl + sm) * (sl - sm) - shift;
    g = sl * el;

/*           CHASE ZEROS. */

    mm1 = m - 1;
    i__1 = mm1;
    for (k = l; k <= i__1; ++k) {
	srotg_(&f, &g, &cs, &sn);
	if (k != l) {
	    e[k - 1] = f;
	}
	f = cs * s[k] + sn * e[k];
	e[k] = cs * e[k] - sn * s[k];
	g = sn * s[k + 1];
	s[k + 1] = cs * s[k + 1];
	if (wantv) {
	    srot_(p, &v[k * v_dim1 + 1], &c__1, &v[(k + 1) * v_dim1 + 1], &
		    c__1, &cs, &sn);
	}
	srotg_(&f, &g, &cs, &sn);
	s[k] = f;
	f = cs * e[k] + sn * s[k + 1];
	s[k + 1] = -sn * e[k] + cs * s[k + 1];
	g = sn * e[k + 1];
	e[k + 1] = cs * e[k + 1];
	if (wantu && k < *n) {
	    srot_(n, &u[k * u_dim1 + 1], &c__1, &u[(k + 1) * u_dim1 + 1], &
		    c__1, &cs, &sn);
	}
/* L560: */
    }
    e[m - 1] = f;
    ++iter;
    goto L610;

/*        CONVERGENCE. */

L570:

/*           MAKE THE SINGULAR VALUE  POSITIVE. */

    if (s[l] >= 0.f) {
	goto L580;
    }
    s[l] = -s[l];
    if (wantv) {
	sscal_(p, &c_b44, &v[l * v_dim1 + 1], &c__1);
    }
L580:

/*           ORDER THE SINGULAR VALUE. */

L590:
    if (l == mm) {
	goto L600;
    }
    if (s[l] >= s[l + 1]) {
	goto L600;
    }
    t = s[l];
    s[l] = s[l + 1];
    s[l + 1] = t;
    if (wantv && l < *p) {
	sswap_(p, &v[l * v_dim1 + 1], &c__1, &v[(l + 1) * v_dim1 + 1], &c__1);
    }
    if (wantu && l < *n) {
	sswap_(n, &u[l * u_dim1 + 1], &c__1, &u[(l + 1) * u_dim1 + 1], &c__1);
    }
    ++l;
    goto L590;
L600:
    iter = 0;
    --m;
L610:
    goto L360;
L620:
    return 0;
} /* ssvdc_ */

