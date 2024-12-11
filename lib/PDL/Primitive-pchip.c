#include <stdio.h>
#include <stdint.h>
#include <math.h>

/* f2c.h  --  Standard Fortran to C header file */
typedef long int integer;
typedef double doublereal;
typedef long int logical;

#define TRUE_ (1)
#define FALSE_ (0)

/* I/O stuff */
typedef long int ftnlen;

#define abs(x) ((x) >= 0 ? (x) : -(x))
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
/* end f2c.h */

/* from libf2c */
double d_sign(doublereal a, doublereal b)
{
  double x = (a >= 0 ? a : - a);
  return b >= 0 ? x : -x;
}

#define xermsg_(lib, func, errstr, nerr, ...) \
  fprintf(stderr, "%s::%s: %s (err=%ld)\n", lib, func, errstr, nerr)

void dintrv(doublereal *, integer *, doublereal *, integer *,
          integer *, integer *);
void dpchkt(integer *, doublereal *, integer *, doublereal *);
void dchfdv(doublereal *, doublereal *,
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, doublereal *, doublereal *, integer *, integer *);
void dchfev(doublereal *, doublereal *,
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, doublereal *, integer *, integer *);
doublereal dchfie(doublereal *, doublereal *, doublereal *,
	    doublereal *, doublereal *, doublereal *, doublereal *,
	    doublereal *);
doublereal dpchid(integer *, doublereal *, doublereal *,
	    doublereal *, integer *, logical *, integer *, integer *,
            integer *);
doublereal dpchst(doublereal *, doublereal *);
void dpchsw(doublereal *, integer *, doublereal *,
	     doublereal *, doublereal *, doublereal *, integer *);
void dpchce(integer *, doublereal *, integer *,
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     integer *);
void dpchci(integer *, doublereal *, doublereal *, doublereal *, integer *);
void dpchcs(doublereal *, integer *,
	    doublereal *, doublereal *, doublereal *, integer *, integer *);
doublereal dpchdf(integer *, doublereal *, doublereal *, integer *);

doublereal d1mach() {
  return 2.3e-16; /* for float, 1.2e-7 */
}

doublereal dbvalu(doublereal *t, doublereal *a, integer *n, integer *k,
	integer *ideriv, doublereal *x, integer *inbv, doublereal *work)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal ret_val;

    /* Local variables */
    static integer i__, j, j1, j2, jj, km1, ip1, ihi, imk, kmj, ipj, ilo, kpk;
    static doublereal fkmj;
    static integer ip1mj, mflag, imkpj, iderp1, kmider, ihmkmj;

/* ***PURPOSE  Evaluate the B-representation of a B-spline at X for the */
/*            function value or any of its derivatives. */
/* ***LIBRARY   SLATEC */
/* ***KEYWORDS  DIFFERENTIATION OF B-SPLINE, EVALUATION OF B-SPLINE */
/* ***AUTHOR  Amos, D. E., (SNLA) */

/*     Written by Carl de Boor and modified by D. E. Amos */

/*     Abstract   **** a double precision routine **** */
/*         DBVALU is the BVALUE function of the reference. */

/*         DBVALU evaluates the B-representation (T,A,N,K) of a B-spline */
/*         at X for the function value on IDERIV=0 or any of its */
/*         derivatives on IDERIV=1,2,...,K-1.  Right limiting values */
/*         (right derivatives) are returned except at the right end */
/*         point X=T(N+1) where left limiting values are computed.  The */
/*         spline is defined on T(K) .LE. X .LE. T(N+1).  DBVALU returns */
/*         a fatal error message when X is outside of this interval. */

/*         To compute left derivatives or left limiting values at a */
/*         knot T(I), replace N by I-1 and set X=T(I), I=K+1,N+1. */

/*         DBVALU calls DINTRV */

/*     Description of Arguments */

/*         Input      T,A,X are double precision */
/*          T       - knot vector of length N+K */
/*          A       - B-spline coefficient vector of length N */
/*          N       - number of B-spline coefficients */
/*                    N = sum of knot multiplicities-K */
/*          K       - order of the B-spline, K .GE. 1 */
/*          IDERIV  - order of the derivative, 0 .LE. IDERIV .LE. K-1 */
/*                    IDERIV = 0 returns the B-spline value */
/*          X       - argument, T(K) .LE. X .LE. T(N+1) */
/*          INBV    - an initialization parameter which must be set */
/*                    to 1 the first time DBVALU is called. */

/*         Output     WORK,DBVALU are double precision */
/*          INBV    - INBV contains information for efficient process- */
/*                    ing after the initial call and INBV must not */
/*                    be changed by the user.  Distinct splines require */
/*                    distinct INBV parameters. */
/*          WORK    - work vector of length 3*K. */
/*          DBVALU  - value of the IDERIV-th derivative at X */

/*     Error Conditions */
/*         An improper input is a fatal error */

/* ***REFERENCES  Carl de Boor, Package for calculating with B-splines, */
/*                 SIAM Journal on Numerical Analysis 14, 3 (June 1977), */
/*                 pp. 441-472. */
/* ***REVISION HISTORY  (YYMMDD) */
/*   800901  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890911  Removed unnecessary intrinsics.  (WRB) */
/*   890911  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */

    /* Parameter adjustments */
    --work;
    --a;
    --t;

    ret_val = 0.;
    if (*k < 1) {
	goto L102;
    }
    if (*n < *k) {
	goto L101;
    }
    if (*ideriv < 0 || *ideriv >= *k) {
	goto L110;
    }
    kmider = *k - *ideriv;

/* *** FIND *I* IN (K,N) SUCH THAT T(I) .LE. X .LT. T(I+1) */
/*     (OR, .LE. T(I+1) IF T(I) .LT. T(I+1) = T(N+1)). */
    km1 = *k - 1;
    i__1 = *n + 1;
    dintrv(&t[1], &i__1, x, inbv, &i__, &mflag);
    if (*x < t[*k]) {
	goto L120;
    }
    if (mflag == 0) {
	goto L20;
    }
    if (*x > t[i__]) {
	goto L130;
    }
L10:
    if (i__ == *k) {
	goto L140;
    }
    --i__;
    if (*x == t[i__]) {
	goto L10;
    }

/* *** DIFFERENCE THE COEFFICIENTS *IDERIV* TIMES */
/*     WORK(I) = AJ(I), WORK(K+I) = DP(I), WORK(K+K+I) = DM(I), I=1.K */

L20:
    imk = i__ - *k;
    i__1 = *k;
    for (j = 1; j <= i__1; ++j) {
	imkpj = imk + j;
	work[j] = a[imkpj];
    }
    if (*ideriv == 0) {
	goto L60;
    }
    i__1 = *ideriv;
    for (j = 1; j <= i__1; ++j) {
	kmj = *k - j;
	fkmj = (doublereal) kmj;
	i__2 = kmj;
	for (jj = 1; jj <= i__2; ++jj) {
	    ihi = i__ + jj;
	    ihmkmj = ihi - kmj;
	    work[jj] = (work[jj + 1] - work[jj]) / (t[ihi] - t[ihmkmj]) *
		    fkmj;
	}
    }

/* *** COMPUTE VALUE AT *X* IN (T(I),(T(I+1)) OF IDERIV-TH DERIVATIVE, */
/*     GIVEN ITS RELEVANT B-SPLINE COEFF. IN AJ(1),...,AJ(K-IDERIV). */
L60:
    if (*ideriv == km1) {
	goto L100;
    }
    ip1 = i__ + 1;
    kpk = *k + *k;
    j1 = *k + 1;
    j2 = kpk + 1;
    i__1 = kmider;
    for (j = 1; j <= i__1; ++j) {
	ipj = i__ + j;
	work[j1] = t[ipj] - *x;
	ip1mj = ip1 - j;
	work[j2] = *x - t[ip1mj];
	++j1;
	++j2;
    }
    iderp1 = *ideriv + 1;
    i__1 = km1;
    for (j = iderp1; j <= i__1; ++j) {
	kmj = *k - j;
	ilo = kmj;
	i__2 = kmj;
	for (jj = 1; jj <= i__2; ++jj) {
	    work[jj] = (work[jj + 1] * work[kpk + ilo] + work[jj] * work[*k +
		    jj]) / (work[kpk + ilo] + work[*k + jj]);
	    --ilo;
	}
    }
L100:
    ret_val = work[1];
    return ret_val;


L101:
    xermsg_("SLATEC", "DBVALU", "N DOES NOT SATISFY N.GE.K", (long)2);
    return ret_val;
L102:
    xermsg_("SLATEC", "DBVALU", "K DOES NOT SATISFY K.GE.1", (long)2);
    return ret_val;
L110:
    xermsg_("SLATEC", "DBVALU", "IDERIV DOES NOT SATISFY 0.LE.IDERIV.LT.K", (long)2);
    return ret_val;
L120:
    xermsg_("SLATEC", "DBVALU", "X IS N0T GREATER THAN OR EQUAL TO T(K)", (long)2);
    return ret_val;
L130:
    xermsg_("SLATEC", "DBVALU", "X IS NOT LESS THAN OR EQUAL TO T(N+1)", (long)2);
    return ret_val;
L140:
    xermsg_("SLATEC", "DBVALU", "A LEFT LIMITING VALUE CANNOT BE OBTAINED AT T(K)", (long)2);
    return ret_val;
}

void dintrv(doublereal *xt, integer *lxt, doublereal *x,
	integer *ilo, integer *ileft, integer *mflag)
{
    static integer ihi, istep, middle;

/* ***PURPOSE  Compute the largest integer ILEFT in 1 .LE. ILEFT .LE. LXT */
/*            such that XT(ILEFT) .LE. X where XT(*) is a subdivision of */
/*            the X interval. */
/* ***LIBRARY   SLATEC */
/* ***KEYWORDS  B-SPLINE, DATA FITTING, INTERPOLATION, SPLINES */
/* ***AUTHOR  Amos, D. E., (SNLA) */

/*     Written by Carl de Boor and modified by D. E. Amos */

/*     Abstract    **** a double precision routine **** */
/*         DINTRV is the INTERV routine of the reference. */

/*         DINTRV computes the largest integer ILEFT in 1 .LE. ILEFT .LE. */
/*         LXT such that XT(ILEFT) .LE. X where XT(*) is a subdivision of */
/*         the X interval.  Precisely, */

/*                      X .LT. XT(1)                1         -1 */
/*         if  XT(I) .LE. X .LT. XT(I+1)  then  ILEFT=I  , MFLAG=0 */
/*           XT(LXT) .LE. X                         LXT        1, */

/*         That is, when multiplicities are present in the break point */
/*         to the left of X, the largest index is taken for ILEFT. */

/*     Description of Arguments */

/*         Input      XT,X are double precision */
/*          XT      - XT is a knot or break point vector of length LXT */
/*          LXT     - length of the XT vector */
/*          X       - argument */
/*          ILO     - an initialization parameter which must be set */
/*                    to 1 the first time the spline array XT is */
/*                    processed by DINTRV. */

/*         Output */
/*          ILO     - ILO contains information for efficient process- */
/*                    ing after the initial call and ILO must not be */
/*                    changed by the user.  Distinct splines require */
/*                    distinct ILO parameters. */
/*          ILEFT   - largest integer satisfying XT(ILEFT) .LE. X */
/*          MFLAG   - signals when X lies out of bounds */

/*     Error Conditions */
/*         None */

/* ***REFERENCES  Carl de Boor, Package for calculating with B-splines, */
/*                 SIAM Journal on Numerical Analysis 14, 3 (June 1977), */
/*                 pp. 441-472. */
/* ***REVISION HISTORY  (YYMMDD) */
/*   800901  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */

    /* Parameter adjustments */
    --xt;

    ihi = *ilo + 1;
    if (ihi < *lxt) {
	goto L10;
    }
    if (*x >= xt[*lxt]) {
	goto L110;
    }
    if (*lxt <= 1) {
	goto L90;
    }
    *ilo = *lxt - 1;
    ihi = *lxt;

L10:
    if (*x >= xt[ihi]) {
	goto L40;
    }
    if (*x >= xt[*ilo]) {
	goto L100;
    }

/* *** NOW X .LT. XT(IHI) . FIND LOWER BOUND */
    istep = 1;
L20:
    ihi = *ilo;
    *ilo = ihi - istep;
    if (*ilo <= 1) {
	goto L30;
    }
    if (*x >= xt[*ilo]) {
	goto L70;
    }
    istep <<= 1;
    goto L20;
L30:
    *ilo = 1;
    if (*x < xt[1]) {
	goto L90;
    }
    goto L70;
/* *** NOW X .GE. XT(ILO) . FIND UPPER BOUND */
L40:
    istep = 1;
L50:
    *ilo = ihi;
    ihi = *ilo + istep;
    if (ihi >= *lxt) {
	goto L60;
    }
    if (*x < xt[ihi]) {
	goto L70;
    }
    istep <<= 1;
    goto L50;
L60:
    if (*x >= xt[*lxt]) {
	goto L110;
    }
    ihi = *lxt;

/* *** NOW XT(ILO) .LE. X .LT. XT(IHI) . NARROW THE INTERVAL */
L70:
    middle = (*ilo + ihi) / 2;
    if (middle == *ilo) {
	goto L100;
    }
/*     NOTE. IT IS ASSUMED THAT MIDDLE = ILO IN CASE IHI = ILO+1 */
    if (*x < xt[middle]) {
	goto L80;
    }
    *ilo = middle;
    goto L70;
L80:
    ihi = middle;
    goto L70;
/* *** SET OUTPUT AND RETURN */
L90:
    *mflag = -1;
    *ileft = 1;
    return;
L100:
    *mflag = 0;
    *ileft = *ilo;
    return;
L110:
    *mflag = 1;
    *ileft = *lxt;
    return;
}

void dpchbs(integer *n, doublereal *x, doublereal *f,
	doublereal *d__, integer *incfd, integer *knotyp, integer *nknots,
	doublereal *t, doublereal *bcoef, integer *ndim, integer *kord,
	integer *ierr)
{
    /* System generated locals */
    integer f_dim1, f_offset, d_dim1, d_offset, i__1;

    /* Local variables */
    static integer k, kk;
    static doublereal dov3, hold, hnew;
    char *libnam = "SLATEC";
    char *subnam = "DPCHBS";

/* ***PURPOSE  Piecewise Cubic Hermite to B-Spline converter. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***KEYWORDS  B-SPLINES, CONVERSION, CUBIC HERMITE INTERPOLATION, */
/*             PIECEWISE CUBIC INTERPOLATION */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Computing and Mathematics Research Division */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */

/* *Usage: */

/*        INTEGER  N, INCFD, KNOTYP, NKNOTS, NDIM, KORD, IERR */
/*        PARAMETER  (INCFD = ...) */
/*        DOUBLE PRECISION  X(nmax), F(INCFD,nmax), D(INCFD,nmax), */
/*       *      T(2*nmax+4), BCOEF(2*nmax) */

/*        CALL DPCHBS (N, X, F, D, INCFD, KNOTYP, NKNOTS, T, BCOEF, */
/*       *             NDIM, KORD, IERR) */

/* *Arguments: */

/*     N:IN  is the number of data points, N.ge.2 .  (not checked) */

/*     X:IN  is the real array of independent variable values.  The */
/*           elements of X must be strictly increasing: */
/*                X(I-1) .LT. X(I),  I = 2(1)N.   (not checked) */
/*           nmax, the dimension of X, must be .ge.N. */

/*     F:IN  is the real array of dependent variable values. */
/*           F(1+(I-1)*INCFD) is the value corresponding to X(I). */
/*           nmax, the second dimension of F, must be .ge.N. */

/*     D:IN  is the real array of derivative values at the data points. */
/*           D(1+(I-1)*INCFD) is the value corresponding to X(I). */
/*           nmax, the second dimension of D, must be .ge.N. */

/*     INCFD:IN  is the increment between successive values in F and D. */
/*           This argument is provided primarily for 2-D applications. */
/*           It may have the value 1 for one-dimensional applications, */
/*           in which case F and D may be singly-subscripted arrays. */

/*     KNOTYP:IN  is a flag to control the knot sequence. */
/*           The knot sequence T is normally computed from X by putting */
/*           a double knot at each X and setting the end knot pairs */
/*           according to the value of KNOTYP: */
/*              KNOTYP = 0:  Quadruple knots at X(1) and X(N).  (default) */
/*              KNOTYP = 1:  Replicate lengths of extreme subintervals: */
/*                           T( 1 ) = T( 2 ) = X(1) - (X(2)-X(1))  ; */
/*                           T(M+4) = T(M+3) = X(N) + (X(N)-X(N-1)). */
/*              KNOTYP = 2:  Periodic placement of boundary knots: */
/*                           T( 1 ) = T( 2 ) = X(1) - (X(N)-X(N-1)); */
/*                           T(M+4) = T(M+3) = X(N) + (X(2)-X(1))  . */
/*              Here M=NDIM=2*N. */
/*           If the input value of KNOTYP is negative, however, it is */
/*           assumed that NKNOTS and T were set in a previous call. */
/*           This option is provided for improved efficiency when used */
/*           in a parametric setting. */

/*     NKNOTS:INOUT  is the number of knots. */
/*           If KNOTYP.GE.0, then NKNOTS will be set to NDIM+4. */
/*           If KNOTYP.LT.0, then NKNOTS is an input variable, and an */
/*              error return will be taken if it is not equal to NDIM+4. */

/*     T:INOUT  is the array of 2*N+4 knots for the B-representation. */
/*           If KNOTYP.GE.0, T will be returned by DPCHBS with the */
/*              interior double knots equal to the X-values and the */
/*              boundary knots set as indicated above. */
/*           If KNOTYP.LT.0, it is assumed that T was set by a */
/*              previous call to DPCHBS.  (This routine does **not** */
/*              verify that T forms a legitimate knot sequence.) */

/*     BCOEF:OUT  is the array of 2*N B-spline coefficients. */

/*     NDIM:OUT  is the dimension of the B-spline space.  (Set to 2*N.) */

/*     KORD:OUT  is the order of the B-spline.  (Set to 4.) */

/*     IERR:OUT  is an error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           "Recoverable" errors: */
/*              IERR = -4  if KNOTYP.GT.2 . */
/*              IERR = -5  if KNOTYP.LT.0 and NKNOTS.NE.(2*N+4). */

/* *Description: */
/*     DPCHBS computes the B-spline representation of the PCH function */
/*     determined by N,X,F,D.  To be compatible with the rest of PCHIP, */
/*     DPCHBS includes INCFD, the increment between successive values of */
/*     the F- and D-arrays. */

/*     The output is the B-representation for the function:  NKNOTS, T, */
/*     BCOEF, NDIM, KORD. */

/* *Caution: */
/*     Since it is assumed that the input PCH function has been */
/*     computed by one of the other routines in the package PCHIP, */
/*     input arguments N, X, INCFD are **not** checked for validity. */

/* *Restrictions/assumptions: */
/*     1. N.GE.2 .  (not checked) */
/*     2. X(i).LT.X(i+1), i=1,...,N .  (not checked) */
/*     3. INCFD.GT.0 .  (not checked) */
/*     4. KNOTYP.LE.2 .  (error return if not) */
/*    *5. NKNOTS = NDIM+4 = 2*N+4 .  (error return if not) */
/*    *6. T(2*k+1) = T(2*k) = X(k), k=1,...,N .  (not checked) */

/*       * Indicates this applies only if KNOTYP.LT.0 . */

/* *Portability: */
/*     Argument INCFD is used only to cause the compiler to generate */
/*     efficient code for the subscript expressions (1+(I-1)*INCFD) . */
/*     The normal usage, in which DPCHBS is called with one-dimensional */
/*     arrays F and D, is probably non-Fortran 77, in the strict sense, */
/*     but it works on all systems on which DPCHBS has been tested. */

/* *See Also: */
/*     PCHIC, PCHIM, or PCHSP can be used to determine an interpolating */
/*        PCH function from a set of data. */
/*     The B-spline routine DBVALU can be used to evaluate the */
/*        B-representation that is output by DPCHBS. */
/*        (See BSPDOC for more information.) */

/* ***REFERENCES  F. N. Fritsch, "Representations for parametric cubic */
/*                 splines," Computer Aided Geometric Design 6 (1989), */
/*                 pp.79-82. */
/* ***REVISION HISTORY  (YYMMDD) */
/*   870701  DATE WRITTEN */
/*   900405  Converted Fortran to upper case. */
/*   900405  Removed requirement that X be dimensioned N+1. */
/*   900406  Modified to make PCHKT a subsidiary routine to simplify */
/*           usage.  In the process, added argument INCFD to be com- */
/*           patible with the rest of PCHIP. */
/*   900410  Converted prologue to SLATEC 4.0 format. */
/*   900410  Added calls to XERMSG and changed constant 3. to 3 to */
/*           reduce single/double differences. */
/*   900411  Added reference. */
/*   900430  Produced double precision version. */
/*   900501  Corrected declarations. */
/*   930317  Minor cosmetic changes.  (FNF) */
/*   930514  Corrected problems with dimensioning of arguments and */
/*   930604  Removed  NKNOTS from DPCHKT call list.  (FNF) */

    /* Parameter adjustments */
    --x;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;
    f_dim1 = *incfd;
    f_offset = 1 + f_dim1;
    f -= f_offset;
    --t;
    --bcoef;

    *ndim = *n << 1;
    *kord = 4;
    *ierr = 0;

/*  Check argument validity.  Set up knot sequence if OK. */

    if (*knotyp > 2) {
	*ierr = -1;
	xermsg_(libnam, subnam, "KNOTYP GREATER THAN 2", *ierr);
	return;
    }
    if (*knotyp < 0) {
	if (*nknots != *ndim + 4) {
	    *ierr = -2;
	    xermsg_(libnam, subnam, "KNOTYP.LT.0 AND NKNOTS.NE.(2*N+4)", *ierr);
	    return;
	}
    } else {
/*          Set up knot sequence. */
	*nknots = *ndim + 4;
	dpchkt(n, &x[1], knotyp, &t[1]);
    }

/*  Compute B-spline coefficients. */

    hnew = t[3] - t[1];
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	kk = k << 1;
	hold = hnew;
/*          The following requires mixed mode arithmetic. */
	dov3 = d__[k * d_dim1 + 1] / 3;
	bcoef[kk - 1] = f[k * f_dim1 + 1] - hold * dov3;
/*          The following assumes T(2*K+1) = X(K). */
	hnew = t[kk + 3] - t[kk + 1];
	bcoef[kk] = f[k * f_dim1 + 1] + hnew * dov3;
    }

/*  Terminate. */

    return;
}

void dpchkt(integer *n, doublereal *x, integer *knotyp,
	doublereal *t)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer j, k;
    static doublereal hbeg, hend;
    static integer ndim;

/* ***PURPOSE  Compute B-spline knot sequence for DPCHBS. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */

/*     Set a knot sequence for the B-spline representation of a PCH */
/*     function with breakpoints X.  All knots will be at least double. */
/*     Endknots are set as: */
/*        (1) quadruple knots at endpoints if KNOTYP=0; */
/*        (2) extrapolate the length of end interval if KNOTYP=1; */
/*        (3) periodic if KNOTYP=2. */

/*  Input arguments:  N, X, KNOTYP. */
/*  Output arguments:  T. */

/*  Restrictions/assumptions: */
/*     1. N.GE.2 .  (not checked) */
/*     2. X(i).LT.X(i+1), i=1,...,N .  (not checked) */
/*     3. 0.LE.KNOTYP.LE.2 .  (Acts like KNOTYP=0 for any other value.) */

/* ***SEE ALSO  DPCHBS */
/* ***REVISION HISTORY  (YYMMDD) */
/*   870701  DATE WRITTEN */
/*   900405  Converted Fortran to upper case. */
/*   900410  Converted prologue to SLATEC 4.0 format. */
/*   900410  Minor cosmetic changes. */
/*   900430  Produced double precision version. */
/*   930514  Changed NKNOTS from an output to an input variable.  (FNF) */
/*   930604  Removed unused variable NKNOTS from argument list.  (FNF) */

/*  Since this is subsidiary to DPCHBS, which validates its input before */
/*  calling, it is unnecessary for such validation to be done here. */

    /* Parameter adjustments */
    --t;
    --x;

    ndim = *n << 1;

/*  Set interior knots. */

    j = 1;
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	j += 2;
	t[j] = x[k];
	t[j + 1] = t[j];
    }
/*     Assertion:  At this point T(3),...,T(NDIM+2) have been set and */
/*                 J=NDIM+1. */

/*  Set end knots according to KNOTYP. */

    hbeg = x[2] - x[1];
    hend = x[*n] - x[*n - 1];
    if (*knotyp == 1) {
/*          Extrapolate. */
	t[2] = x[1] - hbeg;
	t[ndim + 3] = x[*n] + hend;
    } else if (*knotyp == 2) {
/*          Periodic. */
	t[2] = x[1] - hend;
	t[ndim + 3] = x[*n] + hbeg;
    } else {
/*          Quadruple end knots. */
	t[2] = x[1];
	t[ndim + 3] = x[*n];
    }
    t[1] = t[2];
    t[ndim + 4] = t[ndim + 3];

/*  Terminate. */

    return;
}

void dchfdv(doublereal *x1, doublereal *x2, doublereal *f1,
	doublereal *f2, doublereal *d1, doublereal *d2, integer *ne,
	doublereal *xe, doublereal *fe, doublereal *de, integer *next,
	integer *ierr)
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static doublereal h__;
    static integer i__;
    static doublereal x, c2, c3, c2t2, c3t3, xma, xmi, del1, del2, delta;

/* ***PURPOSE  Evaluate a cubic polynomial given in Hermite form and its */
/*            first derivative at an array of points.  While designed for */
/*            use by DPCHFD, it may be useful directly as an evaluator */
/*            for a piecewise cubic Hermite function in applications, */
/*            such as graphing, where the interval is known in advance. */
/*            If only function values are required, use DCHFEV instead. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***KEYWORDS  CUBIC HERMITE DIFFERENTIATION, CUBIC HERMITE EVALUATION, */
/*             CUBIC POLYNOMIAL EVALUATION, PCHIP */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */

/*        DCHFDV:  Cubic Hermite Function and Derivative Evaluator */

/*     Evaluates the cubic polynomial determined by function values */
/*     F1,F2 and derivatives D1,D2 on interval (X1,X2), together with */
/*     its first derivative, at the points  XE(J), J=1(1)NE. */

/*     If only function values are required, use DCHFEV, instead. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        INTEGER  NE, NEXT(2), IERR */
/*        DOUBLE PRECISION  X1, X2, F1, F2, D1, D2, XE(NE), FE(NE), */
/*                          DE(NE) */

/*        CALL  DCHFDV (X1,X2, F1,F2, D1,D2, NE, XE, FE, DE, NEXT, IERR) */

/*   Parameters: */

/*     X1,X2 -- (input) endpoints of interval of definition of cubic. */
/*           (Error return if  X1.EQ.X2 .) */

/*     F1,F2 -- (input) values of function at X1 and X2, respectively. */

/*     D1,D2 -- (input) values of derivative at X1 and X2, respectively. */

/*     NE -- (input) number of evaluation points.  (Error return if */
/*           NE.LT.1 .) */

/*     XE -- (input) real*8 array of points at which the functions are to */
/*           be evaluated.  If any of the XE are outside the interval */
/*           [X1,X2], a warning error is returned in NEXT. */

/*     FE -- (output) real*8 array of values of the cubic function */
/*           defined by  X1,X2, F1,F2, D1,D2  at the points  XE. */

/*     DE -- (output) real*8 array of values of the first derivative of */
/*           the same function at the points  XE. */

/*     NEXT -- (output) integer array indicating number of extrapolation */
/*           points: */
/*            NEXT(1) = number of evaluation points to left of interval. */
/*            NEXT(2) = number of evaluation points to right of interval. */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           "Recoverable" errors: */
/*              IERR = -1  if NE.LT.1 . */
/*              IERR = -2  if X1.EQ.X2 . */
/*                (Output arrays have not been changed in either case.) */

/* ***REVISION HISTORY  (YYMMDD) */
/*   811019  DATE WRITTEN */
/*   820803  Minor cosmetic changes for release 1. */
/*   870707  Corrected XERROR calls for d.p. names(s). */
/*   870813  Minor cosmetic changes. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   891006  Cosmetic changes to prologue.  (WRB) */
/*   891006  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*  Programming notes: */

/*     To produce a single precision version, simply: */
/*        a. Change DCHFDV to CHFDV wherever it occurs, */
/*        b. Change the double precision declaration to real, and */
/*        c. Change the constant ZERO to single precision. */

    /* Parameter adjustments */
    --next;
    --de;
    --fe;
    --xe;


/*  VALIDITY-CHECK ARGUMENTS. */

    if (*ne < 1) {
	goto L5001;
    }
    h__ = *x2 - *x1;
    if (h__ == zero) {
	goto L5002;
    }

/*  INITIALIZE. */

    *ierr = 0;
    next[1] = 0;
    next[2] = 0;
    xmi = min(zero,h__);
    xma = max(zero,h__);

/*  COMPUTE CUBIC COEFFICIENTS (EXPANDED ABOUT X1). */

    delta = (*f2 - *f1) / h__;
    del1 = (*d1 - delta) / h__;
    del2 = (*d2 - delta) / h__;
/*                                           (DELTA IS NO LONGER NEEDED.) */
    c2 = -(del1 + del1 + del2);
    c2t2 = c2 + c2;
    c3 = (del1 + del2) / h__;
/*                               (H, DEL1 AND DEL2 ARE NO LONGER NEEDED.) */
    c3t3 = c3 + c3 + c3;

/*  EVALUATION LOOP. */

    i__1 = *ne;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x = xe[i__] - *x1;
	fe[i__] = *f1 + x * (*d1 + x * (c2 + x * c3));
	de[i__] = *d1 + x * (c2t2 + x * c3t3);
/*          COUNT EXTRAPOLATION POINTS. */
	if (x < xmi) {
	    ++next[1];
	}
	if (x > xma) {
	    ++next[2];
	}
/*        (NOTE REDUNDANCY--IF EITHER CONDITION IS TRUE, OTHER IS FALSE.) */
    }

/*  NORMAL RETURN. */

    return;

/*  ERROR RETURNS. */

L5001:
/*     NE.LT.1 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "DCHFDV", "NUMBER OF EVALUATION POINTS LESS THAN ONE", *ierr);
    return;

L5002:
/*     X1.EQ.X2 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "DCHFDV", "INTERVAL ENDPOINTS EQUAL", *ierr);
    return;
}

void dpchfd(integer *n, doublereal *x, doublereal *f,
	doublereal *d__, integer *incfd, logical *skip, integer *ne,
	doublereal *xe, doublereal *fe, doublereal *de, integer *ierr)
{
    /* System generated locals */
    integer f_dim1, f_offset, d_dim1, d_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j, nj, ir, ierc, next[2];
    static integer jfirst;

/* ***PURPOSE  Evaluate a piecewise cubic Hermite function and its first */
/*            derivative at an array of points.  May be used by itself */
/*            for Hermite interpolation, or as an evaluator for DPCHIM */
/*            or DPCHIC. If only function values are required, use */
/*            DPCHFE instead. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***KEYWORDS  CUBIC HERMITE DIFFERENTIATION, CUBIC HERMITE EVALUATION, */
/*             HERMITE INTERPOLATION, PCHIP, PIECEWISE CUBIC EVALUATION */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */

/*          DPCHFD:  Piecewise Cubic Hermite Function and Derivative */
/*                  evaluator */

/*     Evaluates the cubic Hermite function defined by  N, X, F, D,  to- */
/*     gether with its first derivative, at the points  XE(J), J=1(1)NE. */

/*     If only function values are required, use DPCHFE, instead. */

/*     To provide compatibility with DPCHIM and DPCHIC, includes an */
/*     increment between successive values of the F- and D-arrays. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  N, NE, IERR */
/*        DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N), XE(NE), FE(NE), */
/*                          DE(NE) */
/*        LOGICAL  SKIP */

/*        CALL  DPCHFD (N, X, F, D, INCFD, SKIP, NE, XE, FE, DE, IERR) */

/*   Parameters: */

/*     N -- (input) number of data points.  (Error return if N.LT.2 .) */

/*     X -- (input) real*8 array of independent variable values.  The */
/*           elements of X must be strictly increasing: */
/*                X(I-1) .LT. X(I),  I = 2(1)N. */
/*           (Error return if not.) */

/*     F -- (input) real*8 array of function values.  F(1+(I-1)*INCFD) is */
/*           the value corresponding to X(I). */

/*     D -- (input) real*8 array of derivative values.  D(1+(I-1)*INCFD) */
/*           is the value corresponding to X(I). */

/*     INCFD -- (input) increment between successive values in F and D. */
/*           (Error return if  INCFD.LT.1 .) */

/*     SKIP -- (input/output) logical variable which should be set to */
/*           .TRUE. if the user wishes to skip checks for validity of */
/*           preceding parameters, or to .FALSE. otherwise. */
/*           This will save time in case these checks have already */
/*           been performed (say, in DPCHIM or DPCHIC). */
/*           SKIP will be set to .TRUE. on normal return. */

/*     NE -- (input) number of evaluation points.  (Error return if */
/*           NE.LT.1 .) */

/*     XE -- (input) real*8 array of points at which the functions are to */
/*           be evaluated. */


/*          NOTES: */
/*           1. The evaluation will be most efficient if the elements */
/*              of XE are increasing relative to X; */
/*              that is,   XE(J) .GE. X(I) */
/*              implies    XE(K) .GE. X(I),  all K.GE.J . */
/*           2. If any of the XE are outside the interval [X(1),X(N)], */
/*              values are extrapolated from the nearest extreme cubic, */
/*              and a warning error is returned. */

/*     FE -- (output) real*8 array of values of the cubic Hermite */
/*           function defined by  N, X, F, D  at the points  XE. */

/*     DE -- (output) real*8 array of values of the first derivative of */
/*           the same function at the points  XE. */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           Warning error: */
/*              IERR.GT.0  means that extrapolation was performed at */
/*                 IERR points. */
/*           "Recoverable" errors: */
/*              IERR = -1  if N.LT.2 . */
/*              IERR = -2  if INCFD.LT.1 . */
/*              IERR = -3  if the X-array is not strictly increasing. */
/*              IERR = -4  if NE.LT.1 . */
/*           (Output arrays have not been changed in any of these cases.) */
/*               NOTE:  The above errors are checked in the order listed, */
/*                   and following arguments have **NOT** been validated. */
/*              IERR = -5  if an error has occurred in the lower-level */
/*                         routine DCHFDV.  NB: this should never happen. */
/*                         Notify the author **IMMEDIATELY** if it does. */

/* ***REVISION HISTORY  (YYMMDD) */
/*   811020  DATE WRITTEN */
/*   820803  Minor cosmetic changes for release 1. */
/*   870707  Corrected XERROR calls for d.p. name(s). */
/*   890206  Corrected XERROR calls. */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   891006  Cosmetic changes to prologue.  (WRB) */
/*   891006  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*  Programming notes: */

/*     1. To produce a single precision version, simply: */
/*        a. Change DPCHFD to PCHFD, and DCHFDV to CHFDV, wherever they */
/*           occur, */
/*        b. Change the double precision declaration to real, */

/*     2. Most of the coding between the call to DCHFDV and the end of */
/*        the IR-loop could be eliminated if it were permissible to */
/*        assume that XE is ordered relative to X. */

/*     3. DCHFDV does not assume that X1 is less than X2.  thus, it would */
/*        be possible to write a version of DPCHFD that assumes a strict- */
/*        ly decreasing X-array by simply running the IR-loop backwards */
/*        (and reversing the order of appropriate tests). */

/*     4. The present code has a minor bug, which I have decided is not */
/*        worth the effort that would be required to fix it. */
/*        If XE contains points in [X(N-1),X(N)], followed by points .LT. */
/*        X(N-1), followed by points .GT.X(N), the extrapolation points */
/*        will be counted (at least) twice in the total returned in IERR. */

/*  VALIDITY-CHECK ARGUMENTS. */

    /* Parameter adjustments */
    --x;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;
    f_dim1 = *incfd;
    f_offset = 1 + f_dim1;
    f -= f_offset;
    --xe;
    --fe;
    --de;

    if (*skip) {
	goto L5;
    }

    if (*n < 2) {
	goto L5001;
    }
    if (*incfd < 1) {
	goto L5002;
    }
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__] <= x[i__ - 1]) {
	    goto L5003;
	}
    }

/*  FUNCTION DEFINITION IS OK, GO ON. */

L5:
    if (*ne < 1) {
	goto L5004;
    }
    *ierr = 0;
    *skip = TRUE_;

/*  LOOP OVER INTERVALS.        (   INTERVAL INDEX IS  IL = IR-1  . ) */
/*                              ( INTERVAL IS X(IL).LE.X.LT.X(IR) . ) */
    jfirst = 1;
    ir = 2;
L10:

/*     SKIP OUT OF LOOP IF HAVE PROCESSED ALL EVALUATION POINTS. */

    if (jfirst > *ne) {
	goto L5000;
    }

/*     LOCATE ALL POINTS IN INTERVAL. */

    i__1 = *ne;
    for (j = jfirst; j <= i__1; ++j) {
	if (xe[j] >= x[ir]) {
	    goto L30;
	}
    }
    j = *ne + 1;
    goto L40;

/*     HAVE LOCATED FIRST POINT BEYOND INTERVAL. */

L30:
    if (ir == *n) {
	j = *ne + 1;
    }

L40:
    nj = j - jfirst;

/*     SKIP EVALUATION IF NO POINTS IN INTERVAL. */

    if (nj == 0) {
	goto L50;
    }

/*     EVALUATE CUBIC AT XE(I),  I = JFIRST (1) J-1 . */

/*       ---------------------------------------------------------------- */
    dchfdv(&x[ir - 1], &x[ir], &f[(ir - 1) * f_dim1 + 1], &f[ir * f_dim1 + 1]
	    , &d__[(ir - 1) * d_dim1 + 1], &d__[ir * d_dim1 + 1], &nj, &xe[
	    jfirst], &fe[jfirst], &de[jfirst], next, &ierc);
/*       ---------------------------------------------------------------- */
    if (ierc < 0) {
	goto L5005;
    }

    if (next[1] == 0) {
	goto L42;
    }
/*        IF (NEXT(2) .GT. 0)  THEN */
/*           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(2) TO THE */
/*           RIGHT OF X(IR). */

    if (ir < *n) {
	goto L41;
    }
/*           IF (IR .EQ. N)  THEN */
/*              THESE ARE ACTUALLY EXTRAPOLATION POINTS. */
    *ierr += next[1];
    goto L42;
L41:
/*           ELSE */
/*              WE SHOULD NEVER HAVE GOTTEN HERE. */
    goto L5005;
/*           ENDIF */
/*        ENDIF */
L42:

    if (next[0] == 0) {
	goto L49;
    }
/*        IF (NEXT(1) .GT. 0)  THEN */
/*           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(1) TO THE */
/*           LEFT OF X(IR-1). */

    if (ir > 2) {
	goto L43;
    }
/*           IF (IR .EQ. 2)  THEN */
/*              THESE ARE ACTUALLY EXTRAPOLATION POINTS. */
    *ierr += next[0];
    goto L49;
L43:
/*           ELSE */
/*              XE IS NOT ORDERED RELATIVE TO X, SO MUST ADJUST */
/*              EVALUATION INTERVAL. */

/*              FIRST, LOCATE FIRST POINT TO LEFT OF X(IR-1). */
    i__1 = j - 1;
    for (i__ = jfirst; i__ <= i__1; ++i__) {
	if (xe[i__] < x[ir - 1]) {
	    goto L45;
	}
    }
/*              NOTE-- CANNOT DROP THROUGH HERE UNLESS THERE IS AN ERROR */
/*                     IN DCHFDV. */
    goto L5005;

L45:
/*              RESET J.  (THIS WILL BE THE NEW JFIRST.) */
    j = i__;

/*              NOW FIND OUT HOW FAR TO BACK UP IN THE X-ARRAY. */
    i__1 = ir - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (xe[j] < x[i__]) {
	    goto L47;
	}
    }
/*              NB-- CAN NEVER DROP THROUGH HERE, SINCE XE(J).LT.X(IR-1). */

L47:
/*              AT THIS POINT, EITHER  XE(J) .LT. X(1) */
/*                 OR      X(I-1) .LE. XE(J) .LT. X(I) . */
/*              RESET IR, RECOGNIZING THAT IT WILL BE INCREMENTED BEFORE */
/*              CYCLING. */
/* Computing MAX */
    i__1 = 1, i__2 = i__ - 1;
    ir = max(i__1,i__2);
/*           ENDIF */
/*        ENDIF */
L49:

    jfirst = j;

/*     END OF IR-LOOP. */

L50:
    ++ir;
    if (ir <= *n) {
	goto L10;
    }

/*  NORMAL RETURN. */

L5000:
    return;

/*  ERROR RETURNS. */

L5001:
/*     N.LT.2 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "DPCHFD", "NUMBER OF DATA POINTS LESS THAN TWO", *ierr);
    return;

L5002:
/*     INCFD.LT.1 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "DPCHFD", "INCREMENT LESS THAN ONE", *ierr);
    return;

L5003:
/*     X-ARRAY NOT STRICTLY INCREASING. */
    *ierr = -3;
    xermsg_("SLATEC", "DPCHFD", "X-ARRAY NOT STRICTLY INCREASING", *ierr);
    return;

L5004:
/*     NE.LT.1 RETURN. */
    *ierr = -4;
    xermsg_("SLATEC", "DPCHFD", "NUMBER OF EVALUATION POINTS LESS THAN ONE", *ierr);
    return;

L5005:
/*     ERROR RETURN FROM DCHFDV. */
/*   *** THIS CASE SHOULD NEVER OCCUR *** */
    *ierr = -5;
    xermsg_("SLATEC", "DPCHFD", "ERROR RETURN FROM DCHFDV -- FATAL", *ierr);
    return;
}

void dchfev(doublereal *x1, doublereal *x2, doublereal *f1,
	doublereal *f2, doublereal *d1, doublereal *d2, integer *ne,
	doublereal *xe, doublereal *fe, integer *next, integer *ierr)
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static doublereal h__;
    static integer i__;
    static doublereal x, c2, c3, xma, xmi, del1, del2, delta;

/* ***PURPOSE  Evaluate a cubic polynomial given in Hermite form at an */
/*            array of points.  While designed for use by DPCHFE, it may */
/*            be useful directly as an evaluator for a piecewise cubic */
/*            Hermite function in applications, such as graphing, where */
/*            the interval is known in advance. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***KEYWORDS  CUBIC HERMITE EVALUATION, CUBIC POLYNOMIAL EVALUATION, */
/*             PCHIP */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */

/*          DCHFEV:  Cubic Hermite Function EValuator */

/*     Evaluates the cubic polynomial determined by function values */
/*     F1,F2 and derivatives D1,D2 on interval (X1,X2) at the points */
/*     XE(J), J=1(1)NE. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        INTEGER  NE, NEXT(2), IERR */
/*        DOUBLE PRECISION  X1, X2, F1, F2, D1, D2, XE(NE), FE(NE) */

/*        CALL  DCHFEV (X1,X2, F1,F2, D1,D2, NE, XE, FE, NEXT, IERR) */

/*   Parameters: */

/*     X1,X2 -- (input) endpoints of interval of definition of cubic. */
/*           (Error return if  X1.EQ.X2 .) */

/*     F1,F2 -- (input) values of function at X1 and X2, respectively. */

/*     D1,D2 -- (input) values of derivative at X1 and X2, respectively. */

/*     NE -- (input) number of evaluation points.  (Error return if */
/*           NE.LT.1 .) */

/*     XE -- (input) real*8 array of points at which the function is to */
/*           be evaluated.  If any of the XE are outside the interval */
/*           [X1,X2], a warning error is returned in NEXT. */

/*     FE -- (output) real*8 array of values of the cubic function */
/*           defined by  X1,X2, F1,F2, D1,D2  at the points  XE. */

/*     NEXT -- (output) integer array indicating number of extrapolation */
/*           points: */
/*            NEXT(1) = number of evaluation points to left of interval. */
/*            NEXT(2) = number of evaluation points to right of interval. */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           "Recoverable" errors: */
/*              IERR = -1  if NE.LT.1 . */
/*              IERR = -2  if X1.EQ.X2 . */
/*                (The FE-array has not been changed in either case.) */

/* ***REVISION HISTORY  (YYMMDD) */
/*   811019  DATE WRITTEN */
/*   820803  Minor cosmetic changes for release 1. */
/*   870813  Corrected XERROR calls for d.p. names(s). */
/*   890206  Corrected XERROR calls. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890703  Corrected category record.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   891006  Cosmetic changes to prologue.  (WRB) */
/*   891006  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*  Programming notes: */

/*     To produce a single precision version, simply: */
/*        a. Change DCHFEV to CHFEV wherever it occurs, */
/*        b. Change the double precision declaration to real, and */
/*        c. Change the constant ZERO to single precision. */

/*  DECLARE LOCAL VARIABLES. */

    /* Parameter adjustments */
    --next;
    --fe;
    --xe;


/*  VALIDITY-CHECK ARGUMENTS. */

    if (*ne < 1) {
	goto L5001;
    }
    h__ = *x2 - *x1;
    if (h__ == zero) {
	goto L5002;
    }

/*  INITIALIZE. */

    *ierr = 0;
    next[1] = 0;
    next[2] = 0;
    xmi = min(zero,h__);
    xma = max(zero,h__);

/*  COMPUTE CUBIC COEFFICIENTS (EXPANDED ABOUT X1). */

    delta = (*f2 - *f1) / h__;
    del1 = (*d1 - delta) / h__;
    del2 = (*d2 - delta) / h__;
/*                                           (DELTA IS NO LONGER NEEDED.) */
    c2 = -(del1 + del1 + del2);
    c3 = (del1 + del2) / h__;
/*                               (H, DEL1 AND DEL2 ARE NO LONGER NEEDED.) */

/*  EVALUATION LOOP. */

    i__1 = *ne;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x = xe[i__] - *x1;
	fe[i__] = *f1 + x * (*d1 + x * (c2 + x * c3));
/*          COUNT EXTRAPOLATION POINTS. */
	if (x < xmi) {
	    ++next[1];
	}
	if (x > xma) {
	    ++next[2];
	}
/*        (NOTE REDUNDANCY--IF EITHER CONDITION IS TRUE, OTHER IS FALSE.) */
    }

/*  NORMAL RETURN. */

    return;

/*  ERROR RETURNS. */

L5001:
/*     NE.LT.1 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "DCHFEV", "NUMBER OF EVALUATION POINTS LESS THAN ONE", *ierr);
    return;

L5002:
/*     X1.EQ.X2 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "DCHFEV", "INTERVAL ENDPOINTS EQUAL", *ierr);
    return;
}

void dpchfe(integer *n, doublereal *x, doublereal *f,
	doublereal *d__, integer *incfd, logical *skip, integer *ne,
	doublereal *xe, doublereal *fe, integer *ierr)
{
    /* System generated locals */
    integer f_dim1, f_offset, d_dim1, d_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j, nj, ir, ierc, next[2];
    static integer jfirst;

/* ***PURPOSE  Evaluate a piecewise cubic Hermite function at an array of */
/*            points.  May be used by itself for Hermite interpolation, */
/*            or as an evaluator for DPCHIM or DPCHIC. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***KEYWORDS  CUBIC HERMITE EVALUATION, HERMITE INTERPOLATION, PCHIP, */
/*             PIECEWISE CUBIC EVALUATION */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */

/*          DPCHFE:  Piecewise Cubic Hermite Function Evaluator */

/*     Evaluates the cubic Hermite function defined by  N, X, F, D  at */
/*     the points  XE(J), J=1(1)NE. */

/*     To provide compatibility with DPCHIM and DPCHIC, includes an */
/*     increment between successive values of the F- and D-arrays. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  N, NE, IERR */
/*        DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N), XE(NE), FE(NE) */
/*        LOGICAL  SKIP */

/*        CALL  DPCHFE (N, X, F, D, INCFD, SKIP, NE, XE, FE, IERR) */

/*   Parameters: */

/*     N -- (input) number of data points.  (Error return if N.LT.2 .) */

/*     X -- (input) real*8 array of independent variable values.  The */
/*           elements of X must be strictly increasing: */
/*                X(I-1) .LT. X(I),  I = 2(1)N. */
/*           (Error return if not.) */

/*     F -- (input) real*8 array of function values.  F(1+(I-1)*INCFD) is */
/*           the value corresponding to X(I). */

/*     D -- (input) real*8 array of derivative values.  D(1+(I-1)*INCFD) */
/*           is the value corresponding to X(I). */

/*     INCFD -- (input) increment between successive values in F and D. */
/*           (Error return if  INCFD.LT.1 .) */

/*     SKIP -- (input/output) logical variable which should be set to */
/*           .TRUE. if the user wishes to skip checks for validity of */
/*           preceding parameters, or to .FALSE. otherwise. */
/*           This will save time in case these checks have already */
/*           been performed (say, in DPCHIM or DPCHIC). */
/*           SKIP will be set to .TRUE. on normal return. */

/*     NE -- (input) number of evaluation points.  (Error return if */
/*           NE.LT.1 .) */

/*     XE -- (input) real*8 array of points at which the function is to */
/*           be evaluated. */

/*          NOTES: */
/*           1. The evaluation will be most efficient if the elements */
/*              of XE are increasing relative to X; */
/*              that is,   XE(J) .GE. X(I) */
/*              implies    XE(K) .GE. X(I),  all K.GE.J . */
/*           2. If any of the XE are outside the interval [X(1),X(N)], */
/*              values are extrapolated from the nearest extreme cubic, */
/*              and a warning error is returned. */

/*     FE -- (output) real*8 array of values of the cubic Hermite */
/*           function defined by  N, X, F, D  at the points  XE. */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           Warning error: */
/*              IERR.GT.0  means that extrapolation was performed at */
/*                 IERR points. */
/*           "Recoverable" errors: */
/*              IERR = -1  if N.LT.2 . */
/*              IERR = -2  if INCFD.LT.1 . */
/*              IERR = -3  if the X-array is not strictly increasing. */
/*              IERR = -4  if NE.LT.1 . */
/*             (The FE-array has not been changed in any of these cases.) */
/*               NOTE:  The above errors are checked in the order listed, */
/*                   and following arguments have **NOT** been validated. */

/* ***REVISION HISTORY  (YYMMDD) */
/*   811020  DATE WRITTEN */
/*   820803  Minor cosmetic changes for release 1. */
/*   870707  Corrected XERROR calls for d.p. name(s). */
/*   890206  Corrected XERROR calls. */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   891006  Cosmetic changes to prologue.  (WRB) */
/*   891006  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/* ***END PROLOGUE  DPCHFE */
/*  Programming notes: */

/*     1. To produce a single precision version, simply: */
/*        a. Change DPCHFE to PCHFE, and DCHFEV to CHFEV, wherever they */
/*           occur, */
/*        b. Change the double precision declaration to real, */

/*     2. Most of the coding between the call to DCHFEV and the end of */
/*        the IR-loop could be eliminated if it were permissible to */
/*        assume that XE is ordered relative to X. */

/*     3. DCHFEV does not assume that X1 is less than X2.  thus, it would */
/*        be possible to write a version of DPCHFE that assumes a */
/*        decreasing X-array by simply running the IR-loop backwards */
/*        (and reversing the order of appropriate tests). */

/*     4. The present code has a minor bug, which I have decided is not */
/*        worth the effort that would be required to fix it. */
/*        If XE contains points in [X(N-1),X(N)], followed by points .LT. */
/*        X(N-1), followed by points .GT.X(N), the extrapolation points */
/*        will be counted (at least) twice in the total returned in IERR. */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*  VALIDITY-CHECK ARGUMENTS. */

    /* Parameter adjustments */
    --x;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;
    f_dim1 = *incfd;
    f_offset = 1 + f_dim1;
    f -= f_offset;
    --xe;
    --fe;

    if (*skip) {
	goto L5;
    }

    if (*n < 2) {
	goto L5001;
    }
    if (*incfd < 1) {
	goto L5002;
    }
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__] <= x[i__ - 1]) {
	    goto L5003;
	}
    }

/*  FUNCTION DEFINITION IS OK, GO ON. */

L5:
    if (*ne < 1) {
	goto L5004;
    }
    *ierr = 0;
    *skip = TRUE_;

/*  LOOP OVER INTERVALS.        (   INTERVAL INDEX IS  IL = IR-1  . ) */
/*                              ( INTERVAL IS X(IL).LE.X.LT.X(IR) . ) */
    jfirst = 1;
    ir = 2;
L10:

/*     SKIP OUT OF LOOP IF HAVE PROCESSED ALL EVALUATION POINTS. */

    if (jfirst > *ne) {
	goto L5000;
    }

/*     LOCATE ALL POINTS IN INTERVAL. */

    i__1 = *ne;
    for (j = jfirst; j <= i__1; ++j) {
	if (xe[j] >= x[ir]) {
	    goto L30;
	}
    }
    j = *ne + 1;
    goto L40;

/*     HAVE LOCATED FIRST POINT BEYOND INTERVAL. */

L30:
    if (ir == *n) {
	j = *ne + 1;
    }

L40:
    nj = j - jfirst;

/*     SKIP EVALUATION IF NO POINTS IN INTERVAL. */

    if (nj == 0) {
	goto L50;
    }

/*     EVALUATE CUBIC AT XE(I),  I = JFIRST (1) J-1 . */

/*       ---------------------------------------------------------------- */
    dchfev(&x[ir - 1], &x[ir], &f[(ir - 1) * f_dim1 + 1], &f[ir * f_dim1 + 1]
	    , &d__[(ir - 1) * d_dim1 + 1], &d__[ir * d_dim1 + 1], &nj, &xe[
	    jfirst], &fe[jfirst], next, &ierc);
/*       ---------------------------------------------------------------- */
    if (ierc < 0) {
	goto L5005;
    }

    if (next[1] == 0) {
	goto L42;
    }
/*        IF (NEXT(2) .GT. 0)  THEN */
/*           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(2) TO THE */
/*           RIGHT OF X(IR). */

    if (ir < *n) {
	goto L41;
    }
/*           IF (IR .EQ. N)  THEN */
/*              THESE ARE ACTUALLY EXTRAPOLATION POINTS. */
    *ierr += next[1];
    goto L42;
L41:
/*           ELSE */
/*              WE SHOULD NEVER HAVE GOTTEN HERE. */
    goto L5005;
/*           ENDIF */
/*        ENDIF */
L42:

    if (next[0] == 0) {
	goto L49;
    }
/*        IF (NEXT(1) .GT. 0)  THEN */
/*           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(1) TO THE */
/*           LEFT OF X(IR-1). */

    if (ir > 2) {
	goto L43;
    }
/*           IF (IR .EQ. 2)  THEN */
/*              THESE ARE ACTUALLY EXTRAPOLATION POINTS. */
    *ierr += next[0];
    goto L49;
L43:
/*           ELSE */
/*              XE IS NOT ORDERED RELATIVE TO X, SO MUST ADJUST */
/*              EVALUATION INTERVAL. */

/*              FIRST, LOCATE FIRST POINT TO LEFT OF X(IR-1). */
    i__1 = j - 1;
    for (i__ = jfirst; i__ <= i__1; ++i__) {
	if (xe[i__] < x[ir - 1]) {
	    goto L45;
	}
    }
/*              NOTE-- CANNOT DROP THROUGH HERE UNLESS THERE IS AN ERROR */
/*                     IN DCHFEV. */
    goto L5005;

L45:
/*              RESET J.  (THIS WILL BE THE NEW JFIRST.) */
    j = i__;

/*              NOW FIND OUT HOW FAR TO BACK UP IN THE X-ARRAY. */
    i__1 = ir - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (xe[j] < x[i__]) {
	    goto L47;
	}
    }
/*              NB-- CAN NEVER DROP THROUGH HERE, SINCE XE(J).LT.X(IR-1). */

L47:
/*              AT THIS POINT, EITHER  XE(J) .LT. X(1) */
/*                 OR      X(I-1) .LE. XE(J) .LT. X(I) . */
/*              RESET IR, RECOGNIZING THAT IT WILL BE INCREMENTED BEFORE */
/*              CYCLING. */
/* Computing MAX */
    i__1 = 1, i__2 = i__ - 1;
    ir = max(i__1,i__2);
/*           ENDIF */
/*        ENDIF */
L49:

    jfirst = j;

/*     END OF IR-LOOP. */

L50:
    ++ir;
    if (ir <= *n) {
	goto L10;
    }

/*  NORMAL RETURN. */

L5000:
    return;

/*  ERROR RETURNS. */

L5001:
/*     N.LT.2 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "DPCHFE", "NUMBER OF DATA POINTS LESS THAN TWO", *ierr);
    return;

L5002:
/*     INCFD.LT.1 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "DPCHFE", "INCREMENT LESS THAN ONE", *ierr);
    return;

L5003:
/*     X-ARRAY NOT STRICTLY INCREASING. */
    *ierr = -3;
    xermsg_("SLATEC", "DPCHFE", "X-ARRAY NOT STRICTLY INCREASING", *ierr);
    return;

L5004:
/*     NE.LT.1 RETURN. */
    *ierr = -4;
    xermsg_("SLATEC", "DPCHFE", "NUMBER OF EVALUATION POINTS LESS THAN ONE", *ierr);
    return;

L5005:
/*     ERROR RETURN FROM DCHFEV. */
/*   *** THIS CASE SHOULD NEVER OCCUR *** */
    *ierr = -5;
    xermsg_("SLATEC", "DPCHFE", "ERROR RETURN FROM DCHFEV -- FATAL", *ierr);
    return;
}

doublereal dchfie(doublereal *x1, doublereal *x2, doublereal *f1, doublereal
	*f2, doublereal *d1, doublereal *d2, doublereal *a, doublereal *b)
{
    /* Initialized data */

    static doublereal half = .5;
    static doublereal two = 2.;
    static doublereal three = 3.;
    static doublereal four = 4.;
    static doublereal six = 6.;

    /* System generated locals */
    doublereal ret_val, d__1;

    /* Local variables */
    static doublereal h__, ta1, ta2, tb1, tb2, ua1, ua2, ub1, ub2, phia1,
	    phia2, phib1, phib2, psia1, psia2, psib1, psib2, dterm, fterm;

/* ***BEGIN PROLOGUE  DCHFIE */
/* ***PURPOSE  Evaluates integral of a single cubic for DPCHIA */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */

/*          DCHFIE:  Cubic Hermite Function Integral Evaluator. */

/*     Called by  DPCHIA  to evaluate the integral of a single cubic (in */
/*     Hermite form) over an arbitrary interval (A,B). */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        DOUBLE PRECISION  X1, X2, F1, F2, D1, D2, A, B */
/*        DOUBLE PRECISION  VALUE, DCHFIE */

/*        VALUE = DCHFIE (X1, X2, F1, F2, D1, D2, A, B) */

/*   Parameters: */

/*     VALUE -- (output) value of the requested integral. */

/*     X1,X2 -- (input) endpoints if interval of definition of cubic. */

/*     F1,F2 -- (input) function values at the ends of the interval. */

/*     D1,D2 -- (input) derivative values at the ends of the interval. */

/*     A,B -- (input) endpoints of interval of integration. */

/* ***SEE ALSO  DPCHIA */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820730  DATE WRITTEN */
/*   820805  Converted to SLATEC library version. */
/*   870707  Corrected subroutine name from DCHIV to DCHFIV. */
/*   870813  Minor cosmetic changes. */
/*   890411  1. Added SAVE statements (Vers. 3.2). */
/*           2. Added SIX to DOUBLE PRECISION declaration. */
/*   890411  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   900328  Added TYPE section.  (WRB) */
/*   910408  Updated AUTHOR section in prologue.  (WRB) */
/*   930503  Corrected to set VALUE=0 when IERR.ne.0.  (FNF) */
/*   930504  Eliminated IERR and changed name DCHFIV to DCHFIE.  (FNF) */
/* ***END PROLOGUE  DCHFIE */

/*  Programming notes: */
/*  1. There is no error return from this routine because zero is */
/*     indeed the mathematically correct answer when X1.EQ.X2 . */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*  INITIALIZE. */


/*  VALIDITY CHECK INPUT. */

    if (*x1 == *x2) {
	ret_val = 0.;
    } else {
	h__ = *x2 - *x1;
	ta1 = (*a - *x1) / h__;
	ta2 = (*x2 - *a) / h__;
	tb1 = (*b - *x1) / h__;
	tb2 = (*x2 - *b) / h__;

/* Computing 3rd power */
	d__1 = ta1;
	ua1 = d__1 * (d__1 * d__1);
	phia1 = ua1 * (two - ta1);
	psia1 = ua1 * (three * ta1 - four);
/* Computing 3rd power */
	d__1 = ta2;
	ua2 = d__1 * (d__1 * d__1);
	phia2 = ua2 * (two - ta2);
	psia2 = -ua2 * (three * ta2 - four);

/* Computing 3rd power */
	d__1 = tb1;
	ub1 = d__1 * (d__1 * d__1);
	phib1 = ub1 * (two - tb1);
	psib1 = ub1 * (three * tb1 - four);
/* Computing 3rd power */
	d__1 = tb2;
	ub2 = d__1 * (d__1 * d__1);
	phib2 = ub2 * (two - tb2);
	psib2 = -ub2 * (three * tb2 - four);

	fterm = *f1 * (phia2 - phib2) + *f2 * (phib1 - phia1);
	dterm = (*d1 * (psia2 - psib2) + *d2 * (psib1 - psia1)) * (h__ / six);

	ret_val = half * h__ * (fterm + dterm);
    }

    return ret_val;
}

doublereal dpchia(integer *n, doublereal *x, doublereal *f, doublereal *d__,
	integer *incfd, logical *skip, doublereal *a, doublereal *b, integer *
	ierr)
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer f_dim1, f_offset, d_dim1, d_offset, i__1, i__2;
    doublereal ret_val;

    /* Local variables */
    static integer i__, ia, ib, il;
    static doublereal xa, xb;
    static integer ir, ierd;
    static doublereal value;

/* ***BEGIN PROLOGUE  DPCHIA */
/* ***PURPOSE  Evaluate the definite integral of a piecewise cubic */
/*            Hermite function over an arbitrary interval. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***CATEGORY  E3, H2A1B2 */
/* ***KEYWORDS  CUBIC HERMITE INTERPOLATION, NUMERICAL INTEGRATION, PCHIP, */
/*             QUADRATURE */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */

/*          DPCHIA:  Piecewise Cubic Hermite Integrator, Arbitrary limits */

/*     Evaluates the definite integral of the cubic Hermite function */
/*     defined by  N, X, F, D  over the interval [A, B]. */

/*     To provide compatibility with DPCHIM and DPCHIC, includes an */
/*     increment between successive values of the F- and D-arrays. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  N, IERR */
/*        DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N), A, B */
/*        DOUBLE PRECISION  VALUE, DPCHIA */
/*        LOGICAL  SKIP */

/*        VALUE = DPCHIA (N, X, F, D, INCFD, SKIP, A, B, IERR) */

/*   Parameters: */

/*     VALUE -- (output) value of the requested integral. */

/*     N -- (input) number of data points.  (Error return if N.LT.2 .) */

/*     X -- (input) real*8 array of independent variable values.  The */
/*           elements of X must be strictly increasing: */
/*                X(I-1) .LT. X(I),  I = 2(1)N. */
/*           (Error return if not.) */

/*     F -- (input) real*8 array of function values.  F(1+(I-1)*INCFD) is */
/*           the value corresponding to X(I). */

/*     D -- (input) real*8 array of derivative values.  D(1+(I-1)*INCFD) */
/*           is the value corresponding to X(I). */

/*     INCFD -- (input) increment between successive values in F and D. */
/*           (Error return if  INCFD.LT.1 .) */

/*     SKIP -- (input/output) logical variable which should be set to */
/*           .TRUE. if the user wishes to skip checks for validity of */
/*           preceding parameters, or to .FALSE. otherwise. */
/*           This will save time in case these checks have already */
/*           been performed (say, in DPCHIM or DPCHIC). */
/*           SKIP will be set to .TRUE. on return with IERR.GE.0 . */

/*     A,B -- (input) the limits of integration. */
/*           NOTE:  There is no requirement that [A,B] be contained in */
/*                  [X(1),X(N)].  However, the resulting integral value */
/*                  will be highly suspect, if not. */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           Warning errors: */
/*              IERR = 1  if  A  is outside the interval [X(1),X(N)]. */
/*              IERR = 2  if  B  is outside the interval [X(1),X(N)]. */
/*              IERR = 3  if both of the above are true.  (Note that this */
/*                        means that either [A,B] contains data interval */
/*                        or the intervals do not intersect at all.) */
/*           "Recoverable" errors: */
/*              IERR = -1  if N.LT.2 . */
/*              IERR = -2  if INCFD.LT.1 . */
/*              IERR = -3  if the X-array is not strictly increasing. */
/*                (VALUE will be zero in any of these cases.) */
/*               NOTE:  The above errors are checked in the order listed, */
/*                   and following arguments have **NOT** been validated. */
/*              IERR = -4  in case of an error return from DPCHID (which */
/*                         should never occur). */

/* ***REVISION HISTORY  (YYMMDD) */
/*   820730  DATE WRITTEN */
/*   820804  Converted to SLATEC library version. */
/*   870707  Corrected XERROR calls for d.p. name(s). */
/*   870707  Corrected conversion to double precision. */
/*   870813  Minor cosmetic changes. */
/*   890206  Corrected XERROR calls. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890703  Corrected category record.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   891006  Cosmetic changes to prologue.  (WRB) */
/*   891006  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   930503  Corrected to set VALUE=0 when IERR.lt.0.  (FNF) */
/*   930504  Changed DCHFIV to DCHFIE.  (FNF) */
/* ***END PROLOGUE  DPCHIA */

/*  Programming notes: */
/*  1. The error flag from DPCHID is tested, because a logic flaw */
/*     could conceivably result in IERD=-4, which should be reported. */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*  INITIALIZE. */

    /* Parameter adjustments */
    --x;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;
    f_dim1 = *incfd;
    f_offset = 1 + f_dim1;
    f -= f_offset;

    value = zero;

/*  VALIDITY-CHECK ARGUMENTS. */

    if (*skip) {
	goto L5;
    }

    if (*n < 2) {
	goto L5001;
    }
    if (*incfd < 1) {
	goto L5002;
    }
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__] <= x[i__ - 1]) {
	    goto L5003;
	}
    }

/*  FUNCTION DEFINITION IS OK, GO ON. */

L5:
    *skip = TRUE_;
    *ierr = 0;
    if (*a < x[1] || *a > x[*n]) {
	++(*ierr);
    }
    if (*b < x[1] || *b > x[*n]) {
	*ierr += 2;
    }

/*  COMPUTE INTEGRAL VALUE. */

    if (*a != *b) {
	xa = min(*a,*b);
	xb = max(*a,*b);
	if (xb <= x[2]) {
/*           INTERVAL IS TO LEFT OF X(2), SO USE FIRST CUBIC. */
/*                   --------------------------------------- */
	    value = dchfie(&x[1], &x[2], &f[f_dim1 + 1], &f[(f_dim1 << 1) +
		    1], &d__[d_dim1 + 1], &d__[(d_dim1 << 1) + 1], a, b);
/*                   --------------------------------------- */
	} else if (xa >= x[*n - 1]) {
/*           INTERVAL IS TO RIGHT OF X(N-1), SO USE LAST CUBIC. */
/*                   ------------------------------------------ */
	    value = dchfie(&x[*n - 1], &x[*n], &f[(*n - 1) * f_dim1 + 1], &f[
		    *n * f_dim1 + 1], &d__[(*n - 1) * d_dim1 + 1], &d__[*n *
		    d_dim1 + 1], a, b);
/*                   ------------------------------------------ */
	} else {
/*           'NORMAL' CASE -- XA.LT.XB, XA.LT.X(N-1), XB.GT.X(2). */
/*      ......LOCATE IA AND IB SUCH THAT */
/*               X(IA-1).LT.XA.LE.X(IA).LE.X(IB).LE.XB.LE.X(IB+1) */
	    ia = 1;
	    i__1 = *n - 1;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (xa > x[i__]) {
		    ia = i__ + 1;
		}
	    }
/*             IA = 1 IMPLIES XA.LT.X(1) .  OTHERWISE, */
/*             IA IS LARGEST INDEX SUCH THAT X(IA-1).LT.XA,. */

	    ib = *n;
	    i__1 = ia;
	    for (i__ = *n; i__ >= i__1; --i__) {
		if (xb < x[i__]) {
		    ib = i__ - 1;
		}
	    }
/*             IB = N IMPLIES XB.GT.X(N) .  OTHERWISE, */
/*             IB IS SMALLEST INDEX SUCH THAT XB.LT.X(IB+1) . */

/*     ......COMPUTE THE INTEGRAL. */
	    if (ib < ia) {
/*              THIS MEANS IB = IA-1 AND */
/*                 (A,B) IS A SUBSET OF (X(IB),X(IA)). */
/*                      ------------------------------------------- */
		value = dchfie(&x[ib], &x[ia], &f[ib * f_dim1 + 1], &f[ia *
			f_dim1 + 1], &d__[ib * d_dim1 + 1], &d__[ia * d_dim1
			+ 1], a, b);
/*                      ------------------------------------------- */
	    } else {

/*              FIRST COMPUTE INTEGRAL OVER (X(IA),X(IB)). */
/*                (Case (IB .EQ. IA) is taken care of by initialization */
/*                 of VALUE to ZERO.) */
		if (ib > ia) {
/*                         --------------------------------------------- */
		    value = dpchid(n, &x[1], &f[f_offset], &d__[d_offset],
			    incfd, skip, &ia, &ib, &ierd);
/*                         --------------------------------------------- */
		    if (ierd < 0) {
			goto L5004;
		    }
		}

/*              THEN ADD ON INTEGRAL OVER (XA,X(IA)). */
		if (xa < x[ia]) {
/* Computing MAX */
		    i__1 = 1, i__2 = ia - 1;
		    il = max(i__1,i__2);
		    ir = il + 1;
/*                                 ------------------------------------- */
		    value += dchfie(&x[il], &x[ir], &f[il * f_dim1 + 1], &f[
			    ir * f_dim1 + 1], &d__[il * d_dim1 + 1], &d__[ir *
			     d_dim1 + 1], &xa, &x[ia]);
/*                                 ------------------------------------- */
		}

/*              THEN ADD ON INTEGRAL OVER (X(IB),XB). */
		if (xb > x[ib]) {
/* Computing MIN */
		    i__1 = ib + 1;
		    ir = min(i__1,*n);
		    il = ir - 1;
/*                                 ------------------------------------- */
		    value += dchfie(&x[il], &x[ir], &f[il * f_dim1 + 1], &f[
			    ir * f_dim1 + 1], &d__[il * d_dim1 + 1], &d__[ir *
			     d_dim1 + 1], &x[ib], &xb);
/*                                 ------------------------------------- */
		}

/*              FINALLY, ADJUST SIGN IF NECESSARY. */
		if (*a > *b) {
		    value = -value;
		}
	    }
	}
    }

/*  NORMAL RETURN. */

L5000:
    ret_val = value;
    return ret_val;

/*  ERROR RETURNS. */

L5001:
/*     N.LT.2 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "DPCHIA", "NUMBER OF DATA POINTS LESS THAN TWO", *ierr);
    goto L5000;

L5002:
/*     INCFD.LT.1 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "DPCHIA", "INCREMENT LESS THAN ONE", *ierr);
    goto L5000;

L5003:
/*     X-ARRAY NOT STRICTLY INCREASING. */
    *ierr = -3;
    xermsg_("SLATEC", "DPCHIA", "X-ARRAY NOT STRICTLY INCREASING", *ierr);
    goto L5000;

L5004:
/*     TROUBLE IN DPCHID.  (SHOULD NEVER OCCUR.) */
    *ierr = -4;
    xermsg_("SLATEC", "DPCHIA", "TROUBLE IN DPCHID", *ierr);
    goto L5000;
}

doublereal dpchid(integer *n, doublereal *x, doublereal *f, doublereal *d__,
	integer *incfd, logical *skip, integer *ia, integer *ib, integer *
	ierr)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal half = .5;
    static doublereal six = 6.;

    /* System generated locals */
    integer f_dim1, f_offset, d_dim1, d_offset, i__1;
    doublereal ret_val;

    /* Local variables */
    static doublereal h__;
    static integer i__, iup, low;
    static doublereal sum, value;

/* ***BEGIN PROLOGUE  DPCHID */
/* ***PURPOSE  Evaluate the definite integral of a piecewise cubic */
/*            Hermite function over an interval whose endpoints are data */
/*            points. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***CATEGORY  E3, H2A1B2 */
/* ***KEYWORDS  CUBIC HERMITE INTERPOLATION, NUMERICAL INTEGRATION, PCHIP, */
/*             QUADRATURE */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */

/*          DPCHID:  Piecewise Cubic Hermite Integrator, Data limits */

/*     Evaluates the definite integral of the cubic Hermite function */
/*     defined by  N, X, F, D  over the interval [X(IA), X(IB)]. */

/*     To provide compatibility with DPCHIM and DPCHIC, includes an */
/*     increment between successive values of the F- and D-arrays. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  N, IA, IB, IERR */
/*        DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N) */
/*        LOGICAL  SKIP */

/*        VALUE = DPCHID (N, X, F, D, INCFD, SKIP, IA, IB, IERR) */

/*   Parameters: */

/*     VALUE -- (output) value of the requested integral. */

/*     N -- (input) number of data points.  (Error return if N.LT.2 .) */

/*     X -- (input) real*8 array of independent variable values.  The */
/*           elements of X must be strictly increasing: */
/*                X(I-1) .LT. X(I),  I = 2(1)N. */
/*           (Error return if not.) */

/*     F -- (input) real*8 array of function values.  F(1+(I-1)*INCFD) is */
/*           the value corresponding to X(I). */

/*     D -- (input) real*8 array of derivative values.  D(1+(I-1)*INCFD) */
/*           is the value corresponding to X(I). */

/*     INCFD -- (input) increment between successive values in F and D. */
/*           (Error return if  INCFD.LT.1 .) */

/*     SKIP -- (input/output) logical variable which should be set to */
/*           .TRUE. if the user wishes to skip checks for validity of */
/*           preceding parameters, or to .FALSE. otherwise. */
/*           This will save time in case these checks have already */
/*           been performed (say, in DPCHIM or DPCHIC). */
/*           SKIP will be set to .TRUE. on return with IERR = 0 or -4. */

/*     IA,IB -- (input) indices in X-array for the limits of integration. */
/*           both must be in the range [1,N].  (Error return if not.) */
/*           No restrictions on their relative values. */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           "Recoverable" errors: */
/*              IERR = -1  if N.LT.2 . */
/*              IERR = -2  if INCFD.LT.1 . */
/*              IERR = -3  if the X-array is not strictly increasing. */
/*              IERR = -4  if IA or IB is out of range. */
/*                (VALUE will be zero in any of these cases.) */
/*               NOTE:  The above errors are checked in the order listed, */
/*                   and following arguments have **NOT** been validated. */

/* ***REVISION HISTORY  (YYMMDD) */
/*   820723  DATE WRITTEN */
/*   820804  Converted to SLATEC library version. */
/*   870707  Corrected XERROR calls for d.p. name(s). */
/*   870813  Minor cosmetic changes. */
/*   890206  Corrected XERROR calls. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890703  Corrected category record.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   891006  Cosmetic changes to prologue.  (WRB) */
/*   891006  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   930504  Corrected to set VALUE=0 when IERR.ne.0.  (FNF) */
/* ***END PROLOGUE  DPCHID */

/*  Programming notes: */
/*  1. This routine uses a special formula that is valid only for */
/*     integrals whose limits coincide with data values.  This is */
/*     mathematically equivalent to, but much more efficient than, */
/*     calls to DCHFIE. */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*  INITIALIZE. */

    /* Parameter adjustments */
    --x;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;
    f_dim1 = *incfd;
    f_offset = 1 + f_dim1;
    f -= f_offset;

    value = zero;

/*  VALIDITY-CHECK ARGUMENTS. */

    if (*skip) {
	goto L5;
    }

    if (*n < 2) {
	goto L5001;
    }
    if (*incfd < 1) {
	goto L5002;
    }
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__] <= x[i__ - 1]) {
	    goto L5003;
	}
    }

/*  FUNCTION DEFINITION IS OK, GO ON. */

L5:
    *skip = TRUE_;
    if (*ia < 1 || *ia > *n) {
	goto L5004;
    }
    if (*ib < 1 || *ib > *n) {
	goto L5004;
    }
    *ierr = 0;

/*  COMPUTE INTEGRAL VALUE. */

    if (*ia != *ib) {
	low = min(*ia,*ib);
	iup = max(*ia,*ib) - 1;
	sum = zero;
	i__1 = iup;
	for (i__ = low; i__ <= i__1; ++i__) {
	    h__ = x[i__ + 1] - x[i__];
	    sum += h__ * (f[i__ * f_dim1 + 1] + f[(i__ + 1) * f_dim1 + 1] + (
		    d__[i__ * d_dim1 + 1] - d__[(i__ + 1) * d_dim1 + 1]) * (
		    h__ / six));
	}
	value = half * sum;
	if (*ia > *ib) {
	    value = -value;
	}
    }

/*  NORMAL RETURN. */

L5000:
    ret_val = value;
    return ret_val;

/*  ERROR RETURNS. */

L5001:
/*     N.LT.2 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "DPCHID", "NUMBER OF DATA POINTS LESS THAN TWO", *ierr);
    goto L5000;

L5002:
/*     INCFD.LT.1 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "DPCHID", "INCREMENT LESS THAN ONE", *ierr);
    goto L5000;

L5003:
/*     X-ARRAY NOT STRICTLY INCREASING. */
    *ierr = -3;
    xermsg_("SLATEC", "DPCHID", "X-ARRAY NOT STRICTLY INCREASING", *ierr);
    goto L5000;

L5004:
/*     IA OR IB OUT OF RANGE RETURN. */
    *ierr = -4;
    xermsg_("SLATEC", "DPCHID", "IA OR IB OUT OF RANGE", *ierr);
    goto L5000;
}

void dpchce(integer *ic, doublereal *vc, integer *n,
	doublereal *x, doublereal *h__, doublereal *slope, doublereal *d__,
	integer *incfd, integer *ierr)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal half = .5;
    static doublereal two = 2.;
    static doublereal three = 3.;

    /* System generated locals */
    integer d_dim1, d_offset, i__1;
    doublereal d__1, d__2;

    /* Local variables */
    static integer j, k, ibeg, iend, ierf, index;
    static doublereal stemp[3], xtemp[4];

/* ***BEGIN PROLOGUE  DPCHCE */
/* ***PURPOSE  Set boundary conditions for DPCHIC */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */

/*          DPCHCE:  DPCHIC End Derivative Setter. */

/*    Called by DPCHIC to set end derivatives as requested by the user. */
/*    It must be called after interior derivative values have been set. */
/*                      ----- */

/*    To facilitate two-dimensional applications, includes an increment */
/*    between successive values of the D-array. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  IC(2), N, IERR */
/*        DOUBLE PRECISION  VC(2), X(N), H(N), SLOPE(N), D(INCFD,N) */

/*        CALL  DPCHCE (IC, VC, N, X, H, SLOPE, D, INCFD, IERR) */

/*   Parameters: */

/*     IC -- (input) integer array of length 2 specifying desired */
/*           boundary conditions: */
/*           IC(1) = IBEG, desired condition at beginning of data. */
/*           IC(2) = IEND, desired condition at end of data. */
/*           ( see prologue to DPCHIC for details. ) */

/*     VC -- (input) real*8 array of length 2 specifying desired boundary */
/*           values.  VC(1) need be set only if IC(1) = 2 or 3 . */
/*                    VC(2) need be set only if IC(2) = 2 or 3 . */

/*     N -- (input) number of data points.  (assumes N.GE.2) */

/*     X -- (input) real*8 array of independent variable values.  (the */
/*           elements of X are assumed to be strictly increasing.) */

/*     H -- (input) real*8 array of interval lengths. */
/*     SLOPE -- (input) real*8 array of data slopes. */
/*           If the data are (X(I),Y(I)), I=1(1)N, then these inputs are: */
/*                  H(I) =  X(I+1)-X(I), */
/*              SLOPE(I) = (Y(I+1)-Y(I))/H(I),  I=1(1)N-1. */

/*     D -- (input) real*8 array of derivative values at the data points. */
/*           The value corresponding to X(I) must be stored in */
/*                D(1+(I-1)*INCFD),  I=1(1)N. */
/*          (output) the value of D at X(1) and/or X(N) is changed, if */
/*           necessary, to produce the requested boundary conditions. */
/*           no other entries in D are changed. */

/*     INCFD -- (input) increment between successive values in D. */
/*           This argument is provided primarily for 2-D applications. */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           Warning errors: */
/*              IERR = 1  if IBEG.LT.0 and D(1) had to be adjusted for */
/*                        monotonicity. */
/*              IERR = 2  if IEND.LT.0 and D(1+(N-1)*INCFD) had to be */
/*                        adjusted for monotonicity. */
/*              IERR = 3  if both of the above are true. */

/*    ------- */
/*    WARNING:  This routine does no validity-checking of arguments. */
/*    ------- */

/*  Fortran intrinsics used:  ABS. */

/* ***SEE ALSO  DPCHIC */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820218  DATE WRITTEN */
/*   820805  Converted to SLATEC library version. */
/*   870707  Corrected XERROR calls for d.p. name(s). */
/*   890206  Corrected XERROR calls. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   900328  Added TYPE section.  (WRB) */
/*   910408  Updated AUTHOR section in prologue.  (WRB) */
/*   930503  Improved purpose.  (FNF) */
/* ***END PROLOGUE  DPCHCE */

/*  Programming notes: */
/*     1. The function DPCHST(ARG1,ARG2)  is assumed to return zero if */
/*        either argument is zero, +1 if they are of the same sign, and */
/*        -1 if they are of opposite sign. */
/*     2. One could reduce the number of arguments and amount of local */
/*        storage, at the expense of reduced code clarity, by passing in */
/*        the array WK (rather than splitting it into H and SLOPE) and */
/*        increasing its length enough to incorporate STEMP and XTEMP. */
/*     3. The two monotonicity checks only use the sufficient conditions. */
/*        Thus, it is possible (but unlikely) for a boundary condition to */
/*        be changed, even though the original interpolant was monotonic. */
/*        (At least the result is a continuous function of the data.) */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*  INITIALIZE. */

    /* Parameter adjustments */
    --ic;
    --vc;
    --x;
    --h__;
    --slope;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;


    ibeg = ic[1];
    iend = ic[2];
    *ierr = 0;

/*  SET TO DEFAULT BOUNDARY CONDITIONS IF N IS TOO SMALL. */

    if (abs(ibeg) > *n) {
	ibeg = 0;
    }
    if (abs(iend) > *n) {
	iend = 0;
    }

/*  TREAT BEGINNING BOUNDARY CONDITION. */

    if (ibeg == 0) {
	goto L2000;
    }
    k = abs(ibeg);
    if (k == 1) {
/*        BOUNDARY VALUE PROVIDED. */
	d__[d_dim1 + 1] = vc[1];
    } else if (k == 2) {
/*        BOUNDARY SECOND DERIVATIVE PROVIDED. */
	d__[d_dim1 + 1] = half * (three * slope[1] - d__[(d_dim1 << 1) + 1] -
		half * vc[1] * h__[1]);
    } else if (k < 5) {
/*        USE K-POINT DERIVATIVE FORMULA. */
/*        PICK UP FIRST K POINTS, IN REVERSE ORDER. */
	i__1 = k;
	for (j = 1; j <= i__1; ++j) {
	    index = k - j + 1;
/*           INDEX RUNS FROM K DOWN TO 1. */
	    xtemp[j - 1] = x[index];
	    if (j < k) {
		stemp[j - 1] = slope[index - 1];
	    }
	}
/*                 ----------------------------- */
	d__[d_dim1 + 1] = dpchdf(&k, xtemp, stemp, &ierf);
/*                 ----------------------------- */
	if (ierf != 0) {
	    goto L5001;
	}
    } else {
/*        USE 'NOT A KNOT' CONDITION. */
	d__[d_dim1 + 1] = (three * (h__[1] * slope[2] + h__[2] * slope[1]) -
		two * (h__[1] + h__[2]) * d__[(d_dim1 << 1) + 1] - h__[1] *
		d__[d_dim1 * 3 + 1]) / h__[2];
    }

    if (ibeg > 0) {
	goto L2000;
    }

/*  CHECK D(1,1) FOR COMPATIBILITY WITH MONOTONICITY. */

    if (slope[1] == zero) {
	if (d__[d_dim1 + 1] != zero) {
	    d__[d_dim1 + 1] = zero;
	    ++(*ierr);
	}
    } else if (dpchst(&d__[d_dim1 + 1], &slope[1]) < zero) {
	d__[d_dim1 + 1] = zero;
	++(*ierr);
    } else if ((d__1 = d__[d_dim1 + 1], abs(d__1)) > three * abs(slope[1])) {
	d__[d_dim1 + 1] = three * slope[1];
	++(*ierr);
    }

/*  TREAT END BOUNDARY CONDITION. */

L2000:
    if (iend == 0) {
	goto L5000;
    }
    k = abs(iend);
    if (k == 1) {
/*        BOUNDARY VALUE PROVIDED. */
	d__[*n * d_dim1 + 1] = vc[2];
    } else if (k == 2) {
/*        BOUNDARY SECOND DERIVATIVE PROVIDED. */
	d__[*n * d_dim1 + 1] = half * (three * slope[*n - 1] - d__[(*n - 1) *
		d_dim1 + 1] + half * vc[2] * h__[*n - 1]);
    } else if (k < 5) {
/*        USE K-POINT DERIVATIVE FORMULA. */
/*        PICK UP LAST K POINTS. */
	i__1 = k;
	for (j = 1; j <= i__1; ++j) {
	    index = *n - k + j;
/*           INDEX RUNS FROM N+1-K UP TO N. */
	    xtemp[j - 1] = x[index];
	    if (j < k) {
		stemp[j - 1] = slope[index];
	    }
	}
/*                 ----------------------------- */
	d__[*n * d_dim1 + 1] = dpchdf(&k, xtemp, stemp, &ierf);
/*                 ----------------------------- */
	if (ierf != 0) {
	    goto L5001;
	}
    } else {
/*        USE 'NOT A KNOT' CONDITION. */
	d__[*n * d_dim1 + 1] = (three * (h__[*n - 1] * slope[*n - 2] + h__[*n
		- 2] * slope[*n - 1]) - two * (h__[*n - 1] + h__[*n - 2]) *
		d__[(*n - 1) * d_dim1 + 1] - h__[*n - 1] * d__[(*n - 2) *
		d_dim1 + 1]) / h__[*n - 2];
    }

    if (iend > 0) {
	goto L5000;
    }

/*  CHECK D(1,N) FOR COMPATIBILITY WITH MONOTONICITY. */

    if (slope[*n - 1] == zero) {
	if (d__[*n * d_dim1 + 1] != zero) {
	    d__[*n * d_dim1 + 1] = zero;
	    *ierr += 2;
	}
    } else if (dpchst(&d__[*n * d_dim1 + 1], &slope[*n - 1]) < zero) {
	d__[*n * d_dim1 + 1] = zero;
	*ierr += 2;
    } else if ((d__2 = d__[*n * d_dim1 + 1], abs(d__2)) > three * (d__1 =
	    slope[*n - 1], abs(d__1))) {
	d__[*n * d_dim1 + 1] = three * slope[*n - 1];
	*ierr += 2;
    }

/*  NORMAL RETURN. */

L5000:
    return;

/*  ERROR RETURN. */

L5001:
/*     ERROR RETURN FROM DPCHDF. */
/*   *** THIS CASE SHOULD NEVER OCCUR *** */
    *ierr = -1;
    xermsg_("SLATEC", "DPCHCE", "ERROR RETURN FROM DPCHDF", *ierr);
    return;
}

void dpchci(integer *n, doublereal *h__, doublereal *slope,
	doublereal *d__, integer *incfd)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal three = 3.;

    /* System generated locals */
    integer d_dim1, d_offset, i__1;
    doublereal d__1, d__2;

    /* Local variables */
    static integer i__;
    static doublereal w1, w2, del1, del2, dmin__, dmax__, hsum, drat1, drat2;
    static integer nless1;
    static doublereal hsumt3;

/* ***BEGIN PROLOGUE  DPCHCI */
/* ***PURPOSE  Set interior derivatives for DPCHIC */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */

/*          DPCHCI:  DPCHIC Initial Derivative Setter. */

/*    Called by DPCHIC to set derivatives needed to determine a monotone */
/*    piecewise cubic Hermite interpolant to the data. */

/*    Default boundary conditions are provided which are compatible */
/*    with monotonicity.  If the data are only piecewise monotonic, the */
/*    interpolant will have an extremum at each point where monotonicity */
/*    switches direction. */

/*    To facilitate two-dimensional applications, includes an increment */
/*    between successive values of the D-array. */

/*    The resulting piecewise cubic Hermite function should be identical */
/*    (within roundoff error) to that produced by DPCHIM. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  N */
/*        DOUBLE PRECISION  H(N), SLOPE(N), D(INCFD,N) */

/*        CALL  DPCHCI (N, H, SLOPE, D, INCFD) */

/*   Parameters: */

/*     N -- (input) number of data points. */
/*           If N=2, simply does linear interpolation. */

/*     H -- (input) real*8 array of interval lengths. */
/*     SLOPE -- (input) real*8 array of data slopes. */
/*           If the data are (X(I),Y(I)), I=1(1)N, then these inputs are: */
/*                  H(I) =  X(I+1)-X(I), */
/*              SLOPE(I) = (Y(I+1)-Y(I))/H(I),  I=1(1)N-1. */

/*     D -- (output) real*8 array of derivative values at data points. */
/*           If the data are monotonic, these values will determine a */
/*           a monotone cubic Hermite function. */
/*           The value corresponding to X(I) is stored in */
/*                D(1+(I-1)*INCFD),  I=1(1)N. */
/*           No other entries in D are changed. */

/*     INCFD -- (input) increment between successive values in D. */
/*           This argument is provided primarily for 2-D applications. */

/*    ------- */
/*    WARNING:  This routine does no validity-checking of arguments. */
/*    ------- */

/*  Fortran intrinsics used:  ABS, MAX, MIN. */

/* ***SEE ALSO  DPCHIC */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820218  DATE WRITTEN */
/*   820601  Modified end conditions to be continuous functions of */
/*           data when monotonicity switches in next interval. */
/*   820602  1. Modified formulas so end conditions are less prone */
/*             to over/underflow problems. */
/*           2. Minor modification to HSUM calculation. */
/*   820805  Converted to SLATEC library version. */
/*   870813  Minor cosmetic changes. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900328  Added TYPE section.  (WRB) */
/*   910408  Updated AUTHOR section in prologue.  (WRB) */
/*   930503  Improved purpose.  (FNF) */
/* ***END PROLOGUE  DPCHCI */

/*  Programming notes: */
/*     1. The function  DPCHST(ARG1,ARG2)  is assumed to return zero if */
/*        either argument is zero, +1 if they are of the same sign, and */
/*        -1 if they are of opposite sign. */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*  INITIALIZE. */

    /* Parameter adjustments */
    --h__;
    --slope;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;

    nless1 = *n - 1;
    del1 = slope[1];

/*  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION. */

    if (nless1 > 1) {
	goto L10;
    }
    d__[d_dim1 + 1] = del1;
    d__[*n * d_dim1 + 1] = del1;
    goto L5000;

/*  NORMAL CASE  (N .GE. 3). */

L10:
    del2 = slope[2];

/*  SET D(1) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE */
/*     SHAPE-PRESERVING. */

    hsum = h__[1] + h__[2];
    w1 = (h__[1] + hsum) / hsum;
    w2 = -h__[1] / hsum;
    d__[d_dim1 + 1] = w1 * del1 + w2 * del2;
    if (dpchst(&d__[d_dim1 + 1], &del1) <= zero) {
	d__[d_dim1 + 1] = zero;
    } else if (dpchst(&del1, &del2) < zero) {
/*        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES. */
	dmax__ = three * del1;
	if ((d__1 = d__[d_dim1 + 1], abs(d__1)) > abs(dmax__)) {
	    d__[d_dim1 + 1] = dmax__;
	}
    }

/*  LOOP THROUGH INTERIOR POINTS. */

    i__1 = nless1;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (i__ == 2) {
	    goto L40;
	}

	hsum = h__[i__ - 1] + h__[i__];
	del1 = del2;
	del2 = slope[i__];
L40:

/*        SET D(I)=0 UNLESS DATA ARE STRICTLY MONOTONIC. */

	d__[i__ * d_dim1 + 1] = zero;
	if (dpchst(&del1, &del2) <= zero) {
	    goto L50;
	}

/*        USE BRODLIE MODIFICATION OF BUTLAND FORMULA. */

	hsumt3 = hsum + hsum + hsum;
	w1 = (hsum + h__[i__ - 1]) / hsumt3;
	w2 = (hsum + h__[i__]) / hsumt3;
/* Computing MAX */
	d__1 = abs(del1), d__2 = abs(del2);
	dmax__ = max(d__1,d__2);
/* Computing MIN */
	d__1 = abs(del1), d__2 = abs(del2);
	dmin__ = min(d__1,d__2);
	drat1 = del1 / dmax__;
	drat2 = del2 / dmax__;
	d__[i__ * d_dim1 + 1] = dmin__ / (w1 * drat1 + w2 * drat2);

L50:
	;
    }

/*  SET D(N) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE */
/*     SHAPE-PRESERVING. */

    w1 = -h__[*n - 1] / hsum;
    w2 = (h__[*n - 1] + hsum) / hsum;
    d__[*n * d_dim1 + 1] = w1 * del1 + w2 * del2;
    if (dpchst(&d__[*n * d_dim1 + 1], &del2) <= zero) {
	d__[*n * d_dim1 + 1] = zero;
    } else if (dpchst(&del1, &del2) < zero) {
/*        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES. */
	dmax__ = three * del2;
	if ((d__1 = d__[*n * d_dim1 + 1], abs(d__1)) > abs(dmax__)) {
	    d__[*n * d_dim1 + 1] = dmax__;
	}
    }

/*  NORMAL RETURN. */

L5000:
    return;
}

void dpchcs(doublereal *switch__, integer *n, doublereal *
	h__, doublereal *slope, doublereal *d__, integer *incfd, integer *
	ierr)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal one = 1.;
    static doublereal fudge = 4.;

    /* System generated locals */
    integer d_dim1, d_offset, i__1;
    doublereal d__1, d__2, d__3;

    /* Local variables */
    static integer i__, k;
    static doublereal del[3], fact, dfmx;
    static integer indx;
    static doublereal dext, dfloc, slmax, wtave[2];
    static integer nless1;

/* ***BEGIN PROLOGUE  DPCHCS */
/* ***PURPOSE  Adjusts derivative values for DPCHIC */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */

/*         DPCHCS:  DPCHIC Monotonicity Switch Derivative Setter. */

/*     Called by  DPCHIC  to adjust the values of D in the vicinity of a */
/*     switch in direction of monotonicity, to produce a more "visually */
/*     pleasing" curve than that given by  DPCHIM . */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  N, IERR */
/*        DOUBLE PRECISION  SWITCH, H(N), SLOPE(N), D(INCFD,N) */

/*        CALL  DPCHCS (SWITCH, N, H, SLOPE, D, INCFD, IERR) */

/*   Parameters: */

/*     SWITCH -- (input) indicates the amount of control desired over */
/*           local excursions from data. */

/*     N -- (input) number of data points.  (assumes N.GT.2 .) */

/*     H -- (input) real*8 array of interval lengths. */
/*     SLOPE -- (input) real*8 array of data slopes. */
/*           If the data are (X(I),Y(I)), I=1(1)N, then these inputs are: */
/*                  H(I) =  X(I+1)-X(I), */
/*              SLOPE(I) = (Y(I+1)-Y(I))/H(I),  I=1(1)N-1. */

/*     D -- (input) real*8 array of derivative values at the data points, */
/*           as determined by DPCHCI. */
/*          (output) derivatives in the vicinity of switches in direction */
/*           of monotonicity may be adjusted to produce a more "visually */
/*           pleasing" curve. */
/*           The value corresponding to X(I) is stored in */
/*                D(1+(I-1)*INCFD),  I=1(1)N. */
/*           No other entries in D are changed. */

/*     INCFD -- (input) increment between successive values in D. */
/*           This argument is provided primarily for 2-D applications. */

/*     IERR -- (output) error flag.  should be zero. */
/*           If negative, trouble in DPCHSW.  (should never happen.) */

/*    ------- */
/*    WARNING:  This routine does no validity-checking of arguments. */
/*    ------- */

/*  Fortran intrinsics used:  ABS, MAX, MIN. */

/* ***SEE ALSO  DPCHIC */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820218  DATE WRITTEN */
/*   820617  Redesigned to (1) fix  problem with lack of continuity */
/*           approaching a flat-topped peak (2) be cleaner and */
/*           easier to verify. */
/*           Eliminated subroutines PCHSA and PCHSX in the process. */
/*   820622  1. Limited fact to not exceed one, so computed D is a */
/*             convex combination of DPCHCI value and DPCHSD value. */
/*           2. Changed fudge from 1 to 4 (based on experiments). */
/*   820623  Moved PCHSD to an inline function (eliminating MSWTYP). */
/*   820805  Converted to SLATEC library version. */
/*   870707  Corrected conversion to double precision. */
/*   870813  Minor cosmetic changes. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   891006  Modified spacing in computation of DFLOC.  (WRB) */
/*   891006  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900328  Added TYPE section.  (WRB) */
/*   910408  Updated AUTHOR section in prologue.  (WRB) */
/*   930503  Improved purpose.  (FNF) */
/* ***END PROLOGUE  DPCHCS */

/*  Programming notes: */
/*     1. The function  DPCHST(ARG1,ARG2)  is assumed to return zero if */
/*        either argument is zero, +1 if they are of the same sign, and */
/*        -1 if they are of opposite sign. */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


/*  DEFINE INLINE FUNCTION FOR WEIGHTED AVERAGE OF SLOPES. */


/*  INITIALIZE. */

    /* Parameter adjustments */
    --h__;
    --slope;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;

    *ierr = 0;
    nless1 = *n - 1;

/*  LOOP OVER SEGMENTS. */

    i__1 = nless1;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if ((d__1 = dpchst(&slope[i__ - 1], &slope[i__])) < 0.) {
	    goto L100;
	} else if (d__1 == 0) {
	    goto L300;
	} else {
	    goto L900;
	}
/*             -------------------------- */

L100:

/* ....... SLOPE SWITCHES MONOTONICITY AT I-TH POINT ..................... */

/*           DO NOT CHANGE D IF 'UP-DOWN-UP'. */
	if (i__ > 2) {
	    if (dpchst(&slope[i__ - 2], &slope[i__]) > zero) {
		goto L900;
	    }
/*                   -------------------------- */
	}
	if (i__ < nless1) {
	    if (dpchst(&slope[i__ + 1], &slope[i__ - 1]) > zero) {
		goto L900;
	    }
/*                   ---------------------------- */
	}

/*   ....... COMPUTE PROVISIONAL VALUE FOR D(1,I). */

	dext = h__[i__] / (h__[i__ - 1] + h__[i__]) * slope[i__ - 1] + h__[
		i__ - 1] / (h__[i__ - 1] + h__[i__]) * slope[i__];

/*   ....... DETERMINE WHICH INTERVAL CONTAINS THE EXTREMUM. */

	if ((d__1 = dpchst(&dext, &slope[i__ - 1])) < 0.) {
	    goto L200;
	} else if (d__1 == 0) {
	    goto L900;
	} else {
	    goto L250;
	}
/*                ----------------------- */

L200:
/*              DEXT AND SLOPE(I-1) HAVE OPPOSITE SIGNS -- */
/*                        EXTREMUM IS IN (X(I-1),X(I)). */
	k = i__ - 1;
/*              SET UP TO COMPUTE NEW VALUES FOR D(1,I-1) AND D(1,I). */
	wtave[1] = dext;
	if (k > 1) {
	    wtave[0] = h__[k] / (h__[k - 1] + h__[k]) * slope[k - 1] + h__[k
		    - 1] / (h__[k - 1] + h__[k]) * slope[k];
	}
	goto L400;

L250:
/*              DEXT AND SLOPE(I) HAVE OPPOSITE SIGNS -- */
/*                        EXTREMUM IS IN (X(I),X(I+1)). */
	k = i__;
/*              SET UP TO COMPUTE NEW VALUES FOR D(1,I) AND D(1,I+1). */
	wtave[0] = dext;
	if (k < nless1) {
	    wtave[1] = h__[k + 1] / (h__[k] + h__[k + 1]) * slope[k] + h__[k]
		    / (h__[k] + h__[k + 1]) * slope[k + 1];
	}
	goto L400;

L300:

/* ....... AT LEAST ONE OF SLOPE(I-1) AND SLOPE(I) IS ZERO -- */
/*                     CHECK FOR FLAT-TOPPED PEAK ....................... */

	if (i__ == nless1) {
	    goto L900;
	}
	if (dpchst(&slope[i__ - 1], &slope[i__ + 1]) >= zero) {
	    goto L900;
	}
/*                ----------------------------- */

/*           WE HAVE FLAT-TOPPED PEAK ON (X(I),X(I+1)). */
	k = i__;
/*           SET UP TO COMPUTE NEW VALUES FOR D(1,I) AND D(1,I+1). */
	wtave[0] = h__[k] / (h__[k - 1] + h__[k]) * slope[k - 1] + h__[k - 1]
		/ (h__[k - 1] + h__[k]) * slope[k];
	wtave[1] = h__[k + 1] / (h__[k] + h__[k + 1]) * slope[k] + h__[k] / (
		h__[k] + h__[k + 1]) * slope[k + 1];

L400:

/* ....... AT THIS POINT WE HAVE DETERMINED THAT THERE WILL BE AN EXTREMUM */
/*        ON (X(K),X(K+1)), WHERE K=I OR I-1, AND HAVE SET ARRAY WTAVE-- */
/*           WTAVE(1) IS A WEIGHTED AVERAGE OF SLOPE(K-1) AND SLOPE(K), */
/*                    IF K.GT.1 */
/*           WTAVE(2) IS A WEIGHTED AVERAGE OF SLOPE(K) AND SLOPE(K+1), */
/*                    IF K.LT.N-1 */

	slmax = (d__1 = slope[k], abs(d__1));
	if (k > 1) {
/* Computing MAX */
	    d__2 = slmax, d__3 = (d__1 = slope[k - 1], abs(d__1));
	    slmax = max(d__2,d__3);
	}
	if (k < nless1) {
/* Computing MAX */
	    d__2 = slmax, d__3 = (d__1 = slope[k + 1], abs(d__1));
	    slmax = max(d__2,d__3);
	}

	if (k > 1) {
	    del[0] = slope[k - 1] / slmax;
	}
	del[1] = slope[k] / slmax;
	if (k < nless1) {
	    del[2] = slope[k + 1] / slmax;
	}

	if (k > 1 && k < nless1) {
/*           NORMAL CASE -- EXTREMUM IS NOT IN A BOUNDARY INTERVAL. */
	    fact = fudge * (d__1 = del[2] * (del[0] - del[1]) * (wtave[1] /
		    slmax), abs(d__1));
	    d__[k * d_dim1 + 1] += min(fact,one) * (wtave[0] - d__[k * d_dim1
		    + 1]);
	    fact = fudge * (d__1 = del[0] * (del[2] - del[1]) * (wtave[0] /
		    slmax), abs(d__1));
	    d__[(k + 1) * d_dim1 + 1] += min(fact,one) * (wtave[1] - d__[(k +
		    1) * d_dim1 + 1]);
	} else {
/*           SPECIAL CASE K=1 (WHICH CAN OCCUR ONLY IF I=2) OR */
/*                        K=NLESS1 (WHICH CAN OCCUR ONLY IF I=NLESS1). */
	    fact = fudge * abs(del[1]);
	    d__[i__ * d_dim1 + 1] = min(fact,one) * wtave[i__ - k];
/*              NOTE THAT I-K+1 = 1 IF K=I  (=NLESS1), */
/*                        I-K+1 = 2 IF K=I-1(=1). */
	}


/* ....... ADJUST IF NECESSARY TO LIMIT EXCURSIONS FROM DATA. */

	if (*switch__ <= zero) {
	    goto L900;
	}

	dfloc = h__[k] * (d__1 = slope[k], abs(d__1));
	if (k > 1) {
/* Computing MAX */
	    d__2 = dfloc, d__3 = h__[k - 1] * (d__1 = slope[k - 1], abs(d__1))
		    ;
	    dfloc = max(d__2,d__3);
	}
	if (k < nless1) {
/* Computing MAX */
	    d__2 = dfloc, d__3 = h__[k + 1] * (d__1 = slope[k + 1], abs(d__1))
		    ;
	    dfloc = max(d__2,d__3);
	}
	dfmx = *switch__ * dfloc;
	indx = i__ - k + 1;
/*        INDX = 1 IF K=I, 2 IF K=I-1. */
/*        --------------------------------------------------------------- */
	dpchsw(&dfmx, &indx, &d__[k * d_dim1 + 1], &d__[(k + 1) * d_dim1 + 1]
		, &h__[k], &slope[k], ierr);
/*        --------------------------------------------------------------- */
	if (*ierr != 0) {
	    return;
	}

/* ....... END OF SEGMENT LOOP. */

L900:
	;
    }

    return;
}

doublereal dpchdf(integer *k, doublereal *x, doublereal *s, integer *ierr)
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1, i__2;
    doublereal ret_val;

    /* Local variables */
    static integer i__, j;
    static doublereal value;

/* ***BEGIN PROLOGUE  DPCHDF */
/* ***PURPOSE  Computes divided differences for DPCHCE and DPCHSP */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */

/*          DPCHDF:   DPCHIP Finite Difference Formula */

/*     Uses a divided difference formulation to compute a K-point approx- */
/*     imation to the derivative at X(K) based on the data in X and S. */

/*     Called by  DPCHCE  and  DPCHSP  to compute 3- and 4-point boundary */
/*     derivative approximations. */

/* ---------------------------------------------------------------------- */

/*     On input: */
/*        K      is the order of the desired derivative approximation. */
/*               K must be at least 3 (error return if not). */
/*        X      contains the K values of the independent variable. */
/*               X need not be ordered, but the values **MUST** be */
/*               distinct.  (Not checked here.) */
/*        S      contains the associated slope values: */
/*                  S(I) = (F(I+1)-F(I))/(X(I+1)-X(I)), I=1(1)K-1. */
/*               (Note that S need only be of length K-1.) */

/*     On return: */
/*        S      will be destroyed. */
/*        IERR   will be set to -1 if K.LT.2 . */
/*        DPCHDF  will be set to the desired derivative approximation if */
/*               IERR=0 or to zero if IERR=-1. */

/* ---------------------------------------------------------------------- */

/* ***SEE ALSO  DPCHCE, DPCHSP */
/* ***REFERENCES  Carl de Boor, A Practical Guide to Splines, Springer- */
/*                 Verlag, New York, 1978, pp. 10-16. */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820503  DATE WRITTEN */
/*   820805  Converted to SLATEC library version. */
/*   870707  Corrected XERROR calls for d.p. name(s). */
/*   870813  Minor cosmetic changes. */
/*   890206  Corrected XERROR calls. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890411  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   900328  Added TYPE section.  (WRB) */
/*   910408  Updated AUTHOR and DATE WRITTEN sections in prologue.  (WRB) */
/*   920429  Revised format and order of references.  (WRB,FNF) */
/*   930503  Improved purpose.  (FNF) */
/* ***END PROLOGUE  DPCHDF */

    /* Parameter adjustments */
    --s;
    --x;


/*  CHECK FOR LEGAL VALUE OF K. */

    if (*k < 3) {
	goto L5001;
    }

/*  COMPUTE COEFFICIENTS OF INTERPOLATING POLYNOMIAL. */

    i__1 = *k - 1;
    for (j = 2; j <= i__1; ++j) {
	i__2 = *k - j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    s[i__] = (s[i__ + 1] - s[i__]) / (x[i__ + j] - x[i__]);
	}
    }

/*  EVALUATE DERIVATIVE AT X(K). */

    value = s[1];
    i__1 = *k - 1;
    for (i__ = 2; i__ <= i__1; ++i__) {
	value = s[i__] + value * (x[*k] - x[i__]);
    }

/*  NORMAL RETURN. */

    *ierr = 0;
    ret_val = value;
    return ret_val;

/*  ERROR RETURN. */

L5001:
/*     K.LT.3 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "DPCHDF", "K LESS THAN THREE", *ierr);
    ret_val = zero;
    return ret_val;
}

void dpchic(integer *ic, doublereal *vc, doublereal *
	switch__, integer *n, doublereal *x, doublereal *f, doublereal *d__,
	integer *incfd, doublereal *wk, integer *nwk, integer *ierr)
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer f_dim1, f_offset, d_dim1, d_offset, i__1;

    /* Local variables */
    static integer i__, ibeg, iend, nless1;

/* ***BEGIN PROLOGUE  DPCHIC */
/* ***PURPOSE  Set derivatives needed to determine a piecewise monotone */
/*            piecewise cubic Hermite interpolant to given data. */
/*            User control is available over boundary conditions and/or */
/*            treatment of points where monotonicity switches direction. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***CATEGORY  E1A */
/* ***KEYWORDS  CUBIC HERMITE INTERPOLATION, MONOTONE INTERPOLATION, */
/*             PCHIP, PIECEWISE CUBIC INTERPOLATION, */
/*             SHAPE-PRESERVING INTERPOLATION */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */

/*         DPCHIC:  Piecewise Cubic Hermite Interpolation Coefficients. */

/*     Sets derivatives needed to determine a piecewise monotone piece- */
/*     wise cubic interpolant to the data given in X and F satisfying the */
/*     boundary conditions specified by IC and VC. */

/*     The treatment of points where monotonicity switches direction is */
/*     controlled by argument SWITCH. */

/*     To facilitate two-dimensional applications, includes an increment */
/*     between successive values of the F- and D-arrays. */

/*     The resulting piecewise cubic Hermite function may be evaluated */
/*     by DPCHFE or DPCHFD. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  IC(2), N, NWK, IERR */
/*        DOUBLE PRECISION  VC(2), SWITCH, X(N), F(INCFD,N), D(INCFD,N), */
/*                          WK(NWK) */

/*        CALL DPCHIC (IC, VC, SWITCH, N, X, F, D, INCFD, WK, NWK, IERR) */

/*   Parameters: */

/*     IC -- (input) integer array of length 2 specifying desired */
/*           boundary conditions: */
/*           IC(1) = IBEG, desired condition at beginning of data. */
/*           IC(2) = IEND, desired condition at end of data. */

/*           IBEG = 0  for the default boundary condition (the same as */
/*                     used by DPCHIM). */
/*           If IBEG.NE.0, then its sign indicates whether the boundary */
/*                     derivative is to be adjusted, if necessary, to be */
/*                     compatible with monotonicity: */
/*              IBEG.GT.0  if no adjustment is to be performed. */
/*              IBEG.LT.0  if the derivative is to be adjusted for */
/*                     monotonicity. */

/*           Allowable values for the magnitude of IBEG are: */
/*           IBEG = 1  if first derivative at X(1) is given in VC(1). */
/*           IBEG = 2  if second derivative at X(1) is given in VC(1). */
/*           IBEG = 3  to use the 3-point difference formula for D(1). */
/*                     (Reverts to the default b.c. if N.LT.3 .) */
/*           IBEG = 4  to use the 4-point difference formula for D(1). */
/*                     (Reverts to the default b.c. if N.LT.4 .) */
/*           IBEG = 5  to set D(1) so that the second derivative is con- */
/*              tinuous at X(2). (Reverts to the default b.c. if N.LT.4.) */
/*              This option is somewhat analogous to the "not a knot" */
/*              boundary condition provided by DPCHSP. */

/*          NOTES (IBEG): */
/*           1. An error return is taken if ABS(IBEG).GT.5 . */
/*           2. Only in case  IBEG.LE.0  is it guaranteed that the */
/*              interpolant will be monotonic in the first interval. */
/*              If the returned value of D(1) lies between zero and */
/*              3*SLOPE(1), the interpolant will be monotonic.  This */
/*              is **NOT** checked if IBEG.GT.0 . */
/*           3. If IBEG.LT.0 and D(1) had to be changed to achieve mono- */
/*              tonicity, a warning error is returned. */

/*           IEND may take on the same values as IBEG, but applied to */
/*           derivative at X(N).  In case IEND = 1 or 2, the value is */
/*           given in VC(2). */

/*          NOTES (IEND): */
/*           1. An error return is taken if ABS(IEND).GT.5 . */
/*           2. Only in case  IEND.LE.0  is it guaranteed that the */
/*              interpolant will be monotonic in the last interval. */
/*              If the returned value of D(1+(N-1)*INCFD) lies between */
/*              zero and 3*SLOPE(N-1), the interpolant will be monotonic. */
/*              This is **NOT** checked if IEND.GT.0 . */
/*           3. If IEND.LT.0 and D(1+(N-1)*INCFD) had to be changed to */
/*              achieve monotonicity, a warning error is returned. */

/*     VC -- (input) real*8 array of length 2 specifying desired boundary */
/*           values, as indicated above. */
/*           VC(1) need be set only if IC(1) = 1 or 2 . */
/*           VC(2) need be set only if IC(2) = 1 or 2 . */

/*     SWITCH -- (input) indicates desired treatment of points where */
/*           direction of monotonicity switches: */
/*           Set SWITCH to zero if interpolant is required to be mono- */
/*           tonic in each interval, regardless of monotonicity of data. */
/*             NOTES: */
/*              1. This will cause D to be set to zero at all switch */
/*                 points, thus forcing extrema there. */
/*              2. The result of using this option with the default boun- */
/*                 dary conditions will be identical to using DPCHIM, but */
/*                 will generally cost more compute time. */
/*                 This option is provided only to facilitate comparison */
/*                 of different switch and/or boundary conditions. */
/*           Set SWITCH nonzero to use a formula based on the 3-point */
/*              difference formula in the vicinity of switch points. */
/*           If SWITCH is positive, the interpolant on each interval */
/*              containing an extremum is controlled to not deviate from */
/*              the data by more than SWITCH*DFLOC, where DFLOC is the */
/*              maximum of the change of F on this interval and its two */
/*              immediate neighbors. */
/*           If SWITCH is negative, no such control is to be imposed. */

/*     N -- (input) number of data points.  (Error return if N.LT.2 .) */

/*     X -- (input) real*8 array of independent variable values.  The */
/*           elements of X must be strictly increasing: */
/*                X(I-1) .LT. X(I),  I = 2(1)N. */
/*           (Error return if not.) */

/*     F -- (input) real*8 array of dependent variable values to be */
/*           interpolated.  F(1+(I-1)*INCFD) is value corresponding to */
/*           X(I). */

/*     D -- (output) real*8 array of derivative values at the data */
/*           points.  These values will determine a monotone cubic */
/*           Hermite function on each subinterval on which the data */
/*           are monotonic, except possibly adjacent to switches in */
/*           monotonicity. The value corresponding to X(I) is stored in */
/*                D(1+(I-1)*INCFD),  I=1(1)N. */
/*           No other entries in D are changed. */

/*     INCFD -- (input) increment between successive values in F and D. */
/*           This argument is provided primarily for 2-D applications. */
/*           (Error return if  INCFD.LT.1 .) */

/*     WK -- (scratch) real*8 array of working storage.  The user may */
/*           wish to know that the returned values are: */
/*              WK(I)     = H(I)     = X(I+1) - X(I) ; */
/*              WK(N-1+I) = SLOPE(I) = (F(1,I+1) - F(1,I)) / H(I) */
/*           for  I = 1(1)N-1. */

/*     NWK -- (input) length of work array. */
/*           (Error return if  NWK.LT.2*(N-1) .) */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           Warning errors: */
/*              IERR = 1  if IBEG.LT.0 and D(1) had to be adjusted for */
/*                        monotonicity. */
/*              IERR = 2  if IEND.LT.0 and D(1+(N-1)*INCFD) had to be */
/*                        adjusted for monotonicity. */
/*              IERR = 3  if both of the above are true. */
/*           "Recoverable" errors: */
/*              IERR = -1  if N.LT.2 . */
/*              IERR = -2  if INCFD.LT.1 . */
/*              IERR = -3  if the X-array is not strictly increasing. */
/*              IERR = -4  if ABS(IBEG).GT.5 . */
/*              IERR = -5  if ABS(IEND).GT.5 . */
/*              IERR = -6  if both of the above are true. */
/*              IERR = -7  if NWK.LT.2*(N-1) . */
/*             (The D-array has not been changed in any of these cases.) */
/*               NOTE:  The above errors are checked in the order listed, */
/*                   and following arguments have **NOT** been validated. */

/* ***REFERENCES  1. F. N. Fritsch, Piecewise Cubic Hermite Interpolation */
/*                 Package, Report UCRL-87285, Lawrence Livermore Natio- */
/*                 nal Laboratory, July 1982.  [Poster presented at the */
/*                 SIAM 30th Anniversary Meeting, 19-23 July 1982.] */
/*               2. F. N. Fritsch and J. Butland, A method for construc- */
/*                 ting local monotone piecewise cubic interpolants, SIAM */
/*                 Journal on Scientific and Statistical Computing 5, 2 */
/*                 (June 1984), pp. 300-304. */
/*               3. F. N. Fritsch and R. E. Carlson, Monotone piecewise */
/*                 cubic interpolation, SIAM Journal on Numerical Ana- */
/*                 lysis 17, 2 (April 1980), pp. 238-246. */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820218  DATE WRITTEN */
/*   820804  Converted to SLATEC library version. */
/*   870707  Corrected XERROR calls for d.p. name(s). */
/*   870813  Updated Reference 2. */
/*   890206  Corrected XERROR calls. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890703  Corrected category record.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   891006  Cosmetic changes to prologue.  (WRB) */
/*   891006  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   920429  Revised format and order of references.  (WRB,FNF) */
/* ***END PROLOGUE  DPCHIC */
/*  Programming notes: */

/*     To produce a single precision version, simply: */
/*        a. Change DPCHIC to PCHIC wherever it occurs, */
/*        b. Change DPCHCE to PCHCE wherever it occurs, */
/*        c. Change DPCHCI to PCHCI wherever it occurs, */
/*        d. Change DPCHCS to PCHCS wherever it occurs, */
/*        e. Change the double precision declarations to real, and */
/*        f. Change the constant  ZERO  to single precision. */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */

    /* Parameter adjustments */
    --ic;
    --vc;
    --x;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;
    f_dim1 = *incfd;
    f_offset = 1 + f_dim1;
    f -= f_offset;
    --wk;


/*  VALIDITY-CHECK ARGUMENTS. */

    if (*n < 2) {
	goto L5001;
    }
    if (*incfd < 1) {
	goto L5002;
    }
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__] <= x[i__ - 1]) {
	    goto L5003;
	}
    }

    ibeg = ic[1];
    iend = ic[2];
    *ierr = 0;
    if (abs(ibeg) > 5) {
	--(*ierr);
    }
    if (abs(iend) > 5) {
	*ierr += -2;
    }
    if (*ierr < 0) {
	goto L5004;
    }

/*  FUNCTION DEFINITION IS OK -- GO ON. */

    nless1 = *n - 1;
    if (*nwk < nless1 << 1) {
	goto L5007;
    }

/*  SET UP H AND SLOPE ARRAYS. */

    i__1 = nless1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	wk[i__] = x[i__ + 1] - x[i__];
	wk[nless1 + i__] = (f[(i__ + 1) * f_dim1 + 1] - f[i__ * f_dim1 + 1]) /
		 wk[i__];
    }

/*  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION. */

    if (nless1 > 1) {
	goto L1000;
    }
    d__[d_dim1 + 1] = wk[2];
    d__[*n * d_dim1 + 1] = wk[2];
    goto L3000;

/*  NORMAL CASE  (N .GE. 3) . */

L1000:

/*  SET INTERIOR DERIVATIVES AND DEFAULT END CONDITIONS. */

/*     -------------------------------------- */
    dpchci(n, &wk[1], &wk[*n], &d__[d_offset], incfd);
/*     -------------------------------------- */

/*  SET DERIVATIVES AT POINTS WHERE MONOTONICITY SWITCHES DIRECTION. */

    if (*switch__ == zero) {
	goto L3000;
    }
/*     ---------------------------------------------------- */
    dpchcs(switch__, n, &wk[1], &wk[*n], &d__[d_offset], incfd, ierr);
/*     ---------------------------------------------------- */
    if (*ierr != 0) {
	goto L5008;
    }

/*  SET END CONDITIONS. */

L3000:
    if (ibeg == 0 && iend == 0) {
	goto L5000;
    }
/*     ------------------------------------------------------- */
    dpchce(&ic[1], &vc[1], n, &x[1], &wk[1], &wk[*n], &d__[d_offset], incfd,
	    ierr);
/*     ------------------------------------------------------- */
    if (*ierr < 0) {
	goto L5009;
    }

/*  NORMAL RETURN. */

L5000:
    return;

/*  ERROR RETURNS. */

L5001:
/*     N.LT.2 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "DPCHIC", "NUMBER OF DATA POINTS LESS THAN TWO", *ierr);
    return;

L5002:
/*     INCFD.LT.1 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "DPCHIC", "INCREMENT LESS THAN ONE", *ierr);
    return;

L5003:
/*     X-ARRAY NOT STRICTLY INCREASING. */
    *ierr = -3;
    xermsg_("SLATEC", "DPCHIC", "X-ARRAY NOT STRICTLY INCREASING", *ierr);
    return;

L5004:
/*     IC OUT OF RANGE RETURN. */
    *ierr += -3;
    xermsg_("SLATEC", "DPCHIC", "IC OUT OF RANGE", *ierr);
    return;

L5007:
/*     NWK .LT. 2*(N-1)  RETURN. */
    *ierr = -7;
    xermsg_("SLATEC", "DPCHIC", "WORK ARRAY TOO SMALL", *ierr);
    return;

L5008:
/*     ERROR RETURN FROM DPCHCS. */
    *ierr = -8;
    xermsg_("SLATEC", "DPCHIC", "ERROR RETURN FROM DPCHCS", *ierr);
    return;

L5009:
/*     ERROR RETURN FROM DPCHCE. */
/*   *** THIS CASE SHOULD NEVER OCCUR *** */
    *ierr = -9;
    xermsg_("SLATEC", "DPCHIC", "ERROR RETURN FROM DPCHCE", *ierr);
    return;
}

doublereal dpchst(doublereal *arg1, doublereal *arg2)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal one = 1.;

    /* System generated locals */
    doublereal ret_val;

/* ***BEGIN PROLOGUE  DPCHST */
/* ***PURPOSE  DPCHIP Sign-Testing Routine */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */

/*         DPCHST:  DPCHIP Sign-Testing Routine. */


/*     Returns: */
/*        -1. if ARG1 and ARG2 are of opposite sign. */
/*         0. if either argument is zero. */
/*        +1. if ARG1 and ARG2 are of the same sign. */

/*     The object is to do this without multiplying ARG1*ARG2, to avoid */
/*     possible over/underflow problems. */

/*  Fortran intrinsics used:  SIGN. */

/* ***SEE ALSO  DPCHCE, DPCHCI, DPCHCS, DPCHIM */
/* ***REVISION HISTORY  (YYMMDD) */
/*   811103  DATE WRITTEN */
/*   820805  Converted to SLATEC library version. */
/*   870813  Minor cosmetic changes. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900328  Added TYPE section.  (WRB) */
/*   910408  Updated AUTHOR and DATE WRITTEN sections in prologue.  (WRB) */
/*   930503  Improved purpose.  (FNF) */
/* ***END PROLOGUE  DPCHST */

/*  PERFORM THE TEST. */

    ret_val = d_sign(1, *arg1) * d_sign(1, *arg2);
    if (*arg1 == zero || *arg2 == zero) {
	ret_val = zero;
    }

    return ret_val;
}

void dpchsw(doublereal *dfmax, integer *iextrm, doublereal *
	d1, doublereal *d2, doublereal *h__, doublereal *slope, integer *ierr)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal one = 1.;
    static doublereal two = 2.;
    static doublereal three = 3.;
    static doublereal fact = 100.;
    static doublereal third = .33333;

    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal cp, nu, phi, rho, hphi, that, sigma, small;
    static doublereal lambda, radcal;

/* ***BEGIN PROLOGUE  DPCHSW */
/* ***PURPOSE  Limits excursion from data for DPCHCS */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */

/*         DPCHSW:  DPCHCS Switch Excursion Limiter. */

/*     Called by  DPCHCS  to adjust D1 and D2 if necessary to insure that */
/*     the extremum on this interval is not further than DFMAX from the */
/*     extreme data value. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        INTEGER  IEXTRM, IERR */
/*        DOUBLE PRECISION  DFMAX, D1, D2, H, SLOPE */

/*        CALL  DPCHSW (DFMAX, IEXTRM, D1, D2, H, SLOPE, IERR) */

/*   Parameters: */

/*     DFMAX -- (input) maximum allowed difference between F(IEXTRM) and */
/*           the cubic determined by derivative values D1,D2.  (assumes */
/*           DFMAX.GT.0.) */

/*     IEXTRM -- (input) index of the extreme data value.  (assumes */
/*           IEXTRM = 1 or 2 .  Any value .NE.1 is treated as 2.) */

/*     D1,D2 -- (input) derivative values at the ends of the interval. */
/*           (Assumes D1*D2 .LE. 0.) */
/*          (output) may be modified if necessary to meet the restriction */
/*           imposed by DFMAX. */

/*     H -- (input) interval length.  (Assumes  H.GT.0.) */

/*     SLOPE -- (input) data slope on the interval. */

/*     IERR -- (output) error flag.  should be zero. */
/*           If IERR=-1, assumption on D1 and D2 is not satisfied. */
/*           If IERR=-2, quadratic equation locating extremum has */
/*                       negative discriminant (should never occur). */

/*    ------- */
/*    WARNING:  This routine does no validity-checking of arguments. */
/*    ------- */

/*  Fortran intrinsics used:  ABS, SIGN, SQRT. */

/* ***SEE ALSO  DPCHCS */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820218  DATE WRITTEN */
/*   820805  Converted to SLATEC library version. */
/*   870707  Corrected XERROR calls for d.p. name(s). */
/*   870707  Replaced DATA statement for SMALL with a use of D1MACH. */
/*   870813  Minor cosmetic changes. */
/*   890206  Corrected XERROR calls. */
/*   890411  1. Added SAVE statements (Vers. 3.2). */
/*           2. Added DOUBLE PRECISION declaration for D1MACH. */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   900328  Added TYPE section.  (WRB) */
/*   910408  Updated AUTHOR and DATE WRITTEN sections in prologue.  (WRB) */
/*   920526  Eliminated possible divide by zero problem.  (FNF) */
/*   930503  Improved purpose.  (FNF) */
/* ***END PROLOGUE  DPCHSW */

/*        THIRD SHOULD BE SLIGHTLY LESS THAN 1/3. */

/*  NOTATION AND GENERAL REMARKS. */

/*     RHO IS THE RATIO OF THE DATA SLOPE TO THE DERIVATIVE BEING TESTED. */
/*     LAMBDA IS THE RATIO OF D2 TO D1. */
/*     THAT = T-HAT(RHO) IS THE NORMALIZED LOCATION OF THE EXTREMUM. */
/*     PHI IS THE NORMALIZED VALUE OF P(X)-F1 AT X = XHAT = X-HAT(RHO), */
/*           WHERE  THAT = (XHAT - X1)/H . */
/*        THAT IS, P(XHAT)-F1 = D*H*PHI,  WHERE D=D1 OR D2. */
/*     SIMILARLY,  P(XHAT)-F2 = D*H*(PHI-RHO) . */

/*      SMALL SHOULD BE A FEW ORDERS OF MAGNITUDE GREATER THAN MACHEPS. */
    small = fact * d1mach();

/*  DO MAIN CALCULATION. */

    if (*d1 == zero) {

/*        SPECIAL CASE -- D1.EQ.ZERO . */

/*          IF D2 IS ALSO ZERO, THIS ROUTINE SHOULD NOT HAVE BEEN CALLED. */
	if (*d2 == zero) {
	    goto L5001;
	}

	rho = *slope / *d2;
/*          EXTREMUM IS OUTSIDE INTERVAL WHEN RHO .GE. 1/3 . */
	if (rho >= third) {
	    goto L5000;
	}
	that = two * (three * rho - one) / (three * (two * rho - one));
/* Computing 2nd power */
	d__1 = that;
	phi = d__1 * d__1 * ((three * rho - one) / three);

/*          CONVERT TO DISTANCE FROM F2 IF IEXTRM.NE.1 . */
	if (*iextrm != 1) {
	    phi -= rho;
	}

/*          TEST FOR EXCEEDING LIMIT, AND ADJUST ACCORDINGLY. */
	hphi = *h__ * abs(phi);
	if (hphi * abs(*d2) > *dfmax) {
/*           AT THIS POINT, HPHI.GT.0, SO DIVIDE IS OK. */
	    d__1 = *dfmax / hphi;
	    *d2 = d_sign(d__1, *d2);
	}
    } else {

	rho = *slope / *d1;
	lambda = -(*d2) / *d1;
	if (*d2 == zero) {

/*           SPECIAL CASE -- D2.EQ.ZERO . */

/*             EXTREMUM IS OUTSIDE INTERVAL WHEN RHO .GE. 1/3 . */
	    if (rho >= third) {
		goto L5000;
	    }
	    cp = two - three * rho;
	    nu = one - two * rho;
	    that = one / (three * nu);
	} else {
	    if (lambda <= zero) {
		goto L5001;
	    }

/*           NORMAL CASE -- D1 AND D2 BOTH NONZERO, OPPOSITE SIGNS. */

	    nu = one - lambda - two * rho;
	    sigma = one - rho;
	    cp = nu + sigma;
	    if (abs(nu) > small) {
/* Computing 2nd power */
		d__1 = sigma;
		radcal = (nu - (two * rho + one)) * nu + d__1 * d__1;
		if (radcal < zero) {
		    goto L5002;
		}
		that = (cp - sqrt(radcal)) / (three * nu);
	    } else {
		that = one / (two * sigma);
	    }
	}
	phi = that * ((nu * that - cp) * that + one);

/*          CONVERT TO DISTANCE FROM F2 IF IEXTRM.NE.1 . */
	if (*iextrm != 1) {
	    phi -= rho;
	}

/*          TEST FOR EXCEEDING LIMIT, AND ADJUST ACCORDINGLY. */
	hphi = *h__ * abs(phi);
	if (hphi * abs(*d1) > *dfmax) {
/*           AT THIS POINT, HPHI.GT.0, SO DIVIDE IS OK. */
	    d__1 = *dfmax / hphi;
	    *d1 = d_sign(d__1, *d1);
	    *d2 = -lambda * *d1;
	}
    }

/*  NORMAL RETURN. */

L5000:
    *ierr = 0;
    return;

/*  ERROR RETURNS. */

L5001:
/*     D1 AND D2 BOTH ZERO, OR BOTH NONZERO AND SAME SIGN. */
    *ierr = -1;
    xermsg_("SLATEC", "DPCHSW", "D1 AND/OR D2 INVALID", *ierr);
    return;

L5002:
/*     NEGATIVE VALUE OF RADICAL (SHOULD NEVER OCCUR). */
    *ierr = -2;
    xermsg_("SLATEC", "DPCHSW", "NEGATIVE RADICAL", *ierr);
    return;
}

void dpchim(integer *n, doublereal *x, doublereal *f,
	doublereal *d__, integer *incfd, integer *ierr)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal three = 3.;

    /* System generated locals */
    integer f_dim1, f_offset, d_dim1, d_offset, i__1;
    doublereal d__1, d__2;

    /* Local variables */
    static integer i__;
    static doublereal h1, h2, w1, w2, del1, del2, dmin__, dmax__, hsum, drat1,
	     drat2, dsave;
    static integer nless1;
    static doublereal hsumt3;

/* ***BEGIN PROLOGUE  DPCHIM */
/* ***PURPOSE  Set derivatives needed to determine a monotone piecewise */
/*            cubic Hermite interpolant to given data.  Boundary values */
/*            are provided which are compatible with monotonicity.  The */
/*            interpolant will have an extremum at each point where mono- */
/*            tonicity switches direction.  (See DPCHIC if user control */
/*            is desired over boundary or switch conditions.) */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***CATEGORY  E1A */
/* ***KEYWORDS  CUBIC HERMITE INTERPOLATION, MONOTONE INTERPOLATION, */
/*             PCHIP, PIECEWISE CUBIC INTERPOLATION */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */

/*          DPCHIM:  Piecewise Cubic Hermite Interpolation to */
/*                  Monotone data. */

/*     Sets derivatives needed to determine a monotone piecewise cubic */
/*     Hermite interpolant to the data given in X and F. */

/*     Default boundary conditions are provided which are compatible */
/*     with monotonicity.  (See DPCHIC if user control of boundary con- */
/*     ditions is desired.) */

/*     If the data are only piecewise monotonic, the interpolant will */
/*     have an extremum at each point where monotonicity switches direc- */
/*     tion.  (See DPCHIC if user control is desired in such cases.) */

/*     To facilitate two-dimensional applications, includes an increment */
/*     between successive values of the F- and D-arrays. */

/*     The resulting piecewise cubic Hermite function may be evaluated */
/*     by DPCHFE or DPCHFD. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  N, IERR */
/*        DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N) */

/*        CALL  DPCHIM (N, X, F, D, INCFD, IERR) */

/*   Parameters: */

/*     N -- (input) number of data points.  (Error return if N.LT.2 .) */
/*           If N=2, simply does linear interpolation. */

/*     X -- (input) real*8 array of independent variable values.  The */
/*           elements of X must be strictly increasing: */
/*                X(I-1) .LT. X(I),  I = 2(1)N. */
/*           (Error return if not.) */

/*     F -- (input) real*8 array of dependent variable values to be */
/*           interpolated.  F(1+(I-1)*INCFD) is value corresponding to */
/*           X(I).  DPCHIM is designed for monotonic data, but it will */
/*           work for any F-array.  It will force extrema at points where */
/*           monotonicity switches direction.  If some other treatment of */
/*           switch points is desired, DPCHIC should be used instead. */
/*                                     ----- */
/*     D -- (output) real*8 array of derivative values at the data */
/*           points.  If the data are monotonic, these values will */
/*           determine a monotone cubic Hermite function. */
/*           The value corresponding to X(I) is stored in */
/*                D(1+(I-1)*INCFD),  I=1(1)N. */
/*           No other entries in D are changed. */

/*     INCFD -- (input) increment between successive values in F and D. */
/*           This argument is provided primarily for 2-D applications. */
/*           (Error return if  INCFD.LT.1 .) */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           Warning error: */
/*              IERR.GT.0  means that IERR switches in the direction */
/*                 of monotonicity were detected. */
/*           "Recoverable" errors: */
/*              IERR = -1  if N.LT.2 . */
/*              IERR = -2  if INCFD.LT.1 . */
/*              IERR = -3  if the X-array is not strictly increasing. */
/*             (The D-array has not been changed in any of these cases.) */
/*               NOTE:  The above errors are checked in the order listed, */
/*                   and following arguments have **NOT** been validated. */

/* ***REFERENCES  1. F. N. Fritsch and J. Butland, A method for construc- */
/*                 ting local monotone piecewise cubic interpolants, SIAM */
/*                 Journal on Scientific and Statistical Computing 5, 2 */
/*                 (June 1984), pp. 300-304. */
/*               2. F. N. Fritsch and R. E. Carlson, Monotone piecewise */
/*                 cubic interpolation, SIAM Journal on Numerical Ana- */
/*                 lysis 17, 2 (April 1980), pp. 238-246. */
/* ***REVISION HISTORY  (YYMMDD) */
/*   811103  DATE WRITTEN */
/*   820201  1. Introduced  DPCHST  to reduce possible over/under- */
/*             flow problems. */
/*           2. Rearranged derivative formula for same reason. */
/*   820602  1. Modified end conditions to be continuous functions */
/*             of data when monotonicity switches in next interval. */
/*           2. Modified formulas so end conditions are less prone */
/*             of over/underflow problems. */
/*   820803  Minor cosmetic changes for release 1. */
/*   870707  Corrected XERROR calls for d.p. name(s). */
/*   870813  Updated Reference 1. */
/*   890206  Corrected XERROR calls. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890703  Corrected category record.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   891006  Cosmetic changes to prologue.  (WRB) */
/*   891006  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   920429  Revised format and order of references.  (WRB,FNF) */
/* ***END PROLOGUE  DPCHIM */
/*  Programming notes: */

/*     1. The function  DPCHST(ARG1,ARG2)  is assumed to return zero if */
/*        either argument is zero, +1 if they are of the same sign, and */
/*        -1 if they are of opposite sign. */
/*     2. To produce a single precision version, simply: */
/*        a. Change DPCHIM to PCHIM wherever it occurs, */
/*        b. Change DPCHST to PCHST wherever it occurs, */
/*        c. Change all references to the Fortran intrinsics to their */
/*           single precision equivalents, */
/*        d. Change the double precision declarations to real, and */
/*        e. Change the constants ZERO and THREE to single precision. */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */

    /* Parameter adjustments */
    --x;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;
    f_dim1 = *incfd;
    f_offset = 1 + f_dim1;
    f -= f_offset;


/*  VALIDITY-CHECK ARGUMENTS. */

    if (*n < 2) {
	goto L5001;
    }
    if (*incfd < 1) {
	goto L5002;
    }
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__] <= x[i__ - 1]) {
	    goto L5003;
	}
    }

/*  FUNCTION DEFINITION IS OK, GO ON. */

    *ierr = 0;
    nless1 = *n - 1;
    h1 = x[2] - x[1];
    del1 = (f[(f_dim1 << 1) + 1] - f[f_dim1 + 1]) / h1;
    dsave = del1;

/*  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION. */

    if (nless1 > 1) {
	goto L10;
    }
    d__[d_dim1 + 1] = del1;
    d__[*n * d_dim1 + 1] = del1;
    goto L5000;

/*  NORMAL CASE  (N .GE. 3). */

L10:
    h2 = x[3] - x[2];
    del2 = (f[f_dim1 * 3 + 1] - f[(f_dim1 << 1) + 1]) / h2;

/*  SET D(1) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE */
/*     SHAPE-PRESERVING. */

    hsum = h1 + h2;
    w1 = (h1 + hsum) / hsum;
    w2 = -h1 / hsum;
    d__[d_dim1 + 1] = w1 * del1 + w2 * del2;
    if (dpchst(&d__[d_dim1 + 1], &del1) <= zero) {
	d__[d_dim1 + 1] = zero;
    } else if (dpchst(&del1, &del2) < zero) {
/*        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES. */
	dmax__ = three * del1;
	if ((d__1 = d__[d_dim1 + 1], abs(d__1)) > abs(dmax__)) {
	    d__[d_dim1 + 1] = dmax__;
	}
    }

/*  LOOP THROUGH INTERIOR POINTS. */

    i__1 = nless1;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (i__ == 2) {
	    goto L40;
	}

	h1 = h2;
	h2 = x[i__ + 1] - x[i__];
	hsum = h1 + h2;
	del1 = del2;
	del2 = (f[(i__ + 1) * f_dim1 + 1] - f[i__ * f_dim1 + 1]) / h2;
L40:

/*        SET D(I)=0 UNLESS DATA ARE STRICTLY MONOTONIC. */

	d__[i__ * d_dim1 + 1] = zero;
	if ((d__1 = dpchst(&del1, &del2)) < 0.) {
	    goto L42;
	} else if (d__1 == 0) {
	    goto L41;
	} else {
	    goto L45;
	}

/*        COUNT NUMBER OF CHANGES IN DIRECTION OF MONOTONICITY. */

L41:
	if (del2 == zero) {
	    goto L50;
	}
	if (dpchst(&dsave, &del2) < zero) {
	    ++(*ierr);
	}
	dsave = del2;
	goto L50;

L42:
	++(*ierr);
	dsave = del2;
	goto L50;

/*        USE BRODLIE MODIFICATION OF BUTLAND FORMULA. */

L45:
	hsumt3 = hsum + hsum + hsum;
	w1 = (hsum + h1) / hsumt3;
	w2 = (hsum + h2) / hsumt3;
/* Computing MAX */
	d__1 = abs(del1), d__2 = abs(del2);
	dmax__ = max(d__1,d__2);
/* Computing MIN */
	d__1 = abs(del1), d__2 = abs(del2);
	dmin__ = min(d__1,d__2);
	drat1 = del1 / dmax__;
	drat2 = del2 / dmax__;
	d__[i__ * d_dim1 + 1] = dmin__ / (w1 * drat1 + w2 * drat2);

L50:
	;
    }

/*  SET D(N) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE */
/*     SHAPE-PRESERVING. */

    w1 = -h2 / hsum;
    w2 = (h2 + hsum) / hsum;
    d__[*n * d_dim1 + 1] = w1 * del1 + w2 * del2;
    if (dpchst(&d__[*n * d_dim1 + 1], &del2) <= zero) {
	d__[*n * d_dim1 + 1] = zero;
    } else if (dpchst(&del1, &del2) < zero) {
/*        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES. */
	dmax__ = three * del2;
	if ((d__1 = d__[*n * d_dim1 + 1], abs(d__1)) > abs(dmax__)) {
	    d__[*n * d_dim1 + 1] = dmax__;
	}
    }

/*  NORMAL RETURN. */

L5000:
    return;

/*  ERROR RETURNS. */

L5001:
/*     N.LT.2 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "DPCHIM", "NUMBER OF DATA POINTS LESS THAN TWO", *ierr);
    return;

L5002:
/*     INCFD.LT.1 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "DPCHIM", "INCREMENT LESS THAN ONE", *ierr);
    return;

L5003:
/*     X-ARRAY NOT STRICTLY INCREASING. */
    *ierr = -3;
    xermsg_("SLATEC", "DPCHIM", "X-ARRAY NOT STRICTLY INCREASING", *ierr);
    return;
}

void dpchsp(integer *ic, doublereal *vc, integer *n,
	doublereal *x, doublereal *f, doublereal *d__, integer *incfd,
	doublereal *wk, integer *nwk, integer *ierr)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal half = .5;
    static doublereal one = 1.;
    static doublereal two = 2.;
    static doublereal three = 3.;

    /* System generated locals */
    integer f_dim1, f_offset, d_dim1, d_offset, i__1;
    doublereal d__1;

    /* Local variables */
    static doublereal g;
    static integer j, nm1, ibeg, iend, index;
    static doublereal stemp[3], xtemp[4];

/* ***BEGIN PROLOGUE  DPCHSP */
/* ***PURPOSE  Set derivatives needed to determine the Hermite represen- */
/*            tation of the cubic spline interpolant to given data, with */
/*            specified boundary conditions. */
/* ***LIBRARY   SLATEC (PCHIP) */
/* ***CATEGORY  E1A */
/* ***KEYWORDS  CUBIC HERMITE INTERPOLATION, PCHIP, */
/*             PIECEWISE CUBIC INTERPOLATION, SPLINE INTERPOLATION */
/* ***AUTHOR  Fritsch, F. N., (LLNL) */
/*             Lawrence Livermore National Laboratory */
/*             P.O. Box 808  (L-316) */
/*             Livermore, CA  94550 */
/*             FTS 532-4275, (510) 422-4275 */

/*          DPCHSP:   Piecewise Cubic Hermite Spline */

/*     Computes the Hermite representation of the cubic spline inter- */
/*     polant to the data given in X and F satisfying the boundary */
/*     conditions specified by IC and VC. */

/*     To facilitate two-dimensional applications, includes an increment */
/*     between successive values of the F- and D-arrays. */

/*     The resulting piecewise cubic Hermite function may be evaluated */
/*     by DPCHFE or DPCHFD. */

/*     NOTE:  This is a modified version of C. de Boor's cubic spline */
/*            routine CUBSPL. */

/* ---------------------------------------------------------------------- */

/*  Calling sequence: */

/*        PARAMETER  (INCFD = ...) */
/*        INTEGER  IC(2), N, NWK, IERR */
/*        DOUBLE PRECISION  VC(2), X(N), F(INCFD,N), D(INCFD,N), WK(NWK) */

/*        CALL  DPCHSP (IC, VC, N, X, F, D, INCFD, WK, NWK, IERR) */

/*   Parameters: */

/*     IC -- (input) integer array of length 2 specifying desired */
/*           boundary conditions: */
/*           IC(1) = IBEG, desired condition at beginning of data. */
/*           IC(2) = IEND, desired condition at end of data. */

/*           IBEG = 0  to set D(1) so that the third derivative is con- */
/*              tinuous at X(2).  This is the "not a knot" condition */
/*              provided by de Boor's cubic spline routine CUBSPL. */
/*              < This is the default boundary condition. > */
/*           IBEG = 1  if first derivative at X(1) is given in VC(1). */
/*           IBEG = 2  if second derivative at X(1) is given in VC(1). */
/*           IBEG = 3  to use the 3-point difference formula for D(1). */
/*                     (Reverts to the default b.c. if N.LT.3 .) */
/*           IBEG = 4  to use the 4-point difference formula for D(1). */
/*                     (Reverts to the default b.c. if N.LT.4 .) */
/*          NOTES: */
/*           1. An error return is taken if IBEG is out of range. */
/*           2. For the "natural" boundary condition, use IBEG=2 and */
/*              VC(1)=0. */

/*           IEND may take on the same values as IBEG, but applied to */
/*           derivative at X(N).  In case IEND = 1 or 2, the value is */
/*           given in VC(2). */

/*          NOTES: */
/*           1. An error return is taken if IEND is out of range. */
/*           2. For the "natural" boundary condition, use IEND=2 and */
/*              VC(2)=0. */

/*     VC -- (input) real*8 array of length 2 specifying desired boundary */
/*           values, as indicated above. */
/*           VC(1) need be set only if IC(1) = 1 or 2 . */
/*           VC(2) need be set only if IC(2) = 1 or 2 . */

/*     N -- (input) number of data points.  (Error return if N.LT.2 .) */

/*     X -- (input) real*8 array of independent variable values.  The */
/*           elements of X must be strictly increasing: */
/*                X(I-1) .LT. X(I),  I = 2(1)N. */
/*           (Error return if not.) */

/*     F -- (input) real*8 array of dependent variable values to be */
/*           interpolated.  F(1+(I-1)*INCFD) is value corresponding to */
/*           X(I). */

/*     D -- (output) real*8 array of derivative values at the data */
/*           points.  These values will determine the cubic spline */
/*           interpolant with the requested boundary conditions. */
/*           The value corresponding to X(I) is stored in */
/*                D(1+(I-1)*INCFD),  I=1(1)N. */
/*           No other entries in D are changed. */

/*     INCFD -- (input) increment between successive values in F and D. */
/*           This argument is provided primarily for 2-D applications. */
/*           (Error return if  INCFD.LT.1 .) */

/*     WK -- (scratch) real*8 array of working storage. */

/*     NWK -- (input) length of work array. */
/*           (Error return if NWK.LT.2*N .) */

/*     IERR -- (output) error flag. */
/*           Normal return: */
/*              IERR = 0  (no errors). */
/*           "Recoverable" errors: */
/*              IERR = -1  if N.LT.2 . */
/*              IERR = -2  if INCFD.LT.1 . */
/*              IERR = -3  if the X-array is not strictly increasing. */
/*              IERR = -4  if IBEG.LT.0 or IBEG.GT.4 . */
/*              IERR = -5  if IEND.LT.0 of IEND.GT.4 . */
/*              IERR = -6  if both of the above are true. */
/*              IERR = -7  if NWK is too small. */
/*               NOTE:  The above errors are checked in the order listed, */
/*                   and following arguments have **NOT** been validated. */
/*             (The D-array has not been changed in any of these cases.) */
/*              IERR = -8  in case of trouble solving the linear system */
/*                         for the interior derivative values. */
/*             (The D-array may have been changed in this case.) */
/*             (             Do **NOT** use it!                ) */

/* ***REFERENCES  Carl de Boor, A Practical Guide to Splines, Springer- */
/*                 Verlag, New York, 1978, pp. 53-59. */
/* ***REVISION HISTORY  (YYMMDD) */
/*   820503  DATE WRITTEN */
/*   820804  Converted to SLATEC library version. */
/*   870707  Corrected XERROR calls for d.p. name(s). */
/*   890206  Corrected XERROR calls. */
/*   890411  Added SAVE statements (Vers. 3.2). */
/*   890703  Corrected category record.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   891006  Cosmetic changes to prologue.  (WRB) */
/*   891006  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   920429  Revised format and order of references.  (WRB,FNF) */
/* ***END PROLOGUE  DPCHSP */
/*  Programming notes: */

/*     To produce a single precision version, simply: */
/*        a. Change DPCHSP to PCHSP wherever it occurs, */
/*        b. Change the double precision declarations to real, and */
/*        c. Change the constants ZERO, HALF, ... to single precision. */

/*  DECLARE ARGUMENTS. */


/*  DECLARE LOCAL VARIABLES. */


    /* Parameter adjustments */
    --ic;
    --vc;
    --x;
    d_dim1 = *incfd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;
    f_dim1 = *incfd;
    f_offset = 1 + f_dim1;
    f -= f_offset;
    wk -= 3;

/*  VALIDITY-CHECK ARGUMENTS. */

    if (*n < 2) {
	goto L5001;
    }
    if (*incfd < 1) {
	goto L5002;
    }
    i__1 = *n;
    for (j = 2; j <= i__1; ++j) {
	if (x[j] <= x[j - 1]) {
	    goto L5003;
	}
    }

    ibeg = ic[1];
    iend = ic[2];
    *ierr = 0;
    if (ibeg < 0 || ibeg > 4) {
	--(*ierr);
    }
    if (iend < 0 || iend > 4) {
	*ierr += -2;
    }
    if (*ierr < 0) {
	goto L5004;
    }

/*  FUNCTION DEFINITION IS OK -- GO ON. */

    if (*nwk < *n << 1) {
	goto L5007;
    }

/*  COMPUTE FIRST DIFFERENCES OF X SEQUENCE AND STORE IN WK(1,.). ALSO, */
/*  COMPUTE FIRST DIVIDED DIFFERENCE OF DATA AND STORE IN WK(2,.). */
    i__1 = *n;
    for (j = 2; j <= i__1; ++j) {
	wk[(j << 1) + 1] = x[j] - x[j - 1];
	wk[(j << 1) + 2] = (f[j * f_dim1 + 1] - f[(j - 1) * f_dim1 + 1]) / wk[
		(j << 1) + 1];
    }

/*  SET TO DEFAULT BOUNDARY CONDITIONS IF N IS TOO SMALL. */

    if (ibeg > *n) {
	ibeg = 0;
    }
    if (iend > *n) {
	iend = 0;
    }

/*  SET UP FOR BOUNDARY CONDITIONS. */

    if (ibeg == 1 || ibeg == 2) {
	d__[d_dim1 + 1] = vc[1];
    } else if (ibeg > 2) {
/*        PICK UP FIRST IBEG POINTS, IN REVERSE ORDER. */
	i__1 = ibeg;
	for (j = 1; j <= i__1; ++j) {
	    index = ibeg - j + 1;
/*           INDEX RUNS FROM IBEG DOWN TO 1. */
	    xtemp[j - 1] = x[index];
	    if (j < ibeg) {
		stemp[j - 1] = wk[(index << 1) + 2];
	    }
	}
/*                 -------------------------------- */
	d__[d_dim1 + 1] = dpchdf(&ibeg, xtemp, stemp, ierr);
/*                 -------------------------------- */
	if (*ierr != 0) {
	    goto L5009;
	}
	ibeg = 1;
    }

    if (iend == 1 || iend == 2) {
	d__[*n * d_dim1 + 1] = vc[2];
    } else if (iend > 2) {
/*        PICK UP LAST IEND POINTS. */
	i__1 = iend;
	for (j = 1; j <= i__1; ++j) {
	    index = *n - iend + j;
/*           INDEX RUNS FROM N+1-IEND UP TO N. */
	    xtemp[j - 1] = x[index];
	    if (j < iend) {
		stemp[j - 1] = wk[((index + 1) << 1) + 2];
	    }
	}
/*                 -------------------------------- */
	d__[*n * d_dim1 + 1] = dpchdf(&iend, xtemp, stemp, ierr);
/*                 -------------------------------- */
	if (*ierr != 0) {
	    goto L5009;
	}
	iend = 1;
    }

/* --------------------( BEGIN CODING FROM CUBSPL )-------------------- */

/*  **** A TRIDIAGONAL LINEAR SYSTEM FOR THE UNKNOWN SLOPES S(J) OF */
/*  F  AT X(J), J=1,...,N, IS GENERATED AND THEN SOLVED BY GAUSS ELIM- */
/*  INATION, WITH S(J) ENDING UP IN D(1,J), ALL J. */
/*     WK(1,.) AND WK(2,.) ARE USED FOR TEMPORARY STORAGE. */

/*  CONSTRUCT FIRST EQUATION FROM FIRST BOUNDARY CONDITION, OF THE FORM */
/*             WK(2,1)*S(1) + WK(1,1)*S(2) = D(1,1) */

    if (ibeg == 0) {
	if (*n == 2) {
/*           NO CONDITION AT LEFT END AND N = 2. */
	    wk[4] = one;
	    wk[3] = one;
	    d__[d_dim1 + 1] = two * wk[6];
	} else {
/*           NOT-A-KNOT CONDITION AT LEFT END AND N .GT. 2. */
	    wk[4] = wk[7];
	    wk[3] = wk[5] + wk[7];
/* Computing 2nd power */
	    d__1 = wk[5];
	    d__[d_dim1 + 1] = ((wk[5] + two * wk[3]) * wk[6] * wk[7] + d__1 *
		    d__1 * wk[8]) / wk[3];
	}
    } else if (ibeg == 1) {
/*        SLOPE PRESCRIBED AT LEFT END. */
	wk[4] = one;
	wk[3] = zero;
    } else {
/*        SECOND DERIVATIVE PRESCRIBED AT LEFT END. */
	wk[4] = two;
	wk[3] = one;
	d__[d_dim1 + 1] = three * wk[6] - half * wk[5] * d__[d_dim1 + 1];
    }

/*  IF THERE ARE INTERIOR KNOTS, GENERATE THE CORRESPONDING EQUATIONS AND */
/*  CARRY OUT THE FORWARD PASS OF GAUSS ELIMINATION, AFTER WHICH THE J-TH */
/*  EQUATION READS    WK(2,J)*S(J) + WK(1,J)*S(J+1) = D(1,J). */

    nm1 = *n - 1;
    if (nm1 > 1) {
	i__1 = nm1;
	for (j = 2; j <= i__1; ++j) {
	    if (wk[((j - 1) << 1) + 2] == zero) {
		goto L5008;
	    }
	    g = -wk[((j + 1) << 1) + 1] / wk[((j - 1) << 1) + 2];
	    d__[j * d_dim1 + 1] = g * d__[(j - 1) * d_dim1 + 1] + three * (wk[
		    (j << 1) + 1] * wk[((j + 1) << 1) + 2] + wk[((j + 1) << 1) +
		    1] * wk[(j << 1) + 2]);
	    wk[(j << 1) + 2] = g * wk[((j - 1) << 1) + 1] + two * (wk[(j << 1)
		    + 1] + wk[((j + 1) << 1) + 1]);
	}
    }

/*  CONSTRUCT LAST EQUATION FROM SECOND BOUNDARY CONDITION, OF THE FORM */
/*           (-G*WK(2,N-1))*S(N-1) + WK(2,N)*S(N) = D(1,N) */

/*     IF SLOPE IS PRESCRIBED AT RIGHT END, ONE CAN GO DIRECTLY TO BACK- */
/*     SUBSTITUTION, SINCE ARRAYS HAPPEN TO BE SET UP JUST RIGHT FOR IT */
/*     AT THIS POINT. */
    if (iend == 1) {
	goto L30;
    }

    if (iend == 0) {
	if (*n == 2 && ibeg == 0) {
/*           NOT-A-KNOT AT RIGHT ENDPOINT AND AT LEFT ENDPOINT AND N = 2. */
	    d__[(d_dim1 << 1) + 1] = wk[6];
	    goto L30;
	} else if (*n == 2 || *n == 3 && ibeg == 0) {
/*           EITHER (N=3 AND NOT-A-KNOT ALSO AT LEFT) OR (N=2 AND *NOT* */
/*           NOT-A-KNOT AT LEFT END POINT). */
	    d__[*n * d_dim1 + 1] = two * wk[(*n << 1) + 2];
	    wk[(*n << 1) + 2] = one;
	    if (wk[((*n - 1) << 1) + 2] == zero) {
		goto L5008;
	    }
	    g = -one / wk[((*n - 1) << 1) + 2];
	} else {
/*           NOT-A-KNOT AND N .GE. 3, AND EITHER N.GT.3 OR  ALSO NOT-A- */
/*           KNOT AT LEFT END POINT. */
	    g = wk[((*n - 1) << 1) + 1] + wk[(*n << 1) + 1];
/*           DO NOT NEED TO CHECK FOLLOWING DENOMINATORS (X-DIFFERENCES). */
/* Computing 2nd power */
	    d__1 = wk[(*n << 1) + 1];
	    d__[*n * d_dim1 + 1] = ((wk[(*n << 1) + 1] + two * g) * wk[(*n << 1) + 2] * wk[((*n - 1) << 1) + 1] + d__1 * d__1 * (f[(*n - 1) * f_dim1 + 1] - f[(*n - 2) * f_dim1 + 1]) / wk[((*n - 1) << 1) + 1]) / g;
	    if (wk[((*n - 1) << 1) + 2] == zero) {
		goto L5008;
	    }
	    g = -g / wk[((*n - 1) << 1) + 2];
	    wk[(*n << 1) + 2] = wk[((*n - 1) << 1) + 1];
	}
    } else {
/*        SECOND DERIVATIVE PRESCRIBED AT RIGHT ENDPOINT. */
	d__[*n * d_dim1 + 1] = three * wk[(*n << 1) + 2] + half * wk[(*n << 1) + 1] * d__[*n * d_dim1 + 1];
	wk[(*n << 1) + 2] = two;
	if (wk[((*n - 1) << 1) + 2] == zero) {
	    goto L5008;
	}
	g = -one / wk[((*n - 1) << 1) + 2];
    }

/*  COMPLETE FORWARD PASS OF GAUSS ELIMINATION. */

    wk[(*n << 1) + 2] = g * wk[((*n - 1) << 1) + 1] + wk[(*n << 1) + 2];
    if (wk[(*n << 1) + 2] == zero) {
	goto L5008;
    }
    d__[*n * d_dim1 + 1] = (g * d__[(*n - 1) * d_dim1 + 1] + d__[*n * d_dim1
	    + 1]) / wk[(*n << 1) + 2];

/*  CARRY OUT BACK SUBSTITUTION */

L30:
    for (j = nm1; j >= 1; --j) {
	if (wk[(j << 1) + 2] == zero) {
	    goto L5008;
	}
	d__[j * d_dim1 + 1] = (d__[j * d_dim1 + 1] - wk[(j << 1) + 1] * d__[(
		j + 1) * d_dim1 + 1]) / wk[(j << 1) + 2];
    }
/* --------------------(  END  CODING FROM CUBSPL )-------------------- */

/*  NORMAL RETURN. */

    return;

/*  ERROR RETURNS. */

L5001:
/*     N.LT.2 RETURN. */
    *ierr = -1;
    xermsg_("SLATEC", "DPCHSP", "NUMBER OF DATA POINTS LESS THAN TWO", *ierr);
    return;

L5002:
/*     INCFD.LT.1 RETURN. */
    *ierr = -2;
    xermsg_("SLATEC", "DPCHSP", "INCREMENT LESS THAN ONE", *ierr);
    return;

L5003:
/*     X-ARRAY NOT STRICTLY INCREASING. */
    *ierr = -3;
    xermsg_("SLATEC", "DPCHSP", "X-ARRAY NOT STRICTLY INCREASING", *ierr);
    return;

L5004:
/*     IC OUT OF RANGE RETURN. */
    *ierr += -3;
    xermsg_("SLATEC", "DPCHSP", "IC OUT OF RANGE", *ierr);
    return;

L5007:
/*     NWK TOO SMALL RETURN. */
    *ierr = -7;
    xermsg_("SLATEC", "DPCHSP", "WORK ARRAY TOO SMALL", *ierr);
    return;

L5008:
/*     SINGULAR SYSTEM. */
/*   *** THEORETICALLY, THIS CAN ONLY OCCUR IF SUCCESSIVE X-VALUES   *** */
/*   *** ARE EQUAL, WHICH SHOULD ALREADY HAVE BEEN CAUGHT (IERR=-3). *** */
    *ierr = -8;
    xermsg_("SLATEC", "DPCHSP", "SINGULAR LINEAR SYSTEM", *ierr);
    return;

L5009:
/*     ERROR RETURN FROM DPCHDF. */
/*   *** THIS CASE SHOULD NEVER OCCUR *** */
    *ierr = -9;
    xermsg_("SLATEC", "DPCHSP", "ERROR RETURN FROM DPCHDF", *ierr);
    return;
}
