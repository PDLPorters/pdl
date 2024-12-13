#include <stdio.h>
#include <stdint.h>
#include <math.h>

/* f2c.h  --  Standard Fortran to C header file */
typedef long int integer;
typedef double doublereal;
typedef long int logical;

#define TRUE_ (1)

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

void dintrv(doublereal *, integer, doublereal, integer *, integer *, integer *);
void dpchkt(integer, doublereal *, integer *, doublereal *);
void dchfdv(doublereal, doublereal,
      doublereal, doublereal, doublereal, doublereal, integer,
       doublereal *, doublereal *, doublereal *, integer *, integer *);
void dchfev(doublereal, doublereal,
      doublereal, doublereal, doublereal, doublereal, integer,
       doublereal *, doublereal *, integer *, integer *);
doublereal dchfie(doublereal, doublereal, doublereal,
      doublereal, doublereal, doublereal, doublereal, doublereal);
doublereal dpchid(integer, doublereal *, doublereal *,
      doublereal *, logical *, integer, integer, integer *);
doublereal dpchst(doublereal, doublereal);
integer dpchsw(doublereal, integer, doublereal,
       doublereal, doublereal, doublereal);
integer dpchce(integer *, doublereal *, integer,
      doublereal *, doublereal *, doublereal *, doublereal *);
void dpchci(integer, doublereal *, doublereal *, doublereal *);
integer dpchcs(doublereal, integer, doublereal *, doublereal *, doublereal *);
doublereal dpchdf(integer, doublereal *, doublereal *, integer *);

doublereal d1mach() {
  return 2.3e-16; /* for float, 1.2e-7 */
}

doublereal dbvalu(doublereal *t, doublereal *a, integer n, integer k,
    integer ideriv, doublereal x, integer inbv, doublereal *work)
{
/* Local variables */
  integer i, j, j1, j2, jj, km1, ihi, imk, kmj, ipj, ilo, kpk;
  doublereal fkmj;
  integer mflag, kmider;

  if (k < 1) {
    xermsg_("SLATEC", "DBVALU", "K DOES NOT SATISFY K.GE.1", (long)2);
    return 0.;
  }
  if (n < k) {
    xermsg_("SLATEC", "DBVALU", "N DOES NOT SATISFY N.GE.K", (long)2);
    return 0.;
  }
  if (ideriv < 0 || ideriv >= k) {
    xermsg_("SLATEC", "DBVALU", "IDERIV DOES NOT SATISFY 0.LE.IDERIV.LT.K", (long)2);
    return 0.;
  }
  kmider = k - ideriv;

/* *** FIND *I* IN (K,N) SUCH THAT T(I) .LE. X .LT. T(I+1) */
/*   (OR, .LE. T(I+1) IF T(I) .LT. T(I+1) = T(N+1)). */
  km1 = k - 1;
  dintrv(&t[0], n + 1, x, &inbv, &i, &mflag);
  if (x < t[k-1]) {
    xermsg_("SLATEC", "DBVALU", "X IS N0T GREATER THAN OR EQUAL TO T(K)", (long)2);
    return 0.;
  }
  if (mflag != 0) {
    if (x > t[i-1]) {
      xermsg_("SLATEC", "DBVALU", "X IS NOT LESS THAN OR EQUAL TO T(N+1)", (long)2);
      return 0.;
    }
    while (1) {
      if (i == k) {
        xermsg_("SLATEC", "DBVALU", "A LEFT LIMITING VALUE CANNOT BE OBTAINED AT T(K)", (long)2);
        return 0.;
      }
      --i;
      if (x != t[i-1]) {
        break;
      }
    }

/* *** DIFFERENCE THE COEFFICIENTS *IDERIV* TIMES */
/*   WORK(I) = AJ(I), WORK(K+I) = DP(I), WORK(K+K+I) = DM(I), I=1.K */
  }
  imk = i - k;
  for (j = 0; j < k; ++j) {
    work[j] = a[imk + j];
  }
  if (ideriv != 0) {
    for (j = 0; j < ideriv; ++j) {
      kmj = k - j - 1;
      fkmj = (doublereal) kmj;
      for (jj = 0; jj < kmj; ++jj) {
        ihi = i + jj;
        work[jj] = (work[jj+1] - work[jj]) / (t[ihi] - t[ihi - kmj]) * fkmj;
      }
    }
/* *** COMPUTE VALUE AT *X* IN (T(I),(T(I+1)) OF IDERIV-TH DERIVATIVE, */
/*   GIVEN ITS RELEVANT B-SPLINE COEFF. IN AJ(1),...,AJ(K-IDERIV). */
  }
  if (ideriv != km1) {
    j1 = k;
    j2 = kpk = k + k;
    for (j = 0; j < kmider; ++j) {
      ipj = i + j + 1;
      work[j1] = t[ipj-1] - x;
      work[j2] = x - t[i - j - 1];
      ++j1;
      ++j2;
    }
    for (j = ideriv; j < km1; ++j) {
      kmj = k - j - 1;
      ilo = kpk + kmj - 1;
      for (jj = 0; jj < kmj; ++jj) {
        work[jj] = (work[jj+1] * work[ilo] + work[jj] *
            work[k + jj]) / (work[ilo] + work[k + jj]);
        --ilo;
      }
    }
  }
  return work[0];
}

/* ***PURPOSE  Compute the largest integer ILEFT in 1 .LE. ILEFT .LE. LXT */
/*      such that XT(ILEFT) .LE. X where XT(*) is a subdivision of */
/*      the X interval. */
/*   Written by Carl de Boor and modified by D. E. Amos */
/*   Abstract  **** a double precision routine **** */
/*     DINTRV is the INTERV routine of the reference. */
/*     DINTRV computes the largest integer ILEFT in 1 .LE. ILEFT .LE. */
/*     LXT such that XT(ILEFT) .LE. X where XT(*) is a subdivision of */
/*     the X interval.  Precisely, */
/*            X .LT. XT(1)        1     -1 */
/*     if  XT(I) .LE. X .LT. XT(I+1)  then  ILEFT=I  , MFLAG=0 */
/*       XT(LXT) .LE. X             LXT    1, */
/*     That is, when multiplicities are present in the break point */
/*     to the left of X, the largest index is taken for ILEFT. */
/*   Description of Arguments */
/*     Input    XT,X are double precision */
/*      XT    - XT is a knot or break point vector of length LXT */
/*      LXT   - length of the XT vector */
/*      X     - argument */
/*      ILO   - an initialization parameter which must be set */
/*          to 1 the first time the spline array XT is */
/*          processed by DINTRV. */
/*     Output */
/*      ILO   - ILO contains information for efficient process- */
/*          ing after the initial call and ILO must not be */
/*          changed by the user.  Distinct splines require */
/*          distinct ILO parameters. */
/*      ILEFT   - largest integer satisfying XT(ILEFT) .LE. X */
/*      MFLAG   - signals when X lies out of bounds */
/*   Error Conditions */
/*     None */
/* ***REFERENCES  Carl de Boor, Package for calculating with B-splines, */
/*         SIAM Journal on Numerical Analysis 14, 3 (June 1977), */
/*         pp. 441-472. */
void dintrv(doublereal *xt, integer lxt, doublereal x,
    integer *ilo, integer *ileft, integer *mflag)
{
  integer ihi, istep, middle, skipflag;
  ihi = *ilo + 1;
  if (ihi >= lxt) {
    if (x >= xt[lxt-1]) {
      *mflag = 1; *ileft = lxt; return;
    }
    if (lxt <= 1) {
      *mflag = -1; *ileft = 1; return;
    }
    *ilo = lxt - 1;
    ihi = lxt;
  }
  skipflag = 0;
  if (x < xt[ihi-1]) {
    if (x >= xt[*ilo-1]) {
      *mflag = 0; *ileft = *ilo; return;
    }
/* *** NOW X .LT. XT(IHI) . FIND LOWER BOUND */
    istep = 1;
    while (1) {
      ihi = *ilo;
      *ilo = ihi - istep;
      if (*ilo <= 1) {
        break;
      }
      if (x >= xt[*ilo-1]) {
        skipflag = 1;
        break;
      }
      istep <<= 1;
    }
    if (!skipflag) {
      *ilo = 1;
      if (x < xt[1-1]) {
        *mflag = -1; *ileft = 1; return;
      }
    }
    skipflag = 1;
/* *** NOW X .GE. XT(ILO) . FIND UPPER BOUND */
  }
  if (!skipflag) {
    istep = 1;
    while (1) {
      *ilo = ihi;
      ihi = *ilo + istep;
      if (ihi >= lxt) {
        if (x < xt[ihi-1]) {
          skipflag = 1;
          break;
        }
        istep <<= 1;
        continue;
      }
      break;
    }
    if (!skipflag) {
      if (x >= xt[lxt-1]) {
        *mflag = 1; *ileft = lxt; return;
      }
      ihi = lxt;
    }
  }
/* *** NOW XT(ILO) .LE. X .LT. XT(IHI) . NARROW THE INTERVAL */
  while (1) {
    middle = (*ilo + ihi) / 2;
    if (middle == *ilo) {
      *mflag = 0; *ileft = *ilo; return;
    }
/*   NOTE. IT IS ASSUMED THAT MIDDLE = ILO IN CASE IHI = ILO+1 */
    if (x < xt[middle-1]) {
      ihi = middle;
      continue;
    }
    *ilo = middle;
  }
}

void dpchbs(integer n, doublereal *x, doublereal *f,
    doublereal *d, integer *knotyp, integer *nknots,
    doublereal *t, doublereal *bcoef, integer *ndim, integer *kord,
    integer *ierr)
{
/* Local variables */
  integer k, kk;
  doublereal dov3, hold, hnew;
  char *libnam = "SLATEC";
  char *subnam = "DPCHBS";
  *ndim = n << 1;
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
/*      Set up knot sequence. */
    *nknots = *ndim + 4;
    dpchkt(n, &x[0], knotyp, &t[0]);
  }
/*  Compute B-spline coefficients. */
  hnew = t[2] - t[0];
  for (k = 0; k < n; ++k) {
    kk = k << 1;
    hold = hnew;
/*      The following requires mixed mode arithmetic. */
    dov3 = d[k] / 3;
    bcoef[kk] = f[k] - hold * dov3;
/*      The following assumes T(2*K+1) = X(K). */
    hnew = t[kk + 4] - t[kk + 2];
    bcoef[kk+1] = f[k] + hnew * dov3;
  }
}

/* ***PURPOSE  Compute B-spline knot sequence for DPCHBS. */
/*   Set a knot sequence for the B-spline representation of a PCH */
/*   function with breakpoints X.  All knots will be at least double. */
/*   Endknots are set as: */
/*    (1) quadruple knots at endpoints if KNOTYP=0; */
/*    (2) extrapolate the length of end interval if KNOTYP=1; */
/*    (3) periodic if KNOTYP=2. */
/*  Input arguments:  N, X, KNOTYP. */
/*  Output arguments:  T. */
/*  Restrictions/assumptions: */
/*   1. N.GE.2 .  (not checked) */
/*   2. X(i).LT.X(i+1), i=1,...,N .  (not checked) */
/*   3. 0.LE.KNOTYP.LE.2 .  (Acts like KNOTYP=0 for any other value.) */
/* ***SEE ALSO  DPCHBS */
/*  Since this is subsidiary to DPCHBS, which validates its input before */
/*  calling, it is unnecessary for such validation to be done here. */
void dpchkt(integer n, doublereal *x, integer *knotyp,
    doublereal *t)
{
/* Local variables */
  integer j, k;
  doublereal hbeg, hend;
  integer ndim;
  ndim = n << 1;
/*  Set interior knots. */
  j = 0;
  for (k = 0; k < n; ++k) {
    j += 2;
    t[j+1] = t[j] = x[k];
  }
/*   Assertion:  At this point T(3),...,T(NDIM+2) have been set and */
/*         J=NDIM+1. */
/*  Set end knots according to KNOTYP. */
  hbeg = x[1] - x[0];
  hend = x[n-1] - x[n - 2];
  if (*knotyp == 1) {
/*      Extrapolate. */
    t[1] = x[0] - hbeg;
    t[ndim + 2] = x[n-1] + hend;
  } else if (*knotyp == 2) {
/*      Periodic. */
    t[1] = x[0] - hend;
    t[ndim + 2] = x[n-1] + hbeg;
  } else {
/*      Quadruple end knots. */
    t[1] = x[0];
    t[ndim + 2] = x[n-1];
  }
  t[0] = t[1];
  t[ndim + 3] = t[ndim + 2];
}

/* ***PURPOSE  Evaluate a cubic polynomial given in Hermite form and its */
/*      first derivative at an array of points.  While designed for */
/*      use by DPCHFD, it may be useful directly as an evaluator */
/*      for a piecewise cubic Hermite function in applications, */
/*      such as graphing, where the interval is known in advance. */
/*      If only function values are required, use DCHFEV instead. */
/*    DCHFDV:  Cubic Hermite Function and Derivative Evaluator */
/*   Evaluates the cubic polynomial determined by function values */
/*   F1,F2 and derivatives D1,D2 on interval (X1,X2), together with */
/*   its first derivative, at the points  XE(J), J=1(1)NE. */
/*   If only function values are required, use DCHFEV, instead. */
/* ---------------------------------------------------------------------- */
/*  Calling sequence: */
/*    INTEGER  NE, NEXT(2), IERR */
/*    DOUBLE PRECISION  X1, X2, F1, F2, D1, D2, XE(NE), FE(NE), */
/*              DE(NE) */
/*    CALL  DCHFDV (X1,X2, F1,F2, D1,D2, NE, XE, FE, DE, NEXT, IERR) */
/*   Parameters: */
/*   X1,X2 -- (input) endpoints of interval of definition of cubic. */
/*       (Error return if  X1.EQ.X2 .) */
/*   F1,F2 -- (input) values of function at X1 and X2, respectively. */
/*   D1,D2 -- (input) values of derivative at X1 and X2, respectively. */
/*   NE -- (input) number of evaluation points.  (Error return if */
/*       NE.LT.1 .) */
/*   XE -- (input) real*8 array of points at which the functions are to */
/*       be evaluated.  If any of the XE are outside the interval */
/*       [X1,X2], a warning error is returned in NEXT. */
/*   FE -- (output) real*8 array of values of the cubic function */
/*       defined by  X1,X2, F1,F2, D1,D2  at the points  XE. */
/*   DE -- (output) real*8 array of values of the first derivative of */
/*       the same function at the points  XE. */
/*   NEXT -- (output) integer array indicating number of extrapolation */
/*       points: */
/*      NEXT(1) = number of evaluation points to left of interval. */
/*      NEXT(2) = number of evaluation points to right of interval. */
/*   IERR -- (output) error flag. */
/*       Normal return: */
/*        IERR = 0  (no errors). */
/*       "Recoverable" errors: */
/*        IERR = -1  if NE.LT.1 . */
/*        IERR = -2  if X1.EQ.X2 . */
/*        (Output arrays have not been changed in either case.) */
/*  Programming notes: */
/*   To produce a single precision version, simply: */
/*    a. Change DCHFDV to CHFDV wherever it occurs, */
/*    b. Change the double precision declaration to real, and */
/*    c. Change the constant ZERO to single precision. */
void dchfdv(doublereal x1, doublereal x2, doublereal f1,
    doublereal f2, doublereal d1, doublereal d2, integer ne,
    doublereal *xe, doublereal *fe, doublereal *de, integer *next,
    integer *ierr)
{
/* Local variables */
  doublereal h;
  integer i;
  doublereal x, c2, c3, c2t2, c3t3, xma, xmi, del1, del2, delta;
  if (ne < 1) {
    *ierr = -1;
    xermsg_("SLATEC", "DCHFDV", "NUMBER OF EVALUATION POINTS LESS THAN ONE", *ierr);
    return;
  }
  h = x2 - x1;
  if (h == 0.) {
    *ierr = -2;
    xermsg_("SLATEC", "DCHFDV", "INTERVAL ENDPOINTS EQUAL", *ierr);
    return;
  }
/*  INITIALIZE. */
  *ierr = 0;
  next[0] = 0;
  next[1] = 0;
  xmi = min(0.,h);
  xma = max(0.,h);
/*  COMPUTE CUBIC COEFFICIENTS (EXPANDED ABOUT X1). */
  delta = (f2 - f1) / h;
  del1 = (d1 - delta) / h;
  del2 = (d2 - delta) / h;
/*                       (DELTA IS NO LONGER NEEDED.) */
  c2 = -(del1 + del1 + del2);
  c2t2 = c2 + c2;
  c3 = (del1 + del2) / h;
/*                 (H, DEL1 AND DEL2 ARE NO LONGER NEEDED.) */
  c3t3 = c3 + c3 + c3;
/*  EVALUATION LOOP. */
  for (i = 0; i < ne; ++i) {
    x = xe[i] - x1;
    fe[i] = f1 + x * (d1 + x * (c2 + x * c3));
    de[i] = d1 + x * (c2t2 + x * c3t3);
/*      COUNT EXTRAPOLATION POINTS. */
    if (x < xmi) {
      ++next[0];
    }
    if (x > xma) {
      ++next[1];
    }
/*    (NOTE REDUNDANCY--IF EITHER CONDITION IS TRUE, OTHER IS FALSE.) */
  }
}

/*  Programming notes: */
/*   1. To produce a single precision version, simply: */
/*    a. Change DPCHFD to PCHFD, and DCHFDV to CHFDV, wherever they */
/*       occur, */
/*    b. Change the double precision declaration to real, */
/*   2. Most of the coding between the call to DCHFDV and the end of */
/*    the IR-loop could be eliminated if it were permissible to */
/*    assume that XE is ordered relative to X. */
/*   3. DCHFDV does not assume that X1 is less than X2.  thus, it would */
/*    be possible to write a version of DPCHFD that assumes a strict- */
/*    ly decreasing X-array by simply running the IR-loop backwards */
/*    (and reversing the order of appropriate tests). */
/*   4. The present code has a minor bug, which I have decided is not */
/*    worth the effort that would be required to fix it. */
/*    If XE contains points in [X(N-1),X(N)], followed by points .LT. */
/*    X(N-1), followed by points .GT.X(N), the extrapolation points */
/*    will be counted (at least) twice in the total returned in IERR. */
void dpchfd(integer n, doublereal *x, doublereal *f,
    doublereal *d, logical *skip, integer ne,
    doublereal *xe, doublereal *fe, doublereal *de, integer *ierr)
{
/* Local variables */
  integer i, j, nj, ir, ierc, next[2], jfirst;
  integer located;
/*  VALIDITY-CHECK ARGUMENTS. */
  if (ne < 1) {
    *ierr = -4;
    xermsg_("SLATEC", "DPCHFD", "NUMBER OF EVALUATION POINTS LESS THAN ONE", *ierr);
    return;
  }
  if (!*skip) {
    if (n < 2) {
      *ierr = -1;
      xermsg_("SLATEC", "DPCHFD", "NUMBER OF DATA POINTS LESS THAN TWO", *ierr);
      return;
    }
    for (i = 1; i < n; ++i) {
      if (x[i] <= x[i - 1]) {
        *ierr = -3;
        xermsg_("SLATEC", "DPCHFD", "X-ARRAY NOT STRICTLY INCREASING", *ierr);
        return;
      }
    }
  }
/*  FUNCTION DEFINITION IS OK, GO ON. */
  *ierr = 0;
  *skip = TRUE_;
/*  LOOP OVER INTERVALS.    (   INTERVAL INDEX IS  IL = IR-1  . ) */
/*                ( INTERVAL IS X(IL).LE.X.LT.X(IR) . ) */
  jfirst = 0;
  ir = 1;
  while (1) {
/*   SKIP OUT OF LOOP IF HAVE PROCESSED ALL EVALUATION POINTS. */
    if (jfirst >= ne) {
      return;
    }
/*   LOCATE ALL POINTS IN INTERVAL. */
    located = 0;
    for (j = jfirst; j < ne; ++j) {
      if (xe[j] >= x[ir]) {
        located = 1;
        break;
      }
    }
    if (!located || ir == n-1) {
      j = ne;
    }
/*   HAVE LOCATED FIRST POINT BEYOND INTERVAL. */
    nj = j - jfirst;
/*   SKIP EVALUATION IF NO POINTS IN INTERVAL. */
    if (nj != 0) {
/*   EVALUATE CUBIC AT XE(I),  I = JFIRST (1) J-1 . */
/*     ---------------------------------------------------------------- */
        dchfdv(x[ir - 1], x[ir], f[ir - 1], f[ir],
            d[ir - 1], d[ir], nj,
            &xe[jfirst], &fe[jfirst], &de[jfirst], next, &ierc);
/*     ---------------------------------------------------------------- */
        if (ierc < 0) {
          *ierr = -5;
          xermsg_("SLATEC", "DPCHFD", "ERROR RETURN FROM DCHFDV -- FATAL", *ierr);
          return;
        }
        if (next[1] != 0) {
/*       IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(2) TO THE */
/*       RIGHT OF X(IR). */
          if (ir < n-1) {
/*        WE SHOULD NEVER HAVE GOTTEN HERE. */
            *ierr = -5;
            xermsg_("SLATEC", "DPCHFD", "ERROR RETURN FROM DCHFDV -- FATAL", *ierr);
            return;
          }
/*        THESE ARE ACTUALLY EXTRAPOLATION POINTS. */
          *ierr += next[1];
        }
        if (next[0] != 0) {
/*       IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(1) TO THE */
/*       LEFT OF X(IR-1). */
          if (ir < 2) {
/*        THESE ARE ACTUALLY EXTRAPOLATION POINTS. */
            *ierr += next[0];
            goto L49;
          }
/*        XE IS NOT ORDERED RELATIVE TO X, SO MUST ADJUST */
/*        EVALUATION INTERVAL. */
/*        FIRST, LOCATE FIRST POINT TO LEFT OF X(IR-1). */
          located = 0;
          for (i = jfirst; i < j; ++i) {
            if (xe[i] < x[ir - 1]) {
              located = 1;
              break;
            }
          }
          if (!located) {
/*        NOTE-- CANNOT DROP THROUGH HERE UNLESS THERE IS AN ERROR */
/*           IN DCHFDV. */
            *ierr = -5;
            xermsg_("SLATEC", "DPCHFD", "ERROR RETURN FROM DCHFDV -- FATAL", *ierr);
            return;
          }
/*        RESET J.  (THIS WILL BE THE NEW JFIRST.) */
          j = i;
/*        NOW FIND OUT HOW FAR TO BACK UP IN THE X-ARRAY. */
          for (i = 0; i < ir; ++i) {
            if (xe[j] < x[i]) {
              break;
            }
          }
/*        NB-- CAN NEVER DROP THROUGH HERE, SINCE XE(J).LT.X(IR-1). */
/*        AT THIS POINT, EITHER  XE(J) .LT. X(1) */
/*         OR    X(I-1) .LE. XE(J) .LT. X(I) . */
/*        RESET IR, RECOGNIZING THAT IT WILL BE INCREMENTED BEFORE */
/*        CYCLING. */
/* Computing MAX */
          ir = max(0,i-1);
        }
      L49:
        jfirst = j;
/*   END OF IR-LOOP. */
    }
    ++ir;
    if (ir > n-1) break;
  }
}

/* ***PURPOSE  Evaluate a cubic polynomial given in Hermite form at an */
/*      array of points.  While designed for use by DPCHFE, it may */
/*      be useful directly as an evaluator for a piecewise cubic */
/*      Hermite function in applications, such as graphing, where */
/*      the interval is known in advance. */
/*      DCHFEV:  Cubic Hermite Function EValuator */
/*   Evaluates the cubic polynomial determined by function values */
/*   F1,F2 and derivatives D1,D2 on interval (X1,X2) at the points */
/*   XE(J), J=1(1)NE. */
/* ---------------------------------------------------------------------- */
/*  Calling sequence: */
/*    INTEGER  NE, NEXT(2), IERR */
/*    DOUBLE PRECISION  X1, X2, F1, F2, D1, D2, XE(NE), FE(NE) */
/*    CALL  DCHFEV (X1,X2, F1,F2, D1,D2, NE, XE, FE, NEXT, IERR) */
/*   Parameters: */
/*   X1,X2 -- (input) endpoints of interval of definition of cubic. */
/*       (Error return if  X1.EQ.X2 .) */
/*   F1,F2 -- (input) values of function at X1 and X2, respectively. */
/*   D1,D2 -- (input) values of derivative at X1 and X2, respectively. */
/*   NE -- (input) number of evaluation points.  (Error return if */
/*       NE.LT.1 .) */
/*   XE -- (input) real*8 array of points at which the function is to */
/*       be evaluated.  If any of the XE are outside the interval */
/*       [X1,X2], a warning error is returned in NEXT. */
/*   FE -- (output) real*8 array of values of the cubic function */
/*       defined by  X1,X2, F1,F2, D1,D2  at the points  XE. */
/*   NEXT -- (output) integer array indicating number of extrapolation */
/*       points: */
/*      NEXT(1) = number of evaluation points to left of interval. */
/*      NEXT(2) = number of evaluation points to right of interval. */
/*   IERR -- (output) error flag. */
/*       Normal return: */
/*        IERR = 0  (no errors). */
/*       "Recoverable" errors: */
/*        IERR = -1  if NE.LT.1 . */
/*        IERR = -2  if X1.EQ.X2 . */
/*        (The FE-array has not been changed in either case.) */
/*  Programming notes: */
/*   To produce a single precision version, simply: */
/*    a. Change DCHFEV to CHFEV wherever it occurs, */
/*    b. Change the double precision declaration to real, and */
/*    c. Change the constant ZERO to single precision. */
void dchfev(doublereal x1, doublereal x2, doublereal f1,
    doublereal f2, doublereal d1, doublereal d2, integer ne,
    doublereal *xe, doublereal *fe, integer *next, integer *ierr)
{
/* Local variables */
  doublereal h;
  integer i;
  doublereal x, c2, c3, xma, xmi, del1, del2, delta;
  if (ne < 1) {
    *ierr = -1;
    xermsg_("SLATEC", "DCHFEV", "NUMBER OF EVALUATION POINTS LESS THAN ONE", *ierr);
    return;
  }
  h = x2 - x1;
  if (h == 0.) {
    *ierr = -2;
    xermsg_("SLATEC", "DCHFEV", "INTERVAL ENDPOINTS EQUAL", *ierr);
    return;
  }
/*  INITIALIZE. */
  *ierr = 0;
  next[0] = 0;
  next[1] = 0;
  xmi = min(0.,h);
  xma = max(0.,h);
/*  COMPUTE CUBIC COEFFICIENTS (EXPANDED ABOUT X1). */
  delta = (f2 - f1) / h;
  del1 = (d1 - delta) / h;
  del2 = (d2 - delta) / h;
/*                       (DELTA IS NO LONGER NEEDED.) */
  c2 = -(del1 + del1 + del2);
  c3 = (del1 + del2) / h;
/*                 (H, DEL1 AND DEL2 ARE NO LONGER NEEDED.) */
/*  EVALUATION LOOP. */
  for (i = 0; i < ne; ++i) {
    x = xe[i] - x1;
    fe[i] = f1 + x * (d1 + x * (c2 + x * c3));
/*      COUNT EXTRAPOLATION POINTS. */
    if (x < xmi) {
      ++next[0];
    }
    if (x > xma) {
      ++next[1];
    }
/*    (NOTE REDUNDANCY--IF EITHER CONDITION IS TRUE, OTHER IS FALSE.) */
  }
}

/*  Programming notes: */
/*   1. To produce a single precision version, simply: */
/*    a. Change DPCHFE to PCHFE, and DCHFEV to CHFEV, wherever they */
/*       occur, */
/*    b. Change the double precision declaration to real, */
/*   2. Most of the coding between the call to DCHFEV and the end of */
/*    the IR-loop could be eliminated if it were permissible to */
/*    assume that XE is ordered relative to X. */
/*   3. DCHFEV does not assume that X1 is less than X2.  thus, it would */
/*    be possible to write a version of DPCHFE that assumes a */
/*    decreasing X-array by simply running the IR-loop backwards */
/*    (and reversing the order of appropriate tests). */
/*   4. The present code has a minor bug, which I have decided is not */
/*    worth the effort that would be required to fix it. */
/*    If XE contains points in [X(N-1),X(N)], followed by points .LT. */
/*    X(N-1), followed by points .GT.X(N), the extrapolation points */
/*    will be counted (at least) twice in the total returned in IERR. */
void dpchfe(integer n, doublereal *x, doublereal *f,
    doublereal *d, logical *skip, integer ne,
    doublereal *xe, doublereal *fe, integer *ierr)
{
/* Local variables */
  integer i, j, nj, ir, ierc, next[2];
  integer jfirst, located;
/*  VALIDITY-CHECK ARGUMENTS. */
  if (ne < 1) {
    *ierr = -4;
    xermsg_("SLATEC", "DPCHFE", "NUMBER OF EVALUATION POINTS LESS THAN ONE", *ierr);
    return;
  }
  if (!*skip) {
    if (n < 2) {
      *ierr = -1;
      xermsg_("SLATEC", "DPCHFE", "NUMBER OF DATA POINTS LESS THAN TWO", *ierr);
      return;
    }
    for (i = 1; i < n; ++i) {
      if (x[i] <= x[i - 1]) {
        *ierr = -3;
        xermsg_("SLATEC", "DPCHFE", "X-ARRAY NOT STRICTLY INCREASING", *ierr);
        return;
      }
    }
/*  FUNCTION DEFINITION IS OK, GO ON. */
  }
  *ierr = 0;
  *skip = TRUE_;
/*  LOOP OVER INTERVALS.    (   INTERVAL INDEX IS  IL = IR-1  . ) */
/*                ( INTERVAL IS X(IL).LE.X.LT.X(IR) . ) */
  jfirst = 1;
  ir = 2;
  while (1) {
/*   SKIP OUT OF LOOP IF HAVE PROCESSED ALL EVALUATION POINTS. */
    if (jfirst > ne) {
      return;
    }
/*   LOCATE ALL POINTS IN INTERVAL. */
    located = 0;
    for (j = jfirst; j <= ne; ++j) {
      if (xe[j-1] >= x[ir-1]) {
        located = 1;
        break;
      }
    }
    if (located) {
/*   HAVE LOCATED FIRST POINT BEYOND INTERVAL. */
      if (ir == n) {
        j = ne + 1;
      }
    } else {
      j = ne + 1;
    }
    nj = j - jfirst;
/*   SKIP EVALUATION IF NO POINTS IN INTERVAL. */
    if (nj == 0) {
      ++ir;
      if (ir <= n) {
        continue;
      }
      return;
    }
/*   EVALUATE CUBIC AT XE(I),  I = JFIRST (1) J-1 . */
/*     ---------------------------------------------------------------- */
    dchfev(x[ir - 2], x[ir-1], f[ir - 2], f[ir-1],
        d[ir - 2], d[ir - 1], nj,
        &xe[jfirst-1], &fe[jfirst-1], next, &ierc);
/*     ---------------------------------------------------------------- */
    if (ierc < 0) {
      *ierr = -5;
      xermsg_("SLATEC", "DPCHFE", "ERROR RETURN FROM DCHFEV -- FATAL", *ierr);
      return;
    }
    if (next[1] != 0) {
/*       IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(2) TO THE */
/*       RIGHT OF X(IR). */
      if (ir < n) {
/*        WE SHOULD NEVER HAVE GOTTEN HERE. */
        *ierr = -5;
        xermsg_("SLATEC", "DPCHFE", "ERROR RETURN FROM DCHFEV -- FATAL", *ierr);
        return;
      }
/*        THESE ARE ACTUALLY EXTRAPOLATION POINTS. */
      *ierr += next[1];
    }
    if (next[0] != 0) {
/*       IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(1) TO THE */
/*       LEFT OF X(IR-1). */
      if (ir <= 2) {
/*        THESE ARE ACTUALLY EXTRAPOLATION POINTS. */
        *ierr += next[0];
        goto L49;
      }
/*        XE IS NOT ORDERED RELATIVE TO X, SO MUST ADJUST */
/*        EVALUATION INTERVAL. */
/*        FIRST, LOCATE FIRST POINT TO LEFT OF X(IR-1). */
      located = 0;
      for (i = jfirst; i < j; ++i) {
        if (xe[i-1] < x[ir - 2]) {
          located = 1;
          break;
        }
      }
      if (!located) {
/*        NOTE-- CANNOT DROP THROUGH HERE UNLESS THERE IS AN ERROR */
/*           IN DCHFEV. */
        *ierr = -5;
        xermsg_("SLATEC", "DPCHFE", "ERROR RETURN FROM DCHFEV -- FATAL", *ierr);
        return;
      }
/*        RESET J.  (THIS WILL BE THE NEW JFIRST.) */
      j = i;
/*        NOW FIND OUT HOW FAR TO BACK UP IN THE X-ARRAY. */
      for (i = 1; i < ir; ++i) {
        if (xe[j-1] < x[i-1]) {
          break;
        }
      }
/*        NB-- CAN NEVER DROP THROUGH HERE, SINCE XE(J).LT.X(IR-1). */
/*        AT THIS POINT, EITHER  XE(J) .LT. X(1) */
/*         OR    X(I-1) .LE. XE(J) .LT. X(I) . */
/*        RESET IR, RECOGNIZING THAT IT WILL BE INCREMENTED BEFORE */
/*        CYCLING. */
/* Computing MAX */
      ir = max(1,i - 1);
  L49:;
    }
    jfirst = j;
/*   END OF IR-LOOP. */
    ++ir;
    if (ir > n) break;
  }
}

/* ***PURPOSE  Evaluates integral of a single cubic for DPCHIA */
/*      DCHFIE:  Cubic Hermite Function Integral Evaluator. */
/*   Called by  DPCHIA  to evaluate the integral of a single cubic (in */
/*   Hermite form) over an arbitrary interval (A,B). */
/* ---------------------------------------------------------------------- */
/*  Calling sequence: */
/*    DOUBLE PRECISION  X1, X2, F1, F2, D1, D2, A, B */
/*    DOUBLE PRECISION  VALUE, DCHFIE */
/*    VALUE = DCHFIE (X1, X2, F1, F2, D1, D2, A, B) */
/*   Parameters: */
/*   VALUE -- (output) value of the requested integral. */
/*   X1,X2 -- (input) endpoints if interval of definition of cubic. */
/*   F1,F2 -- (input) function values at the ends of the interval. */
/*   D1,D2 -- (input) derivative values at the ends of the interval. */
/*   A,B -- (input) endpoints of interval of integration. */
/* ***SEE ALSO  DPCHIA */
/*  Programming notes: */
/*  1. There is no error return from this routine because zero is */
/*   indeed the mathematically correct answer when X1.EQ.X2 . */
doublereal dchfie(doublereal x1, doublereal x2, doublereal f1, doublereal f2,
    doublereal d1, doublereal d2, doublereal a, doublereal b)
{
/* Local variables */
  doublereal h, ta1, ta2, tb1, tb2, ua1, ua2, ub1, ub2, phia1,
      phia2, phib1, phib2, psia1, psia2, psib1, psib2, dterm, fterm;
  if (x1 == x2) {
    return 0.;
  }
  h = x2 - x1;
  ta1 = (a - x1) / h;
  ta2 = (x2 - a) / h;
  tb1 = (b - x1) / h;
  tb2 = (x2 - b) / h;
/* Computing 3rd power */
  ua1 = ta1 * (ta1 * ta1);
  phia1 = ua1 * (2. - ta1);
  psia1 = ua1 * (3. * ta1 - 4.);
/* Computing 3rd power */
  ua2 = ta2 * (ta2 * ta2);
  phia2 = ua2 * (2. - ta2);
  psia2 = -ua2 * (3. * ta2 - 4.);
/* Computing 3rd power */
  ub1 = tb1 * (tb1 * tb1);
  phib1 = ub1 * (2. - tb1);
  psib1 = ub1 * (3. * tb1 - 4.);
/* Computing 3rd power */
  ub2 = tb2 * (tb2 * tb2);
  phib2 = ub2 * (2. - tb2);
  psib2 = -ub2 * (3. * tb2 - 4.);
  fterm = f1 * (phia2 - phib2) + f2 * (phib1 - phia1);
  dterm = (d1 * (psia2 - psib2) + d2 * (psib1 - psia1)) * (h / 6.);
  return 0.5 * h * (fterm + dterm);
}

/*  Programming notes: */
/*  1. The error flag from DPCHID is tested, because a logic flaw */
/*   could conceivably result in IERD=-4, which should be reported. */
doublereal dpchia(integer n, doublereal *x, doublereal *f, doublereal *d,
    logical *skip, doublereal a, doublereal b, integer *ierr)
{
/* Local variables */
  integer i, ia, ib, il;
  doublereal xa, xb;
  integer ir, ierd;
  doublereal value;
  value = 0.;
  if (!*skip) {
    if (n < 2) {
      *ierr = -1;
      xermsg_("SLATEC", "DPCHIA", "NUMBER OF DATA POINTS LESS THAN TWO", *ierr);
      return value;
    }
    for (i = 2; i <= n; ++i) {
      if (x[i-1] <= x[i - 2]) {
        *ierr = -3;
        xermsg_("SLATEC", "DPCHIA", "X-ARRAY NOT STRICTLY INCREASING", *ierr);
        return value;
      }
    }
  }
/*  FUNCTION DEFINITION IS OK, GO ON. */
  *skip = TRUE_;
  *ierr = 0;
  if (a < x[0] || a > x[n-1]) {
    ++(*ierr);
  }
  if (b < x[0] || b > x[n-1]) {
    *ierr += 2;
  }
/*  COMPUTE INTEGRAL VALUE. */
  if (a != b) {
    xa = min(a,b);
    xb = max(a,b);
    if (xb <= x[1]) {
/*       INTERVAL IS TO LEFT OF X(2), SO USE FIRST CUBIC. */
/*           --------------------------------------- */
      value = dchfie(x[0], x[1], f[0], f[1],
          d[0], d[1], a, b);
/*           --------------------------------------- */
    } else if (xa >= x[n - 2]) {
/*       INTERVAL IS TO RIGHT OF X(N-1), SO USE LAST CUBIC. */
/*           ------------------------------------------ */
      value = dchfie(x[n - 2], x[n-1], f[n - 2], f[n-1], d[n-2], d[n-1], a, b);
/*           ------------------------------------------ */
    } else {
/*       'NORMAL' CASE -- XA.LT.XB, XA.LT.X(N-1), XB.GT.X(2). */
/*    ......LOCATE IA AND IB SUCH THAT */
/*         X(IA-1).LT.XA.LE.X(IA).LE.X(IB).LE.XB.LE.X(IB+1) */
      ia = 0;
      for (i = 1; i < n; ++i) {
        if (xa > x[i-1]) {
          ia = i;
        }
      }
/*       IA = 1 IMPLIES XA.LT.X(1) .  OTHERWISE, */
/*       IA IS LARGEST INDEX SUCH THAT X(IA-1).LT.XA,. */
      ib = n - 1;
      for (i = n; i > ia; --i) {
        if (xb < x[i-1]) {
          ib = i - 2;
        }
      }
/*       IB = N IMPLIES XB.GT.X(N) .  OTHERWISE, */
/*       IB IS SMALLEST INDEX SUCH THAT XB.LT.X(IB+1) . */
/*   ......COMPUTE THE INTEGRAL. */
      if (ib <= ia) {
/*        THIS MEANS IB = IA-1 AND */
/*         (A,B) IS A SUBSET OF (X(IB),X(IA)). */
/*            ------------------------------------------- */
        value = dchfie(x[ib], x[ia], f[ib],
            f[ia], d[ib], d[ia],
            a, b);
/*            ------------------------------------------- */
      } else {
/*        FIRST COMPUTE INTEGRAL OVER (X(IA),X(IB)). */
/*        (Case (IB .EQ. IA) is taken care of by initialization */
/*         of VALUE to ZERO.) */
        if (ib > ia-1) {
/*             --------------------------------------------- */
          value = dpchid(n, &x[0], &f[0], &d[0],
              skip, ia, ib, &ierd);
/*             --------------------------------------------- */
          if (ierd < 0) {
            *ierr = -4;
            xermsg_("SLATEC", "DPCHIA", "TROUBLE IN DPCHID", *ierr);
            return value;
          }
        }
/*        THEN ADD ON INTEGRAL OVER (XA,X(IA)). */
        if (xa < x[ia]) {
/* Computing MAX */
          il = max(0,ia - 1);
          ir = il + 1;
/*                 ------------------------------------- */
          value += dchfie(x[il], x[ir], f[il], f[ir], d[il], d[ir], xa, x[ia]);
/*                 ------------------------------------- */
        }
/*        THEN ADD ON INTEGRAL OVER (X(IB),XB). */
        if (xb > x[ib]) {
/* Computing MIN */
          ir = min(ib + 1,n-1);
          il = ir - 1;
/*                 ------------------------------------- */
          value += dchfie(x[il], x[ir], f[il], f[ir], d[il], d[ir], x[ib], xb);
/*                 ------------------------------------- */
        }
/*        FINALLY, ADJUST SIGN IF NECESSARY. */
        if (a > b) {
          value = -value;
        }
      }
    }
  }
  return value;
}

/*  Programming notes: */
/*  1. This routine uses a special formula that is valid only for */
/*   integrals whose limits coincide with data values.  This is */
/*   mathematically equivalent to, but much more efficient than, */
/*   calls to DCHFIE. */
doublereal dpchid(integer n, doublereal *x, doublereal *f, doublereal *d,
    logical *skip, integer ia, integer ib, integer *
    ierr)
{
/* Local variables */
  doublereal h;
  integer i, iup, low;
  doublereal sum, value;
  value = 0.;
/*  VALIDITY-CHECK ARGUMENTS. */
  if (!*skip) {
    if (n < 2) {
      *ierr = -1;
      xermsg_("SLATEC", "DPCHID", "NUMBER OF DATA POINTS LESS THAN TWO", *ierr);
      return value;
    }
    for (i = 2; i <= n; ++i) {
      if (x[i-1] <= x[i - 2]) {
        *ierr = -3;
        xermsg_("SLATEC", "DPCHID", "X-ARRAY NOT STRICTLY INCREASING", *ierr);
        return value;
      }
    }
  }
/*  FUNCTION DEFINITION IS OK, GO ON. */
  *skip = TRUE_;
  if (ia < 0 || ia > n-1 || ib < 0 || ib > n-1) {
    *ierr = -4;
    xermsg_("SLATEC", "DPCHID", "IA OR IB OUT OF RANGE", *ierr);
    return value;
  }
  *ierr = 0;
/*  COMPUTE INTEGRAL VALUE. */
  if (ia != ib) {
    low = min(ia,ib);
    iup = max(ia,ib);
    sum = 0.;
    for (i = low; i < iup; ++i) {
      h = x[i+1] - x[i];
      sum += h * (f[i] + f[i + 1] + (d[i] - d[i + 1]) * (h / 6.));
    }
    value = 0.5 * sum;
    if (ia > ib) {
      value = -value;
    }
  }
  return value;
}

/* ***PURPOSE  Set boundary conditions for DPCHIC */
/*      DPCHCE:  DPCHIC End Derivative Setter. */
/*  Called by DPCHIC to set end derivatives as requested by the user. */
/*  It must be called after interior derivative values have been set. */
/*            ----- */
/*  To facilitate two-dimensional applications, includes an increment */
/*  between successive values of the D-array. */
/* ---------------------------------------------------------------------- */
/*  Calling sequence: */
/*    INTEGER  IC(2), N, IERR */
/*    DOUBLE PRECISION  VC(2), X(N), H(N), SLOPE(N), D(N) */
/*    CALL  DPCHCE (IC, VC, N, X, H, SLOPE, D, IERR) */
/*   Parameters: */
/*   IC -- (input) integer array of length 2 specifying desired */
/*       boundary conditions: */
/*       IC(1) = IBEG, desired condition at beginning of data. */
/*       IC(2) = IEND, desired condition at end of data. */
/*       ( see prologue to DPCHIC for details. ) */
/*   VC -- (input) real*8 array of length 2 specifying desired boundary */
/*       values.  VC(1) need be set only if IC(1) = 2 or 3 . */
/*          VC(2) need be set only if IC(2) = 2 or 3 . */
/*   N -- (input) number of data points.  (assumes N.GE.2) */
/*   X -- (input) real*8 array of independent variable values.  (the */
/*       elements of X are assumed to be strictly increasing.) */
/*   H -- (input) real*8 array of interval lengths. */
/*   SLOPE -- (input) real*8 array of data slopes. */
/*       If the data are (X(I),Y(I)), I=1(1)N, then these inputs are: */
/*          H(I) =  X(I+1)-X(I), */
/*        SLOPE(I) = (Y(I+1)-Y(I))/H(I),  I=1(1)N-1. */
/*   D -- (input) real*8 array of derivative values at the data points. */
/*       The value corresponding to X(I) must be stored in */
/*        D(1+(I-1)),  I=1(1)N. */
/*      (output) the value of D at X(1) and/or X(N) is changed, if */
/*       necessary, to produce the requested boundary conditions. */
/*       no other entries in D are changed. */
/*   IERR -- (output) error flag. */
/*       Normal return: */
/*        IERR = 0  (no errors). */
/*       Warning errors: */
/*        IERR = 1  if IBEG.LT.0 and D(1) had to be adjusted for */
/*            monotonicity. */
/*        IERR = 2  if IEND.LT.0 and D(1+(N-1)) had to be */
/*            adjusted for monotonicity. */
/*        IERR = 3  if both of the above are true. */
/*  ------- */
/*  WARNING:  This routine does no validity-checking of arguments. */
/*  ------- */
/* ***SEE ALSO  DPCHIC */
/*  Programming notes: */
/*   1. The function DPCHST(ARG1,ARG2)  is assumed to return zero if */
/*    either argument is zero, +1 if they are of the same sign, and */
/*    -1 if they are of opposite sign. */
/*   2. One could reduce the number of arguments and amount of local */
/*    storage, at the expense of reduced code clarity, by passing in */
/*    the array WK (rather than splitting it into H and SLOPE) and */
/*    increasing its length enough to incorporate STEMP and XTEMP. */
/*   3. The two monotonicity checks only use the sufficient conditions. */
/*    Thus, it is possible (but unlikely) for a boundary condition to */
/*    be changed, even though the original interpolant was monotonic. */
/*    (At least the result is a continuous function of the data.) */
integer dpchce(integer *ic, doublereal *vc, integer n,
    doublereal *x, doublereal *h, doublereal *slope, doublereal *d)
{
/* Local variables */
  integer j, k, ibeg, iend, ierf, index;
  doublereal stemp[3], xtemp[4];
  integer ierr = 0;
  ibeg = ic[0];
  iend = ic[1];
/*  SET TO DEFAULT BOUNDARY CONDITIONS IF N IS TOO SMALL. */
  if (abs(ibeg) > n) {
    ibeg = 0;
  }
  if (abs(iend) > n) {
    iend = 0;
  }
/*  TREAT BEGINNING BOUNDARY CONDITION. */
  if (ibeg != 0) {
    k = abs(ibeg);
    if (k == 1) {
/*    BOUNDARY VALUE PROVIDED. */
      d[0] = vc[0];
    } else if (k == 2) {
/*    BOUNDARY SECOND DERIVATIVE PROVIDED. */
      d[0] = 0.5 * (3. * slope[0] - d[1] - 0.5 * vc[0] * h[0]);
    } else if (k < 5) {
/*    USE K-POINT DERIVATIVE FORMULA. */
/*    PICK UP FIRST K POINTS, IN REVERSE ORDER. */
      for (j = 1; j <= k; ++j) {
        index = k - j + 1;
/*       INDEX RUNS FROM K DOWN TO 1. */
        xtemp[j - 1] = x[index-1];
        if (j < k) {
          stemp[j - 1] = slope[index - 2];
        }
      }
/*         ----------------------------- */
      d[0] = dpchdf(k, xtemp, stemp, &ierf);
/*         ----------------------------- */
      if (ierf != 0) {
        ierr = -1;
        xermsg_("SLATEC", "DPCHCE", "ERROR RETURN FROM DPCHDF", ierr);
        return ierr;
      }
    } else {
/*    USE 'NOT A KNOT' CONDITION. */
      d[0] = (3. * (h[0] * slope[1] + h[1] * slope[0]) -
          2. * (h[0] + h[1]) * d[1] - h[0] * d[2]) / h[1];
    }
/*  CHECK D(1,1) FOR COMPATIBILITY WITH MONOTONICITY. */
    if (ibeg <= 0) {
      if (slope[0] == 0.) {
        if (d[0] != 0.) {
          d[0] = 0.;
          ++(ierr);
        }
      } else if (dpchst(d[0], slope[0]) < 0.) {
        d[0] = 0.;
        ++(ierr);
      } else if (abs(d[0]) > 3. * abs(slope[0])) {
        d[0] = 3. * slope[0];
        ++(ierr);
      }
    }
  }
/*  TREAT END BOUNDARY CONDITION. */
  if (iend == 0) {
    return ierr;
  }
  k = abs(iend);
  if (k == 1) {
/*    BOUNDARY VALUE PROVIDED. */
    d[n-1] = vc[1];
  } else if (k == 2) {
/*    BOUNDARY SECOND DERIVATIVE PROVIDED. */
    d[n-1] = 0.5 * (3. * slope[n - 2] - d[n - 2]
        + 0.5 * vc[1] * h[n - 2]);
  } else if (k < 5) {
/*    USE K-POINT DERIVATIVE FORMULA. */
/*    PICK UP LAST K POINTS. */
    for (j = 1; j <= k; ++j) {
      index = n - k + j;
/*       INDEX RUNS FROM N+1-K UP TO N. */
      xtemp[j - 1] = x[index-1];
      if (j < k) {
        stemp[j - 1] = slope[index-1];
      }
    }
/*         ----------------------------- */
    d[n-1] = dpchdf(k, xtemp, stemp, &ierf);
/*         ----------------------------- */
    if (ierf != 0) {
/*   *** THIS CASE SHOULD NEVER OCCUR *** */
      ierr = -1;
      xermsg_("SLATEC", "DPCHCE", "ERROR RETURN FROM DPCHDF", ierr);
      return ierr;
    }
  } else {
/*    USE 'NOT A KNOT' CONDITION. */
    d[n-1] = (3. * (h[n - 2] * slope[n - 3] +
        h[n - 3] * slope[n - 2]) - 2. * (h[n - 2] + h[n - 3]) *
        d[n - 2] - h[n - 2] * d[n - 3]) / h[n - 3];
  }
  if (iend > 0) {
    return ierr;
  }
/*  CHECK D(1,N) FOR COMPATIBILITY WITH MONOTONICITY. */
  if (slope[n - 2] == 0.) {
    if (d[n-1] != 0.) {
      d[n-1] = 0.;
      ierr += 2;
    }
  } else if (dpchst(d[n-1], slope[n - 2]) < 0.) {
    d[n-1] = 0.;
    ierr += 2;
  } else if (abs(d[n-1]) > 3. * abs(slope[n - 2])) {
    d[n-1] = 3. * slope[n - 2];
    ierr += 2;
  }
  return ierr;
}

/* ***PURPOSE  Set interior derivatives for DPCHIC */
/*      DPCHCI:  DPCHIC Initial Derivative Setter. */
/*  Called by DPCHIC to set derivatives needed to determine a monotone */
/*  piecewise cubic Hermite interpolant to the data. */
/*  Default boundary conditions are provided which are compatible */
/*  with monotonicity.  If the data are only piecewise monotonic, the */
/*  interpolant will have an extremum at each point where monotonicity */
/*  switches direction. */
/*  To facilitate two-dimensional applications, includes an increment */
/*  between successive values of the D-array. */
/*  The resulting piecewise cubic Hermite function should be identical */
/*  (within roundoff error) to that produced by DPCHIM. */
/* ---------------------------------------------------------------------- */
/*  Calling sequence: */
/*    INTEGER  N */
/*    DOUBLE PRECISION  H(N), SLOPE(N), D(N) */
/*    CALL  DPCHCI (N, H, SLOPE, D) */
/*   Parameters: */
/*   N -- (input) number of data points. */
/*       If N=2, simply does linear interpolation. */
/*   H -- (input) real*8 array of interval lengths. */
/*   SLOPE -- (input) real*8 array of data slopes. */
/*       If the data are (X(I),Y(I)), I=1(1)N, then these inputs are: */
/*          H(I) =  X(I+1)-X(I), */
/*        SLOPE(I) = (Y(I+1)-Y(I))/H(I),  I=1(1)N-1. */
/*   D -- (output) real*8 array of derivative values at data points. */
/*       If the data are monotonic, these values will determine a */
/*       a monotone cubic Hermite function. */
/*       The value corresponding to X(I) is stored in */
/*        D(1+(I-1)),  I=1(1)N. */
/*       No other entries in D are changed. */
/*  ------- */
/*  WARNING:  This routine does no validity-checking of arguments. */
/*  ------- */
/* ***SEE ALSO  DPCHIC */
/*  Programming notes: */
/*   1. The function  DPCHST(ARG1,ARG2)  is assumed to return zero if */
/*    either argument is zero, +1 if they are of the same sign, and */
/*    -1 if they are of opposite sign. */
void dpchci(integer n, doublereal *h, doublereal *slope, doublereal *d)
{
/* Local variables */
  integer i;
  doublereal w1, w2, del1, del2, dmin, dmax, hsum, drat1, drat2;
  integer nless1;
  doublereal hsumt3;
  nless1 = n - 1;
  del1 = slope[0];
/*  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION. */
  if (nless1 <= 1) {
    d[0] = d[n-1] = del1;
    return;
  }
/*  NORMAL CASE  (N .GE. 3). */
  del2 = slope[1];
/*  SET D(1) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE */
/*   SHAPE-PRESERVING. */
  hsum = h[0] + h[1];
  w1 = (h[0] + hsum) / hsum;
  w2 = -h[0] / hsum;
  d[0] = w1 * del1 + w2 * del2;
  if (dpchst(d[0], del1) <= 0.) {
    d[0] = 0.;
  } else if (dpchst(del1, del2) < 0.) {
/*    NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES. */
    dmax = 3. * del1;
    if (abs(d[0]) > abs(dmax)) {
      d[0] = dmax;
    }
  }
/*  LOOP THROUGH INTERIOR POINTS. */
  for (i = 2; i <= nless1; ++i) {
    if (i != 2) {
      hsum = h[i - 2] + h[i-1];
      del1 = del2;
      del2 = slope[i-1];
    }
/*    SET D(I)=0 UNLESS DATA ARE STRICTLY MONOTONIC. */
    d[i-1] = 0.;
    if (dpchst(del1, del2) <= 0.) {
      continue;
    }
/*    USE BRODLIE MODIFICATION OF BUTLAND FORMULA. */
    hsumt3 = hsum + hsum + hsum;
    w1 = (hsum + h[i - 2]) / hsumt3;
    w2 = (hsum + h[i-1]) / hsumt3;
/* Computing MAX */
    dmax = max(abs(del1),abs(del2));
/* Computing MIN */
    dmin = min(abs(del1),abs(del2));
    drat1 = del1 / dmax;
    drat2 = del2 / dmax;
    d[i-1] = dmin / (w1 * drat1 + w2 * drat2);
  }
/*  SET D(N) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE */
/*   SHAPE-PRESERVING. */
  w1 = -h[n - 2] / hsum;
  w2 = (h[n - 2] + hsum) / hsum;
  d[n-1] = w1 * del1 + w2 * del2;
  if (dpchst(d[n-1], del2) <= 0.) {
    d[n-1] = 0.;
  } else if (dpchst(del1, del2) < 0.) {
/*    NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES. */
    dmax = 3. * del2;
    if (abs(d[n-1]) > abs(dmax)) {
      d[n-1] = dmax;
    }
  }
}

/* ***PURPOSE  Adjusts derivative values for DPCHIC */
/*     DPCHCS:  DPCHIC Monotonicity Switch Derivative Setter. */
/*   Called by  DPCHIC  to adjust the values of D in the vicinity of a */
/*   switch in direction of monotonicity, to produce a more "visually */
/*   pleasing" curve than that given by  DPCHIM . */
/* ---------------------------------------------------------------------- */
/*  Calling sequence: */
/*    INTEGER  N, IERR */
/*    DOUBLE PRECISION  SWITCH, H(N), SLOPE(N), D(N) */
/*    CALL  DPCHCS (SWITCH, N, H, SLOPE, D, IERR) */
/*   Parameters: */
/*   SWITCH -- (input) indicates the amount of control desired over */
/*       local excursions from data. */
/*   N -- (input) number of data points.  (assumes N.GT.2 .) */
/*   H -- (input) real*8 array of interval lengths. */
/*   SLOPE -- (input) real*8 array of data slopes. */
/*       If the data are (X(I),Y(I)), I=1(1)N, then these inputs are: */
/*          H(I) =  X(I+1)-X(I), */
/*        SLOPE(I) = (Y(I+1)-Y(I))/H(I),  I=1(1)N-1. */
/*   D -- (input) real*8 array of derivative values at the data points, */
/*       as determined by DPCHCI. */
/*      (output) derivatives in the vicinity of switches in direction */
/*       of monotonicity may be adjusted to produce a more "visually */
/*       pleasing" curve. */
/*       The value corresponding to X(I) is stored in */
/*        D(1+(I-1)),  I=1(1)N. */
/*       No other entries in D are changed. */
/*   IERR -- (output) error flag.  should be zero. */
/*       If negative, trouble in DPCHSW.  (should never happen.) */
/*  ------- */
/*  WARNING:  This routine does no validity-checking of arguments. */
/*  ------- */
/*  Fortran intrinsics used:  ABS, MAX, MIN. */
/* ***SEE ALSO  DPCHIC */
/*  Programming notes: */
/*   1. The function  DPCHST(ARG1,ARG2)  is assumed to return zero if */
/*    either argument is zero, +1 if they are of the same sign, and */
/*    -1 if they are of opposite sign. */
integer dpchcs(doublereal mflag, integer n, doublereal *h,
    doublereal *slope, doublereal *d)
{
  static const doublereal fudge = 4.;
/* Local variables */
  integer i, k;
  doublereal del[3], fact, dfmx;
  integer indx;
  doublereal dext, dfloc, slmax, wtave[2];
  integer nless1, ierr = 0;
/*  INITIALIZE. */
  nless1 = n - 1;
/*  LOOP OVER SEGMENTS. */
  for (i = 2; i <= nless1; ++i) {
    doublereal dtmp = dpchst(slope[i - 2], slope[i-1]);
    if (dtmp > 0.) {
      continue;
    }
    if (dtmp != 0.) {
/* ....... SLOPE SWITCHES MONOTONICITY AT I-TH POINT ..................... */
/*       DO NOT CHANGE D IF 'UP-DOWN-UP'. */
      if (i > 2) {
        if (dpchst(slope[i - 3], slope[i-1]) > 0.) {
          continue;
        }
/*           -------------------------- */
      }
      if (i < nless1) {
        if (dpchst(slope[i], slope[i - 2]) > 0.) {
          continue;
        }
/*           ---------------------------- */
      }
/*   ....... COMPUTE PROVISIONAL VALUE FOR D(1,I). */
      dext = h[i-1] / (h[i - 2] + h[i-1]) * slope[i - 2] +
          h[i - 2] / (h[i - 2] + h[i-1]) * slope[i-1];
/*   ....... DETERMINE WHICH INTERVAL CONTAINS THE EXTREMUM. */
      dtmp = dpchst(dext, slope[i - 2]);
      if (dtmp == 0) {
        continue;
      }
      if (dtmp < 0.) {
/*        DEXT AND SLOPE(I-1) HAVE OPPOSITE SIGNS -- */
/*            EXTREMUM IS IN (X(I-1),X(I)). */
        k = i - 1;
/*        SET UP TO COMPUTE NEW VALUES FOR D(1,I-1) AND D(1,I). */
        wtave[1] = dext;
        if (k > 1) {
          wtave[0] = h[k-1] / (h[k - 2] + h[k-1]) * slope[k - 2] +
              h[k - 2] / (h[k - 2] + h[k]) * slope[k-1];
        }
      } else {
/*        DEXT AND SLOPE(I) HAVE OPPOSITE SIGNS -- */
/*            EXTREMUM IS IN (X(I),X(I+1)). */
        k = i;
/*        SET UP TO COMPUTE NEW VALUES FOR D(1,I) AND D(1,I+1). */
        wtave[0] = dext;
        if (k < nless1) {
          wtave[1] = h[k] / (h[k-1] + h[k]) * slope[k-1] + h[k-1]
              / (h[k-1] + h[k]) * slope[k];
        }
      }
    } else {
/* ....... AT LEAST ONE OF SLOPE(I-1) AND SLOPE(I) IS ZERO -- */
/*           CHECK FOR FLAT-TOPPED PEAK ....................... */
      if (i == nless1 || dpchst(slope[i - 2], slope[i]) >= 0.) {
        continue;
      }
/*        ----------------------------- */
/*       WE HAVE FLAT-TOPPED PEAK ON (X(I),X(I+1)). */
      k = i;
/*       SET UP TO COMPUTE NEW VALUES FOR D(1,I) AND D(1,I+1). */
      wtave[0] = h[k-1] / (h[k - 2] + h[k-1]) * slope[k - 2] + h[k - 2]
          / (h[k - 2] + h[k-1]) * slope[k-1];
      wtave[1] = h[k] / (h[k-1] + h[k]) * slope[k-1] + h[k-1] / (
          h[k-1] + h[k]) * slope[k];
    }
/* ....... AT THIS POINT WE HAVE DETERMINED THAT THERE WILL BE AN EXTREMUM */
/*    ON (X(K),X(K+1)), WHERE K=I OR I-1, AND HAVE SET ARRAY WTAVE-- */
/*       WTAVE(1) IS A WEIGHTED AVERAGE OF SLOPE(K-1) AND SLOPE(K), */
/*          IF K.GT.1 */
/*       WTAVE(2) IS A WEIGHTED AVERAGE OF SLOPE(K) AND SLOPE(K+1), */
/*          IF K.LT.N-1 */
    slmax = abs(slope[k-1]);
    if (k > 1) {
/* Computing MAX */
      slmax = max(slmax,abs(slope[k - 2]));
    }
    if (k < nless1) {
/* Computing MAX */
      slmax = max(slmax,abs(slope[k]));
    }
    if (k > 1) {
      del[0] = slope[k - 2] / slmax;
    }
    del[1] = slope[k-1] / slmax;
    if (k < nless1) {
      del[2] = slope[k] / slmax;
    }
    if (k > 1 && k < nless1) {
/*       NORMAL CASE -- EXTREMUM IS NOT IN A BOUNDARY INTERVAL. */
      fact = fudge * abs(del[2] * (del[0] - del[1]) * (wtave[1] / slmax));
      d[k-1] += min(fact,1.) * (wtave[0] - d[k-1]);
      fact = fudge * abs(del[0] * (del[2] - del[1]) * (wtave[0] / slmax));
      d[k] += min(fact,1.) * (wtave[1] -
          d[k]);
    } else {
/*       SPECIAL CASE K=1 (WHICH CAN OCCUR ONLY IF I=2) OR */
/*            K=NLESS1 (WHICH CAN OCCUR ONLY IF I=NLESS1). */
      fact = fudge * abs(del[1]);
      d[i-1] = min(fact,1.) * wtave[i - k];
/*        NOTE THAT I-K+1 = 1 IF K=I  (=NLESS1), */
/*            I-K+1 = 2 IF K=I-1(=1). */
    }
/* ....... ADJUST IF NECESSARY TO LIMIT EXCURSIONS FROM DATA. */
    if (mflag <= 0.) {
      continue;
    }
    dfloc = h[k-1] * abs(slope[k-1]);
    if (k > 1) {
/* Computing MAX */
      dfloc = max(dfloc,h[k - 2] * abs(slope[k - 2]));
    }
    if (k < nless1) {
/* Computing MAX */
      dfloc = max(dfloc,h[k] * abs(slope[k]));
    }
    dfmx = mflag * dfloc;
    indx = i - k + 1;
/*    INDX = 1 IF K=I, 2 IF K=I-1. */
/*    --------------------------------------------------------------- */
    ierr = dpchsw(dfmx, indx, d[k-1], d[k],
        h[k-1], slope[k-1]);
/*    --------------------------------------------------------------- */
    if (ierr != 0) {
      return ierr;
    }
  } /* ....... END OF SEGMENT LOOP. */
  return ierr;
}

/* ***PURPOSE  Computes divided differences for DPCHCE and DPCHSP */
/*      DPCHDF:   DPCHIP Finite Difference Formula */
/*   Uses a divided difference formulation to compute a K-point approx- */
/*   imation to the derivative at X(K) based on the data in X and S. */
/*   Called by  DPCHCE  and  DPCHSP  to compute 3- and 4-point boundary */
/*   derivative approximations. */
/* ---------------------------------------------------------------------- */
/*   On input: */
/*    K    is the order of the desired derivative approximation. */
/*         K must be at least 3 (error return if not). */
/*    X    contains the K values of the independent variable. */
/*         X need not be ordered, but the values **MUST** be */
/*         distinct.  (Not checked here.) */
/*    S    contains the associated slope values: */
/*          S(I) = (F(I+1)-F(I))/(X(I+1)-X(I)), I=1(1)K-1. */
/*         (Note that S need only be of length K-1.) */
/*   On return: */
/*    S    will be destroyed. */
/*    IERR   will be set to -1 if K.LT.2 . */
/*    DPCHDF  will be set to the desired derivative approximation if */
/*         IERR=0 or to zero if IERR=-1. */
/* ---------------------------------------------------------------------- */
/* ***SEE ALSO  DPCHCE, DPCHSP */
/* ***REFERENCES  Carl de Boor, A Practical Guide to Splines, Springer- */
/*         Verlag, New York, 1978, pp. 10-16. */
/*  CHECK FOR LEGAL VALUE OF K. */
doublereal dpchdf(integer k, doublereal *x, doublereal *s, integer *ierr)
{
/* Local variables */
  integer i, j;
  doublereal value;
  if (k < 3) {
    *ierr = -1;
    xermsg_("SLATEC", "DPCHDF", "K LESS THAN THREE", *ierr);
    return 0.;
  }
/*  COMPUTE COEFFICIENTS OF INTERPOLATING POLYNOMIAL. */
  for (j = 2; j < k; ++j) {
    integer itmp = k - j;
    for (i = 0; i < itmp; ++i) {
      s[i] = (s[i+1] - s[i]) / (x[i + j] - x[i]);
    }
  }
/*  EVALUATE DERIVATIVE AT X(K). */
  value = s[0];
  for (i = 2; i < k; ++i) {
    value = s[i-1] + value * (x[k-1] - x[i-1]);
  }
  *ierr = 0;
  return value;
}

void dpchic(integer *ic, doublereal *vc, doublereal mflag,
    integer n, doublereal *x, doublereal *f, doublereal *d,
    doublereal *wk, integer nwk, integer *ierr)
{
/* Local variables */
  integer i, ibeg, iend, nless1;
/*  VALIDITY-CHECK ARGUMENTS. */
  if (n < 2) {
    *ierr = -1;
    xermsg_("SLATEC", "DPCHIC", "NUMBER OF DATA POINTS LESS THAN TWO", *ierr);
    return;
  }
  for (i = 2; i <= n; ++i) {
    if (x[i-1] <= x[i - 2]) {
      *ierr = -3;
      xermsg_("SLATEC", "DPCHIC", "X-ARRAY NOT STRICTLY INCREASING", *ierr);
      return;
    }
  }
  ibeg = ic[0];
  iend = ic[1];
  *ierr = 0;
  if (abs(ibeg) > 5) {
    --(*ierr);
  }
  if (abs(iend) > 5) {
    *ierr += -2;
  }
  if (*ierr < 0) {
    *ierr += -3;
    xermsg_("SLATEC", "DPCHIC", "IC OUT OF RANGE", *ierr);
    return;
  }
/*  FUNCTION DEFINITION IS OK -- GO ON. */
  nless1 = n - 1;
  if (nwk < nless1 << 1) {
    *ierr = -7;
    xermsg_("SLATEC", "DPCHIC", "WORK ARRAY TOO SMALL", *ierr);
    return;
  }
/*  SET UP H AND SLOPE ARRAYS. */
  for (i = 1; i <= nless1; ++i) {
    wk[i-1] = x[i] - x[i-1];
    wk[nless1 + i - 1] = (f[i] - f[i-1]) /
         wk[i-1];
  }
/*  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION. */
  if (nless1 <= 1) {
    d[n-1] = d[0] = wk[1];
  } else {
/*  NORMAL CASE  (N .GE. 3) . */
/*  SET INTERIOR DERIVATIVES AND DEFAULT END CONDITIONS. */
    dpchci(n, &wk[0], &wk[n-1], &d[0]);
/*  SET DERIVATIVES AT POINTS WHERE MONOTONICITY SWITCHES DIRECTION. */
    if (mflag != 0.) {
      *ierr = dpchcs(mflag, n, &wk[0], &wk[n-1], &d[0]);
      if (*ierr != 0) {
        *ierr = -8;
        xermsg_("SLATEC", "DPCHIC", "ERROR RETURN FROM DPCHCS", *ierr);
        return;
      }
    }
  }
/*  SET END CONDITIONS. */
  if (ibeg == 0 && iend == 0) {
    return;
  }
/*   ------------------------------------------------------- */
  *ierr = dpchce(&ic[0], &vc[0], n, &x[0], &wk[0], &wk[n-1], &d[0]);
/*   ------------------------------------------------------- */
  if (*ierr < 0) {
    *ierr = -9;
    xermsg_("SLATEC", "DPCHIC", "ERROR RETURN FROM DPCHCE", *ierr);
    return;
  }
}

/* ***PURPOSE  DPCHIP Sign-Testing Routine */
/*   Returns: */
/*    -1. if ARG1 and ARG2 are of opposite sign. */
/*     0. if either argument is zero. */
/*    +1. if ARG1 and ARG2 are of the same sign. */
/*   The object is to do this without multiplying ARG1*ARG2, to avoid */
/*   possible over/underflow problems. */
/*  Fortran intrinsics used:  SIGN. */
/* ***SEE ALSO  DPCHCE, DPCHCI, DPCHCS, DPCHIM */
doublereal dpchst(doublereal arg1, doublereal arg2)
{
  return (arg1 == 0. || arg2 == 0.) ? 0. : d_sign(1, arg1) * d_sign(1, arg2);
}

/* ***PURPOSE  Limits excursion from data for DPCHCS */
/*     DPCHSW:  DPCHCS Switch Excursion Limiter. */
/*   Called by  DPCHCS  to adjust D1 and D2 if necessary to insure that */
/*   the extremum on this interval is not further than DFMAX from the */
/*   extreme data value. */
/* ---------------------------------------------------------------------- */
/*  Calling sequence: */
/*    INTEGER  IEXTRM, IERR */
/*    DOUBLE PRECISION  DFMAX, D1, D2, H, SLOPE */
/*    CALL  DPCHSW (DFMAX, IEXTRM, D1, D2, H, SLOPE, IERR) */
/*   Parameters: */
/*   DFMAX -- (input) maximum allowed difference between F(IEXTRM) and */
/*       the cubic determined by derivative values D1,D2.  (assumes */
/*       DFMAX.GT.0.) */
/*   IEXTRM -- (input) index of the extreme data value.  (assumes */
/*       IEXTRM = 1 or 2 .  Any value .NE.1 is treated as 2.) */
/*   D1,D2 -- (input) derivative values at the ends of the interval. */
/*       (Assumes D1*D2 .LE. 0.) */
/*      (output) may be modified if necessary to meet the restriction */
/*       imposed by DFMAX. */
/*   H -- (input) interval length.  (Assumes  H.GT.0.) */
/*   SLOPE -- (input) data slope on the interval. */
/*   IERR -- (output) error flag.  should be zero. */
/*       If IERR=-1, assumption on D1 and D2 is not satisfied. */
/*       If IERR=-2, quadratic equation locating extremum has */
/*             negative discriminant (should never occur). */
/*  ------- */
/*  WARNING:  This routine does no validity-checking of arguments. */
/*  ------- */
/*  Fortran intrinsics used:  ABS, SIGN, SQRT. */
/* ***SEE ALSO  DPCHCS */
/*  NOTATION AND GENERAL REMARKS. */
/*   RHO IS THE RATIO OF THE DATA SLOPE TO THE DERIVATIVE BEING TESTED. */
/*   LAMBDA IS THE RATIO OF D2 TO D1. */
/*   THAT = T-HAT(RHO) IS THE NORMALIZED LOCATION OF THE EXTREMUM. */
/*   PHI IS THE NORMALIZED VALUE OF P(X)-F1 AT X = XHAT = X-HAT(RHO), */
/*       WHERE  THAT = (XHAT - X1)/H . */
/*    THAT IS, P(XHAT)-F1 = D*H*PHI,  WHERE D=D1 OR D2. */
/*   SIMILARLY,  P(XHAT)-F2 = D*H*(PHI-RHO) . */
integer dpchsw(doublereal dfmax, integer iextrm, doublereal d1,
    doublereal d2, doublereal h, doublereal slope)
{
/* Local variables */
  doublereal cp, nu, phi, rho, hphi, that, sigma, small;
  doublereal lambda, radcal;
/* Initialized data */
  static const doublereal fact = 100.;
/*    THIRD SHOULD BE SLIGHTLY LESS THAN 1/3. */
  static const doublereal third = .33333;
/*    SMALL SHOULD BE A FEW ORDERS OF MAGNITUDE GREATER THAN MACHEPS. */
  small = fact * d1mach();
/*  DO MAIN CALCULATION. */
  if (d1 == 0.) {
/*    SPECIAL CASE -- D1.EQ.ZERO . */
/*      IF D2 IS ALSO ZERO, THIS ROUTINE SHOULD NOT HAVE BEEN CALLED. */
    if (d2 == 0.) {
      xermsg_("SLATEC", "DPCHSW", "D1 AND/OR D2 INVALID", (long)-1);
      return -1;
    }
    rho = slope / d2;
/*      EXTREMUM IS OUTSIDE INTERVAL WHEN RHO .GE. 1/3 . */
    if (rho >= third) {
      return 0;
    }
    that = 2. * (3. * rho - 1.) / (3. * (2. * rho - 1.));
/* Computing 2nd power */
    phi = that * that * ((3. * rho - 1.) / 3.);
/*      CONVERT TO DISTANCE FROM F2 IF IEXTRM.NE.1 . */
    if (iextrm != 1) {
      phi -= rho;
    }
/*      TEST FOR EXCEEDING LIMIT, AND ADJUST ACCORDINGLY. */
    hphi = h * abs(phi);
    if (hphi * abs(d2) > dfmax) {
/*       AT THIS POINT, HPHI.GT.0, SO DIVIDE IS OK. */
      d2 = d_sign(dfmax / hphi, d2);
    }
  } else {
    rho = slope / d1;
    lambda = -(d2) / d1;
    if (d2 == 0.) {
/*       SPECIAL CASE -- D2.EQ.ZERO . */
/*       EXTREMUM IS OUTSIDE INTERVAL WHEN RHO .GE. 1/3 . */
      if (rho >= third) {
        return 0;
      }
      cp = 2. - 3. * rho;
      nu = 1. - 2. * rho;
      that = 1. / (3. * nu);
    } else {
      if (lambda <= 0.) {
        xermsg_("SLATEC", "DPCHSW", "D1 AND/OR D2 INVALID", (long)-1);
        return -1;
      }
/*       NORMAL CASE -- D1 AND D2 BOTH NONZERO, OPPOSITE SIGNS. */
      nu = 1. - lambda - 2. * rho;
      sigma = 1. - rho;
      cp = nu + sigma;
      if (abs(nu) > small) {
/* Computing 2nd power */
        radcal = (nu - (2. * rho + 1.)) * nu + sigma * sigma;
        if (radcal < 0.) {
          xermsg_("SLATEC", "DPCHSW", "NEGATIVE RADICAL", (long)-2);
          return -2;
        }
        that = (cp - sqrt(radcal)) / (3. * nu);
      } else {
        that = 1. / (2. * sigma);
      }
    }
    phi = that * ((nu * that - cp) * that + 1.);
/*      CONVERT TO DISTANCE FROM F2 IF IEXTRM.NE.1 . */
    if (iextrm != 1) {
      phi -= rho;
    }
/*      TEST FOR EXCEEDING LIMIT, AND ADJUST ACCORDINGLY. */
    hphi = h * abs(phi);
    if (hphi * abs(d1) > dfmax) {
/*       AT THIS POINT, HPHI.GT.0, SO DIVIDE IS OK. */
      d1 = d_sign(dfmax / hphi, d1);
      d2 = -lambda * d1;
    }
  }
  return 0;
}

/*  Programming notes: */
/*   1. The function  DPCHST(ARG1,ARG2)  is assumed to return zero if */
/*    either argument is zero, +1 if they are of the same sign, and */
/*    -1 if they are of opposite sign. */
/*   2. To produce a single precision version, simply: */
/*    a. Change DPCHIM to PCHIM wherever it occurs, */
/*    b. Change DPCHST to PCHST wherever it occurs, */
/*    c. Change all references to the Fortran intrinsics to their */
/*       single precision equivalents, */
/*    d. Change the double precision declarations to real, and */
/*    e. Change the constants ZERO and THREE to single precision. */
void dpchim(integer n, doublereal *x, doublereal *f,
    doublereal *d, integer *ierr)
{
/* System generated locals */
  doublereal dtmp;
/* Local variables */
  integer i;
  doublereal h1, h2, w1, w2, del1, del2, dmin, dmax, hsum, drat1,
       drat2, dsave;
  integer nless1;
  doublereal hsumt3;
/*  VALIDITY-CHECK ARGUMENTS. */
  if (n < 2) {
    *ierr = -1;
    xermsg_("SLATEC", "DPCHIM", "NUMBER OF DATA POINTS LESS THAN TWO", *ierr);
    return;
  }
  for (i = 2; i <= n; ++i) {
    if (x[i-1] <= x[i - 2]) {
      *ierr = -3;
      xermsg_("SLATEC", "DPCHIM", "X-ARRAY NOT STRICTLY INCREASING", *ierr);
      return;
    }
  }
/*  FUNCTION DEFINITION IS OK, GO ON. */
  *ierr = 0;
  nless1 = n - 1;
  h1 = x[1] - x[0];
  del1 = (f[1] - f[0]) / h1;
  dsave = del1;
/*  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION. */
  if (nless1 <= 1) {
    d[0] = d[n-1] = del1;
    return;
  }
/*  NORMAL CASE  (N .GE. 3). */
  h2 = x[2] - x[1];
  del2 = (f[2] - f[1]) / h2;
/*  SET D(1) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE */
/*   SHAPE-PRESERVING. */
  hsum = h1 + h2;
  w1 = (h1 + hsum) / hsum;
  w2 = -h1 / hsum;
  d[0] = w1 * del1 + w2 * del2;
  if (dpchst(d[0], del1) <= 0.) {
    d[0] = 0.;
  } else if (dpchst(del1, del2) < 0.) {
/*    NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES. */
    dmax = 3. * del1;
    if (abs(d[0]) > abs(dmax)) {
      d[0] = dmax;
    }
  }
/*  LOOP THROUGH INTERIOR POINTS. */
  for (i = 2; i <= nless1; ++i) {
    if (i != 2) {
      h1 = h2;
      h2 = x[i] - x[i-1];
      hsum = h1 + h2;
      del1 = del2;
      del2 = (f[i] - f[i-1]) / h2;
    }
/*    SET D(I)=0 UNLESS DATA ARE STRICTLY MONOTONIC. */
    d[i-1] = 0.;
    dtmp = dpchst(del1, del2);
    if (dtmp <= 0) {
      if (dtmp == 0.) {
/*    COUNT NUMBER OF CHANGES IN DIRECTION OF MONOTONICITY. */
        if (del2 == 0.) {
          continue;
        }
        if (dpchst(dsave, del2) < 0.) {
          ++(*ierr);
        }
        dsave = del2;
        continue;
      }
      ++(*ierr);
      dsave = del2;
      continue;
    }
/*    USE BRODLIE MODIFICATION OF BUTLAND FORMULA. */
    hsumt3 = hsum + hsum + hsum;
    w1 = (hsum + h1) / hsumt3;
    w2 = (hsum + h2) / hsumt3;
/* Computing MAX */
    dmax = max(abs(del1),abs(del2));
/* Computing MIN */
    dmin = min(abs(del1),abs(del2));
    drat1 = del1 / dmax;
    drat2 = del2 / dmax;
    d[i-1] = dmin / (w1 * drat1 + w2 * drat2);
  }
/*  SET D(N) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE */
/*   SHAPE-PRESERVING. */
  w1 = -h2 / hsum;
  w2 = (h2 + hsum) / hsum;
  d[n-1] = w1 * del1 + w2 * del2;
  if (dpchst(d[n-1], del2) <= 0.) {
    d[n-1] = 0.;
  } else if (dpchst(del1, del2) < 0.) {
/*    NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES. */
    dmax = 3. * del2;
    if (abs(d[n-1]) > abs(dmax)) {
      d[n-1] = dmax;
    }
  }
}

/*   SINGULAR SYSTEM. */
/*   *** THEORETICALLY, THIS CAN ONLY OCCUR IF SUCCESSIVE X-VALUES   *** */
/*   *** ARE EQUAL, WHICH SHOULD ALREADY HAVE BEEN CAUGHT (IERR=-3). *** */
#define dpchsp_singular \
  *ierr = -8; \
  xermsg_("SLATEC", "DPCHSP", "SINGULAR LINEAR SYSTEM", *ierr); \
  return;
void dpchsp(integer *ic, doublereal *vc, integer n,
    doublereal *x, doublereal *f, doublereal *d,
    doublereal *wk, integer nwk, integer *ierr)
{
/* System generated locals */
  doublereal dtmp;
/* Local variables */
  doublereal g;
  integer j, nm1, ibeg, iend, index;
  doublereal stemp[3], xtemp[4];
  wk -= 3;
/*  VALIDITY-CHECK ARGUMENTS. */
  if (n < 2) {
    *ierr = -1;
    xermsg_("SLATEC", "DPCHSP", "NUMBER OF DATA POINTS LESS THAN TWO", *ierr);
    return;
  }
  for (j = 2; j <= n; ++j) {
    if (x[j-1] <= x[j - 2]) {
      *ierr = -3;
      xermsg_("SLATEC", "DPCHSP", "X-ARRAY NOT STRICTLY INCREASING", *ierr);
      return;
    }
  }
  ibeg = ic[0];
  iend = ic[1];
  *ierr = 0;
  if (ibeg < 0 || ibeg > 4) {
    --(*ierr);
  }
  if (iend < 0 || iend > 4) {
    *ierr += -2;
  }
  if (*ierr < 0) {
    *ierr += -3;
    xermsg_("SLATEC", "DPCHSP", "IC OUT OF RANGE", *ierr);
    return;
  }
/*  FUNCTION DEFINITION IS OK -- GO ON. */
  if (nwk < n << 1) {
    *ierr = -7;
    xermsg_("SLATEC", "DPCHSP", "WORK ARRAY TOO SMALL", *ierr);
    return;
  }
/*  COMPUTE FIRST DIFFERENCES OF X SEQUENCE AND STORE IN WK(1,.). ALSO, */
/*  COMPUTE FIRST DIVIDED DIFFERENCE OF DATA AND STORE IN WK(2,.). */
  for (j = 2; j <= n; ++j) {
    wk[(j << 1) + 1] = x[j-1] - x[j - 2];
    wk[(j << 1) + 2] = (f[j-1] - f[j - 2]) /
        wk[(j << 1) + 1];
  }
/*  SET TO DEFAULT BOUNDARY CONDITIONS IF N IS TOO SMALL. */
  if (ibeg > n) {
    ibeg = 0;
  }
  if (iend > n) {
    iend = 0;
  }
/*  SET UP FOR BOUNDARY CONDITIONS. */
  if (ibeg == 1 || ibeg == 2) {
    d[0] = vc[0];
  } else if (ibeg > 2) {
/*    PICK UP FIRST IBEG POINTS, IN REVERSE ORDER. */
    for (j = 1; j <= ibeg; ++j) {
      index = ibeg - j + 1;
/*       INDEX RUNS FROM IBEG DOWN TO 1. */
      xtemp[j - 1] = x[index-1];
      if (j < ibeg) {
        stemp[j - 1] = wk[(index << 1) + 2];
      }
    }
/*         -------------------------------- */
    d[0] = dpchdf(ibeg, xtemp, stemp, ierr);
/*         -------------------------------- */
    if (*ierr != 0) {
      *ierr = -9;
      xermsg_("SLATEC", "DPCHSP", "ERROR RETURN FROM DPCHDF", *ierr);
      return;
    }
    ibeg = 1;
  }
  if (iend == 1 || iend == 2) {
    d[n-1] = vc[1];
  } else if (iend > 2) {
/*    PICK UP LAST IEND POINTS. */
    for (j = 1; j <= iend; ++j) {
      index = n - iend + j;
/*       INDEX RUNS FROM N+1-IEND UP TO N. */
      xtemp[j - 1] = x[index-1];
      if (j < iend) {
        stemp[j - 1] = wk[((index + 1) << 1) + 2];
      }
    }
/*         -------------------------------- */
    d[n-1] = dpchdf(iend, xtemp, stemp, ierr);
/*         -------------------------------- */
    if (*ierr != 0) {
      *ierr = -9;
      xermsg_("SLATEC", "DPCHSP", "ERROR RETURN FROM DPCHDF", *ierr);
      return;
    }
    iend = 1;
  }
/* --------------------( BEGIN CODING FROM CUBSPL )-------------------- */
/*  **** A TRIDIAGONAL LINEAR SYSTEM FOR THE UNKNOWN SLOPES S(J) OF */
/*  F  AT X(J), J=1,...,N, IS GENERATED AND THEN SOLVED BY GAUSS ELIM- */
/*  INATION, WITH S(J) ENDING UP IN D(1,J), ALL J. */
/*   WK(1,.) AND WK(2,.) ARE USED FOR TEMPORARY STORAGE. */
/*  CONSTRUCT FIRST EQUATION FROM FIRST BOUNDARY CONDITION, OF THE FORM */
/*       WK(2,1)*S(1) + WK(1,1)*S(2) = D(1,1) */
  if (ibeg == 0) {
    if (n == 2) {
/*       NO CONDITION AT LEFT END AND N = 2. */
      wk[4] = 1.;
      wk[3] = 1.;
      d[0] = 2. * wk[6];
    } else {
/*       NOT-A-KNOT CONDITION AT LEFT END AND N .GT. 2. */
      wk[4] = wk[7];
      wk[3] = wk[5] + wk[7];
/* Computing 2nd power */
      d[0] = ((wk[5] + 2. * wk[3]) * wk[6] * wk[7] + wk[5] *
          wk[5] * wk[8]) / wk[3];
    }
  } else if (ibeg == 1) {
/*    SLOPE PRESCRIBED AT LEFT END. */
    wk[4] = 1.;
    wk[3] = 0.;
  } else {
/*    SECOND DERIVATIVE PRESCRIBED AT LEFT END. */
    wk[4] = 2.;
    wk[3] = 1.;
    d[0] = 3. * wk[6] - 0.5 * wk[5] * d[0];
  }
/*  IF THERE ARE INTERIOR KNOTS, GENERATE THE CORRESPONDING EQUATIONS AND */
/*  CARRY OUT THE FORWARD PASS OF GAUSS ELIMINATION, AFTER WHICH THE J-TH */
/*  EQUATION READS  WK(2,J)*S(J) + WK(1,J)*S(J+1) = D(1,J). */
  nm1 = n - 1;
  if (nm1 > 1) {
    for (j = 2; j <= nm1; ++j) {
      if (wk[((j - 1) << 1) + 2] == 0.) {
        dpchsp_singular;
      }
      g = -wk[((j + 1) << 1) + 1] / wk[((j - 1) << 1) + 2];
      d[j-1] = g * d[j - 2] + 3. *
          (wk[(j << 1) + 1] * wk[((j + 1) << 1) + 2] +
          wk[((j + 1) << 1) + 1] * wk[(j << 1) + 2]);
      wk[(j << 1) + 2] = g * wk[((j - 1) << 1) + 1] + 2. *
          (wk[(j << 1) + 1] + wk[((j + 1) << 1) + 1]);
    }
  }
/*  CONSTRUCT LAST EQUATION FROM SECOND BOUNDARY CONDITION, OF THE FORM */
/*       (-G*WK(2,N-1))*S(N-1) + WK(2,N)*S(N) = D(1,N) */
/*   IF SLOPE IS PRESCRIBED AT RIGHT END, ONE CAN GO DIRECTLY TO BACK- */
/*   SUBSTITUTION, SINCE ARRAYS HAPPEN TO BE SET UP JUST RIGHT FOR IT */
/*   AT THIS POINT. */
  if (iend != 1) {
    if (iend == 0 && n == 2 && ibeg == 0) {
/*       NOT-A-KNOT AT RIGHT ENDPOINT AND AT LEFT ENDPOINT AND N = 2. */
      d[1] = wk[6];
    } else {
      if (iend == 0) {
        if (n == 2 || n == 3 && ibeg == 0) {
/*       EITHER (N=3 AND NOT-A-KNOT ALSO AT LEFT) OR (N=2 AND *NOT* */
/*       NOT-A-KNOT AT LEFT END POINT). */
          d[n-1] = 2. * wk[(n << 1) + 2];
          wk[(n << 1) + 2] = 1.;
          if (wk[((n - 1) << 1) + 2] == 0.) {
            dpchsp_singular;
          }
          g = -1. / wk[((n - 1) << 1) + 2];
        } else {
/*       NOT-A-KNOT AND N .GE. 3, AND EITHER N.GT.3 OR  ALSO NOT-A- */
/*       KNOT AT LEFT END POINT. */
          g = wk[((n - 1) << 1) + 1] + wk[(n << 1) + 1];
/*       DO NOT NEED TO CHECK FOLLOWING DENOMINATORS (X-DIFFERENCES). */
/* Computing 2nd power */
          dtmp = wk[(n << 1) + 1];
          d[n-1] = ((wk[(n << 1) + 1] + 2. * g) * wk[(n << 1) + 2] * wk[((n - 1) << 1) + 1] + dtmp * dtmp * (f[n - 2] - f[n - 3]) / wk[((n - 1) << 1) + 1]) / g;
          if (wk[((n - 1) << 1) + 2] == 0.) {
            dpchsp_singular;
          }
          g = -g / wk[((n - 1) << 1) + 2];
          wk[(n << 1) + 2] = wk[((n - 1) << 1) + 1];
        }
      } else {
/*    SECOND DERIVATIVE PRESCRIBED AT RIGHT ENDPOINT. */
        d[n-1] = 3. * wk[(n << 1) + 2] + 0.5 * wk[(n << 1) + 1] * d[n-1];
        wk[(n << 1) + 2] = 2.;
        if (wk[((n - 1) << 1) + 2] == 0.) {
          dpchsp_singular;
        }
        g = -1. / wk[((n - 1) << 1) + 2];
      }
/*  COMPLETE FORWARD PASS OF GAUSS ELIMINATION. */
      wk[(n << 1) + 2] = g * wk[((n - 1) << 1) + 1] + wk[(n << 1) + 2];
      if (wk[(n << 1) + 2] == 0.) {
        dpchsp_singular;
      }
      d[n-1] = (g * d[n - 2] + d[n-1])
           / wk[(n << 1) + 2];
    }
  }
/*  CARRY OUT BACK SUBSTITUTION */
  for (j = nm1; j >= 1; --j) {
    if (wk[(j << 1) + 2] == 0.) {
      dpchsp_singular;
    }
    d[j-1] = (d[j-1] - wk[(j << 1) + 1] * d[j]) / wk[(j << 1) + 2];
  }
/* --------------------(  END  CODING FROM CUBSPL )-------------------- */
}
