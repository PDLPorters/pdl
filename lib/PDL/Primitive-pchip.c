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

/* ***PURPOSE  DPCHIP Sign-Testing Routine */
/*   Returns: */
/*    -1. if ARG1 and ARG2 are of opposite sign. */
/*     0. if either argument is zero. */
/*    +1. if ARG1 and ARG2 are of the same sign. */
/*   The object is to do this without multiplying ARG1*ARG2, to avoid */
/*   possible over/underflow problems. */
/*  Fortran intrinsics used:  SIGN. */
/* ***SEE ALSO  DPCHCE, DPCHCI, DPCHCS, DPCHIM */
#define X(ctype, ppsym) \
  static inline ctype pchst_ ## ppsym(ctype arg1, ctype arg2) \
  { \
    return (arg1 == 0. || arg2 == 0.) ? 0. : d_sign(1, arg1) * d_sign(1, arg2); \
  }
X(doublereal, D)
#undef X

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
    do { /* inline dpchkt */
/* Local variables */
      integer j, k;
      doublereal hbeg, hend;
      integer ndim = n << 1;
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
    } while (0); /* end inline dpchkt */
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

/*  Programming notes: */
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
  integer i, j, nj, ir, ierc, next[2], jfirst, located;
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
    if (nj == 0) {
      ++ir;
      if (ir < n) {
        continue;
      }
      return;
    }
/*   EVALUATE CUBIC AT XE(I),  I = JFIRST (1) J-1 . */
/*     ---------------------------------------------------------------- */
    do { /* inline dchfdv */
/* Local variables */
      doublereal x1 = x[ir - 1], x2 = x[ir];
      doublereal f1 = f[ir - 1], f2 = f[ir];
      doublereal d1 = d[ir - 1], d2 = d[ir];
      integer ne = nj;
      doublereal *xe2 = &xe[jfirst], *fe2 = &fe[jfirst];
      doublereal *de2 = de ? &de[jfirst] : NULL;
      doublereal h;
      integer i;
      doublereal x, c2, c3, c2t2, c3t3, xma, xmi, del1, del2, delta;
      if (ne < 1) {
        ierc = -1;
        xermsg_("SLATEC", "DCHFDV", "NUMBER OF EVALUATION POINTS LESS THAN ONE", ierc);
        break;
      }
      h = x2 - x1;
      if (h == 0.) {
        ierc = -2;
        xermsg_("SLATEC", "DCHFDV", "INTERVAL ENDPOINTS EQUAL", ierc);
        break;
      }
/*  INITIALIZE. */
      ierc = 0;
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
        x = xe2[i] - x1;
        fe2[i] = f1 + x * (d1 + x * (c2 + x * c3));
        if (de2) de2[i] = d1 + x * (c2t2 + x * c3t3);
/*      COUNT EXTRAPOLATION POINTS. */
        if (x < xmi) {
          ++next[0];
        }
        if (x > xma) {
          ++next[1];
        }
/*    (NOTE REDUNDANCY--IF EITHER CONDITION IS TRUE, OTHER IS FALSE.) */
      }
    } while (0); /* end inline dchfdv */
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
    ++ir;
    if (ir >= n) break;
  }
}

void dpchfe(integer n, doublereal *x, doublereal *f,
    doublereal *d, logical *skip, integer ne,
    doublereal *xe, doublereal *fe, integer *ierr)
{
  dpchfd(n, x, f, d, skip, ne, xe, fe, NULL, ierr);
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
#define X(ctype, ppsym) \
  static inline ctype chfie_ ## ppsym(ctype x1, ctype x2, ctype f1, ctype f2, \
      ctype d1, ctype d2, ctype a, ctype b) \
  { \
/* Local variables */ \
    ctype h, ta1, ta2, tb1, tb2, ua1, ua2, ub1, ub2, phia1, \
        phia2, phib1, phib2, psia1, psia2, psib1, psib2, dterm, fterm; \
    if (x1 == x2) { \
      return 0.; \
    } \
    h = x2 - x1; \
    ta1 = (a - x1) / h; \
    ta2 = (x2 - a) / h; \
    tb1 = (b - x1) / h; \
    tb2 = (x2 - b) / h; \
/* Computing 3rd power */ \
    ua1 = ta1 * (ta1 * ta1); \
    phia1 = ua1 * (2. - ta1); \
    psia1 = ua1 * (3. * ta1 - 4.); \
/* Computing 3rd power */ \
    ua2 = ta2 * (ta2 * ta2); \
    phia2 = ua2 * (2. - ta2); \
    psia2 = -ua2 * (3. * ta2 - 4.); \
/* Computing 3rd power */ \
    ub1 = tb1 * (tb1 * tb1); \
    phib1 = ub1 * (2. - tb1); \
    psib1 = ub1 * (3. * tb1 - 4.); \
/* Computing 3rd power */ \
    ub2 = tb2 * (tb2 * tb2); \
    phib2 = ub2 * (2. - tb2); \
    psib2 = -ub2 * (3. * tb2 - 4.); \
    fterm = f1 * (phia2 - phib2) + f2 * (phib1 - phia1); \
    dterm = (d1 * (psia2 - psib2) + d2 * (psib1 - psia1)) * (h / 6.); \
    return 0.5 * h * (fterm + dterm); \
  }
X(doublereal, D)
#undef X

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
    for (i = 1; i < n; ++i) {
      if (x[i] <= x[i - 1]) {
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
    for (i = 1; i < n; ++i) {
      if (x[i] <= x[i - 1]) {
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
      value = chfie_D(x[0], x[1], f[0], f[1],
          d[0], d[1], a, b);
/*           --------------------------------------- */
    } else if (xa >= x[n - 2]) {
/*       INTERVAL IS TO RIGHT OF X(N-1), SO USE LAST CUBIC. */
/*           ------------------------------------------ */
      value = chfie_D(x[n - 2], x[n-1], f[n - 2], f[n-1], d[n-2], d[n-1], a, b);
/*           ------------------------------------------ */
    } else {
/*       'NORMAL' CASE -- XA.LT.XB, XA.LT.X(N-1), XB.GT.X(2). */
/*    ......LOCATE IA AND IB SUCH THAT */
/*         X(IA-1).LT.XA.LE.X(IA).LE.X(IB).LE.XB.LE.X(IB+1) */
      ia = 0;
      for (i = 0; i < n-1; ++i) {
        if (xa > x[i]) {
          ia = i + 1;
        }
      }
/*       IA = 1 IMPLIES XA.LT.X(1) .  OTHERWISE, */
/*       IA IS LARGEST INDEX SUCH THAT X(IA-1).LT.XA,. */
      ib = n - 1;
      for (i = n-1; i >= ia; --i) {
        if (xb < x[i]) {
          ib = i - 1;
        }
      }
/*       IB = N IMPLIES XB.GT.X(N) .  OTHERWISE, */
/*       IB IS SMALLEST INDEX SUCH THAT XB.LT.X(IB+1) . */
/*   ......COMPUTE THE INTEGRAL. */
      if (ib <= ia) {
/*        THIS MEANS IB = IA-1 AND */
/*         (A,B) IS A SUBSET OF (X(IB),X(IA)). */
/*            ------------------------------------------- */
        value = chfie_D(x[ib], x[ia], f[ib],
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
          value += chfie_D(x[il], x[ir], f[il], f[ir], d[il], d[ir], xa, x[ia]);
/*                 ------------------------------------- */
        }
/*        THEN ADD ON INTEGRAL OVER (X(IB),XB). */
        if (xb > x[ib]) {
/* Computing MIN */
          ir = min(ib + 1,n-1);
          il = ir - 1;
/*                 ------------------------------------- */
          value += chfie_D(x[il], x[ir], f[il], f[ir], d[il], d[ir], x[ib], xb);
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
#define X(ctype, ppsym) \
  static inline ctype pchdf_ ## ppsym(integer k, ctype *x, ctype *s, integer *ierr) \
  { \
/* Local variables */ \
    integer i, j; \
    ctype value; \
    if (k < 3) { \
      *ierr = -1; \
      xermsg_("SLATEC", "DPCHDF", "K LESS THAN THREE", *ierr); \
      return 0.; \
    } \
/*  COMPUTE COEFFICIENTS OF INTERPOLATING POLYNOMIAL. */ \
    for (j = 2; j < k; ++j) { \
      integer itmp = k - j; \
      for (i = 0; i < itmp; ++i) { \
        s[i] = (s[i+1] - s[i]) / (x[i + j] - x[i]); \
      } \
    } \
/*  EVALUATE DERIVATIVE AT X(K). */ \
    value = s[0]; \
    for (i = 1; i < k-1; ++i) { \
      value = s[i] + value * (x[k-1] - x[i]); \
    } \
    *ierr = 0; \
    return value; \
  }
X(doublereal, D)
#undef X

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
  for (i = 1; i < n; ++i) {
    if (x[i] <= x[i - 1]) {
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
  if (pchst_D(d[0], del1) <= 0.) {
    d[0] = 0.;
  } else if (pchst_D(del1, del2) < 0.) {
/*    NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES. */
    dmax = 3. * del1;
    if (abs(d[0]) > abs(dmax)) {
      d[0] = dmax;
    }
  }
/*  LOOP THROUGH INTERIOR POINTS. */
  for (i = 1; i < nless1; ++i) {
    if (i != 1) {
      h1 = h2;
      h2 = x[i+1] - x[i];
      hsum = h1 + h2;
      del1 = del2;
      del2 = (f[i+1] - f[i]) / h2;
    }
/*    SET D(I)=0 UNLESS DATA ARE STRICTLY MONOTONIC. */
    d[i] = 0.;
    dtmp = pchst_D(del1, del2);
    if (dtmp <= 0) {
      if (dtmp == 0.) {
/*    COUNT NUMBER OF CHANGES IN DIRECTION OF MONOTONICITY. */
        if (del2 == 0.) {
          continue;
        }
        if (pchst_D(dsave, del2) < 0.) {
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
    d[i] = dmin / (w1 * drat1 + w2 * drat2);
  }
/*  SET D(N) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE */
/*   SHAPE-PRESERVING. */
  w1 = -h2 / hsum;
  w2 = (h2 + hsum) / hsum;
  d[n-1] = w1 * del1 + w2 * del2;
  if (pchst_D(d[n-1], del2) <= 0.) {
    d[n-1] = 0.;
  } else if (pchst_D(del1, del2) < 0.) {
/*    NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES. */
    dmax = 3. * del2;
    if (abs(d[n-1]) > abs(dmax)) {
      d[n-1] = dmax;
    }
  }
}
