#include <stdio.h>
#include <stdint.h>
#include <math.h>

/* f2c.h  --  Standard Fortran to C header file */
typedef long int integer;
typedef double doublereal;
typedef long int logical;

#define TRUE_ (1)

#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
/* end f2c.h */

#define xermsg_(lib, func, errstr, nerr, ...) \
  fprintf(stderr, "%s::%s: %s (err=%ld)\n", lib, func, errstr, nerr)

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
