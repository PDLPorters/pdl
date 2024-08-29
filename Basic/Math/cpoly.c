/* Translated from F77 to C, rjrw 10/04/2000 */
/* replaced 'bool' by 'boolvar' to get it to compile on my
   linux machine, DJB Aug 02 2000 */

/* algorithm 419 collected algorithms from acm.
   algorithm appeared in comm. acm, vol. 15, no. 02, p. 097. */
/* available in 2024 from https://calgo.acm.org/
   see https://en.wikipedia.org/wiki/Jenkins%E2%80%93Traub_algorithm */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <complex.h>
/*
   #if !defined(WIN32) && !defined(_WIN32) && !defined(__APPLE__) && !defined(__CYGWIN__)
   #include <values.h>
   #endif
*/
#include <float.h>
/* #define DEBUGMAIN */   /* Set up debugging main, etc. */
#include "cpoly.h"

/* Internal routines */
static complex double noshft(int l1, int nn, complex double tc, complex double hc[], complex double pc[]);
static int fxshft(int l2, int nn, complex double shc[], complex double qpc[], complex double hc[], complex double pc[], complex double qhc[], complex double *tc, complex double *sc, complex double *pvc, complex double *zc);
static int vrshft(int l3, int nn, complex double qpc[], complex double pc[], complex double qhc[], complex double hc[], complex double *tc, complex double *sc, complex double *pvc, complex double *zc);
static int calct(int nn, complex double sc, complex double pvc, complex double qhc[], complex double hc[], complex double *tc);
static void nexth(int boolvar, int nn, complex double tc, complex double qhc[], complex double qpc[], complex double hc[]);
static complex double polyev(int nn, complex double sc, complex double pc[], complex double qc[]);
static double errev(int nn, double ms, double mp, complex double qc[]);
static double cauchy(int nn, complex double pc[]);
static double scale(int nn, complex double pc[]);
static complex double cdivid(complex double a, complex double b);
static double cmod(complex double a);
static void mcon(void);
static void init(int nncr);

/* Internal global variables */
static double are,mre,eta,infin,smalno,base;

#ifdef DEBUGMAIN
/* driver to test cpoly */
int main()
{
  char *fail = NULL;
  double p[50],pi[50],zr[50],zi[50];

  int i;

  printf("Example 1.  polynomial with zeros 1,2,...,10.\n");
  p[0]=1L;
  p[1]=-55L;
  p[2]=1320L;
  p[3]=-18150L;
  p[4]=157773L;
  p[5]=-902055L;
  p[6] = 3416930L;
  p[7]=-8409500L;
  p[8]=12753576L;
  p[9]=-10628640L;
  p[10]=3628800L;
  for (i=0;i<11;i++)
    pi[i]=0;
  prtc(11,p,pi);
  fail = cpoly(p,pi,10,zr,zi);
  if(fail)
    printf("cpoly has failed on this example\n");
  prtz (10,zr,zi);
  printf("Example 2. zeros on imaginary axis degree 3.\n");
  p[0]=1;
  p[1]=0;
  p[2]=-10001.0001L;
  p[3]=0;
  pi[0]=0;
  pi[1]=-10001.0001L;
  pi[2]=0;
  pi[3]=1;
  prtc(4,p,pi);
  fail = cpoly(p,pi,3,zr,zi);
  if (fail)
    printf("cpoly has failed on this example\n");
  prtz(3,zr,zi);
  printf("Example 3. zeros at 1+i,1/2*(1+i)....1/(2**-9)*(1+i)\n");
  p[0]=1.0;
  p[1]=-1.998046875L;
  p[2]=0.0;
  p[3]=.7567065954208374L;
  p[4]=-.2002119533717632L;
  p[5]=1.271507365163416e-2L;
  p[6]=0;
  p[7]=-1.154642632172909e-5L;
  p[8]=1.584803612786345e-7L;
  p[9]=-4.652065399568528e-10L;
  p[10]=0;
  pi[0]=0;
  pi[1]=p[1];
  pi[2]=2.658859252929688L;
  pi[3]=-7.567065954208374e-1L;
  pi[4]=0;
  pi[5]=p[5];
  pi[6]=-7.820779428584501e-4L;
  pi[7]=-p[7];
  pi[8]=0;
  pi[9]=p[9];
  pi[10]=9.094947017729282e-13L;
  prtc(11,p,pi);
  fail = cpoly(p,pi,10,zr,zi);
  if (fail)
    printf("cpoly has failed on this example\n");
  prtz(10,zr,zi);
  printf("Example 4. multiple zeros\n");
  p[0]=1L;
  p[1]=-10L;
  p[2]=3L;
  p[3]=284L;
  p[4]=-1293L;
  p[5]=2374L;
  p[6]=-1587L;
  p[7]=-920L;
  p[8]=2204L;
  p[9]=-1344L;
  p[10]=288L;
  pi[0]=0;
  pi[1]=-10L;
  pi[2]=100L;
  pi[3]=-334L;
  pi[4]=200L;
  pi[5]=1394L;
  pi[6] =-3836L;
  pi[7]=4334L;
  pi[8]=-2352L;
  pi[9]=504L;
  pi[10]=0;
  prtc(11,p,pi);
  fail = cpoly(p,pi,10,zr,zi);
  if (fail)
    printf("cpoly has failed on this example\n");
  prtz(10,zr,zi);
  printf("Example 5. 12 zeros evenly distributed on a circle of radius 1. centered at 0+2i.\n");
  p[0]=1L;
  p[1]=0;
  p[2]=-264L;
  p[3]=0;
  p[4]=7920L;
  p[5]=0;
  p[6]=-59136L;
  p[7]=0;
  p[8]=126720L;
  p[9]=0;
  p[10]=-67584L;
  p[11]=0;
  p[12]=4095L;
  pi[0]=0;
  pi[1]=-24L;
  pi[2]=0;
  pi[3]=1760L;
  pi[4]=0;
  pi[5]=-25344L;
  pi[6]=0;
  pi[7]=101376L;
  pi[8]=0;
  pi[9]=-112640L;
  pi[10]=0;
  pi[11]=24576L;
  pi[12]=0;
  prtc(13,p,pi);
  fail = cpoly(p,pi,12,zr,zi);
  if(fail)
    printf("cpoly has failed on this example\n");
  prtz(12,zr,zi);
  return 0;
}
void prtc(int n, double p[], double q[])
{
  int i;
  printf("Coefficients\n");
  for (i=0;i<n;i++)
     printf("%26.16g %26.16g\n",p[i],q[i]);
}

void prtz(int n,double zr[], double zi[])
{
  int i;
  printf("Zeroes\n");
  for (i=0;i<n;i++)
    printf("%26.16g %26.16g\n",zr[i],zi[i]);
}
#endif

#define COSR (-0.0697564737441253008L)
#define SINR (0.997564050259824248L)

char *cpoly(double opr[], double opi[], int degree,
	   double zeror[], double zeroi[])
{
  /* Finds the zeros of a complex polynomial.

     opr, opi - double precision vectors of real and imaginary parts
                of the coefficients in order of decreasing powers.
     degree   - integer degree of polynomial
     zeror, zeroi
              - output double precision vectors of real and imaginary
	        parts of the zeros.
     fail     - output logical parameter, TRUE if leading coefficient
                is zero, if cpoly has found fewer than degree zeros,
                or if there is another internal error.

     The program has been written to reduce the chance of overflow
     occurring.  If it does occur, there is still a possibility that
     the zerofinder will work provided the overflowed quantity is
     replaced by a large number. */

  if (degree < 1)
    return "algorithm only works for degree >= 1.";

  if (opr[0] == 0.0 && opi[0] == 0.0)
    return "algorithm fails if the leading coefficient is zero.";

  double xx,yy,xxx,bnd;
  complex double zc,tc,sc,pvc;
  char *failreason = NULL;
  int cnt1,cnt2,i,idnn2;

  /* initialization of constants */
  int nn = degree+1;
  init(nn);
  complex double *pc  = malloc(nn*sizeof(complex double));
  complex double *hc  = malloc(nn*sizeof(complex double));
  complex double *qpc = malloc(nn*sizeof(complex double));
  complex double *qhc = malloc(nn*sizeof(complex double));
  complex double *shc = malloc(nn*sizeof(complex double));

  if (!(pc && hc && qpc && qhc && shc)) {
    failreason = "Couldn't allocate space for cpoly";
    goto returnlab;
  }

  xx = 0.70710678118654752438L;
  yy = -xx;

  /* Remove the zeros at the origin if any */
  while (opr[nn-1] == 0.0 && opi[nn-1] == 0.0) {
    idnn2 = degree+1-nn;
    zeror[idnn2] = 0.0;
    zeroi[idnn2] = 0.0;
    nn--;
  }

  /* Make a copy of the coefficients */
  for (i=0;i<nn;i++) {
    pc[i] = opr[i] + I*opi[i];
    shc[i] = cmod(pc[i]);
  }

  /* Scale the polynomial */
  bnd = scale(nn,shc);
  if (bnd != 1.0) {
    for (i=0;i<nn;i++) {
      pc[i] *= bnd;
    }
  }

  while (!failreason) {

    /* Start the algorithm for one zero */
    if (nn < 3) {
      /* Calculate the final zero and return */
      complex double zeroc = cdivid(-pc[1], pc[0]);
      zeror[degree-1] = creal(zeroc); zeroi[degree-1] = cimag(zeroc);
      goto returnlab;
    }

    /* Calculate bnd, a lower bound on the modulus of the zeros */
    for (i=0;i<nn;i++) {
      shc[i] = cmod(pc[i]) + I*cimag(shc[i]);
    }
    bnd = cauchy(nn,shc);

    /* Outer loop to control 2 major passes with different sequences
       of shifts */
    failreason = "found fewer than degree zeros";
    for(cnt1=1;failreason && (cnt1<=2);cnt1++) {

      /* First stage calculation, no shift */
      tc = noshft(5,nn,tc,hc,pc);
      if (isnan(creal(tc)) || isnan(cimag(tc)))
        return "noshft returned NaN";

      /* Inner loop to select a shift. */
      for (cnt2=1;failreason && (cnt2<10);cnt2++) {
	/* Shift is chosen with modulus bnd and amplitude rotated by
	   94 degrees from the previous shift */
	xxx = COSR*xx-SINR*yy;
	yy  = SINR*xx+COSR*yy;
	xx  = xxx;
	sc  = bnd*xx + I*bnd*yy;

	/* Second stage calculation, fixed shift */
	if (fxshft(10*cnt2,nn,shc,qpc,hc,pc,qhc,&tc,&sc,&pvc,&zc)) {

	  /* The second stage jumps directly to the third stage iteration
	     If successful the zero is stored and the polynomial deflated */
	  idnn2 = degree+1-nn;
	  zeror[idnn2] = creal(zc);
	  zeroi[idnn2] = cimag(zc);
	  nn--;
	  for(i=0;i<nn;i++)
	    pc[i] = qpc[i];
	  failreason = NULL;
	}
	/* If the iteration is unsuccessful another shift is chosen */
      }
      /* If 9 shifts fail, the outer loop is repeated with another
	 sequence of shifts */
    }
  }

returnlab:
  if (shc) free(shc);
  if (qhc) free(qhc);
  if (qpc) free(qpc);
  if (hc) free(hc);
  if (pc) free(pc);
  /* The zerofinder has failed on two major passes
     Return empty handed */
  return failreason;
}

static complex double noshft(int l1, int nn, complex double tc, complex double hc[], complex double pc[])
{
  /*  Computes the derivative polynomial as the initial h
      polynomial and computes l1 no-shift h polynomials. */
  int i,jj,n = nn-1,nm1 = n-1,nm2=nm1-1;
  for (i=0;i<n;i++)
    hc[i] = (n-i)*pc[i] / n;
  for (jj=0;jj<l1;jj++) {
    if (cmod(hc[nm2]) > eta*10.0*cmod(pc[nm2])) {
      tc = cdivid(-pc[n], hc[nm1]);
      for (i=0;i<nm1;i++) {
	int j = nm1-i;
	hc[j] = pc[j] + tc * hc[j-1];
      }
      hc[0] = pc[0];
    } else {
      /*  If the constant term is essentially zero, shift h coefficients */
      for (i=0;i<nm1;i++) {
	int j = nm1-i;
	hc[j] = hc[j-1];
      }
      hc[0] = 0.0;
    }
  }
  return tc;
}

static int fxshft(int l2, int nn, complex double shc[], complex double qpc[], complex double hc[], complex double pc[], complex double qhc[], complex double *tc, complex double *sc, complex double *pvc, complex double *zc)
     /* Computes l2 fixed-shift h polynomials and tests for convergence

	Initiates a variable-shift iteration and returns with the
	approximate zero if successful.

	l2    - Limit of fixed shift steps

        output:
	zc    - Approximate zero if true return
	returns true if convergence of stage 3 iteration
     */
{
  int test,pasd,boolvar;
  int i,j,n = nn-1;

  /* Evaluate p at s */
  *pvc = polyev(nn,*sc,pc,qpc);
  test = TRUE;
  pasd = FALSE;

  /* Calculate first t = -p(s)/h(s) */
  boolvar = calct(nn,*sc,*pvc,qhc,hc,tc);

  /* Main loop for one second stage step */
  for (j=0;j<l2;j++) {
    complex double otc = *tc;

    /* Compute next h polynomial and new t */
    nexth(boolvar,nn, *tc, qhc, qpc, hc);
    boolvar = calct(nn,*sc,*pvc,qhc,hc,tc);
    *zc = *sc+*tc;

    /* Test for convergence unless stage 3 has failed once or
       this is the last h polynomial */
    if (boolvar || !test || j == l2-1) continue;
    if (cmod(*tc-otc) >= .5*cmod(*zc)) { pasd = FALSE; continue; }
    if (!pasd) { pasd = TRUE; continue; }
    /* The weak convergence test has been passed twice, start the
       third stage iteration, after saving the current h polynomial
       and shift */
    for (i=0;i<n;i++) {
      shc[i] = hc[i];
    }
    complex double svsc = *sc;
    if (vrshft(10,nn,qpc,pc,qhc,hc,tc,sc,pvc,zc))
      return TRUE;

    /* The iteration failed to converge
       Turn off testing and restore h,s,pv and t */
    test = FALSE;
    for (i=0;i<n;i++) {
      hc[i] = shc[i];
    }
    *sc = svsc;
    *pvc = polyev(nn,*sc,pc,qpc);
    boolvar = calct(nn,*sc,*pvc,qhc,hc,tc);
  }

  /* Attempt an iteration with final h polynomial from second stage */
  return vrshft(10,nn,qpc,pc,qhc,hc,tc,sc,pvc,zc);
}

static int vrshft(int l3, int nn, complex double qpc[], complex double pc[], complex double qhc[], complex double hc[], complex double *tc, complex double *sc, complex double *pvc, complex double *zc)
     /*  Carries out the third stage iteration

	 l3      - Limit of steps in stage 3
	 zc      - On entry contains the initial iterate,
	           On exit, it contains the final iterate (if it converges).
	 returns TRUE if iteration converges
     */
{
  double omp = 0,relstp = 0;
  int i,j,boolvar;
  int b = FALSE;
  *sc = *zc;

  /* Main loop for stage three */
  for (i=0; i<l3;i++) {

    /* Evaluate p at s and test for convergence */
    *pvc = polyev(nn,*sc,pc,qpc);
    double mp = cmod(*pvc), ms = cmod(*sc);
    if (mp <= 20.0L*errev(nn,ms,mp,qpc)) {
      /* Polynomial value is smaller in value than a bound on the error
	 in evaluating p, terminate the iteration */
      *zc = *sc;
      return TRUE;
    } else {
      if (i!=0) {
	if (!b && mp>=omp && relstp < .05L) {
	  /* Iteration has stalled, probably a cluster of zeros
	     Do 5 fixed shift steps into the cluster to force one zero
	     to dominate */
	  b = TRUE;
	  double tp = (relstp < eta) ? eta : relstp;
	  *sc *= 1.0L + sqrt(tp)*(1 + I);
	  *pvc = polyev(nn,*sc,pc,qpc);
	  for (j=0;j<5;j++) {
	    boolvar = calct(nn,*sc,*pvc,qhc,hc,tc);
	    nexth(boolvar,nn, *tc, qhc, qpc, hc);
	  }
	  omp = infin;
	} else {
	  /* Exit if polynomial value increases significantly */
          if (mp*0.1L > omp)
	    return FALSE;
	  omp = mp;
	}
      } else {
	omp = mp;
      }
    }

    /* Calculate next iterate. */
    boolvar = calct(nn,*sc,*pvc,qhc,hc,tc);
    nexth(boolvar,nn, *tc, qhc, qpc, hc);
    boolvar = calct(nn,*sc,*pvc,qhc,hc,tc);
    if (!boolvar) {
      relstp = cmod(*tc)/cmod(*sc);
      *sc += *tc;
    }
  }
  return FALSE;
}

static int calct(int nn, complex double sc, complex double pvc, complex double qhc[], complex double hc[], complex double *tc)
     /* Computes  t = -p(s)/h(s)
	Returns TRUE if h(s) is essentially zero
     */
{
  complex double hvc = polyev(nn-1,sc,hc,qhc); /* Evaluate h(s) */
  int boolvar = (cmod(hvc) <= are*10.0*cmod(hc[nn-2]));
  *tc = boolvar ? 0.0 : cdivid(-pvc, hvc);
  return boolvar;
}

static void nexth(int boolvar, int nn, complex double tc, complex double qhc[], complex double qpc[], complex double hc[])
  /* Calculates the next shifted h polynomial
     boolvar   -  TRUE if h(s) is essentially zero
  */
{
  int j,n = nn-1;

  if (!boolvar) {
    for (j=1;j<n;j++) {
      hc[j] = qpc[j] + tc*qhc[j-1];
    }
    hc[0] = qpc[0];
  } else {
    /* If h(s) is zero, replace h with qh */
    for (j=1;j<n;j++) {
          hc[j] = qhc[j-1];
    }
    hc[0] = 0.0;
  }
}

static complex double polyev(int nn, complex double sc, complex double pc[],
	    complex double qc[])
     /* Evaluates a polynomial  p  at  s  by the Horner recurrence,
	placing the partial sums in q, returns the computed value
     */
{
  int i;
  complex double vc = pc[0];
  qc[0] = vc;
  for (i=1;i<nn;i++)
    qc[i] = vc = vc*sc + pc[i];
  return vc;
}

static double errev(int nn, double ms, double mp, complex double qc[])
     /* Bounds the error in evaluating the polynomial by the Horner recurrence
	
	qr,qi    - The partial sums
	ms       - Modulus of the point
	mp       - Modulus of polynomial value
     */
{
  double e;
  int i;

  e = cmod(qc[0])*mre/(are+mre);
  for (i=0;i<nn;i++)
    e = e*ms+cmod(qc[i]);
  return e*(are+mre)-mp*mre;
}

static double cauchy(int nn, complex double pc[])
     /* Cauchy computes a lower bound on the moduli of the zeros of a
	polynomial - the real component of pc is the modulus of the coefficients
     */
{
  double x,xm,f,dx,df;
  int n=nn-1, nm=nn-2, i;

  pc[n] = -creal(pc[n]) + I*cimag(pc[n]);

  /* Compute upper estimate of bound */
  x = exp( (log(-creal(pc[n])) - log(creal(pc[0])))/n );
  if (creal(pc[nm]) != 0.0) {
    /* If Newton step at the origin is better, use it */
    xm = -creal(pc[n])/creal(pc[nm]);
    if (xm < x)
      x = xm;
  }

  /* Chop the interval (0,x) until f <= 0 */
  while (1) {
    xm = x*.1;
    f = creal(pc[0]);
    for (i=1;i<nn;i++)
      f = f*xm+creal(pc[i]);
    if (f <= 0.) break;
    x = xm;
  }
  dx = x;

  /* Do Newton iteration until x converges to two decimal places */
  while (fabs(dx/x) > .005L) {
    pc[0] = creal(pc[0]) + I*creal(pc[0]);
    for(i=1;i<nn;i++)
      pc[i] = creal(pc[i]) + I*(cimag(pc[i-1])*x+creal(pc[i]));
    f = cimag(pc[n]);
    df = cimag(pc[0]);
    for (i=1;i<n;i++)
      df = df*x+cimag(pc[i]);
    dx = f/df;
    x -= dx;
  }
  return x;
}

static double scale(int nn, complex double pc[])
     /* Returns a scale factor to multiply the coefficients of the
	polynomial.  The scaling is done to avoid overflow and to avoid
	undetected underflow interfering with the convergence
	criterion.  The factor is a power of the base.
	
	pc - the real components are modulus of coefficients of p
     */
{
  double hi,lo,max,min,x,sc;
  int i,l;

  /* Find largest and smallest moduli of coefficients */
  hi = sqrt(infin);
  lo = smalno/eta;
  max = 0.0;
  min = infin;
  for (i=0;i<nn;i++) {
    x = creal(pc[i]);
    if (x > max)
      max = x;
    if (x != 0.0 && x < min)
      min = x;
  }

  /* Scale only if there are very large or very small components */
  if (min >= lo && max <= hi)
    return 1.0;
  x = lo/min;
  if (x <= 1.0L) {
    sc = 1.0L/(sqrt(max)*sqrt(min));
  } else {
    sc = x;
    if (infin/sc > max)
      sc = 1.0;
  }
  l = log(sc)/log(base) + .500;
  return pow(base,l);
}

static complex double cdivid(complex double a, complex double b)
     /* Complex division c = a/b, avoiding overflow */
{
  double ar=creal(a),ai=cimag(a),br=creal(b),bi=cimag(b);
  if (br == 0.0  && bi == 0.0) {
    /* division by zero, c = infinity. */
    return infin*(1 + I);
  } else if (fabs(br) < fabs(bi)) {
    double r = br/bi;
    double d = bi+r*br;
    return (ar*r+ai + I*(ai*r-ar))/d;
  } else {
    double r = bi/br;
    double d = br+r*bi;
    return (ar+ai*r + I*(ai-ar*r))/d;
  }
}

static double cmod(complex double a)
     /* Modulus of a complex number avoiding overflow */
{
  double ar = fabs(creal(a)), ai = fabs(cimag(a)), f;
  if (ar < ai) {
    f = ar/ai;
    return ai*sqrt(1.0+f*f);
  } else if (ar > ai) {
    f = ai/ar;
    return ar*sqrt(1.0+f*f);
  } else {
    return ar*sqrt(2.0);
  }
}

static void mcon()
     /* mcon provides machine constants used in various parts of the
	program.  The user may either set them directly or use the
	statements below to compute them.  The meaning of the four
	constants are -
	
	eta       the maximum relative representation error
                  which can be described as the smallest positive
		  floating-point number such that 1.0d0 + eta is
		  greater than 1.0d0.
	infin    the largest floating-point number
	smalno    the smallest positive floating-point number
	base      the base of the floating-point number system used

	Let t be the number of base-digits in each floating-point
	number (double precision).  Then eta is either .5*b**(1-t)
	or b**(1-t) depending on whether rounding or truncation
	is used.

	Let m be the largest exponent and n the smallest exponent
	in the number system.  Then infiny is (1-base**(-t))*base**m
	and smalno is base**n.
     */
{

  /*
     #if !defined(WIN32) && !defined(_WIN32) && !defined(__APPLE__) && !defined(__CYGWIN__)
     base = 2;
     eta = DBL_EPSILON;
     smalno = MINDOUBLE;
     infin = MAXDOUBLE;
     #else
  */
  base = 2;
  eta = DBL_EPSILON;
  smalno = DBL_MIN;
  infin = DBL_MAX;
  /* #endif */

#ifdef IBM360
  /* These values for base,t,m,n correspond to the ibm/360. */
  int m,n,t;
  base = 16.0;
  t = 14;
  m = 63;
  n = -65;
  eta = pow(base,1-t);
  infin = (base)*(1.0-pow(base,-t))*pow(base,m-1);
  smalno = pow(base,n+3)/pow(base,3);
#endif
}

static void init(int nncr)
{
  static int nmax=0;

  if (nmax == 0) {
    /* Set up once-off constants */
    mcon();

    /* are, mre - Error bounds on complex addition and multiplication,
       cf e.g. errev() above */
    are = eta;
    mre = 2.0L*sqrt(2.0L)*eta;
    nmax = 1;
  }
}
