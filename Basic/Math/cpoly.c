/* Translated from F77 to C, rjrw 10/04/2000 */
/* replaced 'bool' by 'boolvar' to get it to compile on my 
   linux machine, DJB Aug 02 2000 */

/* algorithm 419 collected algorithms from acm.
   algorithm appeared in comm. acm, vol. 15, no. 02, p. 097. */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/* 
   #if !defined(WIN32) && !defined(_WIN32) && !defined(__APPLE__) && !defined(__CYGWIN__)
   #include <values.h>
   #endif
*/
#include <float.h>
/* #define DEBUGMAIN */   /* Set up debugging main, etc. */
#include "cpoly.h"

/* Internal routines */
static void noshft(int l1);
static int fxshft(int l2, double *zr, double *zi);
static int vrshft(int l3, double *zr, double *zi);
static int calct(void);
static void nexth(int boolvar);
static void polyev(int nn, double sr, double si, double pr[], double pi[],
	    double qr[], double qi[], double *pvr, double *pvi);
static double errev(int nn, double qr[], double qi[], double ms, double mp);
static double cauchy(int nn, double pt[], double q[]);
static double scale(int nn, double pt[]);
static void cdivid(double ar, double ai, double br, double bi, 
	    double *cr, double *ci);
static double cmod(double r, double i);
static void mcon(void);
static int init(int nncr);

/* Internal global variables */
static double *pr,*pi,*hr,*hi,*qpr,*qpi,*qhr,*qhi,*shr,*shi;
static double sr,si,tr,ti,pvr,pvi,are,mre,eta,infin,smalno,base;
static int nn;

#ifdef DEBUGMAIN
/* driver to test cpoly */
int main()
{
  int fail;
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


/* rjrw 10/04/2000: fix for cos 94: was -.060756474L */
#define COSR (-.069756474L)
#define SINR (.99756405L)

int cpoly(double opr[], double opi[], int degree,
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
  
  double xx,yy,xxx,zr,zi,bnd;
  int fail,conv;
  int cnt1,cnt2,i,idnn2;

  /* initialization of constants */
  nn = degree+1;
  if (!init(nn)) {
    fail = TRUE;
    return fail;
  }

  xx = .70710678L;
  yy = -xx;
  fail = FALSE;

  /* algorithm fails if the leading coefficient is zero. */
  if (opr[0] == 0.0 && opi[0] == 0.0) {
    fail = TRUE;
    return fail;
  }

  /* Remove the zeros at the origin if any */
  while (opr[nn-1] == 0.0 && opi[nn-1] == 0.0) {
    idnn2 = degree+1-nn;
    zeror[idnn2] = 0.0;
    zeroi[idnn2] = 0.0;
    nn--;
  }

  /* Make a copy of the coefficients */
  for (i=0;i<nn;i++) {
    pr[i] = opr[i];
    pi[i] = opi[i];
    shr[i] = cmod(pr[i],pi[i]);
  }

  /* Scale the polynomial */
  bnd = scale(nn,shr);
  if (bnd != 1.0) {
    for (i=0;i<nn;i++) {
      pr[i] *= bnd;
      pi[i] *= bnd;
    }
  }

  while (!fail) {

    /* Start the algorithm for one zero */
    if (nn < 3) {
      /* Calculate the final zero and return */
      cdivid(-pr[1],-pi[1],pr[0],pi[0],&(zeror[degree-1]),&(zeroi[degree-1]));
      return fail;
    }

    /* Calculate bnd, a lower bound on the modulus of the zeros */
    for (i=0;i<nn;i++) {
      shr[i] = cmod(pr[i],pi[i]);
    }
    bnd = cauchy(nn,shr,shi);

    /* Outer loop to control 2 major passes with different sequences
       of shifts */
    fail = TRUE;
    for(cnt1=1;fail && (cnt1<=2);cnt1++) {

      /* First stage calculation, no shift */
      noshft(5);

      /* Inner loop to select a shift. */
      for (cnt2=1;fail && (cnt2<10);cnt2++) {
	/* Shift is chosen with modulus bnd and amplitude rotated by
	   94 degrees from the previous shift */
	xxx = COSR*xx-SINR*yy;
	yy  = SINR*xx+COSR*yy;
	xx  = xxx;
	sr  = bnd*xx;
	si  = bnd*yy;

	/* Second stage calculation, fixed shift */
	conv = fxshft(10*cnt2,&zr,&zi);
	if (conv) {

	  /* The second stage jumps directly to the third stage iteration
	     If successful the zero is stored and the polynomial deflated */
	  idnn2 = degree+1-nn;
	  zeror[idnn2] = zr;
	  zeroi[idnn2] = zi;
	  nn--;
	  for(i=0;i<nn;i++) {
	    pr[i] = qpr[i];
	    pi[i] = qpi[i];
	  }
	  fail = FALSE;
	}
	/* If the iteration is unsuccessful another shift is chosen */
      }
      /* If 9 shifts fail, the outer loop is repeated with another
	 sequence of shifts */
    }
  }

  /* The zerofinder has failed on two major passes
     Return empty handed */
  return fail;
}

static void noshft(int l1)
{
  /*  Computes the derivative polynomial as the initial h
      polynomial and computes l1 no-shift h polynomials. */

  double  xni,t1,t2;
  int i,j,jj,n = nn-1,nm1 = n-1,nm2=nm1-1;
  for (i=0;i<n;i++) {
    xni = n-i;
    hr[i] = xni*pr[i]/((double)(n));
    hi[i] = xni*pi[i]/((double)(n));
  }
  for (jj=0;jj<l1;jj++) {
    if (cmod(hr[nm2],hi[nm2]) > eta*10.0*cmod(pr[nm2],pi[nm2])) {
      cdivid(-pr[n],-pi[n],hr[nm1],hi[nm1],&tr,&ti);
      for (i=0;i<nm1;i++) {
	j = nm1-i;
	t1 = hr[j-1];
	t2 = hi[j-1];
	hr[j] = tr*t1-ti*t2+pr[j];
	hi[j] = tr*t2+ti*t1+pi[j];
      }
      hr[0] = pr[0];
      hi[0] = pi[0];
    } else {

      /*  If the constant term is essentially zero, shift h coefficients */
      for (i=0;i<nm1;i++) {
	j = nm1-i;
	hr[j] = hr[j-1];
	hi[j] = hi[j-1];
      }
      hr[0] = 0.0;
      hi[0] = 0.0;
    }
  }
}

static int fxshft(int l2, double *zr, double *zi)
     /* Computes l2 fixed-shift h polynomials and tests for convergence

	Initiates a variable-shift iteration and returns with the
	approximate zero if successful.

	l2    - Limit of fixed shift steps
	zr,zi - Approximate zero if conv is .true.
	conv  - Flag indicating convergence of stage 3 iteration 
     */
{
  double otr,oti,svsr,svsi;
  int conv,test,pasd,boolvar;
  int i,j,n = nn-1;

  /* Evaluate p at s */
  polyev(nn,sr,si,pr,pi,qpr,qpi,&pvr,&pvi);
  test = TRUE;
  pasd = FALSE;

  /* Calculate first t = -p(s)/h(s) */
  boolvar = calct();

  /* Main loop for one second stage step */
  for (j=0;j<l2;j++) {
    otr = tr;
    oti = ti;

    /* Compute next h polynomial and new t */
    nexth(boolvar);
    boolvar = calct();
    *zr = sr+tr;
    *zi = si+ti;

    /* Test for convergence unless stage 3 has failed once or 
       this is the last h polynomial */
    if (!boolvar && test && j != l2) {
      if (cmod(tr-otr,ti-oti) < .5*cmod(*zr,*zi)) {
	if (pasd) {

	  /* The weak convergence test has been passed twice, start the
	     third stage iteration, after saving the current h polynomial
	     and shift */
	  for (i=0;i<n;i++) {
	    shr[i] = hr[i];
	    shi[i] = hi[i];
	  }
	  svsr = sr;
	  svsi = si;
	  conv = vrshft(10,zr,zi);
	  if (conv) 
	    return conv;

	  /* The iteration failed to converge
	     Turn off testing and restore h,s,pv and t */
	  test = FALSE;
	  for (i=0;i<n;i++) {
	    hr[i] = shr[i];
	    hi[i] = shi[i];
	  }
	  sr = svsr;
	  si = svsi;
	  polyev(nn,sr,si,pr,pi,qpr,qpi,&pvr,&pvi);
	  boolvar = calct();
	} else {
	  pasd = TRUE;
	}
      }
    } else {
      pasd = FALSE;
    }
  }

  /* Attempt an iteration with final h polynomial from second stage */
  conv = vrshft(10,zr,zi);
  return conv;
}

static int vrshft(int l3, double *zr, double *zi)
     /*  Carries out the third stage iteration

	 l3      - Limit of steps in stage 3
	 zr,zi   - On entry contains the initial iterate,
	           On exit, it contains the final iterate (if it converges).
	 conv    - TRUE if iteration converges 
     */
{
  double mp,ms,omp,relstp,r1,r2,tp;
  int i,j,conv,b,boolvar;

  conv = FALSE;
  b = FALSE;
  sr = *zr;
  si = *zi;

  /* Main loop for stage three */
  for (i=0; i<l3;i++) {

    /* Evaluate p at s and test for convergence */
    polyev(nn,sr,si,pr,pi,qpr,qpi,&pvr,&pvi);
    mp = cmod(pvr,pvi);
    ms = cmod(sr,si);
    if (mp <= 20.0L*errev(nn,qpr,qpi,ms,mp)) {
      /* Polynomial value is smaller in value than a bound on the error
	 in evaluating p, terminate the iteration */
      conv = TRUE;
      *zr = sr;
      *zi = si;
      return conv;
    } else {
      if (i!=0) {
	if (!b && mp>=omp && relstp < .05L) {
	  /* Iteration has stalled, probably a cluster of zeros 
	     Do 5 fixed shift steps into the cluster to force one zero 
	     to dominate */
	  b = TRUE;
	  if (relstp < eta) 
	    tp = eta;
	  else
	    tp = relstp;
	  r1 = sqrt(tp);
	  r2 = sr*(1.0L+r1)-si*r1;
	  si = sr*r1+si*(1.0L+r1);
	  sr = r2;
	  polyev(nn,sr,si,pr,pi,qpr,qpi,&pvr,&pvi);
	  for (j=0;j<5;j++) {
	    boolvar = calct();
	    nexth(boolvar);
	  }
	  omp = infin;
	} else {
	  /* Exit if polynomial value increases significantly */
          if (mp*0.1L > omp) 
	    return conv;
	  omp = mp;
	}
      } else {
	omp = mp;
      }
    }

    /* Calculate next iterate. */
    boolvar = calct();
    nexth(boolvar);
    boolvar = calct();
    if (!boolvar) {
      relstp = cmod(tr,ti)/cmod(sr,si);
      sr += tr;
      si += ti;
    }
  }
  return conv;
}

static int calct(void)
     /* Computes  t = -p(s)/h(s)
	Returns TRUE if h(s) is essentially zero 
     */
{
  double  hvr,hvi;
  int n = nn-1, boolvar;

  /* Evaluate h(s) */
  polyev(n,sr,si,hr,hi,qhr,qhi,&hvr,&hvi);
  boolvar = (cmod(hvr,hvi) <= are*10.0*cmod(hr[n-1],hi[n-1]));
  if (!boolvar) {
    cdivid(-pvr,-pvi,hvr,hvi,&tr,&ti);
  } else {
    tr = 0.0;
    ti = 0.0;
  }
  return boolvar;
}

static void nexth(int boolvar) 
  /* Calculates the next shifted h polynomial
     boolvar   -  TRUE if h(s) is essentially zero 
  */
{
  double t1,t2;
  int j,n = nn-1;

  if (!boolvar) {
    for (j=1;j<n;j++) {
      t1 = qhr[j-1];
      t2 = qhi[j-1];
      hr[j] = tr*t1-ti*t2+qpr[j];
      hi[j] = tr*t2+ti*t1+qpi[j];
    }
    hr[0] = qpr[0];
    hi[0] = qpi[0];
  } else {
    /* If h(s) is zero, replace h with qh */
    for (j=1;j<n;j++) {
          hr[j] = qhr[j-1];
          hi[j] = qhi[j-1];
    }
    hr[0] = 0.0;
    hi[0] = 0.0;
  }
}
 
static void polyev(int nn, double sr, double si, double pr[], double pi[],
	    double qr[], double qi[], double *tvr, double *tvi)
     /* Evaluates a polynomial  p  at  s  by the Horner recurrence,
	placing the partial sums in q and the computed value in pv 
     */
{
  double t, vr, vi;
  int i;

  qr[0] = pr[0];
  qi[0] = pi[0];
  vr = qr[0];
  vi = qi[0];
  for (i=1;i<nn;i++) {
    t = vr*sr-vi*si+pr[i];
    vi = vr*si+vi*sr+pi[i];
    vr = t;
    qr[i] = vr;
    qi[i] = vi;
  }
  *tvr = vr;
  *tvi = vi;
}

static double errev(int nn, double qr[], double qi[], double ms, double mp)
     /* Bounds the error in evaluating the polynomial by the Horner recurrence
	
	qr,qi    - The partial sums
	ms       - Modulus of the point
	mp       - Modulus of polynomial value
     */
{
  double e;
  int i;

  e = cmod(qr[0],qi[0])*mre/(are+mre);
  for (i=0;i<nn;i++)
    e = e*ms+cmod(qr[i],qi[i]);
  return e*(are+mre)-mp*mre;
}

static double cauchy(int nn, double pt[], double q[])
     /* Cauchy computes a lower bound on the moduli of the zeros of a
	polynomial - pt is the modulus of the coefficients 
     */
{
  double x,xm,f,dx,df;
  int n=nn-1, nm=nn-2, i;

  pt[n] = -pt[n];

  /* Compute upper estimate of bound */
  xm = exp( (log(-pt[n]) - log(pt[0]))/((double)n) );
  if (pt[nm] != 0.0) {
    /* If Newton step at the origin is better, use it */
    x = -pt[n]/pt[nm];
    if (x < xm) 
      xm = x;
  }

  /* Chop the interval (0,x) until f <= 0 */
  do {
    x = xm;
    xm *= .1;
    f = pt[0];
    for (i=1;i<nn;i++)
      f = f*xm+pt[i];
  } while (f > 0.);
  dx = x;
  
  /* Do Newton iteration until x converges to two decimal places */
  while (fabs(dx/x) > .005L) {
    q[0] = pt[0];
    for(i=1;i<nn;i++)
      q[i] = q[i-1]*x+pt[i];
    f = q[n];
    df = q[0];
    for (i=1;i<n;i++)
      df = df*x+q[i];
    dx = f/df;
    x -= dx;
  }
  return x;
}

static double scale(int nn, double pt[])
     /* Returns a scale factor to multiply the coefficients of the
	polynomial.  The scaling is done to avoid overflow and to avoid
	undetected underflow interfering with the convergence
	criterion.  The factor is a power of the base.
	
	pt - modulus of coefficients of p 
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
    x = pt[i];
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

static void cdivid(double ar, double ai, double br, double bi, 
	    double *cr, double *ci)
     /* Complex division c = a/b, avoiding overflow */
{
  double r,d;
  if (br == 0.0  && bi == 0.0) { 
    /* division by zero, c = infinity. */
    *cr = infin;
    *ci = infin;
  } else if (fabs(br) < fabs(bi)) {
    r = br/bi;
    d = bi+r*br;
    *cr = (ar*r+ai)/d;
    *ci = (ai*r-ar)/d;
  } else {
    r = bi/br;
    d = br+r*bi;
    *cr = (ar+ai*r)/d;
    *ci = (ai-ar*r)/d;
  }
  return;
}

static double cmod(double r, double i)
     /* Modulus of a complex number avoiding overflow */
{
  double ar,ai,f;
  ar = fabs(r);
  ai = fabs(i);
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

static int init(int nncr)
{
  static int nmax=0;

  if (nmax == 0) {
    /* Set up once-off constants */
    mcon();

    /* are, mre - Error bounds on complex addition and multiplication,
       cf e.g. errev() above */
    are = eta;
    mre = 2.0L*sqrt(2.0L)*eta;

  } else if (nmax >= nncr) {
    return TRUE;            /* Present arrays are big enough */
  } else {
    /* Free old arrays (no need to preserve contents */
    free(shi); free(shr); free(qhi); free(qhr); 
    free(qpi); free(qpr); free(hi); free(hr); free(pi); free(pr);
  }

  nmax = nncr;

  pr  = (double *) malloc(nmax*sizeof(double));
  pi  = (double *) malloc(nmax*sizeof(double));
  hr  = (double *) malloc(nmax*sizeof(double));
  hi  = (double *) malloc(nmax*sizeof(double));
  qpr = (double *) malloc(nmax*sizeof(double));
  qpi = (double *) malloc(nmax*sizeof(double));
  qhr = (double *) malloc(nmax*sizeof(double));
  qhi = (double *) malloc(nmax*sizeof(double));
  shr = (double *) malloc(nmax*sizeof(double));
  shi = (double *) malloc(nmax*sizeof(double));

  if (!(pr && pi && hr && hi && qpr && qpi && qhr && qhi && shr && shi)) {
    fprintf(stderr,"Couldn't allocate space for cpoly\n");
    return FALSE;
  } else {
    return TRUE;
  }
}
