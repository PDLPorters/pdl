/*LINTLIBRARY*/
/*  gauss.c

    This code provides gaussian fitting routines.

    Copyright (C) 1997  Karl Glazebrook and Alison Offer Real code
    
    Note it is not clear to me that this code is fully debugged. The reason
    I say that is because I tried using the linear eqn solving routines called
    elsewhere and they were giving erroneous results. So steal from this
    code with caution! However it does give good fits to reasonable looking 
    gaussians and tests show correct parameters.
    
             KGB 29/Oct/2002
 */


#include <stdio.h>
#include <math.h>

#define NPAR 3
#define MAXITER 1000


/* Malloc 2D ptr array e.g. a[nx][ny] */

static double **malloc2D (int nx, int ny)
{
   int i;
   double **p;
   p = (double**) malloc( nx*sizeof(double*) ); /* 1D array of ptrs p[i] */
   if (p==NULL)
      return NULL;
   for (i=0;i<nx;i++) {
       p[i] = (double*) malloc( ny*sizeof(double) );
       if (p[i] == NULL) {
          free(p);
          return(NULL);
       }
   }
    
   return (double**) p;
}

/* Free a 2D ptr array */

static void free2D (double **p, int nx, int ny)
{
   int i;
   
   for (i=0;i<nx;i++)
      free(p[i]);
      
   free(p);
}


/*    ======================================================================== */

static void seta (int npar, int npoints, double a[NPAR][NPAR], double **d,
		  double *sig)
{
   int i,j,k;
   
   for(i=0; i<npar; i++)
   {
       for (j=0; j<npar; j++)
       {
	   a[i][j] = 0.0;
	   for(k=0; k<npoints; k++) 
	       a[i][j] += d[k][i] * d[k][j] / sig[k];
       }
   }
}

static void setb (int npar, int npoints, double b[NPAR], double **d, 
		  double *y, double *yfit, double *sig)
{
   int i,k;
   
   for(i=0; i<npar; i++) { 
      b[i]=0.0;
      for(k=0; k<npoints; k++)
        b[i] += d[k][i] * (y[k] - yfit[k]) / sig[k];
   }
}

/* ========================================================================

     returns the fitted function in yfit and the partial derivatives in d

     here f = b exp ( - ((x - c)/a)^2)
*/

static void funct (int npoints, int npar, double *x, double *yfit,
		   double **d, double par[NPAR])
{
   int i;
   double a,b,c,arg;
   
   a=par[0]; b=par[1]; c=par[2];
   
   for(i=0; i<npoints; i++) {
      arg = (x[i]-c) /a; arg *= arg;
      yfit[i] = b * exp(-arg);
      d[i][0] = 2.0 * arg * yfit[i] / a;
      d[i][1] = yfit[i] / b;
      d[i][2] = 2.0 * arg * yfit[i] / (x[i]-c);
   }
}


/* These have been checked */


/* 

 IMPORTANT NOTE: I tried using decomp()+leneq() routines 
    elsewhere and they were giving erroneous results. 
    So steal from this code with caution! 
    
             KGB 29/Oct/2002
 */


/* =======================================================================

   LU decomposition of matrix x using Crouts algotithm with partial
   pivoting.

   Returns decomposition in x and permutation of rows in iorder
   
*/

static void decomp (int n, int ndim, double x[NPAR][NPAR], int iorder[NPAR])
{
   int icol, irow, isum, iexc, ipivot;
   double store, xmax, sum, eps=1e-16;
   
   /* initialise iorder */
   
   for(iexc=1; iexc<=n; iexc++) 
      iorder[iexc-1] = iexc;
      
   /* loop over columns */
   
   for (icol = 1; icol<=n; icol++) {
   
      /* U - matrix */
      
      if (icol>1) {
         for (irow=1; irow<=icol-1; irow++) {
            sum = x[irow-1][icol-1];
            for (isum=1; isum<=irow-1; isum++) 
               sum -= x[irow-1][isum-1]*x[isum-1][irow-1];
            x[irow-1][icol-1] = sum;
         }
      }
      
      /* L - matrix plus diagonal element of U matrix */
      
      xmax = 0; ipivot = icol;
      for (irow=icol; irow<=n; irow++) {
         sum = x[irow-1][icol-1];
         if (icol>1) {
            for(isum=1; isum<=icol-1; isum++) 
               sum -= x[irow-1][isum-1] * x[isum-1][icol-1];
          }
          if (fabs(sum)>xmax) {
             xmax = sum;
             ipivot = irow;
          }
          x[irow-1][icol-1] = sum;
       }
       
       /* if xmax is very small replace by epsilon to avoid
          dividing by zero */
   
       if (fabs(xmax)<eps) 
          x[ipivot-1][icol-1] = eps;
          
       /* Partial Pivoting - look for maximum x(i,j<=i) and interchange 
		   rows if necessary to put this on the diagonal */
		   
       if (ipivot!=icol) {
          iexc = iorder[ipivot-1];
          iorder[ipivot-1] = iorder[icol-1];
          iorder[icol-1] = iexc;
          for(iexc=1; iexc<=n; iexc++) {
             store = x[ipivot-1][iexc-1];
             x[ipivot-1][iexc-1] = x[icol-1][iexc-1];
             x[icol-1][iexc-1] = store;
          }
       }
       
       /* divide new L-matrix elements by new diagonal element */
       
       if (icol<n) {
          for (irow=icol+1; irow<=n; irow++)
             x[irow-1][icol-1] /= x[icol-1][icol-1];
       }
    }
}
		   

/* 

 IMPORTANT NOTE: I tried using decomp()+leneq() routines 
    elsewhere and they were giving erroneous results. 
    So steal from this code with caution! 
    
             KGB 29/Oct/2002
 */
 
/* ======================================================================== 

    x[i<=j[]j] is U-matrix from LU-decomposition
    x[i>j][j]  is L-matrix (diagonal elements are unity)
    iorder is the permutation of the rows

    b is the input vector, d is the solution vector
*/

static void lineq (int n, int ndim, double x[NPAR][NPAR], double b[NPAR],
		   double d[NPAR], int iorder[NPAR])
{
   int i,isum;
   double sum;
   
   /* solving X.b = d  ==> (L.U).b = d 
      or	L.(U.b) = d 
      
      first re-order the vector   */
      
   for (i=1; i<=n; i++) 
      d[i-1] = b[iorder[i-1]-1];
      
   /* first find (U.b) */
   
   for(i=2; i<=n; i++) {
      sum = d[i-1];
      for (isum=1; isum<=i-1; isum++) 
         sum -= x[i-1][isum-1] * d[isum-1];
      d[i-1] = sum;
   }
   
   /* Now fill out d (solution of X.b) by back substitution */

   d[n-1] /= x[n-1][n-1];
   for (i=n-1; i>=1; i--){
      sum = d[i-1];
      for (isum=i+1; isum<=n; isum++) 
         sum -= x[i-1][isum-1] * d[isum-1];
      d[i-1] = sum / x[i-1][i-1];
   }
   
}     
      
      
/* ========================================================================

   My C version of Alison's subroutine to fit a non-linear functions using the 
   Levenberg-Marquardt algorithm 
   
     input: npoints = number of data points
	   npar    = number of parameters in fit
	   par     = initial estimates of parameters
	   sigma   = errors on data (sigma^2)

    output: par    = output parameters
	    r	   = residuals (y(i) - yfit(i))
	    a  = estimated covariance matrix of std errs
		     in fitted params.  
*/
    
    
static int marquardt (int npoints, int npar, double*x, double *y,
		      double* sig, double par[NPAR], double* r,
		      double a[NPAR][NPAR])
{
   int i,k, done, decrease, niter;
   int iorder[NPAR];
   double *yfit, **d, **d2, tmp;
   double  par2[NPAR], delta[NPAR], b[NPAR], aprime[NPAR][NPAR];
   
   double lambda, chisq, chisq2, eps=0.001, lamfac=2.0;
      
   /* Memory allocation */
   
   yfit = (double*) malloc( npoints*sizeof(double));
   if (yfit==NULL)
      return(1);
   
   d = malloc2D( npoints, NPAR);
   if (d==NULL) {
      free(yfit); return(1);
   }
   
   d2 = malloc2D( npoints, NPAR);
   if (d2==NULL) {
      free(yfit); free2D(d,npoints,NPAR); return(1);
   }
   
   /* Not enough points */
   
   if (npoints < npar) {
      free(yfit); free2D(d,npoints,NPAR); free2D(d2,npoints,NPAR);
      return(2);
   }
   
   lambda = 0.001; done = 0; decrease = 0; niter = 1;
   
   /* Get the value for the initial fit and the value of the derivatives
   for the current estimate of the parameters */
   
   funct(npoints, npar, x, yfit, d, par);
   
   /* Calculate chi^2 */
   
   chisq = 0;
   for (k=0; k<npoints; k++) {
      tmp = y[k] - yfit[k];
      chisq += tmp*tmp / sig[k];
   }
    
   /*  Set up initial matrices; curvature matrix a and b */
   
   seta(npar,npoints,a,d,sig); 
   setb(npar,npoints,b,d,y,yfit,sig);
   
   /* Main Loop */
   
   while(!done) {
   
      /* Set up a' */
      
      for (k=0; k<npar; k++) {
         for (i=0; i<npar; i++) {
            aprime[i][k] = a[i][k]; 
         }
         aprime[k][k] = a[k][k] * (1.0 + lambda);
      }
      
      /* Solve Matrix Equation: a'.delta = b */
      

/* 

 IMPORTANT NOTE: I tried using decomp()+leneq() routines 
    elsewhere and they were giving erroneous results. 
    So steal from this code with caution! 
    
             KGB 29/Oct/2002
 */
      decomp(npar ,npar,  aprime, iorder);
      lineq(npar, npar,  aprime, b, delta, iorder); 
      
      /* Increment parameters in work space and evaluate new chisq */
      
      for (i=0; i<NPAR; i++){
         if (i<npar) 
            par2[i] = par[i] + delta[i];
         else
            par2[i] = par[i];
      }
         
      funct(npoints, npar, x, r, d2, par2);
      chisq2 = 0.0;
      
      for (k=0; k<npoints; k++) {
            tmp = y[k] - r[k];
            chisq2 += tmp*tmp / sig[k];
      }      
      
      /* SUccess ? */ 
      
      /* printf ("diag: %lf %lf %lf\n", chisq, chisq2, 
         fabs(((chisq - chisq2)/chisq))); */
      
      if (chisq2<=chisq) {
      
         /* Test convergence */
         
         if (chisq == chisq2) 
            done = 1;
         if (fabs(((chisq - chisq2)/chisq)) < eps && decrease ) 
            done = 1;
         for (k=0; k<npoints; k++) 
            yfit[k] = r[k];
                
         for (i=0; i<NPAR; i++)  {
            par[i] = par2[i];
            for (k=0; k<npoints; k++) {    
              d[k][i] = d2[k][i];
            }
         }
          
         if (!done) {
            seta(npar,npoints,a,d,sig); setb(npar,npoints,b,d,y,yfit,sig);
            chisq = chisq2; lambda = lambda / lamfac; decrease = 1;
         }
      }
      else {
         
         decrease = 0;
         lambda *= lamfac;
      }
      niter++;
      
      /* Failure to converge after numerous iterations */
      
      if (niter>MAXITER) {
         free(yfit); free2D(d,npoints,NPAR); free2D(d2,npoints,NPAR);
         return(3);
      }
   }      
         
   /* Success!!! - compute residual and covariance matrix then return
 
      first calculating inverse of aprime */
 
    for (i=0; i<npar; i++)
       delta[i] = 0.0;
    for (i=0; i<npar; i++) {
       delta[i] = 1.0;
       lineq (npar, NPAR, aprime, delta, b, iorder);
       for (k=0; k<npar; k++) 
          a[k][i] = b[k];
    }
 
    for (k=0; k<npoints; k++)
       r[k] = y[k] - yfit[k];
 
    free(yfit); free2D(d,npoints,NPAR); free2D(d2,npoints,NPAR);
 
    return(0);
}

/* qsort doubles */
static void lqsortD (double* xx, int a, int b)
{
   int i,j;
	
   double t, median;
	
   i = a; j = b;
   median = xx[(i+j) / 2];
   do {
      while (xx[i] < median)
	 i++;
      while (median < xx[j])
	 j--;
      if (i <= j) {
	 t = xx[i]; xx[i] = xx[j]; xx[j] = t;
	 i++; j--;
      }
   } while (i <= j);
	
   if (a < j)
      lqsortD(xx,a,j);
   if (i < b)
      lqsortD(xx,i,b);
}

