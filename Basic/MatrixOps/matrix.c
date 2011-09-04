/*
 * matrix.c - misc. routines for manipulating matrices and vectors.
 *
 * (C) Copyright 2001 by NetGroup A/S. All rights reserved.
 *
 * $Log$
 * Revision 1.1  2006/06/20 15:57:22  djburke
 * Hopefully a saner way to build Basic/MatrixOps
 *
 * Revision 1.1  2005/01/08 09:22:57  zowie
 * Added non-symmetric matrices to eigens; updated version to 2.4.2cvs.
 *
 * Revision 1.1.1.1  2001/07/06 13:39:35  kneth
 * Initial import of code.
 *
 *
 *
 * The matrices and vectors are indexed in C-style, i.e. from 0 to
 * N-1. A matrix is assumed to be declared as double **, and it is
 * allocated by MatrixAlloc.
 *
 *
 * References:
 * [1]   Numerical Recipes in C, 2nd edition,
 *       W.H. Press, S.A. Teukolsky, W.T. Vitterling, and B.P. Flannery,
 *       Cambridge University Press, 1992.
 * [2]   Numerical Analysis,
 *       D. Kincaid and W. Cheney,
 *       Brooks/Cole Publishing Company, 1991.
 * [3]   The C Programming Language, 2nd edition,
 *       B.W. Kernighan and D.M. Ritchie,
 *	 Prentice Hall, 1988.
 * [4]   Advanced Engineering Mathematics, 6th edition,
 *       E. Kreyszig,
 *       Wiley and Sons, 1988.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#ifndef TINY
#  define TINY 1.0e-18
#endif

#include "sslib.h"
#include "matrix.h"


/*
 * MatrixAlloc allocates storage for a square matrix with dimension
 * n*n. An error message is printed, if it was impossible to allocate
 * the neccesary space, [3].
 *
 */

double **MatrixAlloc(const int n) {

  double **matrix;
  int    i;

  matrix=(double **)calloc(n, sizeof(double *));
  if (matrix==NULL)
    SSLerror("No memory available in routine MatrixAlloc");
  else 
    for(i=0; i<n; i++) {
      matrix[i]=(double *)calloc(n, sizeof(double));
      if (matrix[i]==NULL)
	SSLerror("No memory available in routine MatrixAlloc");
    } /* for i=1..n */
  return matrix;
} /* MatrixAlloc */


/*
 * VectorAlloc allocated space for an n-dimensional vector of the type
 * double *, [3]. It can be freed by VectorFree.
 *
 */

double *VectorAlloc(const int n) {

  double *temp;

  temp=(double *)calloc(n, sizeof(double));
  if (temp==NULL)
    SSLerror("No memory available in routine VectorAlloc");
  return temp;
} /* VectorAlloc */


/*
 * IntVectorAlloc is similar to VectorAlloc, except that the base type is 
 * integers (int) instead of reals (double), [3].
 *
 */

int *IntVectorAlloc(const int n) {

  int *temp;

  temp=(int *)calloc(n, sizeof(int));
  if (temp==NULL)
    SSLerror("No memory available in routine IntVectorAlloc");
  return temp;
} /* IntVectorAlloc */


/*
 * SSL_ComplexMatrixAlloc allocates space for a nxn matrix with complex elements.
 *
 */

SSL_Complex **SSL_ComplexMatrixAlloc(const int n) {

  int       i;
  SSL_Complex **temp;

  temp=(SSL_Complex **)calloc(n, sizeof(SSL_Complex *));
  if (temp==NULL)
    SSLerror("No memory available in routine SSL_ComplexMatrixAlloc");
  else {
    for(i=0; i<n; i++) {
      temp[i]=(SSL_Complex *)calloc(n, sizeof(SSL_Complex));
      if (temp[i]==NULL)
	SSLerror("No memory available in routine SSL_ComplexMatrixAlloc");
    } /* for i=1..n */
  } /* if else */
  return temp;
} /* SSL_ComplexMatrixAlloc */


/*
 * SSL_ComplexVectorAlloc allocates a vector of dimension n with complex 
 * elements.
 *
 */
       
SSL_Complex *SSL_ComplexVectorAlloc(const int n) {

  SSL_Complex *temp;

  temp=(SSL_Complex *)calloc(n, sizeof(SSL_Complex));
  if (temp==NULL) 
    SSLerror("No memory available in routine SSL_ComplexVectorAlloc");
  return temp;
} /* SSL_ComplexVectorAlloc */


/*
 * MatrixMul computes the product between two square matrices, [4, pp. 
 * 357-358]. Both matrices are assumed to have the dimension n*n. A and B are 
 * input, and res is the output, i.e. the routine computes res=A*B.
 *
 */

void MatrixMul(const int n, double **res, double **A, double **B) {

  int       i, j, k;
  double    x;

  for(i=0; i<n; i++)
    for(j=0; j<n; j++) {
      x=0.0;
      for(k=0; k<n; k++)
	x+=A[i][k]*B[k][j];
      res[i][j]=x;
    } /* for j=1..n */
} /* MatrixMul */


/*
 * Transpose is simply doing as the name says, i.e. transposing the
 * matrix A, [4, pp. 368-370]. A is assumed to be a square matrix.
 *
 */

void Transpose(const int n, double **res, double **A) {

  int i, j;

  for(i=0; i<n; i++)
    for(j=0; j<n; j++)
      res[j][i]=A[i][j];
} /* Transpose */


/*
 * MatrixFree, VectorFree, IntVectorFree, SSL_ComplexMatrixFree, and 
 * SSL_ComplexVectorFree free the space used by the objects, [3].
 *
 */

void MatrixFree(const int n, double **matrix) {

  int i;

  for(i=0; i<n; i++)
    free((void *)matrix[i]);
  free((void *)matrix);
} /* MatrixFree */

void SSL_ComplexMatrixFree(const int n, SSL_Complex **matrix) {

  int i;

  for(i=0; i<n; i++)
    free((void *)matrix[i]);
  free((void *)matrix);
} /* SSL_ComplexMatrixFree */

void VectorFree(const int n, double *vector) {

  free((void *)vector);
} /* VectorFree */

void IntVectorFree(const int n, int *vector) {

  free((void *)vector);
} /* IntVectorFree */

void SSL_ComplexVectorFree(const int n, SSL_Complex *vector) {

  free((void *)vector);
} /* SSL_ComplexVectorFree */


/*
 * LUfact and LUsubst are plain LU decomposition and substitution routines, 
 * [1, pp. 43-50], [2, pp. 145-149]. The version here is Gaussian elimination 
 * with scaled row pivoting, and it is based on the algorithm given in [2].
 *
 * LUfact_fixed and LUsubst_fixed are specialised versions of LUfact and
 * LUsubst. They are used in OEE solvers.
 *
 * The parameters are used:
 *   n            the dimension of the matrix.
 *   a            the matrix; it contains both L and U at termination.
 *   p            permutation index.  
 *   b            the constant vector, and at termination it will contain
 *                the solution.
 *
 */

void LUfact(const int n, double **a, int *p) {

  int      i, j, k;             /* counters           */
  double   z;                   /* temporary real     */
  double  *s;                   /* pivot elements     */
  int      not_finished;        /* loop control var.  */
  int      i_swap;              /* swap var.          */
  double   temp;                /* another temp. real */

  s=VectorAlloc(n);
  for(i=0; i<n; i++) {
    p[i]=i;
    s[i]=0.0;
    for(j=0; j<n; j++) {
      z=fabs(a[i][j]);
      if (s[i]<z)
	s[i]=z;
    } /* for j */
  } /* for i */

  for(k=0; k<(n-1); k++) {
    j=k-1;           /* select j>=k so ... */
    not_finished=1;
    while (not_finished) {
      j++;
      temp=fabs(a[p[j]][k]/s[p[j]]);
      for(i=k; i<n; i++)  
	if (temp>=(fabs(a[p[i]][k])/s[p[i]]))
	  not_finished=0;          /* end loop */
    } /* while */
    i_swap=p[k];
    p[k]=p[j];
    p[j]=i_swap;
    temp=1.0/a[p[k]][k];
    for(i=(k+1); i<n; i++) {
      z=a[p[i]][k]*temp;
      a[p[i]][k]=z;
      for(j=(k+1); j<n; j++) 
	a[p[i]][j]-=z*a[p[k]][j];
    } /* for i */
  } /* for k */

  VectorFree(n, s);
} /* LUfact */

void LUsubst(const int n, double **a, int *p, double *b) {

  int        i, j, k;           /* counters               */
  double     sum;               /* temporary sum variable */
  double    *x;                 /* solution               */
  
  x=VectorAlloc(n);

  for(k=0; k<(n-1); k++)        /* forward subst */
    for(i=(k+1); i<n; i++)
      b[p[i]]-=a[p[i]][k]*b[p[k]];
  
  for(i=(n-1); i>=0; i--) {     /* back subst */
    sum=b[p[i]];
    for(j=(i+1); j<n; j++)
      sum-=a[p[i]][j]*x[j];
    x[i]=sum/a[p[i]][i];
  } /* for i */
    
  for(i=0; i<n; i++)             /* copy solution */
    b[i]=x[i];

  VectorFree(n, x);
} /* LUsubst */


/*
 * GaussSeidel is an implementation of the Gauss-Seidel method, which is an
 * iterative method, [2, pp. 189-191]. The norm applied is the L1-norm.
 *
 * The parameters are:
 *   n           the dimension.
 *   a           the coefficient matrix.
 *   b           the constant vector.
 *   x           the initial guess, and at termination the solution.
 *   eps         the precision.
 *   max_iter    the maximal number of iterations allowed.
 *
 */

void GaussSeidel(const int n, double **a, double *b, double *x, double eps, 
		 int max_iter) {

  int      iter, i, j;       /* counter        */
  double   sum;              /* temporary real */
  double  *x_old;            /* old solution   */ 
  double   norm;             /* L1-norm        */

  x_old=VectorAlloc(n);

  iter=0;
  do {                       /* repeat until safisfying sol. */
    iter++;
    for(i=0; i<n; i++)       /* copy old solution */
      x_old[i]=x[i];
    norm=0.0;                /* do an iteration */
    for(i=0; i<n; i++) {     
      sum=-a[i][i]*x[i];     /* don't include term i=j */
      for(j=0; j<n; j++) 
	sum+=a[i][j]*x[j];   
      x[i]=(b[i]-sum)/a[i][i];
      norm+=fabs(x_old[i]-x[i]);
    } /* for i */
  } while ((iter<=max_iter) && (norm>=eps));

  VectorFree(n, x_old);
} /* GaussSeidel */


/*
 * Jacobi is an iterative equation solver, [2, pp. 185-189]. The algorithm
 * can be optimised a bit, which is done in this implementation. The method 
 * is suitable for parallel computers.
 *
 * The arguments are the same as in GaussSeidel.
 *
 */

void Jacobi(const int n, double **a, double *b, double *x, double eps, 
	    int max_iter) {

  double    d;              /* temporary real */
  int       i, j, iter;     /* counters       */
  double  **a_new;          /* a is altered   */
  double   *b_new;          /* b is altered   */
  double   *u;              /* new solution   */
  double    norm;           /* L1-norm        */

  a_new=MatrixAlloc(3);
  b_new=VectorAlloc(3);
  u=VectorAlloc(3);

  for(i=0; i<n; i++) {       /* the trick */
    d=1.0/a[i][i];
    b_new[i]=d*b[i];
    for(j=0; j<n; j++)
      a_new[i][j]=d*a[i][j];
  } /* for i */

  iter=0;
  do {
    iter++;
    norm=0.0;
    for(i=0; i<n; i++) {       /* update process */
      d=-a_new[i][i]*x[i];     /* don't include term i=j */
      for(j=0; j<n; j++)
	d+=a_new[i][j]*x[j];   
      u[i]=b_new[i]-d;
      norm=fabs(u[i]-x[i]);
    } /* for i */
    for(i=0; i<n; i++)           /* copy solution */
      x[i]=u[i];
  } while ((iter<=max_iter) && (norm>=eps));
  
  MatrixFree(3, a_new);
  VectorFree(3, b_new);
  VectorFree(3, u);
} /* Jacobi */


/*
 * DotProd computes the dot product between two vectors. They are assumed to
 * be of the same dimension.
 *
 */

double DotProd(const int n, double *u, double *v) {

  int      i;        /* counter        */
  double   sum=0.0;  /* temporary real */
    
  for(i=0; i<n; i++)
    sum+=u[i]*v[i];
  return sum;
} /* DotProd */


/*
 * MatrixVecProd computes the matrix product between a matrix and a vector of
 * the dimension n. The result is found in res.
 *
 */

void MatrixVecProd(const int n, double **A, double *v, double *res) {

  int    i, j;        /* counters       */

  for(i=0; i<n; i++) {
    res[i]=0.0;
    for(j=0; j<n; j++) 
      res[i]+=A[i][j]*v[j];
  } /* for i */
} /* MatrixVecProd */


/*
 * MatrixCopy copies the elements of the matrix A to the B.
 *
 */

void MatrixCopy(const int n, double **B, double **A) {

  int i, j;

  for(i=0; i<n; i++)
    for(j=0; j<n; j++)
      B[i][j]=A[i][j];
} /* MatrixCopy */


/*
 * L2VectorNorm computes the L2 or Eucleadian norm of a vector.
 *
 */

double L2VectorNorm(const int n, double *vec) {

  int    i;
  double norm=0.0;

  for(i=0; i<n; i++)
    norm+=vec[i]*vec[i];
  return sqrt(norm);
} /* L2VectorNorm */

/*
 * GSR is an implementation of the Gram-Schmidt Reorthonormalisation process, 
 * [2, pp. 246-250]. The n vectors are collected in a matrix, and the matrix is
 * both input and output. The implementation is actually the modified algorithm
 * as disucced in [2, p. 248]. Modified by the authors, so the vectors are 
 * normalised at the end.
 *
 */

void GSR(const int n, double **A) {

  int      i, j, k;     /* counters     */
  double   dot; 

  for(k=0; k<n; k++) {               /* orthogonalisation */
    for(j=(k+1); j<n; j++) {
      dot=0.0;                       /* dot product <Aj, Ak> */ 
      for(i=0; i<n; i++) 
	dot+=A[i][j]*A[i][k];
      for(i=0; i<n; i++)
	A[i][j]-=A[i][k]/dot;
    } /* for j */
  } /* for k */

  for(k=0; k<n; k++) {              /* normalisation */
    dot=0.0;                        /* Compute (L2 norm) */
    for(i=0; i<n; i++)
      dot+=A[i][k]*A[i][k];
    dot=sqrt(dot);
    if (dot==0.0) 
      SSLerror("Norm = 0 in routine GSR");
    for(i=0; i<n; i++)
      A[i][k]/=dot;
  } /* for k */
} /* GSR */


/*
 * InversMatrix calculates the inverse matrix. The method is the solution
 * of n linear set of equations which are solved by a LU factorisation.
 *
 */

void InversMatrix(const int n, double **b, double **ib) {

  double  **a;
  double   *e;
  int	    i,j;
  int  	   *p;

  a=MatrixAlloc(n);
  e=VectorAlloc(n);
  p=IntVectorAlloc(n);
  MatrixCopy(n, a, b);
  LUfact(n, a, p);
  for(i=0; i<n; i++) {
    for(j=0; j<n; j++)
      e[j]=0.0;
    e[i]=1.0;
    LUsubst(n, a, p, e);
    for(j=0; j<n; j++)
      ib[j][i]=e[j];
  } /* for i=1..n */

  MatrixFree(n, a);
  VectorFree(n, e);
  IntVectorFree(n, p);
} /* InversMatrix */

