/*
 * eigen.c - calculation of eigen values and vectors.
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
 * Eigen is a library for computing eigenvalues and eigenvectors of general
 * matrices. There is only one routine exported, namely Eigen.
 *
 * The meaning of the arguments to Eigen is:
 *   1.   The dimension of the general matrix (n).
 *   2.   A general matrix (A).   
 *   3.   The maximal number of iterations.
 *   4.   The precision.
 *   5.   A vector with the eigenvalues.
 *   6.   A matrix with the eigenvectors.
 *
 */

#include "complex.h"
#include "matrix.h"
#include <math.h>
#include <stdio.h>

void BlockCheck(double **A, int n, int i, int *block, double epsx) {

  /* block == 1 <=> TRUE, block == 0 <=> FALSE */

  if (i==n)
    *block=0;
  else {
    if ((fabs(A[i-1][i]-A[i][i-1])>epsx) && 
	(fabs(A[i-1][i-1]-A[i][i])<=epsx))
      *block=1;
    else
      *block=0;
  } /* else */
} /* BlockCheck */


void PrintEigen(int n, double **A, double **B, double eps, FILE *outfile) {

  int     i, j;
  int     block;

  fprintf(outfile, "\nEigenvalues:\t\t\tRe\t\t\tIm\n");
  i=1;
  do {
    BlockCheck(A, n, i, &block, eps);
    if (block==1) {
      fprintf(outfile, "\t\t\t\t%e\t\t%e\n", A[i-1][i-1], A[i-1][i]);
      fprintf(outfile, "\t\t\t\t%e\t\t%e\n", A[i][i], A[i][i-1]);
      i+=2;
    } else {
      fprintf(outfile, "\t\t\t\t%e\t\t%e\n", A[i-1][i-1], 0.0);
      i++;
    } /* if else */
  } while (i!=(n+1));
  fprintf(outfile, "\nEigenvectors:\t\t\tRe\t\t\tIm\n");
  i=1;
  do {
    BlockCheck(A, n, i, &block, eps);
    if (block==1) {
      for(j=1; j<=n; j++) 
	fprintf(outfile, "\t\t\t\t%e\t\t%e\n", B[j-1][i-1], B[j-1][i]);
      fprintf(outfile, "\n");
      for(j=1; j<=n; j++)
	fprintf(outfile, "\t\t\t\t%e\t\t%e\n", B[j-1][i-1], -B[j-1][i]);
      fprintf(outfile, "\n");
      i+=2;
    } else {
      for(j=1; j<=n; j++) 
	fprintf(outfile, "\t\t\t\t%e\t\t%e\n", B[j-1][i-1], 0.0);
      fprintf(outfile, "\n");
      i++;
    } /* if else */
  } while (i!=(n+1));
} /* PrintEigen */


void NormalizingMatrix(int n, double **A, int fixedref, int *ref, 
		       double **V, double eps) {

  int      j, col, block;
  SSL_Complex  c1, c2, c3;
  double   cd1, cd2, sqrnorm, norm, normi, max;

  col=1;
  do {
    if (fixedref==0) {
      *ref=1;
      SSL_ComplexAssign(V[*ref-1][col-1], V[*ref-1][col], &c1);
      max=SSL_ComplexNorm(c1);
      for(j=2; j<=n; j++) {
	SSL_ComplexAssign(V[j-1][col-1], V[j-1][col], &c2);
	sqrnorm=SSL_ComplexNorm(c2);
	if (sqrnorm>max) {
	  *ref=j;
	  max=sqrnorm;
	} /* if */
      } /* for j */
    } /* if fixedref */
    BlockCheck(A, n, col, &block, eps);
    if (block==1) {
      SSL_ComplexAssign(V[*ref-1][col-1], V[*ref-1][col], &c1);
      for(j=1; j<=n; j++) {
	SSL_ComplexAssign(V[j-1][col-1], V[j-1][col], &c2);
	SSL_ComplexDiv(c2, c1, &c3);
	V[j-1][col-1]=c3.re;
	V[j-1][col]=c3.im;
      } /* for j */
      col+=2;
    } /* if */
    else {
      norm=fabs(V[*ref-1][col-1]);
      if (norm!=0.0)
	for(j=1; j<=n; j++)
	  V[j-1][col-1]/=norm;
      col++;
    } /* else */
  } while (col<=n);
} /* NormalizingMatrix */

void Permutation(int n, double **P, double **A, double **B, int colon,
		 double eps) {

  int      *nr;
  int      block, OK;
  double   max, y, x;
  int      im, j, ki, u, v, i, k, ii;
  double   **AA;

  nr=IntVectorAlloc(n);
  AA=MatrixAlloc(n);

  MatrixCopy(n, AA, A);
  for(i=1; i<=n; i++) {
    nr[i-1]=i;
    for(k=1; k<=n; k++)
      P[i-1][k-1]=0.0;
  } /* for i */
  i=ii=ki=1;
  while (i<n) {
    BlockCheck(A, n, i, &block, eps);
    if (block==1) {
      A[i][i]=A[i-1][i-1];
      AA[i][i]=AA[i-1][i-1];
      if (A[i-1][i]>0.0) {
	A[i][i-1]=A[i-1][i];
	A[i-1][i]=-A[i][i-1];
	AA[i][i-1]=AA[i-1][i];
	AA[i-1][i]=-AA[i][i-1];
	for(j=1; j<=n; j++) 
	  B[j-1][i]=-B[j-1][i];
      } else {
	A[i][i-1]=-A[i-1][i];
	AA[i][i-1]=-AA[i-1][i];
      } /* else */
      j=i;
      for(k=ii; k<=(ii+1); k++) {
	x=AA[k-1][k-1];
	AA[k-1][k-1]=A[j-1][j-1];
	AA[j-1][j-1]=x;
	u=nr[k-1];
	nr[k-1]=nr[j-1];
	nr[j-1]=u;
	j++;
      } /* for k */
      if (ii>1) {
	if (AA[ii-1][ii-1]>AA[0][0]) {
	  j=ii;
	  for(k=1; k<=2; k++) {
	    x=AA[k-1][k-1];
	    AA[k-1][k-1]=A[j-1][j-1];
	    AA[j-1][j-1]=x;
	    u=nr[k-1];
	    nr[k-1]=nr[j-1];
	    nr[j-1]=u;
	    j++;
	  } /* for k */
	} /* if */
      } /* if */
      ki=i;
      i+=2;
      ii+=2;
    } /* if */
    else
      i++;
  } /* while */

  if (n>3) {
    do {
      im=ii;
      i=ii;
      max=AA[im-1][im-1];
      do {
	i++;
	if (AA[i-1][i-1]>max) {
	  im=i;
	  max=AA[i-1][i-1];
	} /* if */
      } while (i<n);
      if (im>ii) {
	x=AA[ii-1][ii-1];
	u=nr[ii-1];
	AA[ii-1][ii-1]=max;
	nr[ii-1]=nr[im-1];
	AA[im-1][im-1]=x;
	nr[im-1]=u;
      } /* if */
      ii++;
    } while (ii<n);
  } /* if */
  for(i=1; i<=n; i++) {
    if (colon==1)
      P[nr[i-1]-1][i-1]=1.0;
    else
      P[i-1][nr[i-1]-1]=1.0;
  } /* for i */

  MatrixFree(n, AA);
  IntVectorFree(n, nr);
} /* Permutation */


void Swap(int n, double **A, double **B, double epsx) {

  double **PR, **PS;
  double **temp;
  int      i, j;
  
  PR=MatrixAlloc(n);
  PS=MatrixAlloc(n);
  temp=MatrixAlloc(n);

  Permutation(n, PS, A, B, 1, epsx);
  MatrixMul(n, temp, B, PS);
  MatrixCopy(n, B, temp);
  Transpose(n, PR, PS);
  MatrixMul(n, temp, PR, A);
  MatrixCopy(n, A, temp);
  MatrixMul(n, temp, A, PS);
  MatrixCopy(n, A, temp);

  MatrixFree(n, PR);
  MatrixFree(n, PS);
  MatrixFree(n, temp);
} /* Swap */

void Balance(int n, int b, double **a, int *low, int *hi, double *d) {

  int     i, j, k, l;
  double  b2, c, f, g, r, s;
  int     noconv;

  b2=b*b;
  l=1;
  k=n;

  L110:
  for(j=k; j>=1; j--) {
    r=0.0;
    for(i=1; i<=(j-1); i++) 
      r+=fabs(a[j-1][i-1]);
    for(i=(j+1); i<=k; i++)
      r+=fabs(a[j-1][i-1]);
    if (r==0.0) {
      d[k-1]=(double)j; 
      if (j!=k) { 
	for(i=1; i<=k; i++) { 
	  f=a[i-1][j-1]; 
	  a[i-1][j-1]=a[i-1][k-1]; 
	  a[i-1][k-1]=f; 
	} 
	for(i=l; i<=n; i++) { 
	  f=a[j-1][i-1]; 
	  a[j-1][i-1]=a[k-1][i-1]; 
	  a[k-1][i-1]=f; 
	}
      }
      k--;
      goto L110;
    } /* if */
  } /* for j */

  L120:
  for(j=l; j<=k; j++) {
    c=0.0;
    for (i=l; i<=(j-1); i++)
      c+=fabs(a[i-1][j-1]);
    for(i=(j+1); i<=k; i++)
      c+=fabs(a[i-1][j-1]);
    if (c==0.0) {
      d[l-1]=(double)j; 
      if (j!=l) { 
	for(i=1; i<=k; i++) { 
	  f=a[i-1][j-1]; 
	  a[i-1][j-1]=a[i-1][l-1]; 
	  a[i-1][l-1]=f; 
	} 
	for(i=l; i<=n; i++) { 
	  f=a[j-1][i-1]; 
	  a[j-1][i-1]=a[l-1][i-1]; 
	  a[l-1][i-1]=f; 
	} 
      }
      l++;
      goto L120;
    } /* if */
  } /* for j */

  *low=l;
  *hi=k;
  for(i=l; i<=k; i++)
    d[i-1]=1.0;

  L130:
  noconv=0;
  for(i=l; i<=k; i++) {
    r=c=0.0;
    for(j=l; j<=(i-1); j++) {
      c+=fabs(a[j-1][i-1]);
      r+=fabs(a[i-1][j-1]);
    } /* for j */
    for(j=(i+1); j<=k; j++) {
      c+=fabs(a[j-1][i-1]);
      r+=fabs(a[i-1][j-1]);
    } /* for j */
    g=r/((double) b);
    f=1.0;
    s=c+r;

    L140:
    if (c<g) {
      f*=(double) b;
      c*=(double) b2;
      goto L140;
    } /* if */
    g=r*((double) b);
    
    L150:
    if (c>=g) {
      f/=(double) b;
      c/=(double) b2;
      goto L150;
    } /* if */

    if ((c+r)/f<(0.95*s)) {
      g=1.0/f;
      d[i-1]*=f;
      noconv=1;
      for(j=l; j<=n; j++) 
	a[i-1][j-1]*=g;
      for(j=1; j<=k; j++)
	a[j-1][i-1]*=f;
    } /* if */
  } /* for i */
  if (noconv==1)
    goto L130;
} /* Balance */

void BalBak(int n, int low, int hi, int m, double **z, double *d) {

  int     i, j, k;
  double  s;

  for(i=low; i<=hi; i++) {
    s=d[i-1];
    for(j=1; j<=m; j++) 
      z[i-1][j-1]*=s;
  } /* for i */
  for(i=(low-1); i>=1; i--) {
    k=(int)floor(d[i-1]+0.5);
    if (k!=i)
      for(j=1; j<=m; j++) {
	s=z[i-1][j-1];
	z[i-1][j-1]=z[k-1][j-1];
	z[k-1][j-1]=s;
      } /* for j */
  } /* for i */
  for(i=(hi+1); i<=n; i++) {
    k=(int)floor(d[i-1]+0.5);
    if (k!=i)
      for(j=1; j<=m; j++) {
	s=z[i-1][j-1];
	z[i-1][j-1]=z[k-1][j-1];
	z[k-1][j-1]=s;
      } /* for j */
  } /* for i */
} /* BalBak */

void Elmhes(int n, int k, int l, double **a, int *index) {

  int    i, j, la, m;
  double x, y;

  la=l-1;
  for(m=(k+1); m<=la; m++) {
    i=m;
    x=0.0;
    for(j=m; j<=l; j++) 
      if (fabs(a[j-1][m-2])>fabs(x)) {
	x=a[j-1][m-2];
	i=j;
      } /* if */
      index[m-1]=i;
      if (i!=m) {
	for(j=(m-1); j<=n; j++) {
	  y=a[i-1][j-1];
	  a[i-1][j-1]=a[m-1][j-1];
	  a[m-1][j-1]=y;
	} /* for j */
	for(j=1; j<=l; j++) {
	  y=a[j-1][i-1];
	  a[j-1][i-1]=a[j-1][m-1];
	  a[j-1][m-1]=y;
	} /* for j */
      } /* if */
      if (x!=0.0) 
	for(i=(m+1); i<=l; i++) {
	  y=a[i-1][m-2];
	  if (y!=0.0) {
	    a[i-1][m-2]=y/x;
	    y/=x;
	    for(j=m; j<=n; j++)
	      a[i-1][j-1]-=y*a[m-1][j-1];
	    for(j=1; j<=l; j++)
	      a[j-1][m-1]+=y*a[j-1][i-1];
	  } /* if */
	} /* for i */
  } /* for m */
} /* Elmhes */

void Elmtrans(int n, int low, int upp, double **h, int *index, 
	      double **v) {

  int   i, j, k;

  for(i=1; i<=n; i++) {
    for(j=1; j<=n; j++)
      v[i-1][j-1]=0.0;
    v[i-1][i-1]=1.0;
  } /* for i */
  for(i=(upp-1); i>=(low+1); i--) {
    j=index[i-1];
    for(k=(i+1); k<=upp; k++)
      v[k-1][i-1]=h[k-1][i-2];
    if (i!=j) {
      for(k=i; k<=upp; k++) {
	v[i-1][k-1]=v[j-1][k-1];
	v[j-1][k-1]=0.0;
      } /* for k */
      v[j-1][i-1]=1.0;
    } /* if */
  } /* for i */
} /* Elmtrans */


void hqr2(int n, int low, int upp, int maxits, double macheps,
          double **h, double **vecs, double *wr,
          double *wi, int *cnt, int *fail) {

  int     i, j, k, l, m, na, its, en, dummy;
  double  p, q, r, s, t, w, x, y, z, ra, sa, vr, vi, norm;
  int     notlast;
  SSL_Complex c1, c2, c3;

  *fail=0;
  for(i=1; i<=(low-1); i++) {
    wr[i-1]=h[i-1][i-1];
    wi[i-1]=0.0;
    cnt[i-1]=0;
  } /* for i */
  for(i=(upp+1); i<=n; i++) {
    wr[i-1]=h[i-1][i-1];
    wi[i-1]=0.0;
    cnt[i-1]=0;
  } /* for i */
  en=upp;
  t=0.0;

  L210:
  if (en<low)
    goto L260;
  its=0;
  na=en-1;

  L220:
  for(l=en; l>=(low+1); l--)
    if (fabs(h[l-1][l-2])<=
	macheps*(fabs(h[l-2][l-2])+fabs(h[l-1][l-1])))
      goto L231;
  l=low;

  L231:
  x=h[en-1][en-1];
  if (l==en)
    goto L240;
  y=h[na-1][na-1];
  w=h[en-1][na-1]*h[na-1][en-1];
  if (l==na)
    goto L250;
  if (its==maxits) {
    cnt[en-1]=maxits+1;
    *fail=1;
    goto L270;
  } /* if */
  if ((its % 10)==0) {
    t+=x;
    for(i=low; i<=en; i++)
      h[i-1][i-1]-=x;
    s=fabs(h[en-1][na-1])+fabs(h[na-1][en-3]);
    y=0.75*s;
    x=y;
    w=-0.4375*s*s;
  } /* if */
  its++;

  for(m=(en-2); m>=l; m--) {
    z=h[m-1][m-1];
    r=x-z;
    s=y-z;
    p=(r*s-w)/h[m][m-1]+h[m-1][m];
    q=h[m][m]-z-r-s;
    r=h[m+1][m];
    s=fabs(p)+fabs(q)+fabs(r);
    p/=s;
    q/=s;
    r/=s;
    if (m==1)
      goto L232;
    if ((fabs(h[m-1][m-2])*(fabs(q)+fabs(r)))<=
	(macheps*fabs(p)*(fabs(h[m-2][m-2])+fabs(z)+fabs(h[m][m]))))
      goto L232;
  } /* for m */

  L232:
  for(i=(m+2); i<=en; i++)
    h[i-1][i-3]=0.0;
  for(i=(m+3); i<=en; i++)
    h[i-1][i-4]=0.0;

  for(k=m; k<=na; k++) {
    if (k!=na)
      notlast=1;
    else
      notlast=0;
    if (k!=m) {
      p=h[k-1][k-2];
      q=h[k][k-2];
      if (notlast==1) 
	r=h[k+1][k-2];
      else
	r=0.0;
      x=fabs(p)+fabs(q)+fabs(r);
      if (x==0.0) 
	goto L233;
      p/=x;
      q/=x;
      r/=x;
    } /* if */
    s=sqrt(p*p+q*q+r*r);
    if (p<0)
      s=-s;
    if (k!=m)
      h[k-1][k-2]=-s*x;
    else
      if (l!=m)
	h[k-1][k-2]=-h[k-1][k-2];
    p+=s;
    x=p/s;
    y=q/s;
    z=r/s;
    q/=p;
    r/=p;

    for(j=k; j<=n; j++) {
      p=h[k-1][j-1]+q*h[k][j-1];
      if (notlast==1) {
	p+=r*h[k+1][j-1];
	h[k][j-1]-=p*z;
      } /* if */
      h[k][j-1]-=p*y;
      h[k-1][j-1]-=p*x;
    } /* for j */
    if ((k+3)<en)
      j=k+3;
    else
      j=en;
    
    for(i=1; i<=j; i++) {
      p=x*h[i-1][k-1]+y*h[i-1][k];
      if (notlast==1) {
	p+=z*h[i-1][k+1];
	h[i-1][k+1]-=p*r;
      } /* if */
      h[i-1][k]-=p*q;
      h[i-1][k-1]-=p;
    } /* for i */

    for(i=low; i<=upp; i++) {
      p=x*vecs[i-1][k-1]+y*vecs[i-1][k];
      if (notlast==1) {
	p+=z*vecs[i-1][k+1];
	vecs[i-1][k+1]-=p*r;
      } /* if */
      vecs[i-1][k]-=p*q;
      vecs[i-1][k-1]-=p;
    } /* for i */

  L233:
    dummy=0; 
  } /* for k */
  goto L220;
  
  L240:
  h[en-1][en-1]=x+t;
  wr[en-1]=h[en-1][en-1];
  wi[en-1]=0.0;
  cnt[en-1]=its;
  en=na;
  goto L210;

  L250:
  p=0.5*(y-x);
  q=p*p+w;
  z=sqrt(fabs(q));
  h[en-1][en-1]=x+t;
  x=h[en-1][en-1];
  h[na-1][na-1]=y+t;
  cnt[en-1]=-its;
  cnt[na-1]=its;
  if (q>0.0) {
    if (p<0.0)
      z=p-z;
    else
      z+=p;
    wr[na-1]=x+z;
    s=x-w/z;
    wr[en-1]=s;
    wi[na-1]=0.0;
    wi[en-1]=0.0;
    x=h[en-1][na-1];
    r=sqrt(x*x+z*z);
    p=x/r;
    q=z/r;
    for(j=na; j<=n; j++) {
      z=h[na-1][j-1];
      h[na-1][j-1]=q*z+p*h[en-1][j-1];
      
      /* h[en-1][j-1]=q*h[en-1][j-1]-p*z */
      h[en-1][j-1]*=q; 
      h[en-1][j-1]-=p*z; 
    } /* for j */
    for(i=1; i<=en; i++) {
      z=h[i-1][na-1];
      h[i-1][na-1]=q*z+p*h[i-1][en-1];

      /* h[i-1][en-1]=q*h[i-1][en-1]-p*z */
      h[i-1][en-1]*=q; 
      h[i-1][en-1]-=p*z; 
    } /* for i */
    for(i=low; i<=upp; i++) {
      z=vecs[i-1][na-1];
      vecs[i-1][na-1]=q*z+p*vecs[i-1][en-1];

      /* vecs[i-1][en-1]=q*vecs[i-1][en-1]-p*z */
      vecs[i-1][en-1]*=q; 
      vecs[i-1][en-1]-=p*z; 
    } /* for i */
  } /* if */
  else {
    wr[na-1]=x+p;
    wr[en-1]=x+p;
    wi[na-1]=z;
    wi[en-1]=-z;
  } /* else */
  en-=2;
  goto L210;

  L260:
  norm=0.0;
  k=1;
  for(i=1; i<=n; i++) {
    for(j=k; j<=n; j++)
      norm+=fabs(h[i-1][j-1]);
    k=i;
  } /* for i */

  for(en=n; en>=1; en--) {
    p=wr[en-1];
    q=wi[en-1];
    na=en-1;
    if (q==0.0) {
      m=en;
      h[en-1][en-1]=1.0;
      for(i=na; i>=1; i--) {
	w=h[i-1][i-1]-p;
	r=h[i-1][en-1];
	for(j=m; j<=na; j++)
	  r+=h[i-1][j-1]*h[j-1][en-1];
	if (wi[i-1]<0.0) {
	  z=w;
	  s=r;
	} /* if */
	else {
	  m=i;
	  if (wi[i-1]==0.0)  {
	    if (w!=0.0)
	      h[i-1][en-1]=-r/w;
	    else
	      h[i-1][en-1]=-r/macheps/norm;
	  } else {
	    x=h[i-1][i];
	    y=h[i][i-1];
	    q=pow(wr[i-1]-p, 2.0)+wi[i-1]*wi[i-1];
	    t=(x*s-z*r)/q;
	    h[i-1][en-1]=t;
	    if (fabs(x)>fabs(z))
	      h[i][en-1]=(-r-w*t)/x;
	    else
	      h[i][en-1]=(-s-y*t)/z;
	  } /* else */
	} /* else */
      } /* i */
    } else
      if (q<0.0) {
	m=na;
	if (fabs(h[en-1][na-1])>fabs(h[na-1][en-1])) {
	  h[na-1][na-1]=-(h[en-1][en-1]-p)/h[en-1][na-1];
	  h[na-1][en-1]=-q/h[en-1][na-1];
	} /* if */ 
	else {
	  SSL_ComplexAssign(-h[na-1][en-1], 0.0, &c1);
	  SSL_ComplexAssign(h[na-1][na-1]-p, q, &c2);
	  SSL_ComplexDiv(c1, c2, &c3);
	  h[na-1][na-1]=c3.re;
	  h[na-1][en-1]=c3.im;
	} /* else */
	h[en-1][na-1]=1.0;
	h[en-1][en-1]=0.0;
	for(i=(na-1); i>=1; i--) {
	  w=h[i-1][i-1]-p;
	  ra=h[i-1][en-1];
	  sa=0.0;
	  for(j=m; j<=na; j++) {
	    ra+=h[i-1][j-1]*h[j-1][na-1];
	    sa+=h[i-1][j-1]*h[j-1][en-1];
	  } /* for j */
	  if (wi[i-1]<0.0) {
	    z=w;
	    r=ra;
	    s=sa;
	  } /* if */
	  else {
	    m=i;
	    if (wi[i-1]==0.0) {
	      SSL_ComplexAssign(-ra, -sa, &c1);
	      SSL_ComplexAssign(w, q, &c2);
	      SSL_ComplexDiv(c1, c2, &c3);
	      h[i-1][na-1]=c3.re;
	      h[i-1][en-1]=c3.im;
	    } /* if */
	    else {
	      x=h[i-1][i];
	      y=h[i][i-1];
	      vr=pow(wr[i-1]-p, 2.0)+wi[i-1]*wi[i-1]-q*q;
	      vi=(wr[i-1]-p)*2.0*q;
	      if ((vr==0.0) && (vi==0.0)) 
		vr=macheps*norm*(fabs(w)+fabs(q)+fabs(x)+fabs(y)+fabs(z));
	      SSL_ComplexAssign(x*r-z*ra+q*sa, x*s-z*sa-q*ra, &c1);
	      SSL_ComplexAssign(vr, vi, &c2);
	      SSL_ComplexDiv(c1, c2, &c3);
	      h[i-1][na-1]=c3.re;
	      h[i-1][en-1]=c3.im;
	      if (fabs(x)>(fabs(z)+fabs(q))) {
		h[i][na-1]=(-ra-w*h[i-1][na-1]+q*h[i-1][en-1])/x;
		h[i][en-1]=(-sa-w*h[i-1][en-1]-q*h[i-1][na-1])/x;
	      } /* if */
	      else {
		SSL_ComplexAssign(-r-y*h[i-1][na-1], -s-y*h[i-1][en-1],
			      &c1);
		SSL_ComplexAssign(z, q, &c2);
		SSL_ComplexDiv(c1, c2, &c3);
		h[i][na-1]=c3.re;
		h[i][en-1]=c3.im;
	      } /* else */
	    } /* else */
	  } /* else */
	} /* for i */
      } /* if */
  } /* for en */

  for(i=1; i<=(low-1); i++)
    for(j=(i+1); j<=n; j++)
      vecs[i-1][j-1]=h[i-1][j-1];
  for(i=(upp+1); i<=n; i++)
    for(j=(i+1); j<=n; j++) 
      vecs[i-1][j-1]=h[i-1][j-1];
  
  for(j=n; j>=low; j--) {
    if (j<=upp)
      m=j;
    else
      m=upp;
    l=j-1;
    if (wi[j-1]<0.0) {
      for(i=low; i<=upp; i++) {
	y=z=0.0;
	for(k=low; k<=m; k++) {
	  y+=vecs[i-1][k-1]*h[k-1][l-1];
	  z+=vecs[i-1][k-1]*h[k-1][j-1];
	} /* for k */
	vecs[i-1][l-1]=y;
	vecs[i-1][j-1]=z;
      } /* for i */
    } /* if */
    else
      if (wi[j-1]==0.0)
	for(i=low; i<=upp; i++) {
	  z=0.0;
	  for(k=low; k<=m; k++)
	    z+=vecs[i-1][k-1]*h[k-1][j-1];
	  vecs[i-1][j-1]=z;
	} /* for i */
  } /* for j */
  
 L270:
  dummy=0;
} /* hqr2 */
	  
void Eigen(int n, int ref, double **AJAC, int maxit, double eps, 
	   int fixedref, SSL_Complex *values, SSL_Complex **vectors) {

  double  *wr, *wi, *bald, **T, **A;
  int     i, j, ballow, balhi, max, block;
  int     *intout;
  int     fail;

  intout=IntVectorAlloc(n);
  wr=VectorAlloc(n);
  wi=VectorAlloc(n);
  bald=VectorAlloc(n);
  T=MatrixAlloc(n);
  A=MatrixAlloc(n);

  for(i=1; i<=n; i++)
    for(j=1; j<=n; j++)
      A[i-1][j-1]=AJAC[i-1][j-1];

  Balance(n, 10, A, &ballow, &balhi, bald);
  Elmhes(n, ballow, balhi, A, intout);
  Elmtrans(n, ballow, balhi, A, intout, T);

  hqr2(n, ballow, balhi, maxit, eps, A, T, wr, wi, intout, &fail);
  if (fail==1) 
    (void) fprintf(stderr, "Failure in hqr2 function. Do not trust the given eigenvectors and -values\n");
  /*
  tmxx=0;
  for(i=1; i<=n; i++)
    if (abs(intout[i-1])>tmxx)
      tmxx=(int)ceil(abs(intout[i-1]));
  */
  for(i=1; i<=n; i++)
    for(j=1; j<=n; j++)
      A[i-1][j-1]=0.0;
  i=1;
  do {
    if (wi[i-1]!=0.0) {
      A[i-1][i-1]=wr[i-1];
      A[i][i]=wr[i-1];
      A[i-1][i]=wi[i-1];
      A[i][i-1]=wi[i];
      i+=2;
    } /* if */
    else {
      A[i-1][i-1]=wr[i-1];
      i++;
    } /* else */
  } while (i<n);
  if (i==n)
    A[i-1][i-1]=wr[i-1];
 
  Swap(n, A, T, eps);
  BalBak(n, ballow, balhi, n, T, bald);
  NormalizingMatrix(n, A, fixedref, &ref, T, eps);

  /* store eigenvectors and eigenvalues nicely */
  i=1;              /* eigenvalues */
  do {
    BlockCheck(A, n, i, &block, eps);
    if (block==1) {
      SSL_ComplexAssign(A[i-1][i-1], A[i-1][i], &values[i-1]);
      SSL_ComplexAssign(A[i][i], A[i][i-1], &values[i]);
      i+=2;
    } else {
      SSL_ComplexAssign(A[i-1][i-1], 0.0, &values[i-1]);
      i++;
    } /* if else */
  } while (i!=(n+1));
  i=1;               /* eigenvectors */
  do {
    BlockCheck(A, n, i, &block, eps);
    if (block==1) {
      for(j=1; j<=n; j++)
	SSL_ComplexAssign(T[j-1][i-1], T[j-1][i], &vectors[i-1][j-1]);
      for(j=1; j<=n; j++)
	SSL_ComplexAssign(T[j-1][i-1], -T[j-1][i], &vectors[i][j-1]);
      i+=2;
    } else {
      for(j=1; j<=n; j++)
	SSL_ComplexAssign(T[j-1][i-1], 0.0, &vectors[i-1][j-1]);
      i++;
    } /* if else */
  } while (i!=(n+1));

  VectorFree(n, wi);
  VectorFree(n, wr);
  VectorFree(n, bald);
  IntVectorFree(n, intout);
  MatrixFree(n, A);
  MatrixFree(n, T);
} /* Eigen */





