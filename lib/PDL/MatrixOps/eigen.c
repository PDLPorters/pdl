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

#include <complex.h>
#include "matrix.h"
#include <math.h>
#include <stdio.h>

void BlockCheck(double **A, int n, int i, int *block, double epsx) {

  /* block == 1 <=> TRUE, block == 0 <=> FALSE */

  if (i>=n-1)
    *block=0;
  else {
    if ((fabs(A[i][i+1]-A[i+1][i])>epsx) &&
	(fabs(A[i][i]-A[i+1][i+1])<=epsx))
      *block=1;
    else
      *block=0;
  } /* else */
} /* BlockCheck */


void PrintEigen(int n, double **A, double **B, double eps, FILE *outfile) {

  int     i, j;
  int     block;

  fprintf(outfile, "\nEigenvalues:\t\t\tRe\t\t\tIm\n");
  i=0;
  do {
    BlockCheck(A, n, i, &block, eps);
    if (block==1) {
      fprintf(outfile, "\t\t\t\t%e\t\t%e\n", A[i][i], A[i][i+1]);
      fprintf(outfile, "\t\t\t\t%e\t\t%e\n", A[i+1][i+1], A[i+1][i]);
      i+=2;
    } else {
      fprintf(outfile, "\t\t\t\t%e\t\t%e\n", A[i][i], 0.0);
      i++;
    } /* if else */
  } while (i<n);
  fprintf(outfile, "\nEigenvectors:\t\t\tRe\t\t\tIm\n");
  i=0;
  do {
    BlockCheck(A, n, i, &block, eps);
    if (block==1) {
      for(j=0; j<n; j++)
	fprintf(outfile, "\t\t\t\t%e\t\t%e\n", B[j][i], B[j][i+1]);
      fprintf(outfile, "\n");
      for(j=0; j<n; j++)
	fprintf(outfile, "\t\t\t\t%e\t\t%e\n", B[j][i], -B[j][i+1]);
      fprintf(outfile, "\n");
      i+=2;
    } else {
      for(j=0; j<n; j++)
	fprintf(outfile, "\t\t\t\t%e\t\t%e\n", B[j][i], 0.0);
      fprintf(outfile, "\n");
      i++;
    } /* if else */
  } while (i<n);
} /* PrintEigen */


void NormalizingMatrix(int n, double **A,
		       double **V, double eps) {

  int      j, col=0, block;

  do {
    double sumsq = 0;
    BlockCheck(A, n, col, &block, eps);
    for(j=0; j<n; j++)
      sumsq += (V[j][col] * V[j][col]) + (block==1 ? (V[j][col+1] * V[j][col+1]) : 0);
    double norm = sqrt(sumsq);
    if (norm == 0.0) continue;
    if (block==1) {
      for(j=0; j<n; j++) {
	complex double c2 = V[j][col] + I * V[j][col+1], c3 = c2 / norm;
	V[j][col]=creal(c3);
	V[j][col+1]=cimag(c3);
      } /* for j */
      col+=2;
    } else {
      for(j=0; j<n; j++)
	V[j][col] /= norm;
      col++;
    }
  } while (col<n);
} /* NormalizingMatrix */

void Permutation(int n, double **P, double **A, double **B, int colon,
		 double eps) {

  int      *nr;
  int      block;
  double   max, x;
  int      im, j, u, i, k, ii;
  double   **AA;

  nr=IntVectorAlloc(n);
  AA=MatrixAlloc(n);

  MatrixCopy(n, AA, A);
  for(i=0; i<n; i++) {
    nr[i]=i;
    for(k=0; k<n; k++)
      P[i][k]=0.0;
  } /* for i */
  i=ii=0;
  while (i<n-1) {
    BlockCheck(A, n, i, &block, eps);
    if (block==1) {
      A[i+1][i+1]=A[i][i];
      AA[i+1][i+1]=AA[i][i];
      if (A[i][i+1]>0.0) {
	A[i+1][i]=A[i][i+1];
	A[i][i+1]=-A[i+1][i];
	AA[i+1][i]=AA[i][i+1];
	AA[i][i+1]=-AA[i+1][i];
	for(j=0; j<n; j++)
	  B[j][i+1]=-B[j][i+1];
      } else {
	A[i+1][i]=-A[i][i+1];
	AA[i+1][i]=-AA[i][i+1];
      } /* else */
      j=i;
      for(k=ii; k<ii; k++) {
	x=AA[k][k];
	AA[k][k]=A[j][j];
	AA[j][j]=x;
	u=nr[k];
	nr[k]=nr[j];
	nr[j]=u;
	j++;
      } /* for k */
      if (ii>0) {
	if (AA[ii][ii]>AA[0][0]) {
	  j=ii;
	  for(k=0; k<2; k++) {
	    x=AA[k][k];
	    AA[k][k]=A[j][j];
	    AA[j][j]=x;
	    u=nr[k];
	    nr[k]=nr[j];
	    nr[j]=u;
	    j++;
	  } /* for k */
	} /* if */
      } /* if */
      i+=2;
      ii+=2;
    } /* if */
    else
      i++;
  } /* while */

  if (n>3) {
    do {
      i=im=ii;
      max=AA[im][im];
      do {
	i++;
	if (AA[i][i]>max) {
	  im=i;
	  max=AA[i][i];
	} /* if */
      } while (i<n-1);
      if (im>ii) {
	x=AA[ii][ii];
	u=nr[ii];
	AA[ii][ii]=max;
	nr[ii]=nr[im];
	AA[im][im]=x;
	nr[im]=u;
      } /* if */
      ii++;
    } while (ii<n-1);
  } /* if */
  for(i=0; i<n; i++) {
    if (colon==1)
      P[nr[i]][i]=1.0;
    else
      P[i][nr[i]]=1.0;
  } /* for i */

  MatrixFree(n, AA);
  IntVectorFree(n, nr);
} /* Permutation */


void Swap(int n, double **A, double **B, double epsx) {

  double **PR, **PS;
  double **temp;

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
  l=0;
  k=n-1;

  L110:
  for(j=k; j>=0; j--) {
    r=0.0;
    for(i=0; i<j; i++)
      r+=fabs(a[j][i]);
    for(i=j+1; i<k+1; i++)
      r+=fabs(a[j][i]);
    if (r==0.0) {
      d[k]=(double)(j+1);
      if (j!=k) {
	for(i=0; i<k+1; i++) {
	  f=a[i][j];
	  a[i][j]=a[i][k];
	  a[i][k]=f;
	}
	for(i=l; i<n; i++) {
	  f=a[j][i];
	  a[j][i]=a[k][i];
	  a[k][i]=f;
	}
      }
      k--;
      goto L110;
    } /* if */
  } /* for j */

  L120:
  for(j=l; j<=k; j++) {
    c=0.0;
    for (i=l; i<=j; i++)
      c+=fabs(a[i][j]);
    for(i=(j+1); i<=k; i++)
      c+=fabs(a[i][j]);
    if (c==0.0) {
      d[l]=(double)(j+1);
      if (j!=l) {
	for(i=0; i<=k; i++) {
	  f=a[i][j];
	  a[i][j]=a[i][l];
	  a[i][l]=f;
	}
	for(i=l; i<n; i++) {
	  f=a[j][i];
	  a[j][i]=a[l][i];
	  a[l][i]=f;
	}
      }
      l++;
      goto L120;
    } /* if */
  } /* for j */

  *low=l;
  *hi=k;
  for(i=l; i<=k; i++)
    d[i]=1.0;

  L130:
  noconv=0;
  for(i=l; i<=k; i++) {
    r=c=0.0;
    for(j=l; j<i; j++) {
      c+=fabs(a[j][i]);
      r+=fabs(a[i][j]);
    } /* for j */
    for(j=i+1; j<=k; j++) {
      c+=fabs(a[j][i]);
      r+=fabs(a[i][j]);
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
      d[i]*=f;
      noconv=1;
      for(j=l; j<n; j++)
	a[i][j]*=g;
      for(j=0; j<=k; j++)
	a[j][i]*=f;
    } /* if */
  } /* for i */
  if (noconv==1)
    goto L130;
} /* Balance */

void BalBak(int n, int low, int hi, int m, double **z, double *d) {

  int     i, j, k;
  double  s;

  for(i=low; i<hi+1; i++) {
    s=d[i];
    for(j=0; j<m; j++)
      z[i][j]*=s;
  } /* for i */
  for(i=low-1; i>=0; i--) {
    k=(int)floor(d[i]+0.5)-1;
    if (k!=i)
      for(j=0; j<m; j++) {
	s=z[i][j];
	z[i][j]=z[k][j];
	z[k][j]=s;
      } /* for j */
  } /* for i */
  for(i=hi+1; i<n; i++) {
    k=(int)floor(d[i]+0.5)-1;
    if (k!=i)
      for(j=0; j<m; j++) {
	s=z[i][j];
	z[i][j]=z[k][j];
	z[k][j]=s;
      } /* for j */
  } /* for i */
} /* BalBak */

void Elmhes(int n, int k, int l, double **a, int *index) {

  int    i, j, la, m;
  double x, y;

  la=l;
  for(m=k+1; m<la; m++) {
    i=m;
    x=0.0;
    for(j=m; j<l+1; j++)
      if (fabs(a[j][m-1])>fabs(x)) {
	x=a[j][m-1];
	i=j-1;
      } /* if */
    index[m]=i+1;
    if (i!=m) {
      for(j=m-1; j<n; j++) {
        y=a[i][j];
        a[i][j]=a[m][j];
        a[m][j]=y;
      } /* for j */
      for(j=0; j<l+1; j++) {
        y=a[j][i];
        a[j][i]=a[j][m];
        a[j][m]=y;
      } /* for j */
    } /* if */
    if (x!=0.0)
      for(i=(m+1); i<l+1; i++) {
        y=a[i][m-1];
        if (y!=0.0) {
          a[i][m-1]=y/x;
          y/=x;
          for(j=m; j<n; j++)
            a[i][j]-=y*a[m][j];
          for(j=0; j<l+1; j++)
            a[j][m]+=y*a[j][i];
        } /* if */
      } /* for i */
  } /* for m */
} /* Elmhes */

void Elmtrans(int n, int low, int upp, double **h, int *index,
	      double **v) {

  int   i, j, k;

  for(i=0; i<n; i++) {
    for(j=0; j<n; j++)
      v[i][j]=0.0;
    v[i][i]=1.0;
  } /* for i */
  for(i=(upp-1); i>=low+1; i--) {
    j=index[i]-1;
    for(k=(i+1); k<upp+1; k++)
      v[k][i]=h[k][i-1];
    if (i!=j) {
      for(k=i; k<upp+1; k++) {
	v[i][k]=v[j][k];
	v[j][k]=0.0;
      } /* for k */
      v[j][i]=1.0;
    } /* if */
  } /* for i */
} /* Elmtrans */


void hqr2(int n, int low, int upp, int maxits, double macheps,
          double **h, double **vecs, double *wr,
          double *wi, int *cnt, int *fail) {

  int     i, j, k, l, m, na, its, en;
  double  p = 0, q = 0, r = 0, s = 0, t, w, x, y, z = 0, ra, sa, vr, vi, norm;
  int     notlast;
  complex double c1, c2, c3;

  *fail=0;
  for(i=0; i<low; i++) {
    wr[i]=h[i][i];
    wi[i]=0.0;
    cnt[i]=0;
  } /* for i */
  for(i=upp+1; i<n; i++) {
    wr[i]=h[i][i];
    wi[i]=0.0;
    cnt[i]=0;
  } /* for i */
  en=upp;
  t=0.0;

  L210:
  if (en<low)
    goto L260;
  its=0;
  na=en-1;

  L220:
  for(l=en; l>=low+1; l--)
    if (fabs(h[l][l-1])<=
	macheps*(fabs(h[l-1][l-1])+fabs(h[l][l])))
      goto L231;
  l=low+1;

  L231:
  x=h[en][en];
  if (l==en+1)
    goto L240;
  y=h[na][na];
  w=h[en][na]*h[na][en];
  if (l==na+1)
    goto L250;
  if (its==maxits) {
    cnt[en]=maxits+1;
    *fail=1;
    goto L270;
  } /* if */
  if ((its % 10)==0) {
    t+=x;
    for(i=low; i<en+1; i++)
      h[i][i]-=x;
    s=fabs(h[en][na])+fabs(h[na][en-2]);
    y=0.75*s;
    x=y;
    w=-0.4375*s*s;
  } /* if */
  its++;

  for(m=(en-2); m>=l-1; m--) {
    z=h[m][m];
    r=x-z;
    s=y-z;
    p=(r*s-w)/h[m+1][m]+h[m][m+1];
    q=h[m+1][m+1]-z-r-s;
    r=h[m+2][m+1];
    s=fabs(p)+fabs(q)+fabs(r);
    p/=s;
    q/=s;
    r/=s;
    if (m==0)
      goto L232;
    if ((fabs(h[m][m-1])*(fabs(q)+fabs(r)))<=
	(macheps*fabs(p)*(fabs(h[m-1][m-1])+fabs(z)+fabs(h[m+1][m+1]))))
      goto L232;
  } /* for m */

  L232:
  for(i=m+2; i<en+1; i++)
    h[i][i-2]=0.0;
  for(i=(m+3); i<en+1; i++)
    h[i][i-3]=0.0;

  for(k=m; k<na+1; k++) {
    if (k!=na)
      notlast=1;
    else
      notlast=0;
    if (k!=m) {
      p=h[k][k-1];
      q=h[k+1][k-1];
      if (notlast==1)
	r=h[k+2][k-1];
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
    if (k+1!=m+1)
      h[k][k-1]=-s*x;
    else
      if (l!=m+1)
	h[k][k-1]=-h[k][k-1];
    p+=s;
    x=p/s;
    y=q/s;
    z=r/s;
    q/=p;
    r/=p;

    for(j=k; j<n; j++) {
      p=h[k][j]+q*h[k+1][j];
      if (notlast==1) {
	p+=r*h[k+2][j];
	h[k+1][j]-=p*z;
      } /* if */
      h[k+1][j]-=p*y;
      h[k][j]-=p*x;
    } /* for j */
    if ((k+4)<en+1)
      j=k+4;
    else
      j=en+1;

    for(i=0; i<j; i++) {
      p=x*h[i][k]+y*h[i][k+1];
      if (notlast==1) {
	p+=z*h[i][k+2];
	h[i][k+2]-=p*r;
      } /* if */
      h[i][k+1]-=p*q;
      h[i][k]-=p;
    } /* for i */

    for(i=low; i<upp+1; i++) {
      p=x*vecs[i][k]+y*vecs[i][k+1];
      if (notlast==1) {
	p+=z*vecs[i][k+2];
	vecs[i][k+2]-=p*r;
      } /* if */
      vecs[i][k+1]-=p*q;
      vecs[i][k]-=p;
    } /* for i */

  L233:;
  } /* for k */
  goto L220;

  L240:
  h[en][en]=x+t;
  wr[en]=h[en][en];
  wi[en]=0.0;
  cnt[en]=its;
  en=na;
  goto L210;

  L250:
  p=0.5*(y-x);
  q=p*p+w;
  z=sqrt(fabs(q));
  h[en][en]=x+t;
  x=h[en][en];
  h[na][na]=y+t;
  cnt[en]=-its;
  cnt[na]=its;
  if (q>0.0) {
    if (p<0.0)
      z=p-z;
    else
      z+=p;
    wr[na]=x+z;
    s=x-w/z;
    wr[en]=s;
    wi[na]=0.0;
    wi[en]=0.0;
    x=h[en][na];
    r=sqrt(x*x+z*z);
    p=x/r;
    q=z/r;
    for(j=na; j<n; j++) {
      z=h[na][j];
      h[na][j]=q*z+p*h[en][j];

      /* h[en][j]=q*h[en][j]-p*z */
      h[en][j]*=q;
      h[en][j]-=p*z;
    } /* for j */
    for(i=0; i<en+1; i++) {
      z=h[i][na];
      h[i][na]=q*z+p*h[i][en];

      /* h[i][en]=q*h[i][en]-p*z */
      h[i][en]*=q;
      h[i][en]-=p*z;
    } /* for i */
    for(i=low; i<upp+1; i++) {
      z=vecs[i][na];
      vecs[i][na]=q*z+p*vecs[i][en];

      /* vecs[i][en]=q*vecs[i][en]-p*z */
      vecs[i][en]*=q;
      vecs[i][en]-=p*z;
    } /* for i */
  } /* if */
  else {
    wr[na]=x+p;
    wr[en]=x+p;
    wi[na]=z;
    wi[en]=-z;
  } /* else */
  en-=2;
  goto L210;

  L260:
  norm=0.0;
  k=0;
  for(i=0; i<n; i++) {
    for(j=k; j<n; j++)
      norm+=fabs(h[i][j]);
    k=i;
  } /* for i */

  for(en=n-1; en>=0; en--) {
    p=wr[en];
    q=wi[en];
    na=en-1;
    if (q==0.0) {
      m=en;
      h[en][en]=1.0;
      for(i=na; i>=0; i--) {
	w=h[i][i]-p;
	r=h[i][en];
	for(j=m; j<na+1; j++)
	  r+=h[i][j]*h[j][en];
	if (wi[i]<0.0) {
	  z=w;
	  s=r;
	} /* if */
	else {
	  m=i;
	  if (wi[i]==0.0)  {
	    if (w!=0.0)
	      h[i][en]=-r/w;
	    else
	      h[i][en]=-r/macheps/norm;
	  } else {
	    x=h[i][i+1];
	    y=h[i+1][i];
	    q=pow(wr[i]-p, 2.0)+wi[i]*wi[i];
	    t=(x*s-z*r)/q;
	    h[i][en]=t;
	    if (fabs(x)>fabs(z))
	      h[i+1][en]=(-r-w*t)/x;
	    else
	      h[i+1][en]=(-s-y*t)/z;
	  } /* else */
	} /* else */
      } /* i */
    } else
      if (q<0.0) {
	m=na;
	if (fabs(h[en][na])>fabs(h[na][en])) {
	  h[na][na]=-(h[en][en]-p)/h[en][na];
	  h[na][en]=-q/h[en][na];
	} /* if */
	else {
	  c1 = -h[na][en];
	  c2 = h[na][na]-p + I * q;
	  c3 = c1 / c2;
	  h[na][na]=creal(c3);
	  h[na][en]=cimag(c3);
	} /* else */
	h[en][na]=1.0;
	h[en][en]=0.0;
	for(i=na-1; i>=0; i--) {
	  w=h[i][i]-p;
	  ra=h[i][en];
	  sa=0.0;
	  for(j=m; j<na+1; j++) {
	    ra+=h[i][j]*h[j][na];
	    sa+=h[i][j]*h[j][en];
	  } /* for j */
	  if (wi[i]<0.0) {
	    z=w;
	    r=ra;
	    s=sa;
	  } /* if */
	  else {
	    m=i;
	    if (wi[i]==0.0) {
	      c1 = -ra + I * -sa;
	      c2 = w + I * q;
	      c3 = c1 / c2;
	      h[i][na]=creal(c3);
	      h[i][en]=cimag(c3);
	    } /* if */
	    else {
	      x=h[i][i+1];
	      y=h[i+1][i];
	      vr=pow(wr[i]-p, 2.0)+wi[i]*wi[i]-q*q;
	      vi=(wr[i]-p)*2.0*q;
	      if ((vr==0.0) && (vi==0.0))
		vr=macheps*norm*(fabs(w)+fabs(q)+fabs(x)+fabs(y)+fabs(z));
	      c1 = x*r-z*ra+q*sa + I * x*s-z*sa-q*ra;
	      c2 = vr + I * vi;
	      c3 = c1 / c2;
	      h[i][na]=creal(c3);
	      h[i][en]=cimag(c3);
	      if (fabs(x)>(fabs(z)+fabs(q))) {
		h[i+1][na]=(-ra-w*h[i][na]+q*h[i][en])/x;
		h[i+1][en]=(-sa-w*h[i][en]-q*h[i][na])/x;
	      } /* if */
	      else {
		c1 = -r-y*h[i][na] + I * -s-y*h[i][en];
		c2 = z + I * q;
		c3 = c1 / c2;
		h[i+1][na]=creal(c3);
		h[i+1][en]=cimag(c3);
	      } /* else */
	    } /* else */
	  } /* else */
	} /* for i */
      } /* if */
  } /* for en */

  for(i=0; i<low; i++)
    for(j=i+1; j<n; j++)
      vecs[i][j]=h[i][j];
  for(i=upp+1; i<n; i++)
    for(j=i-1; j<n; j++)
      vecs[i][j]=h[i][j];

  for(j=n-1; j>=low; j--) {
    if (j<=upp)
      m=j+1;
    else
      m=upp+1;
    l=j-1;
    if (wi[j]<0.0) {
      for(i=low; i<upp+1; i++) {
	y=z=0.0;
	for(k=low; k<m; k++) {
	  y+=vecs[i][k]*h[k][l];
	  z+=vecs[i][k]*h[k][j];
	} /* for k */
	vecs[i][l]=y;
	vecs[i][j]=z;
      } /* for i */
    } /* if */
    else
      if (wi[j]==0.0)
	for(i=low; i<upp+1; i++) {
	  z=0.0;
	  for(k=low; k<m; k++)
	    z+=vecs[i][k]*h[k][j];
	  vecs[i][j]=z;
	} /* for i */
  } /* for j */

 L270:;
} /* hqr2 */

char *Eigen(int n, double *AJAC, int maxit, double eps,
	   complex double *values, complex double *vectors) {

  double  *wr, *wi, *bald, **T, **A;
  int     i, j, ballow, balhi, block;
  int     *intout;
  int     fail;

  intout=IntVectorAlloc(n);
  wr=VectorAlloc(n);
  wi=VectorAlloc(n);
  bald=VectorAlloc(n);
  T=MatrixAlloc(n);
  A=MatrixAlloc(n);

  for(i=0; i<n; i++)
    for(j=0; j<n; j++)
      A[i][j]=AJAC[i*n + j];

  Balance(n, 10, A, &ballow, &balhi, bald);
  Elmhes(n, ballow, balhi, A, intout);
  Elmtrans(n, ballow, balhi, A, intout, T);

  hqr2(n, ballow, balhi, maxit, eps, A, T, wr, wi, intout, &fail);
  if (fail==1)
    return "Failure in hqr2 function";
  for(i=0; i<n; i++)
    for(j=0; j<n; j++)
      A[i][j]=0.0;
  i=0;
  do {
    if (wi[i]!=0.0) {
      A[i][i]=wr[i];
      A[i+1][i+1]=wr[i];
      A[i][i+1]=wi[i];
      A[i+1][i]=wi[i+1];
      i+=2;
    } /* if */
    else {
      A[i][i]=wr[i];
      i++;
    } /* else */
  } while (i<n-1);
  if (i==n-1)
    A[i][i]=wr[i];

  Swap(n, A, T, eps);
  BalBak(n, ballow, balhi, n, T, bald);
  NormalizingMatrix(n, A, T, eps);

  /* store eigenvectors and eigenvalues nicely */
  i=0;              /* eigenvalues */
  do {
    BlockCheck(A, n, i, &block, eps);
    if (block==1) {
      values[i] = A[i][i] + I * A[i][i+1];
      values[i+1] = A[i+1][i+1] + I * A[i+1][i];
      i+=2;
    } else {
      values[i] = A[i][i] + I * 0.0;
      i++;
    } /* if else */
  } while (i!=n);
  i=0;               /* eigenvectors */
  do {
    BlockCheck(A, n, i, &block, eps);
    if (block==1) {
      for(j=0; j<n; j++)
	vectors[j*n + i] = T[j][i] + I * T[j][i+1];
      for(j=0; j<n; j++)
	vectors[j*n + i+1] = T[j][i] - I * T[j][i+1];
      i+=2;
    } else {
      for(j=0; j<n; j++)
	vectors[j*n + i] = T[j][i];
      i++;
    } /* if else */
  } while (i!=n);

  VectorFree(n, wi);
  VectorFree(n, wr);
  VectorFree(n, bald);
  IntVectorFree(n, intout);
  MatrixFree(n, A);
  MatrixFree(n, T);
  return NULL;
} /* Eigen */
