/* From bryant@sioux.stanford.edu Sat Apr  3 14:57:54 1993
Return-Path: <bryant@sioux.stanford.edu>
Received: from sioux.stanford.edu by alnitak.usc.edu (4.1/SMI-4.1+ucs-3.6)
        id AA12724; Sat, 3 Apr 93 14:57:52 PST
Received: from oglala.ice (oglala.Stanford.EDU) by sioux.stanford.edu (4.1/inc-1.0)
        id AA07300; Sat, 3 Apr 93 14:53:25 PST
Date: Sat, 3 Apr 93 14:53:25 PST
From: bryant@sioux.stanford.edu (Bryant Marks)
Message-Id: <9304032253.AA07300@sioux.stanford.edu>
To: ajayshah@rcf.usc.edu
Subject: Re:  SVD
Status: ORr


> Hi!   Long ago you sent me an svd routine in C based on code
> from Nash in Pascal.  Has this changed any over the years?  (Your
> email is dated July 1992).  Is your code available by anon ftp?

Hi Ajay,

I don't think I have changed the code -- but here's my most recent
version of the code, you can check to see if it's any different.
Currently it's not available via anonymous ftp but feel free to
redistribute the code -- it seems to work well in the application
I'm using it in.


Bryant
*/

/* This SVD routine is based on pgs 30-48 of "Compact Numerical Methods
   for Computers" by J.C. Nash (1990), used to compute the pseudoinverse.
   Modifications include:
        Translation from Pascal to ANSI C.
        Array indexing from 0 rather than 1.
        Float replaced by double everywhere.
        Support for the Matrix structure.
        I changed the array indexing so that the matricies (float [][])
           could be replaced be a single list (double *) for more
           efficient communication with Mathematica.
*/

/* rjrw 7/7/99: changed z back to a vector, moved one line... */

/*
   Derek A. Lamb 2016: added comments to aid understanding, since
   Nash's book will only get more difficult to find in the future.
*/

/*
   Form a singular value decomposition of matrix A which is stored in
   the first nRow*nCol elements of working array W. Upon return, the
   first nRow*nCol elements of W will become the product U x S of a
   thin svd, where S is the diagonal (rectangular) matrix of singular
   values. The last nCol*nCol elements of W will be the square matrix
   V of a thin svd. On return, Z will contain the squares of the
   singular values.

   The input matrix A is assumed to have nRows >= nCol. If it does
   not, one should input the transpose of A, A", to find the the svd
   of A, since A = U x S x V" and A" = V x S x U". (The " means
   transpose.)
*/

#include <math.h>
#define TOLERANCE 1.0e-22

#ifdef MAIN
#include <stdio.h>
#define NC 2
#define NR 2
int main()
{
  int i,j,n,m;
  double w[NC*(NR+NC)], z[NC*NC];
  void SVD(double *W, double *Z, int nRow, int nCol);

  for (i=0;i<NC*(NR+NC);i++) {
    w[i] = 0.;
  }

  for (i=0;i<NC*NC;i++) {
    z[i] = 0.;
  }
  w[0] = 1; w[1] = 3; w[NC] = -4; w[NC+1] = 3;

  SVD(w, z, NR, NC);

  printf("W:\n");
  for (i=0;i<NC*(NR+NC);i++) {
    printf("%d %g\n",i,w[i]);
  }
  printf("\nZ:\n");
  for (i=0;i<NC*NC;i++) {
    printf("%d %g\n",i,z[i]);
  }
  return 0;
}
#endif

void SVD(double *W, double *Z, int nRow, int nCol)
{
  int i, j, k, EstColRank, RotCount, SweepCount, slimit;
  double eps, e2, tol, vt, p, x0, y0, q, r, c0, s0, d1, d2;
  eps = TOLERANCE;
  /*set a limit on the number of sweeps allowed. A suggested limit is slimit=max([nCol/4],6).*/
  slimit = nCol/4;
  if (slimit < 6.0)
    slimit = 6;
  SweepCount = 0;
  e2 = 10.0*nRow*eps*eps;
  tol = eps*.1; /*set convergence tolerances */
  EstColRank = nCol; /* current estimate of rank */
  /* Set V matrix to the unit matrix of order nCol.
     V is stored in elements nCol*nRow to nCol*(nRow+nCol)-1 of array W. */
  for (i=0; i<nCol; i++) {
    for (j=0; j<nCol; j++) {
      W[nCol*(nRow+i)+j] = 0.0;
    }
    W[nCol*(nRow+i)+i] = 1.0;  /* rjrw 7/7/99: moved this line out of j loop */
  }
  RotCount = EstColRank*(EstColRank-1)/2;

  /*until convergence is achieved or too many sweeps are carried out*/
  while (RotCount != 0 && SweepCount <= slimit)
    {
      RotCount = EstColRank*(EstColRank-1)/2; /* rotation counter */
      SweepCount++;
      for (j=0; j<EstColRank-1; j++) /* main cyclic Jacobi sweep */
        {
          for (k=j+1; k<EstColRank; k++)
            {
              p = q = r = 0.0;
	      /*
		Some helpful definitions from Nash's book (pg 34) to
		understand this p, q, & r rotation business:

		p = x"y; (" means transpose)
		q = x"x - y"y;
		v = sqrt(4*p^2+q^2);
		if (q>=0) {
		  cos(phi) = sqrt((v+q)/(2*v));
		  sin(phi) = p/(v*cos(phi));
		} else if (q<0) {,
		  (sgn(p)=+1 for p>=0, -1 for p<0);
		  sin(phi) = sgn(p)*sqrt((v-q)/(2*v));
		  cos(phi) = p/(v*sin(phi));
		}
		This formulation avoids the subtraction of two nearly
		equal numbers, which is bound to happen to q and v as
		the matrix approaches orthogonality.
	      */
              for (i=0; i<nRow; i++)
                {
                  x0 = W[nCol*i+j]; y0 = W[nCol*i+k];
                  p += x0*y0; q += x0*x0; r += y0*y0;
                }
              Z[j] = q; Z[k] = r;
	      /* Convergence test: will rotation exchange order of columns?*/
	      if (q >= r) /* check if columns are ordered */
                { /* columns are ordered, so try convergence test */
                  if (q<=e2*Z[0] || fabs(p)<=tol*q) RotCount--;
		  /* There is no more work on this particular pair of
		     columns in the current sweep. The first condition
		     checks for very small column norms in BOTH
		     columns, for which no rotation makes sense. The
		     second condition determines if the inner product
		     is small with respect to the larger of the
		     columns, which implies a very small rotation
		     angle. */
                  else
                    {/* columns are in order, but their inner product is not small */
                      p /= q; r = 1 - r/q; vt = sqrt(4*p*p+r*r);
		      /* DAL thinks the fabs is unnecessary in the
			 next line: vt is non-negative; q>=r, r/q <=1
			 so after the assignment 0<= r <=1, so r/vt is
			 positive, so everything inside the sqrt
			 should be positive even without the fabs. abs
			 isn't in Nash's book. c0 and s0 are cos(phi) and sin(phi) as above for q>=0*/
                      c0 = sqrt(fabs(.5*(1+r/vt))); s0 = p/(vt*c0);
		      /* this little for loop (and the one below) is just rotation, inlined here for efficiency */
                      for (i=0; i<nRow+nCol; i++)
                        {
                          d1 = W[nCol*i+j]; d2 = W[nCol*i+k];
                          W[nCol*i+j] = d1*c0+d2*s0; W[nCol*i+k] = -d1*s0+d2*c0;
                        }
                    }
                }
              else /* columns out of order -- must rotate */
                { /* note: r>q, and cannot be zero since both are sums
		     of squares for the svd. In the case of a real
		     symmetric matrix, this assumption must be
		     questioned. */
                  p /= r; q = q/r-1; vt = sqrt(4*p*p+q*q);
                  s0 = sqrt(fabs(.5*(1-q/vt)));
		  /* DAL wondering about the fabs again, since after
		     assignment q is <0 and vt is again positive, so
		     everything inside the fabs should be positive
		     already */
                  if (p<0) s0 = -s0; /*s0 and c0 are sin(phi) and cos(phi) as above for q<0 */
                  c0 = p/(vt*s0);
                  for (i=0; i<nRow+nCol; i++) /* rotation again */
                    {
                      d1 = W[nCol*i+j]; d2 = W[nCol*i+k];
                      W[nCol*i+j] = d1*c0+d2*s0; W[nCol*i+k] = -d1*s0+d2*c0;
                    }
                }
		  /* Both angle calculations have been set up so that
		     large numbers do not occur in intermediate
		     quantities. This is easy in the svd case, since
		     quantities x2,y2 cannot be negative.*/
            } /* loop on k */
        } /* loop on j */
      while (EstColRank>=3 && Z[(EstColRank-1)]<=Z[0]*tol+tol*tol)
        EstColRank--;
    }
#if DEBUG
  if (SweepCount > slimit)
    fprintf(stderr, "Sweeps = %d\n", SweepCount);
#endif
}
