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

#include <math.h>
#define TOLERANCE 1.0e-22

#ifdef MAIN
#include <stdio.h>
#define NC 2
#define NR 2
main()
{
  int i,j,n,m;
  double w[NC*(NR+NC)*20], z[NC*NC*20];
  void SVD(double *W, double *Z, int nRow, int nCol);
  
  for (i=0;i<NC*(NR+NC);i++) {
    w[i] = 0.;
  }

  w[0] = 1; w[1] = 3; w[NC] = -4; w[NC+1] = 3;

  SVD(w, z, NR, NC);

  for (i=0;i<NC*(NR+NC);i++) {
    printf("%d %g\n",i,w[i]);
  }
  for (i=0;i<NC*NC;i++) {
    printf("%d %g\n",i,z[i]);
  }

}
#endif

void SVD(double *W, double *Z, int nRow, int nCol)
{
  int i, j, k, EstColRank, RotCount, SweepCount, slimit;
  double eps, e2, tol, vt, p, h2, x0, y0, q, r, c0, s0, c2, d1, d2;
  eps = TOLERANCE;
  slimit = nCol/4;
  if (slimit < 6.0)
    slimit = 6;
  SweepCount = 0;
  e2 = 10.0*nRow*eps*eps;
  tol = eps*.1;
  EstColRank = nCol;
  for (i=0; i<nCol; i++) {
    for (j=0; j<nCol; j++) {
      W[nCol*(nRow+i)+j] = 0.0;
    }
    W[nCol*(nRow+i)+i] = 1.0;  /* rjrw 7/7/99: moved this line out of j loop */
  }
  RotCount = EstColRank*(EstColRank-1)/2;
  while (RotCount != 0 && SweepCount <= slimit)
    {
      RotCount = EstColRank*(EstColRank-1)/2;
      SweepCount++;
      for (j=0; j<EstColRank-1; j++)
        {
          for (k=j+1; k<EstColRank; k++)
            {
              p = q = r = 0.0;
              for (i=0; i<nRow; i++)
                {
                  x0 = W[nCol*i+j]; y0 = W[nCol*i+k];
                  p += x0*y0; q += x0*x0; r += y0*y0;
                }
              Z[j] = q; Z[k] = r;
              if (q >= r)
                {
                  if (q<=e2*Z[0] || fabs(p)<=tol*q) RotCount--;
                  else
                    {
                      p /= q; r = 1 - r/q; vt = sqrt(4*p*p+r*r);
                      c0 = sqrt(fabs(.5*(1+r/vt))); s0 = p/(vt*c0);
                      for (i=0; i<nRow+nCol; i++)
                        {
                          d1 = W[nCol*i+j]; d2 = W[nCol*i+k];
                          W[nCol*i+j] = d1*c0+d2*s0; W[nCol*i+k] = -d1*s0+d2*c0;
                        }
                    }
                }
              else
                {
                  p /= r; q = q/r-1; vt = sqrt(4*p*p+q*q);
                  s0 = sqrt(fabs(.5*(1-q/vt)));
                  if (p<0) s0 = -s0;
                  c0 = p/(vt*s0);
                  for (i=0; i<nRow+nCol; i++)
                    {
                      d1 = W[nCol*i+j]; d2 = W[nCol*i+k];
                      W[nCol*i+j] = d1*c0+d2*s0; W[nCol*i+k] = -d1*s0+d2*c0;
                    }
                }
            }
        }
      while (EstColRank>=3 && Z[(EstColRank-1)]<=Z[0]*tol+tol*tol)
        EstColRank--;
    }
#if DEBUG
  if (SweepCount > slimit)
    fprintf(stderr, "Sweeps = %d\n", SweepCount);
#endif
}
