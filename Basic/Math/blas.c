/* Assorted matrix functions.
 */


/* Multiply r (rows) by c (columns) matrix A on the left
 * by column vector V of dimension c on the right
 * to produce a (column) vector Y output of dimension r.
 */
mvmpy( r, c, A, V, Y )
int r, c;
double *A, *V, *Y;
{
register double s;
double *pA, *pV, *pY;
int i, j;

pA = A;
pY = Y;
for( i=0; i<r; i++ )
	{
	pV = V;
	s = 0.0;
	for( j=0; j<c; j++ )
		{
		s += *pA++ * *pV++;
		}
	*pY++ = s;
	}
}


/* Multiply an r (rows) by c (columns) matrix A on the left
 * by a c (rows) by r (columns) matrix B on the right
 * to produce an r by r matrix Y.
 */
mmmpy( r, c, A, B, Y )
int r, c;
double *A, *B, *Y;
{
register double s;
double *pA, *pB, *pY, *pt;
int i, j, k;

pY = Y;
pB = B;
for( i=0; i<r; i++ )
	{
	pA = A;
	for( j=0; j<r; j++ )
		{
		pt = pB;
		s = 0.0;
		for( k=0; k<c; k++ )
			{
			s += *pA++ * *pt;
			pt += r; /* increment to next row underneath */
			}
		*pY++ = s;
		}
	pB += 1;
	}
}


/* Transpose the n by n square matrix A and put the result in T.
 * T may occupy the same storage as A.
 */
mtransp( n, A, T )
int n;
double *A, *T;
{
int i, j, np1;
double *pAc, *pAr, *pTc, *pTr, *pA0, *pT0;
double x, y;

np1 = n+1;
pA0 = A;
pT0 = T;
for( i=0; i<n-1; i++ ) /* row index */
	{
	pAc = pA0; /* next diagonal element of input */
	pAr = pAc + n; /* next row down underneath the diagonal element */
	pTc = pT0; /* next diagonal element of the output */
	pTr = pTc + n; /* next row underneath */
	*pTc++ = *pAc++; /* copy the diagonal element */
	for( j=i+1; j<n; j++ ) /* column index */
		{
		x = *pAr;
		*pTr = *pAc++;
		*pTc++ = x;
		pAr += n;
		pTr += n;
		}
	pA0 += np1; /* &A[n*i+i] for next i */
	pT0 += np1; /* &T[n*i+i] for next i */
	}
*pT0 = *pA0; /* copy the diagonal element */
}


/* Return maximum off-diagonal element of n by n square matrix A
 */
double maxoffd( n, A )
int n;
double *A;
{
double e, x;
int i, j, nm1;
double *pA;

nm1 = n-1;
e = 0.0;
pA = A;
for( i=0; i<nm1; i++ )
	{
	++pA; /* skip over the diagonal element */
	for( j=0; j<n; j++ )
		{
		x = *pA++;
		if( x < 0 )
			x = -x;
		if( x > e )
			e = x;
		}
	}
return( e );
}




/* Unpack symmetric matrix T stored in lower triangular form
 * into a symmetric n by n square matrix S.
 */
tritosquare( n, T, S )
int n;
double T[], S[];
{
double *pT;
int i, j, ni, nj;

/* offset to (i,j) element is (j*j+j)/2 + i */
pT = T;
ni = 0;
for( i=0; i<n; i++ )
	{
	nj = 0;
	for( j=0; j<i; j++ )
		{
		S[ni+j] = *pT;
		S[nj+i] = *pT++;
		nj += n;
		}
	S[ni+i] = *pT++;
	ni += n;
	}
}
