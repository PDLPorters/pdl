/*							asinh.c
 *
 *	Inverse hyperbolic sine
 *
 *
 *
 * SYNOPSIS:
 *
 * double x, y, asinh();
 *
 * y = asinh( x );
 *
 *
 *
 * DESCRIPTION:
 *
 * Returns inverse hyperbolic sine of argument.
 *
 * If |x| < 0.5, the function is approximated by a rational
 * form  x + x**3 P(x)/Q(x).  Otherwise,
 *
 *     asinh(x) = log( x + sqrt(1 + x*x) ).
 *
 *
 *
 * ACCURACY:
 *
 *                      Relative error:
 * arithmetic   domain     # trials      peak         rms
 *    DEC      -3,3         75000       4.6e-17     1.1e-17
 *    IEEE     -1,1         30000       3.7e-16     7.8e-17
 *    IEEE      1,3         30000       2.5e-16     6.7e-17
 *
 */

/*						asinh.c	*/

/*
Cephes Math Library Release 2.3:  March, 1995
Copyright 1984, 1995 by Stephen L. Moshier
*/


#include "mconf.h"

static double P[] = {
-4.33231683752342103572E-3,
-5.91750212056387121207E-1,
-4.37390226194356683570E0,
-9.09030533308377316566E0,
-5.56682227230859640450E0
};
static double Q[] = {
/* 1.00000000000000000000E0,*/
 1.28757002067426453537E1,
 4.86042483805291788324E1,
 6.95722521337257608734E1,
 3.34009336338516356383E1
};

#ifndef ANSIPROT
double log(), sqrt(), polevl(), p1evl();
#endif
extern double LOGE2;

double asinh(xx)
double xx;
{
double a, z, x;
int sign;

#ifdef MINUSZERO
if( xx == 0.0 )
  return(xx);
#endif
if( xx < 0.0 )
	{
	sign = -1;
	x = -xx;
	}
else
	{
	sign = 1;
	x = xx;
	}

if( x > 1.0e8 )
	{
	  if(!finite(x))
	    return(xx);
	  return( sign * (log(x) + LOGE2) );
	}

z = x * x;
if( x < 0.5 )
	{
	a = ( polevl(z, P, 4)/p1evl(z, Q, 4) ) * z;
	a = a * x  +  x;
	if( sign < 0 )
		a = -a;
	return(a);
	}

a = sqrt( z + 1.0 );
return( sign * log(x + a) );
}
