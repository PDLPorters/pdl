/*							acosh.c
 *
 *	Inverse hyperbolic cosine
 *
 *
 *
 * SYNOPSIS:
 *
 * double x, y, acosh();
 *
 * y = acosh( x );
 *
 *
 *
 * DESCRIPTION:
 *
 * Returns inverse hyperbolic cosine of argument.
 *
 * If 1 <= x < 1.5, a rational approximation
 *
 *	sqrt(z) * P(z)/Q(z)
 *
 * where z = x-1, is used.  Otherwise,
 *
 * acosh(x)  =  log( x + sqrt( (x-1)(x+1) ).
 *
 *
 *
 * ACCURACY:
 *
 *                      Relative error:
 * arithmetic   domain     # trials      peak         rms
 *    DEC       1,3         30000       4.2e-17     1.1e-17
 *    IEEE      1,3         30000       4.6e-16     8.7e-17
 *
 *
 * ERROR MESSAGES:
 *
 *   message         condition      value returned
 * acosh domain       |x| < 1            NAN
 *
 */

/*							acosh.c	*/

/*
Cephes Math Library Release 2.3:  March, 1995
Copyright 1984, 1995 by Stephen L. Moshier
*/


/* acosh(z) = sqrt(x) * R(x), z = x + 1, interval 0 < x < 0.5 */

#include "mconf.h"

static double P[] = {
 1.18801130533544501356E2,
 3.94726656571334401102E3,
 3.43989375926195455866E4,
 1.08102874834699867335E5,
 1.10855947270161294369E5
};
static double Q[] = {
/* 1.00000000000000000000E0,*/
 1.86145380837903397292E2,
 4.15352677227719831579E3,
 2.97683430363289370382E4,
 8.29725251988426222434E4,
 7.83869920495893927727E4
};

#ifndef ANSIPROT
double log(), sqrt(), polevl(), p1evl();
#endif
extern double LOGE2;

double acosh(x)
double x;
{
double a, z;

if( x < 1.0 )
	{
	mtherr( "acosh", DOMAIN );
	return(quiet_nan());
	}

if( x > 1.0e8 )
	{
	  if( !finite(x) )
	    return(x);
	return( log(x) + LOGE2 );
	}

z = x - 1.0;

if( z < 0.5 )
	{
	a = sqrt(z) * (polevl(z, P, 4) / p1evl(z, Q, 5) );
	return( a );
	}

a = sqrt( z*(x+1.0) );
return( log(x + a) );
}
