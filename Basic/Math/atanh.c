/*							atanh.c
 *
 *	Inverse hyperbolic tangent
 *
 *
 *
 * SYNOPSIS:
 *
 * double x, y, atanh();
 *
 * y = atanh( x );
 *
 *
 *
 * DESCRIPTION:
 *
 * Returns inverse hyperbolic tangent of argument in the range
 * MINLOG to MAXLOG.
 *
 * If |x| < 0.5, the rational form x + x**3 P(x)/Q(x) is
 * employed.  Otherwise,
 *        atanh(x) = 0.5 * log( (1+x)/(1-x) ).
 *
 *
 *
 * ACCURACY:
 *
 *                      Relative error:
 * arithmetic   domain     # trials      peak         rms
 *    DEC       -1,1        50000       2.4e-17     6.4e-18
 *    IEEE      -1,1        30000       1.9e-16     5.2e-17
 *
 */

/*						atanh.c	*/


/*
Cephes Math Library Release 2.3:  March, 1995
Copyright (C) 1987, 1995 by Stephen L. Moshier
*/

#include "mconf.h"

static double P[] = {
-8.54074331929669305196E-1,
 1.20426861384072379242E1,
-4.61252884198732692637E1,
 6.54566728676544377376E1,
-3.09092539379866942570E1
};
static double Q[] = {
/* 1.00000000000000000000E0,*/
-1.95638849376911654834E1,
 1.08938092147140262656E2,
-2.49839401325893582852E2,
 2.52006675691344555838E2,
-9.27277618139601130017E1
};
#ifndef ANSIPROT
double fabs(), log(), polevl(), p1evl();
#endif

double atanh(x)
double x;
{
double s, z;

#ifdef MINUSZERO
if( x == 0.0 )
	return(x);
#endif
z = fabs(x);
if( z >= 1.0 )
	{
	if( x == 1.0 )
		return( infinity() );
	if( x == -1.0 )
		return( -infinity() );
	mtherr( "atanh", DOMAIN );
	return( quiet_nan() );
	}

if( z < 1.0e-7 )
	return(x);

if( z < 0.5 )
	{
	z = x * x;
	s = x   +  x * z * (polevl(z, P, 4) / p1evl(z, Q, 5));
	return(s);
	}

return( 0.5 * log((1.0+x)/(1.0-x)) );
}
