/*
 * Round to neareast integer
 *
 * SYNOPSIS:
 *
 * double rint(double x);
 *
 * DESCRIPTION:
 *
 * Returns the integer (represented as a double precision number)
 * nearest to x.
 *
 * Relies on floor().
 *
 * rint(x)  =  floor(x + 0.5);
 *
 * Copyright (c) 1998, Raphael Manfredi <Raphael_Manfredi@hp.com>

 All rights reserved. There is no warranty. You are allowed
 to redistribute this software / documentation under certain
 conditions. For details, see the file COPYING in the PDL
 distribution. If this file is separated from the PDL distribution,
 the copyright notice should be included in the file.

 */

#include "mconf.h"

double rint(x)
double x;
{
	return floor(x + 0.5);
}

