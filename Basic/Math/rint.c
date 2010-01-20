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
 * nearest to x. For half-integers, this implements "banker's
 * rounding", or round-half-to-even, in which half integers are
 * rounded to the nearest even integer. For other floating-point
 * numbers, returns floor(x + 0.5);
 *
 * Copyright (c) 1998, Raphael Manfredi <Raphael_Manfredi@hp.com>
 *           (c) 2010, Derek Lamb <lambd@users.sourceforge.net>

 Note that other functions in PDL::Math use code from the Cephes math
 library. If this new Banker's rounding code causes some problems, it
 is possible to rename the round.c provided therein and use it here.

All rights reserved. There is no warranty. You are allowed to
 redistribute this software / documentation under certain
 conditions. For details, see the file COPYING in the PDL
 distribution. If this file is separated from the PDL distribution,
 the copyright notice should be included in the file.

 */

#include "mconf.h"

double rint(x)
double x;
{  //could do recursion but this is probably more memory efficient.
  int i = x; //just get the integer part
  if (x<0 && (i==x+0.5)){//if it is a neg half integer
    return i-(i&1); //subtract one if it is odd
  } else if (x>0 && (i==x-0.5)){//if it is a pos half integer
    return i+(i&1); //add one if it is odd
  } else return floor(x+0.5);
}
