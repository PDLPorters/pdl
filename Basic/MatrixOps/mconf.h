/*							mconf.h
 *
 *	Common include file for math routines
 *
 *
 *
 * SYNOPSIS:
 *
 * #include "mconf.h"
 *
 *
 *
 * DESCRIPTION:
 *
 * This file contains definitions for error codes that are
 * passed to the common error handling routine mtherr()
 * (which see).
 *
 * The file also includes a conditional assembly definition
 * for the type of computer arithmetic (IEEE, DEC, Motorola
 * IEEE, or UNKnown).
 *
 * For Digital Equipment PDP-11 and VAX computers, certain
 * IBM systems, and others that use numbers with a 56-bit
 * significand, the symbol DEC should be defined.  In this
 * mode, most floating point constants are given as arrays
 * of octal integers to eliminate decimal to binary conversion
 * errors that might be introduced by the compiler.
 *
 * For little-endian computers, such as IBM PC, that follow the
 * IEEE Standard for Binary Floating Point Arithmetic (ANSI/IEEE
 * Std 754-1985), the symbol IBMPC should be defined.  These
 * numbers have 53-bit significands.  In this mode, constants
 * are provided as arrays of hexadecimal 16 bit integers.
 *
 * Big-endian IEEE format is denoted MIEEE.  On some RISC
 * systems such as Sun SPARC, double precision constants
 * must be stored on 8-byte address boundaries.  Since integer
 * arrays may be aligned differently, the MIEEE configuration
 * may fail on such machines.
 *
 * To accommodate other types of computer arithmetic, all
 * constants are also provided in a normal decimal radix
 * which one can hope are correctly converted to a suitable
 * format by the available C language compiler.  To invoke
 * this mode, define the symbol UNK.
 *
 * An important difference among these modes is a predefined
 * set of machine arithmetic constants for each.  The numbers
 * MACHEP (the machine roundoff error), MAXNUM (largest number
 * represented), and several other parameters are preset by
 * the configuration symbol.  Check the file const.c to
 * ensure that these values are correct for your computer.
 *
 * Configurations NANS, INFINITIES, MINUSZERO, and DENORMAL
 * may fail on many systems.  Verify that they are supposed
 * to work on your computer.
 */

/*
Cephes Math Library Release 2.3:  June, 1995
Copyright 1984, 1987, 1989, 1995 by Stephen L. Moshier
*/

/* For PDL, use system defaults where possible */
#include <math.h>
#if !defined(WIN32) && !defined(_WIN32) && !defined(__APPLE__)
/* values.h is gone on OpenBSD(?) and depracated on GNU systems */
/* can we use values.h on all UN*X systems? */
#if defined __GNUC__
#include <limits.h>
#else
#include <values.h>
#endif   /* __GNUC__ */
#endif
#if defined(_WIN32) || defined(WIN32)
#include <float.h>
#define finite _finite
#endif

/* Now include system-specific stuff */

/* Look for system quiet_nan function */
#if defined __sun && ! defined __GNUC__
#include <sunmath.h>
#include <ieeefp.h>
#include <float.h>
#define NANARG 1L
#endif
#if defined __alpha && ! defined __linux
#include <float.h>
#include <nan.h>
#endif
#ifndef NANARG
#define NANARG
#endif

/* Redefine nan so PDL doesn't die when we see one.
   OK, nasty, but means the C-code is still as in the original */
#define nan() quiet_nan(NANARG)

/* Constant definitions for math error conditions */

#ifndef DOMAIN
#define DOMAIN		1	/* argument domain error */
#endif
#ifndef SING
#define SING		2	/* argument singularity */
#endif
#ifndef OVERFLOW
#define OVERFLOW	3	/* overflow range error */
#endif
#ifndef UNDERFLOW
#define UNDERFLOW	4	/* underflow range error */
#endif
#ifndef TLOSS
#define TLOSS		5	/* total loss of precision */
#endif
#ifndef PLOSS
#define PLOSS		6	/* partial loss of precision */
#endif

#ifndef EDOM
#define EDOM		33
#endif
#ifndef ERANGE
#define ERANGE		34
#endif

/* Complex numeral.  */
typedef struct
	{
	double r;
	double i;
	} cmplx;

/* Long double complex numeral.  */
typedef struct
	{
	double r;
	double i;
	} cmplxl;


/* Get ANSI function prototypes, if you want them. */
#ifdef __STDC__
#define ANSIPROT
#include "protos.h"
#else
int mtherr();
#endif

/* Variable for error reporting.  See mtherr.c.  */
extern int merror;

#ifdef MY_QUIET_NAN
extern double quiet_nan();
#endif
#ifdef MY_INFINITY
extern double infinity();
#endif

extern double MACHEP;
extern double UFLOWTHRESH;
extern double MAXLOG;
extern double MINLOG;
extern double MAXNUM;
#ifndef PI
extern double PI;
#endif
extern double PIO2;
extern double PIO4;
extern double SQRT2;
extern double SQRTH;
extern double LOG2E;
extern double SQ2OPI;
extern double LOGE2; 
extern double LOGSQ2;
extern double THPIO4;
extern double TWOOPI;
