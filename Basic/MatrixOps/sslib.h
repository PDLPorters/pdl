/*
 * ssl.h - Small Scientific Library.
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
 * Revision 1.2  2001/07/11 08:06:01  kneth
 * Added SWAP macro
 *
 * Revision 1.1.1.1  2001/07/06 13:39:35  kneth
 * Initial import of code.
 *
 *
 */

#ifndef SSL_H_
#define SSL_H_

#ifndef PI
#  define PI 3.141592653589793238462643
#endif

/***** A boolean type       *****/
typedef enum {false=0, true=1} bool;

/***** Pratical macros      *****/
#define min(x, y)   ((x)>(y))?(y):(x)
#define max(x, y)   ((x)>(y))?(x):(y)
#define SWAP(x, y)  { double tmp; tmp=x; x=y; y=tmp; }

/***** General functions    *****/
extern void   SSLerror(char *);
extern double Sqr(double);
extern double Cube(double);

#endif /* SSL_H_ */
