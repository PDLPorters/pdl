/*
 * ssl.c - misc. functions for Small Scientific Library
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
 * Revision 1.1.1.1  2001/07/06 13:39:35  kneth
 * Initial import of code.
 *
 *
 */

#include <stdio.h>

/*
 * SSLerror is an error handling routine.
 *
 */

void SSLerror(char *msg) {

  fprintf(stderr, "Fatal error in SSL.\n%s\n", msg);
} /* SSLerror */


/*
 * Sqr and Cube is two simple power-raising routines. 
 *
 */

double Sqr(double x) {

  return x*x;
} /* Sqr */

double Cube(double x) {

  return x*x*x;
} /* Cube */
