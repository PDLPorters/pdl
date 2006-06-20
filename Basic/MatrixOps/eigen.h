/*
 * eigen.c - calculation of eigen values and vectors.
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

#ifndef SSL_EIGEN_H_
#define SSL_EIGEN_H_

#include <stdio.h>
#include "complex.h"

extern void Eigen(int, int, double **, int, double, int, SSL_Complex *, SSL_Complex **);

#endif /* SSL_EIGEN_SSL */
