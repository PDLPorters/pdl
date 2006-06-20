/*
 * matrix.h - misc. routines for manipulating matrices and solving linear 
 * equations. Matrices are assumed to be declared as **double and 
 * allocated by the function MatrixAlloc. A matrix can be freed by 
 * MatrixFree. Similar for vectors.
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

#ifndef SSL_MATRIX_H_
#define SSL_MATRIX_H_

#include "sslib.h"
#include "complex.h"

extern double  **MatrixAlloc(const int);
extern double   *VectorAlloc(const int);
extern int      *IntVectorAlloc(const int);
extern SSL_Complex  *SSL_ComplexVectorAlloc(const int);
extern SSL_Complex **SSL_ComplexMatrixAlloc(const int);
extern void      MatrixMul(const int, double **, double **, double **);
extern void      Transpose(const int, double **, double **);
extern void      MatrixFree(const int, double **);
extern void      VectorFree(const int, double *);
extern void      IntVectorFree(const int, int *);
extern void      SSL_ComplexMatrixFree(const int, SSL_Complex **);
extern void      SSL_ComplexVectorFree(const int, SSL_Complex *);
extern void      LUfact(const int, double **, int *);
extern void      LUsubst(const int, double **, int *, double *);
extern void      Tridiag(const int, double *, double *, double *, double *);
extern void      GaussSeidel(const int, double **, double *, double *, 
			     double, int); 
extern void      Jacobi(const int, double **, double *, double *, double, 
			int); 
extern double    DotProd(const int, double *, double *);
extern void      MatrixVecProd(const int, double **, double *, double *);
extern void      MatrixCopy(const int, double **, double **);
extern void      GSR(const int, double **);
extern double    L2VectorNorm(const int, double *);
extern void      InversMatrix(const int, double **, double **);

#endif /* SSL_MATRIX_H_ */
