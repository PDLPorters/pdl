/* complex.h - a small library for doing complex algebra in C.
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

#ifndef SSL_COMPLEX_H_
#define SSL_COMPLEX_H_

struct SSL_ComplexStruct {
  double   re, im;
}; /* struct SSL_ComplexStruct */

typedef struct SSL_ComplexStruct SSL_Complex;

extern void    SSL_ComplexAssign(double, double, SSL_Complex *);
extern void    SSL_ComplexAdd(SSL_Complex, SSL_Complex, SSL_Complex *);
extern void    SSL_ComplexSub(SSL_Complex, SSL_Complex, SSL_Complex *); 
extern void    SSL_ComplexMul(SSL_Complex, SSL_Complex, SSL_Complex *);
extern void    SSL_ComplexDiv(SSL_Complex, SSL_Complex, SSL_Complex *);
extern double  SSL_ComplexNorm(SSL_Complex);
extern double  SSL_ComplexArg(SSL_Complex);

#endif /* SSL_COMPLEX_H_ */
