/* complex.c - a small library for doing complex algebra in C.
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


#include <math.h>
#include "complex.h"

void SSL_ComplexAssign(double re, double im, SSL_Complex *z) {

  z->re=re;
  z->im=im;
} /* SSL_ComplexAssign */ 
  

void SSL_ComplexAdd(SSL_Complex z1, SSL_Complex z2, SSL_Complex *res) {

  res->re=z1.re+z2.re;
  res->im=z1.im+z2.im;
} /* SSL_ComplexAdd */


void SSL_ComplexSub(SSL_Complex z1, SSL_Complex z2, SSL_Complex *res) {

  res->re=z1.re-z2.re;
  res->im=z1.im-z2.im;
} /* SSL_ComplexSub */
 

void SSL_ComplexMul(SSL_Complex z1, SSL_Complex z2, SSL_Complex *res) {

  res->re=z1.re*z2.re-z1.im*z2.im;
  res->im=z1.re*z2.im+z1.im*z2.re;
} /* SSL_ComplexMul */


void SSL_ComplexDiv(SSL_Complex a, SSL_Complex b, SSL_Complex *res) {

  double temp;

  temp=b.re*b.re+b.im*b.im;
  res->re=(a.re*b.re+a.im*b.im)/temp;
  res->im=(a.im*b.re-a.re*b.im)/temp;
} /* SSL_ComplexDiv */


double SSL_ComplexNorm(SSL_Complex z) {

  return (sqrt(z.re*z.re+z.im*z.im));
} /* SSL_ComplexNorm */


double SSL_ComplexArg(SSL_Complex z) {

  return (atan2(z.im, z.re));
} /* SSL_ComplexArg */ 

