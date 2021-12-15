// This file copyright (C) 2006 Andres Jordan <ajordan@eso.org> 
// and Simon Casassus <simon@das.uchile.cl>
// All rights reserved. There is no warranty. You are allowed to 
// redistribute this  software/documentation under certain conditions. 
// For details, see the file COPYING in the PDL distribution. If this file 
// is separated from the PDL distribution, the copyright notice should be 
// included in the file.

#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>
#include "EXTERN.h"
#include "perl.h"
#include "pdl.h"
#include "pdlcore.h"

#define PDL PDL_GSL_MROOT
extern Core *PDL;

static SV* ext_funname1;
static PDL_Indx ene;
void set_funname(SV *fn, PDL_Indx n) {
  ext_funname1 = fn;
  ene = n;
}

void DFF(double* xval, double* vector){
   //this version tries just to get the output
  SV* funname;

  double* xpass; int i;
  int count;
  I32 ax ; 

  pdl* px;
  SV* pxsv;

  pdl* pvector;
  SV* pvectorsv;

  int ndims;

  dSP;
  ENTER;
  SAVETMPS;

  ndims = 1;
  PDL_Indx pdims[ndims];
  
  pdims[0] = (PDL_Indx) ene;

  PUSHMARK(SP);
  XPUSHs(sv_2mortal(newSVpv("PDL", 0)));
  PUTBACK;
  perl_call_method("initialize", G_SCALAR);
  SPAGAIN;
  pxsv = POPs;
  PUTBACK;
  px = PDL->SvPDLV(pxsv);
  
  PDL->barf_if_error(PDL->converttype( px, PDL_D ));
  PDL->setdims (px,pdims,ndims);
  px->state |= PDL_ALLOCATED | PDL_DONTTOUCHDATA;

  px->data = (void *) xval;

  /* get function name on the perl side */
  funname = ext_funname1;

  PUSHMARK(SP);

  XPUSHs(pxsv);

  PUTBACK;

  count=call_sv(funname,G_SCALAR);


  SPAGAIN; 
  SP -= count ;
  ax = (SP - PL_stack_base) + 1 ;

  if (count!=1)
    croak("error calling perl function\n");

  /* recover output value */


  pvectorsv = ST(0);
  pvector = PDL->SvPDLV(pvectorsv);
  
  PDL->barf_if_error(PDL->make_physical(pvector));
  
  xpass  =  (double *) pvector->data;
  
  
  for(i=0;i<ene;i++) {
    vector[i] =  xpass[i];
  }
   
  PUTBACK;
  FREETMPS;
  LEAVE;

}


int my_f (const gsl_vector * v, void * params, gsl_vector * df)
{
  double dp = *((double *)params);
  int nelem = (int) dp;
  double xfree[nelem], vector[nelem];
  int iloop;
  for (iloop=0;iloop< nelem;iloop++) {
    xfree[iloop] = gsl_vector_get(v, iloop);
    vector[iloop] = gsl_vector_get(v, iloop) * gsl_vector_get(v, iloop);
  }
  DFF(xfree, vector);
  for (iloop=0;iloop< nelem;iloop++) {
    gsl_vector_set(df, iloop, vector[iloop]);
  }
  return GSL_SUCCESS;
}

int
print_state (size_t iter, gsl_multiroot_fsolver * s)
{
  printf ("iter = %3zu x = % .3f % .3f "
	  "f(x) = % .3e % .3e\n",
	  iter,
	  gsl_vector_get (s->x, 0),
	  gsl_vector_get (s->x, 1),
	  gsl_vector_get (s->f, 0),
	  gsl_vector_get (s->f, 1));
  return 1;
}


int fsolver (double *xfree, int  nelem, double epsabs, int method) 
{
  gsl_multiroot_fsolver_type *T;
  gsl_multiroot_fsolver *s;
  int status;
  size_t i, iter = 0;
  size_t n = nelem;
  double p[1] = { nelem };
  int iloop;
  //  struct func_params p = {1.0, 10.0};
  gsl_multiroot_function func = {&my_f, n,  p};
  gsl_vector *x = gsl_vector_alloc (n);
  for (iloop=0;iloop<nelem; iloop++) {
    //printf("in fsovler2D, C side, input is %g \n",xfree[iloop]);
    gsl_vector_set (x, iloop, xfree[iloop]);
  }
  switch (method){
  case 0 : T = (gsl_multiroot_fsolver_type *) gsl_multiroot_fsolver_hybrids; break;
  case 1 : T = (gsl_multiroot_fsolver_type *) gsl_multiroot_fsolver_hybrid;  break;
  case 2 : T = (gsl_multiroot_fsolver_type *) gsl_multiroot_fsolver_dnewton; break;
  case 3 : T = (gsl_multiroot_fsolver_type *) gsl_multiroot_fsolver_broyden; break;
  default: return GSL_EINVAL; break;
  }
  s = gsl_multiroot_fsolver_alloc (T, nelem);
  gsl_multiroot_fsolver_set (s, &func, x);
  do
    {
      iter++;
      //printf("GSL iter %d \n",iter);
      status = gsl_multiroot_fsolver_iterate (s);
      
      if (status)   /* check if solver is stuck */
	break;
      status =
	  gsl_multiroot_test_residual (s->f, epsabs);
    }
  while (status == GSL_CONTINUE && iter < 1000);
  if (status) 
      warn ("Final status = %s\n", gsl_strerror (status));
  for (iloop=0;iloop<nelem; iloop++) {
    xfree[iloop] = gsl_vector_get (s->x, iloop);
  }
  gsl_multiroot_fsolver_free (s);
  gsl_vector_free (x);
  return GSL_SUCCESS;
}
