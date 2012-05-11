// This file copyright (C) 2006 Andres Jordan <ajordan@eso.org> 
// and Simon Casassus <simon@das.uchile.cl>
// All rights reserved. There is no warranty. You are allowed to 
// redistribute this  software/documentation under certain conditions. 
// For details, see the file COPYING in the PDL distribution. If this file 
// is separated from the PDL distribution, the copyright notice should be 
// included in the file.

#include <stdlib.h>
#include <stdio.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>

static SV* ext_funname1;

static int ene;

void DFF(int* n, double* x, double* vector);
int my_f (const gsl_vector * v, void * params, gsl_vector * df);

void DFF(int* n, double* xval, double* vector){
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
  PDL_Long *pdims;

  dSP;
  ENTER;
  SAVETMPS;

  ndims = 1;
  pdims = (PDL_Long *)  PDL->smalloc((STRLEN) ((ndims) * sizeof(*pdims)) );
  
  pdims[0] = (PDL_Long) ene;

  PUSHMARK(SP);
  XPUSHs(sv_2mortal(newSVpv("PDL", 0)));
  PUTBACK;
  perl_call_method("initialize", G_SCALAR);
  SPAGAIN;
  pxsv = POPs;
  PUTBACK;
  px = PDL->SvPDLV(pxsv);
  
  PDL->converttype( &px, PDL_D, PDL_PERM );
  PDL->children_changesoon(px,PDL_PARENTDIMSCHANGED|PDL_PARENTDATACHANGED);
  PDL->setdims (px,pdims,ndims);
  px->state &= ~PDL_NOMYDIMS;
  px->state |= PDL_ALLOCATED;
  PDL->changed(px,PDL_PARENTDIMSCHANGED|PDL_PARENTDATACHANGED,0);

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
  
  PDL->make_physical(pvector);
  
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
  double *dp = (double *)params;
  int* nelem; int i;
  double* xfree;
  double* vector;

  int iloop;

  nelem = (int *) malloc(sizeof(int));
  *nelem = (int) dp[0];

  xfree = (double *) malloc(*nelem * sizeof(double));
  
  vector = (double *) malloc(*nelem * sizeof(double));
    
  for (iloop=0;iloop< *nelem;iloop++) {
    xfree[iloop] = gsl_vector_get(v, iloop);
    vector[iloop] = gsl_vector_get(v, iloop) * gsl_vector_get(v, iloop);
  }


  DFF(nelem, xfree, vector);

  for (iloop=0;iloop< *nelem;iloop++) {
    gsl_vector_set(df, iloop, vector[iloop]);
   }
  

  free(nelem);
  free(xfree);
  free(vector);
  return  GSL_SUCCESS ; 
  
}

int
print_state (size_t iter, gsl_multiroot_fsolver * s)
{
  printf ("iter = %3u x = % .3f % .3f "
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
  default: barf("Something is wrong: could not assing fsolver type...\n"); break;
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


  return 0;

}
