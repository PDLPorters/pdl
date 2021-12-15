#include "FCN.h"

SV* mnfunname;
int ene;

#define PDL_FCN_SETUP(pvar) \
  pdl *pvar; \
  PUSHMARK(SP); \
  XPUSHs(sv_2mortal(newSVpv("PDL", 0))); \
  PUTBACK; \
  perl_call_method("initialize", G_SCALAR); \
  SPAGAIN; \
  SV *pvar ## sv = POPs; \
  PUTBACK; \
  pvar = PDL->SvPDLV(pvar ## sv); \
  PDL->barf_if_error(PDL->converttype( pvar, PDL_D )); \
  PDL->setdims (pvar,pdims,ndims); \
  pvar->state |= PDL_ALLOCATED | PDL_DONTTOUCHDATA;

void FCN(int* npar,double* grad,double* fval,double* xval,int* iflag,double* futil){

  SV* funname;

  int count,i;
  double* x;

  I32 ax ; 
  
  dSP;
  ENTER;
  SAVETMPS;

  /* get name of function on the Perl side */
  funname = mnfunname;

  int ndims = 1;
  PDL_Indx pdims[ndims];
  
  pdims[0] = (PDL_Indx) ene;

  PDL_FCN_SETUP(pxval)
  PDL_FCN_SETUP(pgrad)

  pxval->data = (void *) xval;
  pgrad->data = (void *) grad;  

  PUSHMARK(SP);

  XPUSHs(sv_2mortal(newSViv(*npar)));
  XPUSHs(pgradsv);
  XPUSHs(sv_2mortal(newSVnv(*fval)));
  XPUSHs(pxvalsv);
  XPUSHs(sv_2mortal(newSViv(*iflag)));

  PUTBACK;

  count=call_sv(funname,G_ARRAY);

  SPAGAIN; 
  SP -= count ;
  ax = (SP - PL_stack_base) + 1 ;

  if (count!=2)
    croak("error calling perl function\n");

  pgradsv = ST(1);
  pgrad = PDL->SvPDLV(pgradsv);

  x = (double *) pgrad->data;

  for(i=0;i<ene;i++)
    grad[i] = x[i];

  *fval = SvNV(ST(0));

  PUTBACK;
  FREETMPS;
  LEAVE;

}
