#include "FCN.h"

SV* mnfunname;
int ene;

#define PDL_FCN_SETUP(pvar, pdata) \
  pdl* pvar = PDL->pdlnew(); \
  if (!pvar) PDL->pdl_barf("Failed to create pdl"); \
  SV *pvar ## sv = sv_newmortal(); \
  PDL->SetSV_PDL(pvar ## sv, pvar); \
  pvar->datatype = PDL_D; \
  PDL->barf_if_error(PDL->setdims(pvar,pdims,ndims)); \
  pvar->data = (void *) pdata; \
  pvar->state |= PDL_ALLOCATED | PDL_DONTTOUCHDATA;

void FCN(int* npar,double* grad,double* fval,double* xval,int* iflag,double* futil){
  dSP;
  ENTER;
  SAVETMPS;

  PDL_Indx ndims = 1, i;
  PDL_Indx pdims[] = { (PDL_Indx) ene };

  PDL_FCN_SETUP(pgrad, grad)
  PDL_FCN_SETUP(pxval, xval)

  PUSHMARK(SP);
  XPUSHs(sv_2mortal(newSViv(*npar)));
  XPUSHs(pgradsv);
  XPUSHs(sv_2mortal(newSVnv(*fval)));
  XPUSHs(pxvalsv);
  XPUSHs(sv_2mortal(newSViv(*iflag)));
  PUTBACK;
  int count=call_sv(mnfunname,G_ARRAY); /* name of function on the Perl side */
  if (count!=2)
    croak("error calling perl function\n");
  SPAGAIN;
  SP -= count ;
  I32 ax = (SP - PL_stack_base) + 1 ;

  *fval = SvNV(ST(0));

  double* x = (double *) PDL->SvPDLV(ST(1))->data;
  for(i=0;i<ene;i++)
    grad[i] = x[i];

  pxval->data = NULL;
  pgrad->data = NULL;

  PUTBACK;
  FREETMPS;
  LEAVE;
}
