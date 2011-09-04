#ifdef NO_TRAILING_USCORE

#define MNINIT mninit
#define MNSETI mnseti
#define MNPARM mnparm
#define MNPARS mnpars
#define MNEXCM mnexcm
#define MNCOMD mncomd
#define MNPOUT mnpout
#define MNSTAT mnstat
#define MNEMAT mnemat
#define MNERRS mnerrs
#define MNCONT mncont

#define ABRE   abre
#define CIERRA cierra

#else

#define MNINIT mninit_
#define MNSETI mnseti_
#define MNPARM mnparm_
#define MNPARS mnpars_
#define MNEXCM mnexcm_
#define MNCOMD mncomd_
#define MNPOUT mnpout_
#define MNSTAT mnstat_
#define MNEMAT mnemat_
#define MNERRS mnerrs_
#define MNCONT mncont_

#define ABRE   abre_
#define CIERRA cierra_

#endif 

static SV* mnfunname;
static int ene;

void FCN(int* npar,double* grad,double* fval,double* xval,int* iflag,double* futil);

void FCN(int* npar,double* grad,double* fval,double* xval,int* iflag,double* futil){

  SV* funname;

  int count,i;
  double* x;

  I32 ax ; 
  
  pdl* pgrad;
  SV* pgradsv;

  pdl* pxval;
  SV* pxvalsv;
  
  int ndims;
  PDL_Long *pdims;

  dSP;
  ENTER;
  SAVETMPS;

  /* get name of function on the Perl side */
  funname = mnfunname;

  ndims = 1;
  pdims = (PDL_Long *)  PDL->smalloc( (STRLEN) ((ndims) * sizeof(*pdims)) );
  
  pdims[0] = (PDL_Long) ene;

  PUSHMARK(SP);
  XPUSHs(sv_2mortal(newSVpv("PDL", 0)));
  PUTBACK;
  perl_call_method("initialize", G_SCALAR);
  SPAGAIN;
  pxvalsv = POPs;
  PUTBACK;
  pxval = PDL->SvPDLV(pxvalsv);
 
  PDL->converttype( &pxval, PDL_D, PDL_PERM );
  PDL->children_changesoon(pxval,PDL_PARENTDIMSCHANGED|PDL_PARENTDATACHANGED);
  PDL->setdims (pxval,pdims,ndims);
  pxval->state &= ~PDL_NOMYDIMS;
  pxval->state |= PDL_ALLOCATED;
  PDL->changed(pxval,PDL_PARENTDIMSCHANGED|PDL_PARENTDATACHANGED,0);

  PUSHMARK(SP);
  XPUSHs(sv_2mortal(newSVpv("PDL", 0)));
  PUTBACK;
  perl_call_method("initialize", G_SCALAR);
  SPAGAIN;
  pgradsv = POPs;
  PUTBACK;
  pgrad = PDL->SvPDLV(pgradsv);
  
  PDL->converttype( &pgrad, PDL_D, PDL_PERM );
  PDL->children_changesoon(pgrad,PDL_PARENTDIMSCHANGED|PDL_PARENTDATACHANGED);
  PDL->setdims (pgrad,pdims,ndims);
  pgrad->state &= ~PDL_NOMYDIMS;
  pgrad->state |= PDL_ALLOCATED;
  PDL->changed(pgrad,PDL_PARENTDIMSCHANGED|PDL_PARENTDATACHANGED,0);

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
