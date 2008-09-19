#define max_nested_integrals  20
 
static SV* ext_funname[max_nested_integrals - 1];
static int current_fun = -1;
static gsl_function F;

double FUNC(double x,void * p);

double FUNC(double x,void * p){

  SV* funname;

  int count;

  I32 ax ; 

  double res;
  double* resp;

  dSP;

  resp = &res;


  ENTER;
  SAVETMPS;

  /* get function name on the perl side */
  funname = ext_funname[current_fun];

  PUSHMARK(SP);

  XPUSHs(sv_2mortal(newSVnv(x)));

  PUTBACK;

  count=call_sv(funname,G_SCALAR);

  SPAGAIN; 
  SP -= count ;
  ax = (SP - PL_stack_base) + 1 ;

  if (count!=1)
    croak("error calling perl function\n");

  /* recover output value */
  /*res = POPn;*/
  *resp = SvNV(ST(0));

  PUTBACK;
  FREETMPS;
  LEAVE;
  
  return res;
}
