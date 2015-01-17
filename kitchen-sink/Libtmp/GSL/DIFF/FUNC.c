static SV* ext_funname;
static gsl_function F;

double FUNC(double x,void * p);

double FUNC(double x,void * p){

  double res;
  int count;

  dSP;
  SV* funname;

  /* get function name on the perl side */
  funname = ext_funname;

  ENTER;
  SAVETMPS;

  PUSHMARK(SP);

  XPUSHs(sv_2mortal(newSVnv(x)));

  PUTBACK;

  count=call_sv(funname,G_SCALAR);

  SPAGAIN; 

  if (count!=1)
    croak("error calling perl function\n");

  /* recover output value */
  res = POPn;

  PUTBACK;
  FREETMPS;
  LEAVE;
  
  return res;
}
