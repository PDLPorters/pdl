#define PDL_APPEND2(x, y) PDL_APPEND(x, y) /* forces expansion of args */
#define PDL_APPEND(x, y) x ## y
#define PDL_FORTRAN(x) PDL_APPEND2(x, PDL_USCORE)

#define MNINIT PDL_FORTRAN(mninit)
#define MNSETI PDL_FORTRAN(mnseti)
#define MNPARM PDL_FORTRAN(mnparm)
#define MNPARS PDL_FORTRAN(mnpars)
#define MNEXCM PDL_FORTRAN(mnexcm)
#define MNCOMD PDL_FORTRAN(mncomd)
#define MNPOUT PDL_FORTRAN(mnpout)
#define MNSTAT PDL_FORTRAN(mnstat)
#define MNEMAT PDL_FORTRAN(mnemat)
#define MNERRS PDL_FORTRAN(mnerrs)
#define MNCONT PDL_FORTRAN(mncont)
#define ABRE   PDL_FORTRAN(abre)
#define CIERRA PDL_FORTRAN(cierra)

#include "EXTERN.h"
#include "perl.h"
#include "pdl.h"
#include "pdlcore.h"

#define PDL PDL_Minuit
extern Core *PDL;
extern SV* mnfunname;
extern int ene;

extern void MNINIT(int*,int*,int*);
extern void MNSETI(char*,int);
extern void MNPARM(int*,char*,double*,double*,double*,double*,int*,int);
extern void MNEXCM(void* f,char*,double*,int*,int*,double* futil,int);
extern void MNPOUT(int*,char*,double*,double*,double*,double*,int*,int);
extern void MNSTAT(double*,double*,double*,int*,int*,int*);
extern void MNEMAT(double*,int*); /* Matrix here! */
extern void MNERRS(int*,double*,double*,double*,double*);
extern void MNCONT(void* f,int*,int*,int*,double*,double*,int*,double* futil);

extern void ABRE(int*,char*,char*,int,int);
extern void CIERRA(int*);

void FCN(int* npar,double* grad,double* fval,double* xval,int* iflag,double* futil);
