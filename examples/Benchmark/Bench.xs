#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#ifdef __cplusplus
}
#endif

MODULE = PDL::Bench		PACKAGE = PDL::Bench

void
c_use_pp(sv)
	SV *sv;
	CODE:
	/* Let's hope the C compiler isn't smart enough to optimize
	 * away everything
	 */
	double *p = (double *) SvPV(sv,PL_na);
	int i = SvCUR(sv) / sizeof(double);
	while(i--) {
		(*p)++; p++;
	}

void
c_use_add(sv1,sv2,sv3)
	SV *sv1;
	SV *sv2;
	SV *sv3;
	CODE:
	double *p1 = (double *) SvPV(sv1,PL_na);
	int i = SvCUR(sv1) / sizeof(double);
	double *p2 = (double *) SvPV(sv2,PL_na);
	double *p3 = (double *) SvPV(sv3,PL_na);
	while(i--) {
		*p1 = *p2 + *p3;
		p1 ++; p2 ++; p3 ++;
	}

void
c_use_add_incr(sv1,sv2,sv3,i1,i2,i3)
	SV *sv1;
	SV *sv2;
	SV *sv3;
	int i1;
	int i2;
	int i3;
	CODE:
	double *p1 = (double *) SvPV(sv1,PL_na);
	int i = SvCUR(sv1) / sizeof(double);
	double *p2 = (double *) SvPV(sv2,PL_na);
	double *p3 = (double *) SvPV(sv3,PL_na);
	while(i--) {
		*p1 = *p2 + *p3;
		p1 += i1; p2 += i2; p3 += i3;
	}

