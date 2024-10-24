use strict;
use warnings;
use Config; # for ivsize

require './Types.pm';

my $file = shift @ARGV;
print "Extracting $file\n";
open OUT,">$file" or die "Can't create $file: $!";
chmod 0644, $file;

print OUT sprintf qq{#line %d "%s"\n}, __LINE__ + 2,  __FILE__;
print OUT <<'EOF';
/*
 * THIS FILE IS GENERATED FROM pdlperl.h.PL! Do NOT edit!
 */

#ifndef __PDLPERL_H
#define __PDLPERL_H

#define PDL_XS_PREAMBLE(nret) \
  char *objname = "PDL"; /* XXX maybe that class should actually depend on the value set \
                            by pp_bless ? (CS) */ \
  HV *bless_stash = 0; \
  SV *parent = 0; \
  int   nreturn = (nret); \
  (void)nreturn; \
  /* Check if you can get a package name for this input value. */ \
  /* It can be either a PDL (SVt_PVMG) or a hash which is a */ \
  /* derived PDL subclass (SVt_PVHV) */ \
  do { \
    if (SvROK(ST(0)) && ((SvTYPE(SvRV(ST(0))) == SVt_PVMG) || (SvTYPE(SvRV(ST(0))) == SVt_PVHV))) { \
      parent = ST(0); \
      if (sv_isobject(parent)){ \
          bless_stash = SvSTASH(SvRV(parent)); \
          objname = HvNAME((bless_stash)); /* The package to bless output vars into is taken from the first input var */ \
      } \
    } \
  } while (0)

static inline pdl *PDL_XS_pdlinit(pTHX_ char *objname, HV *bless_stash, SV *to_push, char *method, SV **svp, Core *core) {
  dSP;
  pdl *ret;
  if (strcmp(objname,"PDL") == 0) { /* shortcut if just PDL */
     ret = core->pdlnew();
     if (!ret) core->pdl_barf("Error making null pdl");
     if (svp) {
       *svp = sv_newmortal();
       core->SetSV_PDL(*svp, ret);
       if (bless_stash) *svp = sv_bless(*svp, bless_stash);
     }
  } else {
     PUSHMARK(SP);
     XPUSHs(to_push);
     PUTBACK;
     perl_call_method(method, G_SCALAR);
     SPAGAIN;
     SV *sv = POPs;
     PUTBACK;
     ret = core->SvPDLV(sv);
     if (svp) *svp = sv;
  }
  return ret;
}
#define PDL_XS_PERLINIT_initsv(sv) \
  PDL_XS_pdlinit(aTHX_ objname, bless_stash, parent ? parent : sv_2mortal(newSVpv(objname, 0)), "initialize", &sv, PDL)

#define PDL_XS_RETURN(clause1) \
    if (nreturn) { \
      if (nreturn > 0) EXTEND (SP, nreturn); \
      clause1; \
      XSRETURN(nreturn); \
    } else { \
      XSRETURN(0); \
    }

#define PDL_IS_INPLACE(in) ((in)->state & PDL_INPLACE)
#define PDL_XS_INPLACE(in, out) \
    if (PDL_IS_INPLACE(in)) { \
        if (out ## _SV) barf("inplace input but different output given"); \
        out ## _SV = sv_newmortal(); \
        in->state &= ~PDL_INPLACE; \
        out = in; \
        PDL->SetSV_PDL(out ## _SV,out); \
    } else \
        out = out ## _SV ? PDL_CORE_(SvPDLV)(out ## _SV) : \
          PDL_XS_PERLINIT_initsv(out ## _SV);

#define PDL_XS_SCALAR(thistype, ppsym, val) \
  PDL_Anyval av = {PDL_CLD, {.H=0 + 0I}}; /* guarantee all bits set */ \
  av.type = thistype; av.value.ppsym=val; \
  pdl *b = pdl_scalar(av); \
  if (!b) XSRETURN_UNDEF; \
  SV *b_SV = sv_newmortal(); \
  pdl_SetSV_PDL(b_SV, b); \
  EXTEND(SP, 1); \
  ST(0) = b_SV; \
  XSRETURN(1);

#define PDL_MAKE_PERL_COMPLEX(output,r,i) { \
        dSP; NV rval = r, ival = i; \
        perl_require_pv("PDL/Complex/Overloads.pm"); \
        ENTER; SAVETMPS; \
        PUSHMARK(SP); mXPUSHn(rval); mXPUSHn(ival); PUTBACK; \
        int count = perl_call_pv("PDL::Complex::Overloads::cplx", G_SCALAR); \
        SPAGAIN; \
        if (count != 1) croak("Failed to create PDL::Complex::Overloads object (%.9" NVgf ", %.9" NVgf ")", rval, ival); \
        sv_setsv(output, POPs); \
        PUTBACK; \
        FREETMPS; LEAVE; \
}

/***************
 * So many ways to be undefined...
 */
#define PDL_SV_IS_UNDEF(sv)  ( (!(sv) || ((sv)==&PL_sv_undef)) || !(SvNIOK(sv) || (SvTYPE(sv)==SVt_PVMG) || SvPOK(sv) || SvROK(sv)))

#define ANYVAL_FROM_SV(outany,insv,use_undefval,forced_type,warn_undef) do { \
    SV *sv2 = insv; \
    if (PDL_SV_IS_UNDEF(sv2)) { \
        if (!use_undefval) { \
            outany.type = forced_type >=0 ? forced_type : -1; \
            outany.value.B = 0; \
            break; \
        } \
        sv2 = get_sv("PDL::undefval",1); \
        if ((warn_undef) && SvIV(get_sv("PDL::debug",1))) \
            fprintf(stderr,"Warning: SvPDLV converted undef to $PDL::undefval (%"NVgf").\n",SvNV(sv2)); \
        if (PDL_SV_IS_UNDEF(sv2)) { \
            outany.type = forced_type >=0 ? forced_type : PDL_B; \
            outany.value.B = 0; \
            break; \
        } \
    } \
    if (SvROK(sv2)) { \
        if (sv_derived_from(sv2, "PDL")) { \
            pdl *it = PDL_CORE_(SvPDLV)(sv2); \
            outany.type = PDL_INVALID; \
            if (it->nvals == 1) \
                ANYVAL_FROM_CTYPE_OFFSET(outany, it->datatype, PDL_REPRP(it), PDL_REPROFFS(it)); \
            if (outany.type < 0) PDL_CORE_(pdl_barf)("Position out of range"); \
            break; \
        } \
        if (sv_derived_from(sv2, "Math::Complex")) { \
            ANYVAL_FROM_MCOMPLEX(outany, sv2); \
            break; \
        } \
        PDL_CORE_(pdl_barf)("Can't convert ref '%s' to Anyval", sv_reftype(SvRV(sv2), 1)); \
    } else if (!SvIOK(sv2)) { /* Perl Double (e.g. 2.0) */ \
        NV tmp_NV = SvNV(sv2); \
        int datatype = forced_type >=0 ? forced_type : _pdl_whichdatatype_double(tmp_NV); \
        ANYVAL_FROM_CTYPE(outany, datatype, tmp_NV); \
    } else if (SvIsUV(sv2)) { /* Perl unsigned int */ \
        UV tmp_UV = SvUV(sv2); \
        int datatype = forced_type >=0 ? forced_type : _pdl_whichdatatype_uint(tmp_UV); \
        ANYVAL_FROM_CTYPE(outany, datatype, tmp_UV); \
    } else { /* Perl Int (e.g. 2) */ \
        IV tmp_IV = SvIV(sv2); \
        int datatype = forced_type >=0 ? forced_type : _pdl_whichdatatype_int(tmp_IV); \
        ANYVAL_FROM_CTYPE(outany, datatype, tmp_IV); \
    } \
} while (0)

/* only to CD, same as whichdatatype_double only D. only if know is M:C */
#define ANYVAL_FROM_MCOMPLEX(outany,insv) do { \
    dSP; \
    int i; \
    double vals[2]; \
    char *meths[] = { "Re", "Im" }; \
    ENTER; SAVETMPS; \
    for (i = 0; i < 2; i++) { \
      PUSHMARK(SP); XPUSHs(insv); PUTBACK; \
      int count = perl_call_method(meths[i], G_SCALAR); \
      SPAGAIN; \
      if (count != 1) PDL_CORE_(pdl_barf)("Failed Math::Complex method '%s'", meths[i]); \
      vals[i] = (double)POPn; \
      PUTBACK; \
    } \
    FREETMPS; LEAVE; \
    outany.type = PDL_CD; \
    outany.value.C = (PDL_CDouble)(vals[0] + I * vals[1]); \
  } while (0)

#define ANYVAL_TO_SV(outsv,inany) do { switch (inany.type) { \
EOF
for (PDL::Types::types()) {
  print OUT "case @{[$_->sym]}: ";
  if ($_->real) {
    my $upper = uc(my $letter = $_->unsigned ? 'u' : $_->integer ? 'i' : 'n');
    print OUT "sv_set${letter}v(outsv, (${upper}V)(inany.value.".$_->ppsym."))";
  } else {
    my ($fs, $ppsym) = ($_->floatsuffix, $_->ppsym);
    print OUT "PDL_MAKE_PERL_COMPLEX(outsv, creal$fs(inany.value.$ppsym), cimag$fs(inany.value.$ppsym))"
  }
  print OUT "; break; \\\n";
}
print OUT <<'EOF';
   default:      outsv = &PL_sv_undef; \
  } \
 } while (0)
EOF

print OUT sprintf qq{#line %d "%s"\n}, __LINE__ + 2,  __FILE__;
print OUT <<'EOF';
/* Check minimum datatype required to represent number */
#define PDL_TESTTYPE(sym, ctype, v) {ctype foo = v; if (v == foo) return sym;}
static inline int _pdl_whichdatatype_uint(UV uv) {
#define X(sym, ctype, ...) PDL_TESTTYPE(sym, ctype, uv)
  PDL_TYPELIST_UNSIGNED(X)
#undef X
  croak("Something's gone wrong: %llu cannot be converted by whichdatatype", (unsigned long long)uv);
}
static inline int _pdl_whichdatatype_int(IV iv) {
#define X(sym, ctype, ...) PDL_TESTTYPE(sym, ctype, iv)
  PDL_TYPELIST_SIGNED(X)
#undef X
  croak("Something's gone wrong: %lld cannot be converted by whichdatatype", (long long)iv);
}
/* Check minimum, at least double, datatype required to represent number */
static inline int _pdl_whichdatatype_double(NV nv) {
  PDL_TESTTYPE(PDL_D,PDL_Double, nv)
  PDL_TESTTYPE(PDL_D,PDL_LDouble, nv)
#undef PDL_TESTTYPE
  return PDL_D; /* handles NaN */
}

/* __PDLPERL_H */
#endif
EOF
