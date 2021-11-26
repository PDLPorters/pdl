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

#define PDL_XS_SCALAR(type, ctype, val) \
  SV *b_SV = sv_newmortal(); \
  pdl *b = pdl_pdlnew(); \
  if (!b) XSRETURN_UNDEF; \
  PDL_Anyval av; \
  ANYVAL_FROM_CTYPE(av,type,val); \
  pdl_makescratchhash(b, av); \
  pdl_SetSV_PDL(b_SV, b); \
  EXTEND(SP, 1); \
  ST(0) = b_SV; \
  XSRETURN(1);

#define PDL_MAKE_PERL_COMPLEX(output,r,i) { \
        dSP; int count; double rval = r, ival = i; SV *ret; \
        ENTER; SAVETMPS; PUSHMARK(sp); \
        perl_require_pv("PDL/Complex/Overloads.pm"); \
        mXPUSHn(rval); \
        mXPUSHn(ival); \
        PUTBACK; \
        count = perl_call_pv("PDL::Complex::Overloads::cplx", G_SCALAR); \
        SPAGAIN; \
        if (count != 1) croak("Failed to create PDL::Complex::Overloads object (%.9g, %.9g)", rval, ival); \
        ret = POPs; \
        SvREFCNT_inc(ret); \
        output = ret; \
        PUTBACK; FREETMPS; LEAVE; \
}

/***************
 * So many ways to be undefined...
 */
#define PDL_SV_IS_UNDEF(sv)  ( (!(sv) || ((sv)==&PL_sv_undef)) || !(SvNIOK(sv) || (SvTYPE(sv)==SVt_PVMG) || SvPOK(sv) || SvROK(sv)))

#define ANYVAL_FROM_SV(outany,insv,use_undefval,forced_type) do { \
    SV *sv2 = insv; \
    if (PDL_SV_IS_UNDEF(sv2)) { \
        if (!use_undefval) { \
            outany.type = forced_type >=0 ? forced_type : -1; \
            outany.value.B = 0; \
            break; \
        } else { \
            sv2 = get_sv("PDL::undefval",1); \
            if(SvIV(get_sv("PDL::debug",1))) \
                fprintf(stderr,"Warning: SvPDLV converted undef to $PDL::undefval (%g).\n",SvNV(sv2)); \
            if (PDL_SV_IS_UNDEF(sv2)) { \
                outany.type = forced_type >=0 ? forced_type : PDL_B; \
                outany.value.B = 0; \
                break; \
            } \
        } \
    } \
    if (sv_derived_from(sv2, "PDL")) { \
        pdl *it = PDL_CORE_(SvPDLV)(sv2); \
        outany = PDL_CORE_(at0)(it); \
    } else if (!SvIOK(sv2)) { /* Perl Double (e.g. 2.0) */ \
        NV tmp_NV = SvNV(sv2); \
        int datatype = forced_type >=0 ? forced_type : _pdl_whichdatatype_double(tmp_NV); \
        ANYVAL_FROM_CTYPE(outany, datatype, tmp_NV); \
    } else { /* Perl Int (e.g. 2) */ \
        IV tmp_IV = SvIV(sv2); \
        int datatype = forced_type >=0 ? forced_type : _pdl_whichdatatype(tmp_IV); \
        ANYVAL_FROM_CTYPE(outany, datatype, tmp_IV); \
    } \
} while (0)

#define ANYVAL_TO_SV(outsv,inany) do { switch (inany.type) { \
EOF
for (PDL::Types::types()) {
  print OUT "case @{[$_->sym]}: ";
  if ($_->real) {
    my $upper = uc(my $letter = $_->integer ? 'i' : 'n');
    print OUT "outsv = newSV${letter}v( (${upper}V)(inany.value.".$_->ppsym.") )";
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
#define PDL_TESTTYPE(sym, ctype) {ctype foo = nv; if(nv == foo) return sym;}
static inline int _pdl_whichdatatype (IV nv) {
#define X(sym, ctype, ppsym, shortctype, defbval) \
  PDL_TESTTYPE(sym, ctype)
   PDL_GENERICLIST(X)
#undef X
        croak("Something's gone wrong: %ld cannot be converted by whichdatatype", nv);
}
/* Check minimum, at least float, datatype required to represent number */
static inline int _pdl_whichdatatype_double (NV nv) {
        PDL_TESTTYPE(PDL_F,PDL_Float)
        PDL_TESTTYPE(PDL_D,PDL_Double)
#undef PDL_TESTTYPE
        /* Default return type PDL_Double */
        return PDL_D;
}

/* __PDLPERL_H */
#endif
EOF
