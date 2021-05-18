use strict;
use warnings;
use Config; # for ivsize

require './Types.pm';

my $file = shift @ARGV;
print "Extracting $file\n";
open OUT,">$file" or die "Can't create $file: $!";
chmod 0644, $file;

print OUT <<"!GROK!THIS!";

/*
 * THIS FILE IS GENERATED FROM pdlperl.h.PL! Do NOT edit!
 */

#ifndef __PDLPERL_H
#define __PDLPERL_H

#define PDL_MAKE_PERL_COMPLEX(output,r,i) { \\
        dSP; int count; double rval = r, ival = i; SV *ret; \\
        ENTER; SAVETMPS; PUSHMARK(sp); \\
        perl_require_pv("PDL/Complex/Overloads.pm"); \\
        mXPUSHn(rval); \\
        mXPUSHn(ival); \\
        PUTBACK; \\
        count = perl_call_pv("PDL::Complex::Overloads::cplx", G_SCALAR); \\
        SPAGAIN; \\
        if (count != 1) croak("Failed to create PDL::Complex::Overloads object (%.9g, %.9g)", rval, ival); \\
        ret = POPs; \\
        SvREFCNT_inc(ret); \\
        output = ret; \\
        PUTBACK; FREETMPS; LEAVE; \\
}

!GROK!THIS!

print OUT <<'EOF';
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

/* __PDLPERL_H */
#endif

EOF
