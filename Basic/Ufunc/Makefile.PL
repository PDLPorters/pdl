use strict;
use warnings;
use ExtUtils::MakeMaker;
use ExtUtils::MakeMaker::Config;

my @pack = (["ufunc.pd", qw(Ufunc PDL::Ufunc)]);

if ($^O eq 'dec_osf') {
       if ($Config{cc} =~ /^cc/) {
               my $no_optimize = $Config{optimize} || '-g2';
               $no_optimize =~ s/(\s|^)(-O)\d/$1${2}0/;
               $no_optimize =~ s/(\s|^)(-g)\d/$1${2}2/;
               print <<EOT;
       Digital Unix cc optimizer has a bug that is
       triggered by primitive.c. Therefore
EOT
               # Becaue OPTIMIZE in PASTHRU it can't be overridden so
               # it gets hard coded in Makefile
               eval q|
                       sub MY::const_cccmd {
                               package MY;
                               my $defval = shift->SUPER::const_cccmd(@_);
                               $defval =~ s/\$\(OPTIMIZE\)/|
                                       . $no_optimize . q|/gs;
                               print "$defval\n";
                               return $defval;
                       };
               |;
       }
}

my %hash = pdlpp_stdargs_int(@pack);
$hash{LIBS}->[0] .= ' -lm';

#suppress warning from "$GENERIC(b) foo = 0.25;", which is intentional.
$hash{INC} .= ' -Wno-literal-conversion ' if $Config{cc} =~ /\bclang\b/;

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
