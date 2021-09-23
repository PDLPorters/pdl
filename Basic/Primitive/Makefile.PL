use strict;
use warnings;
use ExtUtils::MakeMaker;
use ExtUtils::MakeMaker::Config;

my @pack = (["primitive.pd", qw(Primitive PDL::Primitive)]);

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

# If we don't do this, and Perl core is using the wrapped API, then it will
# call (say) srand48_r(), and get its random numbers from drand48_r(), but we
# will get ours from drand48(), and srand48() never gets called.
$hash{CCFLAGS} ||= $Config{ccflags};
$hash{CCFLAGS} .= ' -DPERL_REENTR_API';

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
