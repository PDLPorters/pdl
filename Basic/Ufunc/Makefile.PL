use strict;
use warnings;
use ExtUtils::MakeMaker;
use ExtUtils::MakeMaker::Config;
use File::Spec::Functions;

my @pack = (["ufunc.pd", qw(Ufunc PDL::Ufunc)]);

my %hash = pdlpp_stdargs_int(@pack);
$hash{LIBS}[0] .= ' -lm';

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
