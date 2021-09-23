use strict;
use warnings;
use ExtUtils::MakeMaker;
use ExtUtils::MakeMaker::Config;

my @pack = (["ufunc.pd", qw(Ufunc PDL::Ufunc)]);

my %hash = pdlpp_stdargs_int(@pack);
$hash{LIBS}->[0] .= ' -lm';

#suppress warning from "$GENERIC(b) foo = 0.25;", which is intentional.
$hash{INC} .= ' -Wno-literal-conversion ' if $Config{cc} =~ /\bclang\b/;

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
