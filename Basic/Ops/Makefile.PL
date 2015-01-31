use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pack = (["ops.pd",qw(Ops PDL::Ops)]);
my %hash = pdlpp_stdargs_int(@pack);
$hash{LIBS}->[0] .= ' -lm ';

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
