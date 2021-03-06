use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pack = (["bad.pd",qw(Bad PDL::Bad)]);

my %hash = pdlpp_stdargs_int(@pack);

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
