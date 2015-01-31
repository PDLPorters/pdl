use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pack = (["tests.pd", qw(Tests PDL::Tests)]);

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};  # Add genpp rule

WriteMakefile(pdlpp_stdargs_int(@pack));
