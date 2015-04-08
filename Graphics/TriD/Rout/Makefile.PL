use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pack = (["rout.pd", qw(Rout PDL::Graphics::TriD::Rout)]);
my %hash = pdlpp_stdargs_int(@pack);
$hash{LIBS} = ['-lm'];

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
