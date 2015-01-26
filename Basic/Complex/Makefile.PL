use strict;
use warnings;
use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

my @pack = (["complex.pd", qw(Complex PDL::Complex)]);

my %hash = pdlpp_stdargs_int(@pack);
$hash{LIBS} = ['-lm'];

undef &MY::postamble; # suppress warning
*MY::postamble = sub{ pdlpp_postamble_int(@pack); };

WriteMakefile(%hash);
