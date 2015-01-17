use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pack = (["gaussian.pd", qw(Gaussian PDL::Fit::Gaussian)]);
my %hash = pdlpp_stdargs_int(@pack);
$hash{OBJECT} .= ' ';
$hash{DEFINE} .= ' ';
$hash{LIBS} = ['-lm'] unless $^O =~ /MSWin/;

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };

WriteMakefile(%hash);
