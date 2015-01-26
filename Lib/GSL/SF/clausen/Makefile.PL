use strict;
use warnings;
use ExtUtils::MakeMaker;

PDL::Core::Dev->import;

our ($GSL_includes, $GSL_libs); 
my @pack = (['gsl_sf_clausen.pd', qw(CLAUSEN PDL::GSLSF::CLAUSEN)]);
my %hash = pdlpp_stdargs_int(@pack);
$hash{INC} .= ' '.$GSL_includes;
push @{$hash{LIBS}},$GSL_libs;

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };
WriteMakefile(%hash);
