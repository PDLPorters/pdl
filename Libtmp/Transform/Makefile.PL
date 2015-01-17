use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pack = (["transform.pd", qw(Transform PDL::Transform)]);
my %hash = pdlpp_stdargs_int(@pack);
$hash{DIR} = ['Cartography', 'Proj4'];

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };

WriteMakefile( %hash );
