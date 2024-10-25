use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pack = (["imagend.pd", qw(ImageND PDL::ImageND)]);

# Add genpp rule
undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };

WriteMakefile( pdlpp_stdargs_int(@pack) );
