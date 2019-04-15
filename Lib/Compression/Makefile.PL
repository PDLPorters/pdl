use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pack = (["compression.pd", qw(Compression PDL::Compression)]);
my %hash = pdlpp_stdargs_int(@pack);

# Add genpp rule
undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };

WriteMakefile( pdlpp_stdargs_int(@pack) );
