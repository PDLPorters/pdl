use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pack = (["image2d.pd", qw(Image2D PDL::Image2D)]);

my %hash = pdlpp_stdargs_int( @pack );
$hash{LIBS} = [ '-lm' ];
$hash{OBJECT} .= " $_\$(OBJ_EXT)" for qw(rotate resample select misc);

# Add genpp rule
undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };

WriteMakefile( %hash );
