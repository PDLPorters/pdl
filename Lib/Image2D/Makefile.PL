
# Makefile.PL for PDL::Image2D module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;
PDL::Core::Dev->import();

@pack = (["image2d.pd",Image2D,PDL::Image2D]);

WriteMakefile( pdlpp_stdargs_int(@::pack) );

# Add genpp rule
sub MY::postamble { pdlpp_postamble_int(@::pack); }  

