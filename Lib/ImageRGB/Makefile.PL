
# Makefile.PL for PDL::Examples module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;
PDL::Core::Dev->import();

@pack = (["imagergb.pd",ImageRGB,PDL::ImageRGB]);

%hash = pdlpp_stdargs_int(@::pack);
$hash{'OBJECT'} .= ' ppm_quant$(OBJ_EXT)';
$hash{'clean'}{FILES} .= ' ppm_quant$(OBJ_EXT)';

WriteMakefile(%hash);

# Add genpp rule
sub MY::postamble { pdlpp_postamble_int(@::pack); }

