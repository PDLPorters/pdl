
# Makefile.PL for PDL::Slices module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

@pack = (["slices.pd",Slices,PDL::Slices]);

WriteMakefile(
 pdlpp_stdargs_int(@::pack)
);

sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule
