
# Makefile.PL for PDL::Ops module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

@pack = (["ops.pd",Ops,PDL::Ops]);

WriteMakefile(
 pdlpp_stdargs_int(@::pack)
);

sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule
