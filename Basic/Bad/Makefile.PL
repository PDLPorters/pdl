
# Makefile.PL for PDL::Bad module.

# we create a Bad,pm whatever the value of
# WITH_CONFIG - it's just the contents that weill be
# different...
#
use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

@pack = (["bad.pd",Bad,PDL::Bad]);

%hash = pdlpp_stdargs_int(@::pack);

#$hash{LIBS} = ['-lm'];

WriteMakefile(%hash);

sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule

