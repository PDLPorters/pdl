
# Makefile.PL for PDL::Examples module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;

PDL::Core::Dev->import;
@pack = (["storable.pd",Storable,PDL::IO::Storable]);

%hash = pdlpp_stdargs_int(@::pack);
# $hash{'OPTIMIZE'}  = '-g'; # If you want to debug, uncomment this.
$hash{'VERSION'}  = '0.5';

WriteMakefile(%hash);

# Add genpp rule
sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}

