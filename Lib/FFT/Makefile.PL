
# Makefile.PL for PDL::Primitive module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

@pack = (["fft.pd",FFT,PDL::FFT]);
%hash = pdlpp_stdargs_int(@::pack);
$hash{OBJECT} .= ' fftn.o';
$hash{DEFINE} .= ' -DFFT_FLOAT';
$hash{LIBS} = ['-lm'];

WriteMakefile(%hash);

sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule
