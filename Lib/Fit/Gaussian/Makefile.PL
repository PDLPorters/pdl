
# Makefile.PL for PDL::Fit::Gaussian module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

@pack = (["gaussian.pd",Gaussian,PDL::Fit::Gaussian]);
%hash = pdlpp_stdargs_int(@::pack);
$hash{OBJECT} .= ' ';
$hash{DEFINE} .= ' ';
$hash{LIBS} = ['-lm'] unless $^O =~ /MSWin/;

WriteMakefile(%hash);

sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule
