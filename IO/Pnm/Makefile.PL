
# Makefile.PL for PDL::IO module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

@pack = (["pnm.pd",Pnm,PDL::IO::Pnm]);

%hash = pdlpp_stdargs_int(@::pack);
$hash{'PM'}->{'Pic.pm'} = '$(INST_LIBDIR)/Pic.pm';
WriteMakefile(%hash);

# Add genpp rule
sub MY::postamble { pdlpp_postamble_int(@::pack); }  


