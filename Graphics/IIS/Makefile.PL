BEGIN {
  PDL::Core::Dev->import();
  $skip = 0;
  if ($^O =~ /win32/i) {
    write_dummy_make(unsupported('PDL::Graphics::IIS','win32'));
    $skip=1;
  }
}

goto SKIP if $skip;

# Makefile.PL for PDL::Graphics::IIS module.

# Use this as a template for the Makefile.PL for
# any external PDL module.


#use PDL::Core::Dev;
PDL::Core::Dev->import();

use ExtUtils::MakeMaker;

# Replace this wit:e

@pack = (["iis.pd",IIS,PDL::Graphics::IIS]);

#WriteMakefile(
# pdlpp_stdargs_int(@::pack)
#);
%hash = pdlpp_stdargs_int(@::pack);
$hash{LIBS} = ['-lm'];
WriteMakefile(%hash);

sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule

SKIP: return;
