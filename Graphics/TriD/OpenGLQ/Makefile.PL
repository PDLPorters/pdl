
# should now be handled by ../Makefile.PL
#
#BEGIN {
#  PDL::Core::Dev->import();
#  $skip = 0;
#  if ($^O =~ /win32/i) {
#    write_dummy_make(unsupported('PDL::Graphics::OpenGLQ','win32'));
#    $skip=1;
#  }
#}
#
#goto SKIP if $skip;

use ExtUtils::MakeMaker;

 PDL::Core::Dev->import();

@pack = (["openglq.pd",OpenGLQ,PDL::Graphics::OpenGLQ]);
%hash = pdlpp_stdargs_int(@::pack);
foreach (@{$hash{LIBS}}) {
	$_ .= ' '.$PDL_CONFIG{OPENGL_LIBS};
}
$hash{DEFINE} .= ' '.$PDL_CONFIG{OPENGL_DEFINE};
$hash{INC} .= ' '.$PDL_CONFIG{OPENGL_INC};
${$hash{LIBS}}[0] .= ' -lm';
WriteMakefile(%hash);

sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule

# SKIP: return;
