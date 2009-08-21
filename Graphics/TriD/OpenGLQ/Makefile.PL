
use ExtUtils::MakeMaker;

# PDL::Core::Dev->import();

@pack = (["openglq.pd",OpenGLQ,PDL::Graphics::OpenGLQ]);
%hash = pdlpp_stdargs_int(@::pack);

push @{$hash{LIBS}}, $PDL::Config{OPENGL_LIBS};
$hash{DEFINE} .= ' '.$PDL::Config{OPENGL_DEFINE};
$hash{INC} .= ' '.$PDL::Config{OPENGL_INC};
${$hash{LIBS}}[0] .= ' -lm';
WriteMakefile(%hash);

sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule

# SKIP: return;
