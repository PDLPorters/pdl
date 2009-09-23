
use ExtUtils::MakeMaker;

BEGIN {
   if ( $PDL::Config{USE_POGL} ) {
      use OpenGL 0.58_004 qw();
      use OpenGL::Config;
   }
}

PDL::Core::Dev->import();

@pack = (["openglq.pd",OpenGLQ,PDL::Graphics::OpenGLQ]);
%hash = pdlpp_stdargs_int(@::pack);

if ( $PDL::Config{USE_POGL} ) {
   push @{$hash{LIBS}}, $OpenGL::Config->{LIBS};
   $hash{DEFINE} .= ' '.$OpenGL::Config->{DEFINE};
   $hash{INC} .= ' '.$OpenGL::Config->{INC};
} else {
   push @{$hash{LIBS}}, $PDL::Config{OPENGL_LIBS};
   $hash{DEFINE} .= ' '.$PDL::Config{OPENGL_DEFINE};
   $hash{INC} .= ' '.$PDL::Config{OPENGL_INC};
}

${$hash{LIBS}}[0] .= ' -lm';

WriteMakefile(%hash);

sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule

# SKIP: return;
