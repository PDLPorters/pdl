
use ExtUtils::MakeMaker;

BEGIN {
   if ( $PDL::Config{USE_POGL} ) {
      eval "require OpenGL $PDL::Config{POGL_VERSION} qw()";
      if (!$@) {
         eval "use OpenGL::Config";
      }
   }
}

PDL::Core::Dev->import();

@pack = (["openglq.pd",OpenGLQ,PDL::Graphics::OpenGLQ]);
%hash = pdlpp_stdargs_int(@::pack);

if ( $PDL::Config{USE_POGL} ) {
   push @{$hash{LIBS}}, $OpenGL::Config->{LIBS};
   $hash{DEFINE} .= ' '.$OpenGL::Config->{DEFINE};
   $hash{INC} .= ' '.$OpenGL::Config->{INC};
   if($^O eq 'MSWin32') {
     $hash{LDFROM} .= ' '. $OpenGL::Config->{LDFROM};
     $hash{LDFROM} =~ s/\-lfreeglut//g;
   }
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
