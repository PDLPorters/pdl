
use ExtUtils::MakeMaker;
use OpenGL 0.58 qw();
use OpenGL::Config;

# PDL::Core::Dev->import();

@pack = (["openglq.pd",OpenGLQ,PDL::Graphics::OpenGLQ]);
%hash = pdlpp_stdargs_int(@::pack);

print STDERR "pdlpp_stdargs_int() libs are ->@{$hash{LIBS}}<-\n";

push @{$hash{LIBS}}, $OpenGL::Config->{LIBS};

print STDERR "\$OpenGL::Config->{LIBS} are ->$OpenGL::Config->{LIBS}<-\n";

$hash{DEFINE} .= ' '.$OpenGL::Config->{DEFINE};
$hash{INC} .= ' '.$OpenGL::Config->{INC};
${$hash{LIBS}}[0] .= ' -lm';

print STDERR "final \$hash{LIBS} value is ->@{$hash{LIBS}}<-\n";

WriteMakefile(%hash);

sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule

# SKIP: return;
