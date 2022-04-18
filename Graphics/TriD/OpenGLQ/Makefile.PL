use strict;
use warnings;
use ExtUtils::MakeMaker;
use OpenGL::Config;

my @pack = (["openglq.pd", qw(OpenGLQ PDL::Graphics::OpenGLQ)]);
my %hash = pdlpp_stdargs_int(@pack);

$hash{LIBS}[0] = $OpenGL::Config->{LIBS} if $OpenGL::Config->{LIBS};
$hash{DEFINE} .= ' '.$OpenGL::Config->{DEFINE} if $OpenGL::Config->{DEFINE};
$hash{INC} .= ' '.$OpenGL::Config->{INC} if $OpenGL::Config->{INC};
if($^O eq 'MSWin32') {
  $hash{LDFROM} .= ' '. $OpenGL::Config->{LDFROM};
  $hash{LDFROM} =~ s/\-lfreeglut//g;
  $hash{DEFINE} .= ' -DGLUT_DISABLE_ATEXIT_HACK'; # else get errors about PerlProc_exit on at least Strawberry Perl 5.32
}

${$hash{LIBS}}[0] .= ' -lm';

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
