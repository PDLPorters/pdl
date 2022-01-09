use strict;
use warnings;
use ExtUtils::MakeMaker;

my @subdirs = qw(PGPLOT LUT IIS Limits);

if (eval {
  require OpenGL; OpenGL->VERSION(0.70);
  require OpenGL::GLUT; OpenGL::GLUT->VERSION(0.72);
  1
}) {
  unshift @subdirs, "TriD";
} else {
  print "Will not build TriD graphics.\n";
}

my @pm_names = qw (Graphics2D.pm State.pm);
WriteMakefile(
  NAME => 'PDL',
  VERSION_FROM => '../Basic/PDL.pm',
  PM => { map {
    my $h = '$(INST_LIBDIR)/';
    $h .= 'PDL/' if $_ !~ /PDL.pm$/;
    $h .= 'Graphics/' if $_ =~ /State.pm$/;
    ( $_, $h . $_ );
  } @pm_names },
  MAN3PODS => { map {
    my $h = '$(INST_MAN3DIR)/';
    $h .= 'PDL::' if $_ !~ /PDL.pm$/;
    ( $_, $h . substr($_,0,length($_)-3) . '.$(MAN3EXT)' );
  } @pm_names },
  DIR => \@subdirs,
  NO_MYMETA => 1,
);
