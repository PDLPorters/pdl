use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pm_names = qw (Graphics2D.pm State.pm);
WriteMakefile(
  NAME => 'PDL',
  VERSION_FROM => '../Basic/PDL.pm',
  PM => { map {
    my $h = '$(INST_LIBDIR)/PDL/';
    $h .= 'Graphics/' if $_ =~ /State.pm$/;
    ( $_, $h . $_ );
  } @pm_names },
  MAN3PODS => { map {
    my $h = '$(INST_MAN3DIR)/PDL::';
    $h .= 'Graphics::' if $_ =~ /State.pm$/;
    ( $_, $h . substr($_,0,length($_)-3) . '.$(MAN3EXT)' );
  } @pm_names },
  NO_MYMETA => 1,
);
