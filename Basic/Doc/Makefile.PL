use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
  NAME         => 'PDL::Doc',
  VERSION_FROM => '../lib/PDL.pm',
  PM           => {
    'Doc.pm' => '$(INST_LIB)$(DFSEP)PDL$(DFSEP)Doc.pm',
    'Doc$(DFSEP)Config.pm' => '$(INST_LIB)$(DFSEP)PDL$(DFSEP)Doc$(DFSEP)Config.pm',
    'Doc$(DFSEP)Perldl.pm' => '$(INST_LIB)$(DFSEP)PDL$(DFSEP)Doc$(DFSEP)Perldl.pm',
  },
  NO_MYMETA => 1,
);

package MY;
# this corrects EUMM not knowing about subdirs separated by $(DFSEP)
sub init_MANPODS {
  my ($self) = @_;
  $self->SUPER::init_MANPODS;
  for my $doc (keys %{ $self->{MAN3PODS} }) {
    $self->{MAN3PODS}{$doc} =~ s#\Q$(DFSEP)\E#::#g;
  }
}
