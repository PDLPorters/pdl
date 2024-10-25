use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME         => 'PDL::Doc',
    VERSION_FROM => '../PDL.pm',
    PM           => { 'Doc.pm' => '$(INST_LIB)$(DFSEP)PDL$(DFSEP)Doc.pm',
                      'Doc$(DFSEP)Config.pm' => '$(INST_LIB)$(DFSEP)PDL$(DFSEP)Doc$(DFSEP)Config.pm', # left side must be same as PL_FILES right side
                      'Doc$(DFSEP)Perldl.pm' => '$(INST_LIB)$(DFSEP)PDL$(DFSEP)Doc$(DFSEP)Perldl.pm',
    },
    PL_FILES     => {q[Doc$(DFSEP)Config.pm.PL]=>q[Doc$(DFSEP)Config.pm]},
    clean        => { FILES => q[Doc/Config.pm] },
    NO_MYMETA => 1,
);

package MY;
# this corrects EUMM not knowing about subdirs separated by $(DFSEP)
sub init_MANPODS {
    my ($self) = @_;
    $self->SUPER::init_MANPODS;
    for my $doc (sort keys %{ $self->{MAN3PODS} }) {
        $self->{MAN3PODS}->{$doc} =~ s#\Q$(DFSEP)\E#::#g;
    }
}
