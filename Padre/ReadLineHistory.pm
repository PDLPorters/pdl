# First cut at using the readline history directly rather than reimplementing
# it. It does save history but it's a little crappy; still playing with it ;)
#
# epitaph, 22nd April 2007

package Devel::REPL::Plugin::ReadLineHistory;

use Devel::REPL::Plugin;
use File::HomeDir;
use File::Spec;

my $hist_file = $ENV{PERLREPL_HISTFILE} ||
    File::Spec->catfile(File::HomeDir->my_home, '.perlreplhist');

# HISTLEN should probably be in a config file to stop people accidentally
# truncating their history if they start the program and forget to set
# PERLREPL_HISTLEN
my $hist_len=$ENV{PERLREPL_HISTLEN} || 100;

around 'run' => sub {
  my $orig=shift;
  my ($self, @args)=@_;
  if ($self->term->Attribs->{ReadLine} eq 'Term::ReadLine::Gnu') {
     $self->term->stifle_history($hist_len);
  }
  if ($self->term->Attribs->{ReadLine} eq 'Term::ReadLine::Perl') {
     $self->term->Attribs->{MaxHistorySize} = $hist_len;
  }
  -f($hist_file) && $self->term->ReadHistory($hist_file);
  $self->term->Attribs->{do_expand}=1;
  $self->$orig(@args);
  $self->term->WriteHistory($hist_file) ||
    $self->print("warning: failed to write history file $hist_file");
};

1;

__END__

=head1 NAME

Devel::REPL::Plugin::ReadLineHistory - Integrate history with the facilities provided by L<Term::ReadLine>

=cut

