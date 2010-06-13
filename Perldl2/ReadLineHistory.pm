# First cut at using the readline history directly rather than reimplementing
# it. It does save history but it's a little crappy; still playing with it ;)
#
# epitaph, 22nd April 2007

package  # prevent indexing
    Devel::REPL::Plugin::ReadLineHistory;

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
   if ($self->term->ReadLine eq 'Term::ReadLine::Gnu') {
      $self->term->stifle_history($hist_len);
   }
   if ($self->term->ReadLine eq 'Term::ReadLine::Perl') {
      $self->term->Attribs->{MaxHistorySize} = $hist_len;
   }
   if (-f($hist_file)) {
      if ($self->term->ReadLine eq 'Term::ReadLine::Gnu') {
         $self->term->ReadHistory($hist_file);
      }
      if ($self->term->ReadLine eq 'Term::ReadLine::Perl') {
         open HIST, $hist_file or die "ReadLineHistory: could not open $hist_file: $!\n";
         while (my $line = <HIST>) {
            chomp $line;
            $self->term->addhistory($line);
         }
         close HIST;
      }
   }

   $self->term->Attribs->{do_expand}=1;  # for Term::ReadLine::Gnu
   $self->term->MinLine(2);              # don't save one letter commands

   # let History plugin know we have Term::ReadLine support
   $self->have_readline_history(1) if $self->can('have_readline_history');


   $self->$orig(@args);

   if ($self->term->ReadLine eq 'Term::ReadLine::Gnu') {
      $self->term->WriteHistory($hist_file) ||
      $self->print("warning: failed to write history file $hist_file");
   }
   if ($self->term->ReadLine eq 'Term::ReadLine::Perl') {
      my @lines = $self->term->GetHistory() if $self->term->can('GetHistory');
      if( open HIST, ">$hist_file" ) {
         print HIST join("\n",@lines);
         close HIST;
      } else {
         $self->print("warning: unable to WriteHistory to $hist_file");
      }
   }
};

1;

__END__

=head1 NAME

Devel::REPL::Plugin::ReadLineHistory - Integrate history with the facilities provided by L<Term::ReadLine>

=head1 DESCRIPTION

This plugin enables loading and saving command line history from
a file as well has history expansion of previous commands using
the !-syntax a la bash.

By default, history expansion is enabled with this plugin when
using L<Term::ReadLine::Gnu|Term::ReadLine::Gnu>. That means that
"loose" '!' characters will be treated as history events which
may not be what you wish.

To avoid this, you need to quote the '!' with '\':

  my $var = "foo\!";

or place the arguments in single quotes---but enable the
C<Term::ReadLine> attribute C<history_quotes_inhibit_expansion>:

  $_REPL->term->Attribs->{history_quotes_inhibit_expansion} = 1;
  my $var = 'foo!';

and to disable history expansion from GNU readline/history do

  $_REPL->term->Attribs->{do_expand} = 0;

=head1 CONFLICTS

Note that Term::ReadLine::Perl does not support a history
expansion method.  In that case, you may wish to use the
Devel::REPL History plugin which provides similar functions.
Work is underway to make use of either History or
ReadLineHistory consistent for expansion with either the
Term::ReadLine::Gnu support or Term::ReadLine::Perl.

=cut

