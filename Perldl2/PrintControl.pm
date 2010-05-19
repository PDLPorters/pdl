package Devel::REPL::Plugin::PrintControl;

use Devel::REPL::Plugin;

use namespace::clean -except => [ 'meta' ];

has 'do_print' => (
             is  => 'rw',
             default => 0,
         );

around 'format_result' => sub {

  my ($orig, $self) = (shift, shift);
  my ($lines, @args) = @_;

  return $self->do_print ? $orig->($self, @_) : ();

};

1;
