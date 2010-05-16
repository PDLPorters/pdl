package Devel::REPL::Plugin::PrintControl;

use Devel::REPL::Plugin;

use namespace::clean -except => [ 'meta' ];

around 'format_result' => sub {

  my ($orig, $self) = (shift, shift);
  my ($lines, @args) = @_;

  return ();

};

1;
