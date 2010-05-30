package PDL::Perldl2::Plugin::PrintControl;

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

__END__

=head1 NAME

PDL::Perldl2::Plugin::PrintControl - disable default print output

=head1 SYNOPSIS

 > $a = 3;
 3
 > $_REPL->load_plugin('PDL::Perldl2::Plugin::PrintControl');

 > $a;

 > $_REPL->do_print(1);
 1
 > $a;
 2

=head1 DESCRIPTION

By default the Devel::REPL always prints the results of its
evaluation.  This is fine for small objects but for things
like large data objects (e.g. a 100x100 matrix in PDL) the
result can be hundreds of lines of output for each command.

This plugin disables the default print output and adds an
attribute with accessor method C<do_print> which can be
used to toggle the print default on or off.

=head1 METHODS

=head2 do_print

By default, the C<PrintControl> plugin sets C<do_print> to
0 (false), which disables automatic printing of results.
Call the do_print accessor with a 1 (true value) to enable
default printing.

=head1 SEE ALSO

C<Devel::REPL>

=head1 AUTHOR

Chris Marshall, C<< <chm at cpan dot org> >>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2010 by Christopher Marshall

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
