package PDL::Perldl2::Plugin::PrintControl;

use Devel::REPL::Plugin;

use namespace::clean -except => [ 'meta' ];

has 'print_by_default' => (
             is  => 'rw',
             default => 0,
         );

around 'format_result' => sub {

  my ($orig, $self) = (shift, shift);
  my ($lines, @args) = @_;

  return $self->print_by_default ? $orig->($self, @_) : ();

};

# convenience method to set/toggle print default settings
# sets like accessor if given a value, otherwise toggles status
sub do_print {
   my ($repl, $value) = @_;
   $value = (defined $value) ? $value : ! $repl->print_by_default;
   return $repl->print_by_default($value);
}

1;

__END__

=head1 NAME

PDL::Perldl2::Plugin::PrintControl - disable default print output

=head1 SYNOPSIS

  pdl> $x = 3;
  3
  pdl> $_REPL->load_plugin('PDL::Perldl2::Plugin::PrintControl');

  pdl> $x;

  pdl> $_REPL->print_by_default(1);
  1
  pdl> $x;
  3

=head1 DESCRIPTION

By default the Devel::REPL always prints the results of its
evaluation.  This is fine for small objects but for things
like large data objects (e.g. a 100x100 matrix in PDL) the
result can be hundreds of lines of output for each command.

This plugin disables the default print output and adds an
attribute with accessor method C<print_by_default> which can be
used to toggle the print default on or off.

=head1 METHODS

=head2 print_by_default

By default, the C<PrintControl> plugin sets C<print_by_default> to
0 (false), which disables automatic printing of results.
Call the print_by_default accessor with a 1 (true value) to enable
default printing.

=head2 do_print

This is a convenience accessor for the print_by_default attribute.
If you call this method without a value, it toggles the current
setting.  Otherwise, it just sets print_by_default to the value.

It is also available in the C<pdl2> shell as the do_print sub
with the same operation but with an implicit use of C<$_REPL>.

=head1 SEE ALSO

C<Devel::REPL>

=head1 AUTHOR

Chris Marshall, C<< <chm at cpan dot org> >>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2010 by Christopher Marshall

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
