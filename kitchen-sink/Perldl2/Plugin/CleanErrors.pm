package PDL::Perldl2::Plugin::CleanErrors;

use Devel::REPL::Plugin;

use namespace::clean -except => [ 'meta' ];

around 'error_return' => sub {
   my ($orig, $self) = (shift, shift);
   my ($type, $error) = @_;

   return $orig->($self, $type, clean_error_string($error));
};

# filter out the Devel::REPL, Class::MOP, ... from pdl2 errors
sub clean_error_string {
   my $bigerr = $_[0];
   $bigerr =~ s/^\s+Devel::REPL.*$//ms;
   $bigerr =~ s/^\s+Class::MOP.*$//ms;
   $bigerr =~ s/^\s+Lexical::Persistence.*$//ms;
   $bigerr =~ s/^\s+main::.*$//ms;
   $bigerr =~ s/^\s+eval \{.*$//ms;
   $bigerr =~ s/^\s+PDL::Core::barf.*$//ms;
   return $bigerr;
}

1;

__END__

=head1 NAME

PDL::Perldl2::Plugin::CleanErrors - filter out Moose cruft

=head1 DESCRIPTION

Runtime errors in pdl2 are extremely verbose since they
include the entire call chain from the start of the interactive
Devel::REPL shell, through the Moose and Class::MOP stuff and
including Lexical::Persistence as well.  This plugin, which
is loaded by default, strips out the non-PDL stuff to make the
error messages much more concise.


=head1 SEE ALSO

C<Devel::REPL>

=head1 AUTHOR

Chris Marshall, C<< <chm at cpan dot org> >>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2011 by Christopher Marshall

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
