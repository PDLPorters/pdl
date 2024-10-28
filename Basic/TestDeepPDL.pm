use strict;
use warnings;

package Test::Deep::PDL;

# ABSTRACT: Test ndarrays inside data structures with Test::Deep

=for Pod::Coverage init descend diag_message renderExp renderGot

=cut

use Test::Deep::Cmp;
require Test::PDL;

=head1 DESCRIPTION

This is just an implementation class. Look at the documentation for test_pdl()
in L<Test::PDL>.

=cut

sub init
{
	my $self = shift;
	my $expected = shift;
	die "Supplied value is not an ndarray" unless eval { $expected->isa('PDL') };
	$self->{expected} = $expected;
}

sub descend
{
	my $self = shift;
	my $got = shift;
	( my $ok, $self->data->{diag} ) = Test::PDL::eq_pdl( $got, $self->{expected} );
	return $ok;
}

sub diag_message
{
	my $self = shift;
	my $where = shift;
	return "Comparing $where as an ndarray:\n" . $self->data->{diag};
}

sub renderExp
{
	my $self = shift;
	return $self->renderGot( $self->{expected} );
}

sub renderGot
{
	my $self = shift;
	my $val = shift;
	my $fmt = '%-8T %-12D (%-5S) ';
	return eval { $val->isa('PDL') } ? ($val->info($fmt) . $val) : ("(" . Test::Deep::render_val($val) . ")");
}

=head1 BUGS

The implementation of this class depends on undocumented subroutines in
L<Test::Deep>. This may break if L<Test::Deep> gets refactored.

=head1 SEE ALSO

L<PDL>, L<Test::PDL>, L<Test::Deep>

=cut

1;
