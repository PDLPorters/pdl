package Test::PDL;

=head1 NAME

Test::PDL -- test piddles for equality

=head1 SYNOPSIS

	use PDL;
	use Test::PDL tests => 2;

	# an example of a test that succeeds
	$got      = sequence 5;
	$expected = pdl( 0,1,2,3,4 );
	is_pdl( $got, $expected, 'sequence() works as expected' );
	#   OUTPUT:
	# ok 1 - sequence() works as expected

	# if a test fails, detailed diagnostics are printed; the output is
	# similar to that of is() from L<Test::More>
	$got      = pdl( 0,-1,-2,3,4 );
	$expected = sequence 5;
	is_pdl( $got, $expected, 'demonstrate the output of a failing test' );
	#   OUTPUT:
	# not ok 2 - demonstrate the output of a failing test
	#
	#   Failed test 'demonstrate the output of a failing test'
	#   at aux/pod.t line 15.
	#     values do not match
	#          got: Double   D [5]        (P    ) [0 -1 -2 3 4]
	#     expected: Double   D [5]        (P    ) [0 1 2 3 4]

=cut

use strict;
use warnings;
use base qw( Test::Builder::Module );
use PDL::Lite;
our @EXPORT = qw( is_pdl );
our $VERSION = '0.01';

=head1 DESCRIPTION

With Test::PDL, you can compare two piddles for equality. The comparison is
performed as thoroughly as possible, comparing types, dimensions, bad value
patterns, and finally the values themselves. Test::PDL is mostly useful in test
scripts.

Test::PDL exports only one function: is_pdl().

=head1 FUNCTIONS

=head2 _comparison_fails

Internal function which does the real work of comparing two piddles. If the
comparison fails, _comparison_fails() returns a string containing the reason for
failure. If the comparison succeeds, _comparison_fails() returns zero.

The criteria for equality are the following:

=over 4

=item *

Both arguments must be piddles for the comparison to succeed. Currently, there
is no implicit conversion from scalar to piddle.

=item *

The type of both piddles must be equal.

=item *

The number of dimensions must be equal. That is, a two-dimensional piddle only
compares equal with another two-dimensional piddle.

=item *

The extent of the dimensions are compared one by one and must match. That is, a
piddle with dimensions (5,4) cannot compare equal with a piddle of dimensions
(5,3). Note that degenerate dimensions are not treated specially, and thus a
piddle with dimensions (5,4,1) is considered different from a piddle with
dimensions (5,4).

=item *

For piddles that conform in type and shape, the bad value pattern is examined.
If the two piddles have bad values in different positions, the piddles are
considered different. Note that two piddles may compare equal even though their
bad flag is different, if there are no bad values.

=item *

And last but not least, the values themselves are examined one by one. For
integer types, the comparison is performed exactly, whereas an approximate
equality is used for floating-point types. The approximate comparison is
implemented using approx(). Note that approx() will use the tolerance set by a
previous call to approx(), or the default tolerance if none was set before.

=back

=cut

sub _comparison_fails
{
	my ( $got, $expected ) = @_;
	if( not eval { $got->isa('PDL') } ) {
		return 'received value is not a PDL';
	}
	if( not eval { $expected->isa('PDL') } ) {
		return 'expected value is not a PDL';
	}
	if( $got->type != $expected->type ) {
		return 'types do not match';
	}
	if( $got->ndims != $expected->ndims ) {
		return 'dimensions do not match in number';
	}
	if( not _dimensions_match( [$got->dims], [$expected->dims] ) ) {
		return 'dimensions do not match in extent';
	}
	# evaluating these only makes sense for piddles that conform in shape
	if( ( $got->badflag == 1 || $expected->badflag == 1 ) &&
		not eval { PDL::all( PDL::isbad($got) == PDL::isbad($expected) ) } ) {
		return 'bad value patterns do not match';
	}
	# if we get here, types and bad value patterns are sure to match
	if( $got->type < PDL::float ) {
		if( not eval { PDL::all( $got == $expected ) } ) {
			return 'values do not match';
		}
	}
	else {
		# floating-point comparison must be approximate
		if( not eval { PDL::all( PDL::approx $got, $expected ) } ) {
			return 'values do not match';
		}
	}
	# if we get here, we didn't fail
	return 0;
}

=head2 _dimensions_match

Internal function which compares the extent of each of the dimensions of two
piddles, one by one. The dimensions must be passed in as two array references.
Returns 1 if all dimensions match pairwise. Returns 0 otherwise.

This function will not operate correctly if the number of dimensions does not
match between the piddles, so be sure to check that before calling this
function.

=cut

sub _dimensions_match
{
	my @A = @{ +shift };
	my @B = @{ +shift };
	while( my $a = shift @A and my $b = shift @B ) {
		if( $a != $b ) { return 0 }
	}
	return 1;
}

=head2 is_pdl

=for ref

Run a test comparing a piddle to an expected piddle, and fail with detailed
diagnostics if they don't compare equal.

=for usage

	is_pdl( $got, $expected, $test_name );

Yields ok if the first two arguments are piddles that compare equal, not ok if
the piddles are different, or if at least one is not a piddle. Prints a
diagnostic when the comparison fails, with the reason and a brief printout of
both arguments. See the documentation of _comparison_fails() for the comparison
criteria. $test_name is optional.

Named after is() from L<Test::More>.

=cut

sub is_pdl
{
	my ( $got, $expected, $name ) = @_;
	$name ||= "piddles are equal";
	my $tb = __PACKAGE__->builder;
	if( my $reason = _comparison_fails $got, $expected ) {
		my $rc = $tb->ok( 0, $name );
		my $fmt = '%-8T %-12D (%-5S) ';
		$tb->diag( "    $reason\n",
			   "         got: ", eval { $got->isa('PDL') }      ? $got->info( $fmt )      : '', $got, "\n",
			   "    expected: ", eval { $expected->isa('PDL') } ? $expected->info( $fmt ) : '', $expected );
		return $rc;
	}
	else {
		return $tb->ok( 1, $name );
	}
}

1;
__END__

=head1 BUGS

None reported so far.

=head1 SEE ALSO

L<PDL>, L<Test::More>

=head1 AUTHOR

Edward Baudrez C<< <ebaudrez@cpan.org> >>

=head1 COPYRIGHT AND LICENSE

Copyright (c) 2010 -- 2012 Edward Baudrez. All rights reserved.

This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut

