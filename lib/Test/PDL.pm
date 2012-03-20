package Test::PDL;

=head1 NAME

Test::PDL - test piddles for equality

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
use Test::Builder;
use PDL::Lite;

use base 'Exporter';
our @EXPORT = qw( is_pdl );
our $VERSION = '0.01';

my $tb = Test::Builder->new;

=head1 DESCRIPTION

With Test::PDL, you can compare two piddles for equality. The comparison is
performed as thoroughly as possible, comparing types, dimensions, bad value
patterns, and finally the values themselves. The exact behaviour can be
configured by setting certain options (see set_options() and %OPTIONS below).
Test::PDL is mostly useful in test scripts.

Test::PDL exports only one function: is_pdl().

=head1 VARIABLES

=head2 %OPTIONS

The comparison criteria used by is_pdl() can be configured by setting the
values in the %OPTIONS hash. This can be done directly, by addressing
%Test::PDL::OPTIONS directly. However, it is preferred that set_options() is
used instead.

=over 4

=item TOLERANCE

The tolerance used to compare floating-point values. Initially set to 1e-6.
This is currently an absolute tolerance, meaning that two values compare equal
if the absolute value of their difference is below the tolerance.

=item EQUAL_TYPES

If true, only piddles with equal type can be considered equal. If false, the
types of the piddles being compared is not taken into consideration. Defaults
to false. The default allows to write tests like

	is_pdl( $got, pdl([ 1, 3, 5, 6 ]) );

without having to worry about the type of the piddle being exactly I<double>
(which is the default type of the pdl() constructor).

=back

=cut

our %OPTIONS = (
	TOLERANCE => 1e-6,
	EQUAL_TYPES => 0,
);

=head1 FUNCTIONS

=head2 _approx

Internal function reimplementing the functionality of PDL::approx(), but with a
tolerance that is not remembered across invocations. Rather, the tolerance can
be set by the user (see set_options() and $OPTIONS{TOLERANCE}), and defaults to
1e-6.

=cut

sub _approx
{
	my( $a, $b ) = @_;
	return abs( $a - $b ) < $OPTIONS{ TOLERANCE };
}

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
implemented using a private reimplementation of PDL::approx(). See _approx()
for more information.

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
	if( $OPTIONS{ EQUAL_TYPES } && $got->type != $expected->type ) {
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
	# if we get here, bad value patterns are sure to match
	if( $got->type < PDL::float && $expected->type < PDL::float ) {
		if( not eval { PDL::all( $got == $expected ) } ) {
			return 'values do not match';
		}
	}
	else {
		# floating-point comparison must be approximate
		if( not eval { PDL::all( _approx $got, $expected ) } ) {
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

=head2 set_options

=for ref

Configure the comparison carried out by is_pdl().

=for example

	# e.g., if a tolerance of 1e-6 is too tight
	Test::PDL::set_options( TOLERANCE => 1e-4 );

The preferred way to set the options to this module. See %OPTIONS for all
allowed options. set_options() dies with an error if an unknown option is
passed. Note that sensible default values are provided for all options, so you
needn't use this routine if you are fine with the defaults.

This function is not exported. Rather, it must be called as

	Test::PDL::set_options( KEY => VALUE, ... );

=cut

sub set_options
{
	while( my( $key, $value ) = splice @_, 0, 2 ) {
		barf( "invalid option $key" ) unless grep { $key eq $_ } keys %OPTIONS;
		barf( "undefined value for $key" ) unless defined $value;
		$OPTIONS{ $key } = $value;
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

