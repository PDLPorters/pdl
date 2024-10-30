package Test::PDL;

# ABSTRACT: Test Perl Data Language arrays (a.k.a. ndarrays) for equality

=head1 SYNOPSIS

	use PDL;
	use Test::More tests => 3;
	use Test::PDL qw( is_pdl :deep );

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
	#   at maint/pod.t line 16.
	#     2/5 values do not match
	#          got: Double   D [5]        (P    ) [0 -1 -2 3 4]
	#     expected: Double   D [5]        (P    ) [0 1 2 3 4]
	# First <=5 values differ at:
	# [
	#  [1]
	#  [2]
	# ]
	# Those 'got' values: [-1 -2]
	# Those 'expected' values: [1 2]

	# ndarrays within other data structures can be tested with Test::Deep
	use Test::Deep qw( cmp_deeply );
	$got      = { name => 'Histogram', data => long( 17,0,1 ) };
	$expected = { name => 'Histogram', data => test_long( 17,0,0,1 ) };
	cmp_deeply( $got, $expected, 'demonstrate the output of a failing deep comparison' );
	#   OUTPUT:
	# not ok 3 - demonstrate the output of a failing deep comparison
	#
	#   Failed test 'demonstrate the output of a failing deep comparison'
	#   at maint/pod.t line 30.
	# Comparing $data->{"data"} as an ndarray:
	# dimensions do not match in extent
	#    got : Long     D [3]        (P    ) [17 0 1]
	# expect : Long     D [4]        (P    ) [17 0 0 1]

=cut

use strict;
use warnings;
use PDL::Lite;
use PDL::Types ();

use base qw( Exporter );
our @EXPORT = qw( is_pdl );
our @EXPORT_OK = qw( eq_pdl is_pdl test_pdl );
our %EXPORT_TAGS = ( deep => [ qw( test_pdl ) ] );
our $VERSION = '0.22';

=head1 DESCRIPTION

With Test::PDL, you can compare two ndarrays for equality. The comparison is
performed as thoroughly as possible, comparing types, dimensions, bad value
patterns, and finally the values themselves. The exact behaviour can be
configured by setting certain package-wide defaults (see %DEFAULTS below), or
by supplying options in a function call.
Test::PDL is mostly useful in test scripts.

Test::PDL is to be used with the Perl Data Language (L<PDL>).

By default, Test::PDL exports only one function: is_pdl(). The other functions
are exported on demand only. The export tag C<:deep> exports test_pdl() and one
function for each PDL type constructor (like short(), double(), etc.), prefixed
with C<test_>: test_short(), test_double(), ...

=head1 VARIABLES

=head2 %DEFAULTS

The default comparison criteria used by Test::PDL can be configured by setting
the values in the %DEFAULTS hash. This can be done directly, by addressing
%Test::PDL::DEFAULTS directly.

=over 4

=item atol

The absolute tolerance used to compare values. Initially set to 1e-6.

=item require_equal_types

If true, only ndarrays with equal type can be considered equal. If false, the
types of the ndarrays being compared is not taken into consideration. Defaults
to true: types must match for the comparison to succeed. If you want to
write tests like

	is_pdl( $got, pdl([ 1, 3, 5, 6 ]) );

without having to worry about the type of the ndarray being exactly I<double>
(which is the default type of the pdl() constructor), set I<require_equal_types> equal to
0.

=item rtol

The relative tolerance used to compare values. Initially set to 1e-6.

=back

=cut

our %DEFAULTS = (
	atol                => 1e-6,
	require_equal_types => 1,
	rtol                => 1e-6,
);

=head1 FUNCTIONS

=head2 import

Custom importer that recognizes configuration defaults specified at use time, as
in

	use Test::PDL -require_equal_types => 0;

=cut

sub import
{
	my $i = 0;
	while( $i < @_ ) {
		if( $_[ $i ] =~ /^-/ ) {
			my( $key, $val ) = splice @_, $i, 2;
			$key =~ s/^-(.*)/$1/;
			PDL::barf( "invalid name $key" ) unless grep { $key eq $_ } keys %DEFAULTS;
			PDL::barf( "undefined value for $key" ) unless defined $val;
			$DEFAULTS{ $key } = $val;
		}
		else { $i++ }
	}
	__PACKAGE__->export_to_level( 1, @_ );
}

=head2 is_pdl

=for ref # PDL

Run a test comparing an ndarray to an expected ndarray, and fail with detailed
diagnostics if they don't compare equal.

=for usage # PDL

	is_pdl( $got, $expected );
	is_pdl( $got, $expected, $test_name );
	is_pdl( $got, $expected, { test_name => $test_name } );
	is_pdl( $got, $expected, { atol => $absolute_tolerance, ... } );

Yields ok if the first two arguments are ndarrays that compare equal, not ok if
the ndarrays are different, or if at least one is not an ndarray. Prints a
diagnostic when the comparison fails, with the reason and a brief printout of
both arguments. See the documentation of eq_pdl() for the comparison
criteria. $test_name is optional.

Named after is() from L<Test::More>.

=cut

sub is_pdl {
  require Test::Builder;
  my $tb = Test::Builder->new;
  $tb->croak('error in arguments: > 3 given') if @_ > 3;
  my ( $got, $expected, $arg ) = @_;
  $tb->croak('error in arguments: third argument is an ndarray')
    if eval { $arg->isa('PDL') };
  my $opt = { %DEFAULTS };
  my $name;
  if ($arg) {
    if (ref $arg eq 'HASH') { $opt = { %$opt, %$arg } }
    else { $name = $arg }
  }
  $name ||= $opt->{test_name} || "ndarrays are equal";
  my ($ok, $reason, $mask) = eq_pdl($got, $expected, $opt);
  return $tb->ok(1, $name) if $ok;
  my $rc = $tb->ok( 0, $name );
  my $fmt = '%-8T %-12D (%-5S) ';
  my @mismatch;
  if (defined $mask) {
    my $coords = defined $mask ? $mask->not->whichND : undef;
    $coords = $coords->slice(',0:4') if defined $coords and $coords->dim(1) > 5;
    my $cstr = $coords->string; $cstr =~ s#\n+\z##;
    push @mismatch, (
      "\nFirst <=5 values differ at: $cstr\n",
      "Those 'got' values: ", $got->indexND($coords),
      "\nThose 'expected' values: ", $expected->indexND($coords),
    );
  }
  $tb->diag(
    "    $reason\n",
    "         got: ", eval { $got->isa('PDL')      && !$got->isnull      } ? $got->info( $fmt )      : '', $got, "\n",
    "    expected: ", eval { $expected->isa('PDL') && !$expected->isnull } ? $expected->info( $fmt ) : '', $expected,
    @mismatch,
  );
  return $rc;
}

=head2 eq_pdl

=for ref # PDL

Return true if two ndarrays compare equal, false otherwise. In list context,
additionally returns a diagnostic string.

=for usage # PDL

	my $equal = eq_pdl( $got, $expected );
	my $equal = eq_pdl( $got, $expected, { atol => $absolute_tolerance, ... } );
	my( $equal, $diag ) = eq_pdl( $got, $expected );
	my( $equal, $diag ) = eq_pdl( $got, $expected, { atol => $absolute_tolerance, ... } );

eq_pdl() contains just the comparison part of is_pdl(), without the
infrastructure required to write tests with L<Test::More>. It could be used as
part of a larger test in which the equality of two ndarrays must be verified. By
itself, eq_pdl() does not generate any output, so it should be safe to use
outside test suites.

In list context, eq_pdl() returns a list with three elements, the first one being
a boolean whether the ndarrays compared equal, the second being a diagnostic
string explaining why the comparison failed (or the empty string, if it didn't
fail). The third is either the mask of not-equal if the values didn't
match, or C<undef>.
This is useful in combination with L<Test::Deep>, but might also be
useful on its own.

eq_pd() does not need L<Test::Builder>, so you can use it as part of something
else, without side effects (like generating output).

The criteria for equality are the following:

=over 4

=item *

Both arguments must be ndarrays for the comparison to succeed. Currently, there
is no implicit conversion from scalar to ndarray.

=item *

The type of both ndarrays must be equal if (and only if) I<require_equal_types> is true.

=item *

The number of dimensions must be equal. That is, a two-dimensional ndarray only
compares equal with another two-dimensional ndarray.

=item *

The extent of the dimensions are compared one by one and must match. That is, a
ndarray with dimensions (5,4) cannot compare equal with an ndarray of dimensions
(5,3). Note that degenerate dimensions are not treated specially, and thus a
ndarray with dimensions (5,4,1) is considered different from an ndarray with
dimensions (5,4).

=item *

For ndarrays that conform in type and shape, the bad value pattern is examined.
If the two ndarrays have bad values in different positions, the ndarrays are
considered different. Note that two ndarrays may compare equal even though their
bad flag is different, if there are no bad values.

=item *

And last but not least, the values themselves are examined one by one.
As of 0.21, both integer and floating-point types are compared approximately.
The approximate comparison is
implemented using a combination of relative and absolute tolerances, which can
be set by supplying an argument to C<use Test::PDL>, or by supplying an
optional hash to this function. By default, the absolute and relative
tolerances are both equal to 1e-6. The user can specify a pure relative
tolerance by specifying C<atol =E<gt> 0>, and a pure absolute tolerance by
specifying C<rtol =E<gt> 0>. If both tolerances are specified, values compare
equal if I<either> their difference is lower than or equal to the absolute
tolerance I<or> their relative difference (with respect to the expected
value) is lower than or equal to the relative tolerance. For expected
values equal to zero, relative differences (with respect to the expected
value) make no sense, and the use of combined absolute and relative
tolerances is recommended.

=back

=cut

sub eq_pdl {
  my ($got, $expected, $arg) = @_;
  my $opt = { %DEFAULTS, ref $arg eq 'HASH' ? %$arg : () };
  PDL::barf( 'need an absolute or a relative tolerance, or both' ) unless defined $opt->{atol} || defined $opt->{rtol};
  $opt->{atol} //= 0;
  $opt->{rtol} //= 0;
  PDL::barf('absolute tolerance cannot be negative') if $opt->{atol} < 0;
  PDL::barf('relative tolerance cannot be negative') if $opt->{rtol} < 0;
  return wantarray ? (0, 'received value is not an ndarray', undef) : 0
    if !eval { $got->isa('PDL') };
  return wantarray ? (0, 'expected value is not an ndarray', undef) : 0
    if !eval { $expected->isa('PDL') };
  return wantarray ? (0, 'types do not match (\'require_equal_types\' is true)', undef) : 0
    if $opt->{require_equal_types} && $got->type != $expected->type;
  my @got_dims = $got->dims;
  my @exp_dims = $expected->dims;
  return wantarray ? (0, 'dimensions do not match in number', undef) : 0
    if @got_dims != @exp_dims;
  while (@got_dims) {
    return wantarray ? (0, 'dimensions do not match in extent', undef) : 0
      if shift(@got_dims) != shift(@exp_dims);
  }
  return wantarray ? (1, '', undef) : 1
    if $got->isempty and $expected->isempty;
  # both are now non-empty
  my $res = PDL::Primitive::approx_artol( $got, $expected, @$opt{qw(atol rtol)} );
  return wantarray ? (1, '', undef) : 1 if $res->all;
  my $exp_nelem = $expected->nelem;
  my $reason = ($exp_nelem-$res->sum)."/$exp_nelem values do not match";
  return wantarray ? (0, $reason, $res) : 0;
}

=head2 test_pdl

=for ref # PDL

Special comparison to be used in conjunction with L<Test::Deep> to test ndarrays
inside data structures.

=for usage # PDL

	my $expected = { ..., some_field => test_pdl( 1,2,-7 ), ... };
	my $expected = [ ..., test_short( 1,2,-7 ), ... ];

Suppose you want to compare data structures that happen to contain ndarrays. You
use is_deeply() (from L<Test::More>) or cmp_deeply() (from L<Test::Deep>) to
compare the structures element by element. Unfortunately, you cannot just write

	my $got = my_sub( ... );
	my $expected = {
		...,
		some_field => pdl( ... ),
		...
	};
	is_deeply $got, $expected;

Neither does cmp_deeply() work in the same situation. is_deeply() tries to
compare the ndarrays using the (overloaded) C<==> comparison operator, which
doesn't work. It simply dies with an error message saying that multidimensional
ndarrays cannot be compared, whereas cmp_deeply() performs only a shallow
comparison of the references.

What you need is a special comparison, which is provided by this function, to
be used with cmp_deeply(). You need to rewrite $expected as follows

	my $expected = {
		...,
		some_field => test_pdl( ... ),
		...
	};
	cmp_deeply $got, $expected;

Note that you need to write test_pdl() instead of pdl(). You could achieve the
same thing with

	my $expected = {
		...,
		some_field => code( sub { eq_pdl( shift, pdl( ... ) ) } ),
		...
	};

but the diagnostics provided by test_pdl() are better, and it's easier to use.
test_pdl() accepts the same arguments as the PDL constructor pdl() does. If you
need to compare an ndarray with a type different from the default type, use one
of the provided test_byte(), test_short(), test_long(), etc.:

	my $expected = { data => test_short( -4,-9,13 ) };

If you need to manipulate the expected value, you should keep in mind that the
return value of test_pdl() and the like are not ndarrays. Therefore, in-place
modification of the expected value won't work:

	my $expected = { data => test_short( -99,-9,13 )->inplace->setvaltobad( -99 ) }; # won't work!

You should rather do

	my $expected = { data => test_pdl( short(-99,-9,13)->inplace->setvaltobad(-99) ) };

test_pdl() will correctly set the type of the expected value to I<short> in the
above example.

=cut

sub test_pdl
{
	require Test::Deep::PDL;
	my $expected = PDL::Core::pdl( @_ );
	return Test::Deep::PDL->new( $expected );
}

=for Pod::Coverage test_anyval test_byte test_short test_ushort test_long
test_indx test_longlong test_float test_double test_cfloat test_cdouble
test_cldouble test_ldouble test_sbyte test_ulong test_ulonglong

=cut

for my $type ( PDL::Types::types ) {
	my $sub = sub {
		require Test::Deep::PDL;
		my $expected = PDL::convert(
			PDL::Core::alltopdl( 'PDL', scalar(@_) > 1 ? [@_] : shift ),
			$type->numval
		);
		return Test::Deep::PDL->new( $expected );
	};
	my $sub_name = 'test_' . $type->convertfunc;
	{
		no strict 'refs';
		*$sub_name = $sub;
	}
	push @EXPORT_OK, $sub_name;
	push @{ $EXPORT_TAGS{deep} }, $sub_name;
}

=head1 BUGS

None reported so far.

=head1 SEE ALSO

L<PDL>, L<Test::More>, L<Test::Deep>, L<Test::PDL::Deep>

=head1 ACKNOWLEDGMENTS

Thanks to PDL Porters Joel Berger, Chris Marshall, and David Mertens for
feedback and improvements.

Thanks to Ed J, Zakariyya Mughal, and Diab Jerius for feedback, improvements,
maintenance of the code, and encouragement!

=cut

1;
