use strict;
use warnings;
use Test::More;
use PDL;
use Test::PDL qw( eq_pdl );
my @warns; $SIG{__WARN__} = sub {push @warns, @_};

sub run_eq_pdl
{
	my $scalar = eq_pdl(@_);
	my @list = eq_pdl(@_);
	cmp_ok(scalar @list, '==', 3, 'eq_pdl() returns a list with two elements in list context');
	cmp_ok $scalar, '==', $list[0], '... and first element matches the return value in scalar context';
	return @list;
}

my $values_not_match = qr/values do not match/;

my ( $got, $expected, $ok, $diag );

( $ok, $diag ) = run_eq_pdl();
ok !$ok, 'rejects missing arguments';
is $diag, 'received value is not an ndarray';

$got = pdl( 9,-10 );
( $ok, $diag ) = run_eq_pdl( $got );
ok !$ok, 'rejects missing arguments';
is $diag, 'expected value is not an ndarray';

$expected = 3;
$got = 4;
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok !$ok, 'rejects non-ndarray arguments';
is $diag, 'received value is not an ndarray';

$expected = 3;
$got = long( 3,4 );
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok !$ok, 'rejects non-ndarray arguments';
is $diag, 'expected value is not an ndarray';

$expected = short( 1,2 );
$got = -2;
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok !$ok, 'rejects non-ndarray arguments';
is $diag, 'received value is not an ndarray';

$expected = long( 3,4 );
$got = pdl( 3,4 );
( $ok, $diag ) = run_eq_pdl( $got, $expected, { require_equal_types => 0 } );
ok $ok, 'all else being equal, compares equal on differing types when \'require_equal_types\' is false';
is $diag, '';

$expected = long( 3,4 );
$got = pdl( 3,4 );
( $ok, $diag ) = run_eq_pdl( $got, $expected, { require_equal_types => 1 } );
ok !$ok, 'catches type mismatch, but only when \'require_equal_types\' is true';
is $diag, 'types do not match (\'require_equal_types\' is true)';

$expected = long( 3 );
$got = long( 3,4 );
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok !$ok, 'catches dimensions mismatches (number of dimensions)';
is $diag, 'dimensions do not match in number';

$expected = zeroes( double, 3,4 );
$got = zeroes( double, 3,4,1 );
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok !$ok, 'does not treat degenerate dimensions specially';
is $diag, 'dimensions do not match in number';

$expected = long( [ [3,4],[1,2] ] );
$got = long( [ [3,4,5], [1,2,3] ] );
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok !$ok, 'catches dimensions mismatches (extent of dimensions)';
is $diag, 'dimensions do not match in extent';

$expected = long( 4,5,6,-1,8,9 )->inplace->setvaltobad( -1 );
$got = long( 4,5,6,7,-1,9 )->inplace->setvaltobad( -1 );
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok !$ok, 'catches bad value pattern mismatch';
like $diag, $values_not_match;

$expected = long( 4,5,6,7,8,9 );
$got = long( 4,5,6,7,-8,9 );
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok !$ok, 'catches value mismatches for integer data';
like $diag, $values_not_match;

$expected = pdl( 4,5,6,7,8,9 );
$got = pdl( 4,5,6,7,-8,9 );
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok !$ok, 'catches value mismatches for floating-point data';
like $diag, $values_not_match;

$expected = pdl( 4,5,6,7,8,9 );
$got = pdl( 4,5,6,7,8.001,9 );
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok !$ok, 'approximate comparison for floating-point data fails correctly at documented default tolerance of 1e-6';
like $diag, $values_not_match;

$expected = pdl( 4,5,6,7,8,9 );
$got = pdl( 4,5,6,7,8.0000001,9 );
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok $ok, 'approximate comparison for floating-point data succeeds correctly at documented default tolerance of 1e-6';
is $diag, '';

$expected = pdl( 4,5,6,7,8,9 );
$got = pdl( 4,5,6,7,8.001,9 );
( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 1e-2 } );
ok $ok, 'approximate comparison for floating-point data succeeds correctly at user-specified tolerance of 1e-2';
is $diag, '';

$expected = pdl( 0,1,2,3,4 );
$got = sequence 5;
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok $ok, 'succeeds when it should succeed';
is $diag, '';

$expected = null;
$got = null;
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok $ok, 'null == null';
is $diag, '';

$got = zeroes(0,3);
$expected = zeroes(0,2);
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok !$ok, 'differently-shaped empties are different';
is $diag, 'dimensions do not match in extent';

$got = zeroes(0);
$expected = null;
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok $ok, 'null == empty';
is $diag, '';

$expected = null;
$got = pdl( 1,2,3 );
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok !$ok, 'pdl( ... ) != null';
is $diag, 'dimensions do not match in extent';

$expected = pdl( 1,2,3 );
$got = null;
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok !$ok, 'null != pdl( ... )';
is $diag, 'dimensions do not match in extent';

################################################################################
note 'mixed-type comparisons';

$expected = double( 0,1,2.001,3,4 );
$got = long( 0,1,2,3,4 );

( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 1e-2, require_equal_types => 0 } );
ok $ok, 'succeeds correctly for long/double';
is $diag, '';

( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 1e-6, require_equal_types => 0 } );
ok !$ok, 'fails correctly for long/double';
like $diag, $values_not_match;

$expected = short( 0,1,2,3,4 );
$got = float( 0,1,2.001,3,4 );

( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 1e-2, require_equal_types => 0 } );
ok $ok, 'succeeds correctly for float/short';
is $diag, '';

( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 1e-6, require_equal_types => 0 } );
ok !$ok, 'fails correctly for float/short';
like $diag, $values_not_match;

$expected = float( 0,-1,2.001,3,49999.998 );
$got = double( 0,-0.9999,1.999,3,49999.999 );

( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 1e-2, require_equal_types => 0 } );
ok $ok, 'succeeds correctly for double/float';
is $diag, '';

( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 1e-6, require_equal_types => 0 } );
ok !$ok, 'fails correctly for double/float';
like $diag, $values_not_match;

################################################################################
note 'tests with values of significantly different magnitudes, no zeroes';
$expected = double( 1e+3, 1, 1e-3 );
$got = double( 1.001e+3, 1.001, 1.001e-3 );

( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 0.999, rtol => 0 } );
ok !$ok, 'still fails with an absolute tolerance of 0.999';
like $diag, $values_not_match;

( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 1, rtol => 0 } );
ok $ok, 'passes with an absolute tolerance of 1';
is $diag, '';

( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 1e-4, rtol => 0 } );
ok !$ok, 'fails for case with different magnitudes and pure absolute tolerance of 1e-4';
like $diag, $values_not_match;

( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 1e-2, rtol => 0 } );
ok !$ok, 'still fails with an absolute tolerance of 1e-2';
like $diag, $values_not_match;

( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 10, rtol => 0 } );
ok $ok, 'needs an absolute tolerance of 10 to pass';
is $diag, '';

# notice the other values that are way off ...
( $ok, $diag ) = run_eq_pdl( double( 1.001e+3, 9, 9 ), $expected, { atol => 10, rtol => 0 } );
ok $ok, '... and this leads to large errors in the smaller components';
is $diag, '';

( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 0, rtol => 1e-4 } );
ok !$ok, 'should not pass with a pure relative tolerance of 1e-4';
like $diag, $values_not_match;

( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 0, rtol => 1e-2 } );
ok $ok, 'but passes with a pure relative tolerance of 1e-2';
is $diag, '';

################################################################################
note 'tests with values of significantly different magnitudes, with zeroes';
$expected = double( 1e+3, 1, 1e-9, 0 );
$got = double( 1.00001e+3, .99999, 1.00001e-9, 1e-5 );

( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 0, rtol => 1e-6 } );
ok !$ok, 'fails at pure relative tolerance of 1e-6';
like $diag, $values_not_match;

( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 0, rtol => 1e-4 } );
ok !$ok, 'but also fails at pure relative tolerance of 1e-4';
like $diag, $values_not_match;

( $ok, $diag ) = run_eq_pdl( $got, $expected, { atol => 1e-4, rtol => 1e-4 } );
ok $ok, 'needs both absolute and relative tolerances to pass';
is $diag, '';

( $ok, $diag ) = run_eq_pdl( double( 1e+3, 1, 0.001, 0 ), $expected, { atol => 1e-4, rtol => 1e-4 } );
ok !$ok, 'combination of relative and absolute tolerances avoids large relative errors in small components'; # (provided atol is not too high)
like $diag, $values_not_match;

################################################################################
note 'test perfect equality';

( $ok, $diag ) = run_eq_pdl( pdl(1), pdl(1), { atol => 1e-10 } );
ok $ok, 'perfectly equal ndarrays should always pass';
is $diag, '';

( $ok, $diag ) = run_eq_pdl( pdl(0), pdl(0), { atol => 1e-10 } );
ok $ok, 'perfectly equal ndarrays should always pass';
is $diag, '';

( $ok, $diag ) = run_eq_pdl( pdl(0), pdl(0), { rtol => 1e-10 } );
ok $ok, 'perfectly equal ndarrays should always pass';
is $diag, '';

( $ok, $diag ) = run_eq_pdl( pdl(-1e20), pdl(-1e20), { atol => 1e-10 } );
ok $ok, 'perfectly equal ndarrays should always pass';
is $diag, '';

( $ok, $diag ) = run_eq_pdl( pdl(-1e-20), pdl(-1e-20), { atol => 1e-10 } );
ok $ok, 'perfectly equal ndarrays should always pass';
is $diag, '';

( $ok, $diag ) = run_eq_pdl( double( 0,0,0 ), double( 0,0,0 ), { atol => 1e-10 } );
ok $ok, 'perfectly equal ndarrays should always pass';
is $diag, '';

( $ok, $diag ) = run_eq_pdl( double( 0,0,0 ), double( 0,0,0 ), { rtol => 1e-10 } );
ok $ok, 'perfectly equal ndarrays should always pass';
is $diag, '';

( $ok, $diag ) = run_eq_pdl( double( 0,0,0 ), double( 0,0,0 ), { atol => 1e-10, rtol => 1e-10 } );
ok $ok, 'perfectly equal ndarrays should always pass';
is $diag, '';

################################################################################
note 'test tolerance "sharpness"';

( $ok, $diag ) = run_eq_pdl( double(1.99), double(1), { atol => 0, rtol => 1 } );
ok $ok, 'passes correctly within tolerance';
is $diag, '';

( $ok, $diag ) = run_eq_pdl( double(1.99), double(1), { atol => 1, rtol => 0 } );
ok $ok, 'passes correctly within tolerance';
is $diag, '';

( $ok, $diag ) = run_eq_pdl( double(1.99), double(1), { atol => 1, rtol => 1 } );
ok $ok, 'passes correctly within tolerance';
is $diag, '';

( $ok, $diag ) = run_eq_pdl( double(2.01), double(1), { atol => 0, rtol => 1 } );
ok !$ok, 'fails correctly just outside of tolerance';
like $diag, $values_not_match;

( $ok, $diag ) = run_eq_pdl( double(2.01), double(1), { atol => 1, rtol => 0 } );
ok !$ok, 'fails correctly just outside of tolerance';
like $diag, $values_not_match;

( $ok, $diag ) = run_eq_pdl( double(2.01), double(1), { atol => 1, rtol => 1 } );
ok !$ok, 'combined tolerances should not yield a larger comparison margin';
like $diag, $values_not_match;

################################################################################
note 'miscellaneous';

$expected = long( 4,5,6,7,8,9 );
$expected->badflag( 1 );
$got = long( 4,5,6,7,8,9 );
$got->badflag( 0 );
( $ok, $diag ) = run_eq_pdl( $got, $expected );
ok $ok, "isn't fooled by differing badflags";
is $diag, '';

is "@warns", "", "no warnings";
done_testing;
