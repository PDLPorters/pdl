use strict;
use warnings;

use Test::More;
use Test::Builder::Tester;
use Test::Exception;
use PDL;
use Test::PDL;
my @warns; $SIG{__WARN__} = sub {push @warns, @_};

my ( $got, $expected );

my $values_not_match = '/#\s+\d+\/\d+\s+values do not match\n(.|\n)*/';

$expected = 3;
$got = long( 3,4 );
test_out( "not ok 1 - ndarrays are equal" );
test_fail( +2 );
test_err( '/#\s+expected value is not an ndarray\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'rejects non-ndarray arguments' );

$expected = short( 1,2 );
$got = -2;
test_out( "not ok 1 - ndarrays are equal" );
test_fail( +2 );
test_err( '/#\s+received value is not an ndarray\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'rejects non-ndarray arguments' );

$expected = long( 3,4 );
$got = pdl( 3,4 );
test_out( "ok 1 - ndarrays are equal" );
is_pdl( $got, $expected, { require_equal_types => 0 } );
test_test( 'all else being equal, compares equal on differing types when \'require_equal_types\' is false' );

$expected = long( 3,4 );
$got = pdl( 3,4 );
test_out( "not ok 1 - ndarrays are equal" );
test_fail( +2 );
test_err( '/#\s+types do not match \([\']require_equal_types[\'] is true\)\n(.|\n)*/' );
is_pdl( $got, $expected, { require_equal_types => 1 } );
test_test( 'catches type mismatch, but only when \'require_equal_types\' is true' );

$expected = long( 3 );
$got = long( 3,4 );
test_out( "not ok 1 - ndarrays are equal" );
test_fail( +2 );
test_err( '/#\s+dimensions do not match in number\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'catches dimensions mismatches (number of dimensions)' );

$expected = zeroes( double, 3,4 );
$got = zeroes( double, 3,4,1 );
test_out( "not ok 1 - ndarrays are equal" );
test_fail( +2 );
test_err( '/#\s+dimensions do not match in number\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'does not treat degenerate dimensions specially' );

$expected = long( [ [3,4],[1,2] ] );
$got = long( [ [3,4,5], [1,2,3] ] );
test_out( "not ok 1 - ndarrays are equal" );
test_fail( +2 );
test_err( '/#\s+dimensions do not match in extent\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'catches dimensions mismatches (extent of dimensions)' );

$expected = long( 4,5,6,-1,8,9 )->inplace->setvaltobad( -1 );
$got = long( 4,5,6,7,-1,9 )->inplace->setvaltobad( -1 );
test_out( "not ok 1 - ndarrays are equal" );
test_fail( +2 );
test_err( $values_not_match );
is_pdl( $got, $expected );
test_test( 'catches bad value pattern mismatch' );

$expected = long( 4,5,6,7,8,9 );
$got = long( 4,5,6,7,-8,9 );
test_out( "not ok 1 - ndarrays are equal" );
test_fail( +2 );
test_err( $values_not_match );
is_pdl( $got, $expected );
test_test( 'catches value mismatches for integer data' );

$expected = pdl( 4,5,6,7,8,9 );
$got = pdl( 4,5,6,7,-8,9 );
test_out( "not ok 1 - ndarrays are equal" );
test_fail( +2 );
test_err( $values_not_match );
is_pdl( $got, $expected );
test_test( 'catches value mismatches for floating-point data' );

$expected = pdl( 4,5,6,7,8,9 );
$got = pdl( 4,5,6,7,8.001,9 );
test_out( "not ok 1 - ndarrays are equal" );
test_fail( +2 );
test_err( $values_not_match );
is_pdl( $got, $expected );
test_test( 'approximate comparison for floating-point data fails correctly at documented default tolerance of 1e-6' );

$expected = pdl( 4,5,6,7,8,9 );
$got = pdl( 4,5,6,7,8.0000001,9 );
test_out( "ok 1 - ndarrays are equal" );
is_pdl( $got, $expected );
test_test( 'approximate comparison for floating-point data succeeds correctly at documented default tolerance of 1e-6' );

$expected = pdl( 4,5,6,7,8,9 );
$got = pdl( 4,5,6,7,8.001,9 );
test_out( "ok 1 - ndarrays are equal" );
is_pdl( $got, $expected, { atol => 1e-2 } );
test_test( 'approximate comparison for floating-point data succeeds correctly at user-specified tolerance of 1e-2' );

$expected = pdl( 0,1,2,3,4 );
$got = sequence 5;
test_out( "ok 1 - ndarrays are equal" );
is_pdl( $got, $expected );
test_test( 'succeeds when it should succeed' );

$expected = null;
$got = null;
test_out( "ok 1 - ndarrays are equal" );
is_pdl( $got, $expected );
test_test( 'null == null' );

$expected = null;
$got = pdl( 1,2,3 );
test_out( "not ok 1 - ndarrays are equal" );
test_fail( +4 );
test_err( '/#\s+dimensions do not match in extent/' );
test_err( '/#\s+got:.*/' );
test_err( '/#\s+expected:\s+Null/' );
is_pdl( $got, $expected, { require_equal_types => 0 } );
test_test( 'pdl( ... ) != null' );

$expected = pdl( 1,2,3 );
$got = null;
test_out( "not ok 1 - ndarrays are equal" );
test_fail( +4 );
test_err( '/#\s+dimensions do not match in extent/' );
test_err( '/#\s+got:\s+Null/' );
test_err( '/#\s+expected:.*/' );
is_pdl( $got, $expected, { require_equal_types => 0 } );
test_test( 'null != pdl( ... )' );

note 'mixed-type comparisons';

$expected = double( 0,1,2.001,3,4 );
$got = long( 0,1,2,3,4 );

test_out( "ok 1 - ndarrays are equal" );
is_pdl( $got, $expected, { atol => 1e-2, require_equal_types => 0 } );
test_test( 'succeeds correctly for long/double' );

test_out( "not ok 1 - ndarrays are equal" );
test_fail( +2 );
test_err( $values_not_match );
is_pdl( $got, $expected, { atol => 1e-6, require_equal_types => 0 } );
test_test( 'fails correctly for long/double' );

$expected = short( 0,1,2,3,4 );
$got = float( 0,1,2.001,3,4 );

test_out( "ok 1 - ndarrays are equal" );
is_pdl( $got, $expected, { atol => 1e-2, require_equal_types => 0 } );
test_test( 'succeeds correctly for float/short' );

test_out( "not ok 1 - ndarrays are equal" );
test_fail( +2 );
test_err( $values_not_match );
is_pdl( $got, $expected, { atol => 1e-6, require_equal_types => 0 } );
test_test( 'fails correctly for float/short' );

$expected = float( 0,-1,2.001,3,49999.998 );
$got = double( 0,-0.9999,1.999,3,49999.999 );

test_out( "ok 1 - ndarrays are equal" );
is_pdl( $got, $expected, { atol => 1e-2, require_equal_types => 0 } );
test_test( 'succeeds correctly for double/float' );

test_out( "not ok 1 - ndarrays are equal" );
test_fail( +2 );
test_err( $values_not_match );
is_pdl( $got, $expected, { atol => 1e-6, require_equal_types => 0 } );
test_test( 'fails correctly for double/float' );

note 'miscellaneous';

$expected = pdl( 0,1,2,3,4 );
$got = sequence 5;
test_out( "ok 1 - insert custom test name" );
is_pdl( $got, $expected, 'insert custom test name' );
test_test( 'custom test name is displayed correctly' );

test_out( "ok 1 - insert custom test name" );
is_pdl( $got, $expected, { test_name => 'insert custom test name' } );
test_test( 'custom test name is also displayed correctly when supplied as an option hash' );

# Although the next test may appear strange, the case it tests can be produced
# by the following test:
#	is_pdl hist( pdl(2,3,4,5) ), pdl(1,1,1,1);
# Since hist() returns two ndarrays in list context, the expected ndarray ends up
# as the third argument. Since this is probably not what the user intended, an
# error is raised.
throws_ok { is_pdl( $got, $expected, pdl(1,1,1,1) ) }
	qr/^error in arguments: third argument is an ndarray at /, 'third argument is an ndarray';

$expected = long( 4,5,6,7,8,9 );
$expected->badflag( 1 );
$got = long( 4,5,6,7,8,9 );
$got->badflag( 0 );
test_out( "ok 1 - ndarrays are equal" );
is_pdl( $got, $expected );
test_test( "isn't fooled by differing badflags" );

is "@warns", "", "no warnings";
done_testing;
