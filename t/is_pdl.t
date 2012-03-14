# the tests for Test::PDL

use strict;
use warnings;

use Test::More tests => 18;
use Test::Builder::Tester;

BEGIN {
	use_ok( 'PDL' ) or BAIL_OUT( 'need PDL' );
	use_ok( 'Test::PDL' );
}

my ( $got, $expected );

$expected = 3;
$got = long( 3,4 );
test_out( "not ok 1 - piddles are equal" );
test_fail( +2 );
test_err( '/#\s+expected value is not a PDL\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'rejects non-PDL arguments' );

$expected = short( 1,2 );
$got = -2;
test_out( "not ok 1 - piddles are equal" );
test_fail( +2 );
test_err( '/#\s+received value is not a PDL\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'rejects non-PDL arguments' );

$expected = long( 3,4 );
$got = pdl( 3,4 );
test_out( "not ok 1 - piddles are equal" );
test_fail( +2 );
test_err( '/#\s+types do not match\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'catches type mismatch' );

$expected = long( 3 );
$got = long( 3,4 );
test_out( "not ok 1 - piddles are equal" );
test_fail( +2 );
test_err( '/#\s+dimensions do not match in number\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'catches dimensions mismatches (number of dimensions)' );

$expected = zeroes( double, 3,4 );
$got = zeroes( double, 3,4,1 );
test_out( "not ok 1 - piddles are equal" );
test_fail( +2 );
test_err( '/#\s+dimensions do not match in number\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'does not treat degenerate dimensions specially' );

$expected = long( [ [3,4],[1,2] ] );
$got = long( [ [3,4,5], [1,2,3] ] );
test_out( "not ok 1 - piddles are equal" );
test_fail( +2 );
test_err( '/#\s+dimensions do not match in extent\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'catches dimensions mismatches (extent of dimensions)' );

$expected = long( 4,5,6,-1,8,9 )->inplace->setvaltobad( -1 );
$got = long( 4,5,6,7,-1,9 )->inplace->setvaltobad( -1 );
test_out( "not ok 1 - piddles are equal" );
test_fail( +2 );
test_err( '/#\s+bad value patterns do not match\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'catches bad value pattern mismatch' );

$expected = long( 4,5,6,7,8,9 );
$got = long( 4,5,6,7,-8,9 );
test_out( "not ok 1 - piddles are equal" );
test_fail( +2 );
test_err( '/#\s+values do not match\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'catches value mismatches for integer data' );

$expected = pdl( 4,5,6,7,8,9 );
$got = pdl( 4,5,6,7,-8,9 );
test_out( "not ok 1 - piddles are equal" );
test_fail( +2 );
test_err( '/#\s+values do not match\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'catches value mismatches for floating-point data' );

$expected = pdl( 4,5,6,7,8,9 );
$got = pdl( 4,5,6,7,8.001,9 );
# remember that approx remembers the tolerance across invocations
ok( all( approx $got, $expected, 1e-2 ), "differ by less than 0.01" );
test_out( "ok 1 - piddles are equal" );
is_pdl( $got, $expected );
test_test( 'approximate comparison for floating-point data succeeds correctly at 1e-2' );

$expected = pdl( 4,5,6,7,8,9 );
$got = pdl( 4,5,6,7,8.001,9 );
ok( !all( approx $got, $expected, 1e-6 ), "differ by more than 0.000001" );
test_out( "not ok 1 - piddles are equal" );
test_fail( +2 );
test_err( '/#\s+values do not match\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'approximate comparison for floating-point data fails correctly at 1e-6' );

$expected = pdl( 0,1,2,3,4 );
$got = sequence 5;
test_out( "ok 1 - piddles are equal" );
is_pdl( $got, $expected );
test_test( 'succeeds when it should succeed' );

$expected = pdl( 0,1,2,3,4 );
$got = sequence 5;
test_out( "ok 1 - insert custom test name" );
is_pdl( $got, $expected, 'insert custom test name' );
test_test( 'custom test name is displayed correctly' );

$expected = long( 4,5,6,7,8,9 );
$expected->badflag( 1 );
$got = long( 4,5,6,7,8,9 );
$got->badflag( 0 );
test_out( "ok 1 - piddles are equal" );
is_pdl( $got, $expected );
test_test( "isn't fooled by differing badflags" );
