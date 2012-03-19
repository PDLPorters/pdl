# the tests for Test::PDL

use strict;
use warnings;

use Test::More tests => 38;
use Test::Builder::Tester;
use Test::Exception;

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
test_out( "ok 1 - piddles are equal" );
is_pdl( $got, $expected );
test_test( 'all else being equal, compares equal on differing types when EQUAL_TYPES is false' );

Test::PDL::set_options( EQUAL_TYPES => 1 );
$expected = long( 3,4 );
$got = pdl( 3,4 );
test_out( "not ok 1 - piddles are equal" );
test_fail( +2 );
test_err( '/#\s+types do not match\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'catches type mismatch, but only when EQUAL_TYPES is true' );
Test::PDL::set_options( EQUAL_TYPES => 0 );

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
# remember that approx() remembers the tolerance across invocations, so we
# explicitly specify the tolerance at each invocation
ok( !all( approx $got, $expected, 1e-6 ), "differ by more than 0.000001" );
test_out( "not ok 1 - piddles are equal" );
test_fail( +2 );
test_err( '/#\s+values do not match\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'approximate comparison for floating-point data fails correctly at documented default tolerance of 1e-6' );

$expected = pdl( 4,5,6,7,8,9 );
$got = pdl( 4,5,6,7,8.0000001,9 );
ok( all( approx $got, $expected, 1e-6 ), "differ by less than 0.000001" );
test_out( "ok 1 - piddles are equal" );
is_pdl( $got, $expected );
test_test( 'approximate comparison for floating-point data succeeds correctly at documented default tolerance of 1e-6' );

Test::PDL::set_options( TOLERANCE => 1e-2 );
$expected = pdl( 4,5,6,7,8,9 );
$got = pdl( 4,5,6,7,8.001,9 );
ok( all( approx $got, $expected, 1e-2 ), "differ by less than 0.01" );
test_out( "ok 1 - piddles are equal" );
is_pdl( $got, $expected );
test_test( 'approximate comparison for floating-point data succeeds correctly at user-specified tolerance of 1e-2' );

$expected = pdl( 0,1,2,3,4 );
$got = sequence 5;
test_out( "ok 1 - piddles are equal" );
is_pdl( $got, $expected );
test_test( 'succeeds when it should succeed' );

note 'mixed-type comparisons';

$expected = double( 0,1,2.001,3,4 );
$got = long( 0,1,2,3,4 );

ok( all( approx $got, $expected, 1e-2 ), "differ by less than 0.01" );
Test::PDL::set_options( TOLERANCE => 1e-2 );
test_out( "ok 1 - piddles are equal" );
is_pdl( $got, $expected );
test_test( 'succeeds correctly for long/double' );

ok( !all( approx $got, $expected, 1e-6 ), "differ by more than 0.000001" );
Test::PDL::set_options( TOLERANCE => 1e-6 );
test_out( "not ok 1 - piddles are equal" );
test_fail( +2 );
test_err( '/#\s+values do not match\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'fails correctly for long/double' );

$expected = short( 0,1,2,3,4 );
$got = float( 0,1,2.001,3,4 );

ok( all( approx $got, $expected, 1e-2 ), "differ by less than 0.01" );
Test::PDL::set_options( TOLERANCE => 1e-2 );
test_out( "ok 1 - piddles are equal" );
is_pdl( $got, $expected );
test_test( 'succeeds correctly for float/short' );

ok( !all( approx $got, $expected, 1e-6 ), "differ by more than 0.000001" );
Test::PDL::set_options( TOLERANCE => 1e-6 );
test_out( "not ok 1 - piddles are equal" );
test_fail( +2 );
test_err( '/#\s+values do not match\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'fails correctly for float/short' );

$expected = float( 0,-1,2.001,3,49999.998 );
$got = double( 0,-0.9999,1.999,3,49999.999 );

ok( all( approx $got, $expected, 1e-2 ), "differ by less than 0.01" );
Test::PDL::set_options( TOLERANCE => 1e-2 );
test_out( "ok 1 - piddles are equal" );
is_pdl( $got, $expected );
test_test( 'succeeds correctly for double/float' );

ok( !all( approx $got, $expected, 1e-6 ), "differ by more than 0.000001" );
Test::PDL::set_options( TOLERANCE => 1e-6 );
test_out( "not ok 1 - piddles are equal" );
test_fail( +2 );
test_err( '/#\s+values do not match\n(.|\n)*/' );
is_pdl( $got, $expected );
test_test( 'fails correctly for double/float' );

note 'miscellaneous';

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

note 'setting options';

throws_ok { Test::PDL::set_options( SOME_INVALID_OPTION => 1 ) }
	qr/invalid option SOME_INVALID_OPTION\b/, 'does not accept unknown options';
throws_ok { Test::PDL::set_options( 'TOLERANCE' ) }
	qr/undefined value for TOLERANCE/, 'refuses options without a value';
lives_ok { Test::PDL::set_options(
			TOLERANCE => 1e-4,
			EQUAL_TYPES => 2 )
	} 'accepts two options at the same time';
is( $Test::PDL::OPTIONS{TOLERANCE}, 1e-4, 'TOLERANCE set correctly' );
is( $Test::PDL::OPTIONS{EQUAL_TYPES}, 2, 'EQUAL_TYPES set correctly' );
