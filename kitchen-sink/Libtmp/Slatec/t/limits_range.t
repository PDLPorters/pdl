
use Test::More;
use PDL;

BEGIN {
  eval "use PDL::Slatec;";
  if ( !$@ ) {
    eval "use PDL::Graphics::Limits;";
    plan tests => 6;
  } else {
    plan skip_all => 'PDL::Slatec not available';
  }
};

#################################################################

$x1 = pdl( 1, 2 );
$y1 = pdl( 1, 2 );

$x2 = pdl( 2, 3 );
$y2 = pdl( 2, 4 );

@minmax = ( 1, 3, 1, 4 );
$minmax_hash = { q1 => { min => 1, max => 3 },
	         q2 => { min => 1, max => 4 } };
@minmax_range = ( 0.9, 3.1, 0.85, 4.15 );
@minmax_round = ( 0.5, 5, 0.5, 5 );

@udsets = ( [ $x1, $y1 ], [ $x2, $y2 ] );

@range = limits( @udsets, { Bounds => 'MinMax', Clean => 'None' } );
ok( eq_array( \@range, \@minmax ), "MinMax, None" );

$range = limits( @udsets, {Bounds => 'MinMax', Clean => 'None' } );
ok( eq_hash( $range, $minmax_hash ), "MinMax, None, hash" );

@range = limits( @udsets, {Bounds => 'MinMax', Clean => 'RangeFrac' } );
ok( eq_array( \@range, \@minmax_range ), "MinMax, Range" );

@range = limits( @udsets, {Bounds => 'MinMax', Clean => 'RoundPow' } );
ok( eq_array( \@range, \@minmax_round ), "MinMax, Range" );


$x1 = pdl( 1, 2, 3, 4 );
$y1 = pdl( 0, 10, 3, 4 );

@range = limits( [ $x1, $y1 ], { Bounds => 'Zscale', Clean => 'None' } );
ok( all(approx( pdl(@range), pdl ( 1, 4, -0.4, 8.9 ) )), 'Zscale, None' );

@range = limits( [ 1, 2 ], [ 3, 4 ], { Bounds => 'MinMax', Clean => 'None' } );
ok( eq_array( \@range, [ 1, 3, 2, 4 ] ), "scalars in the mix " );


