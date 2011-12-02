no warnings qw(misc);

use Test::More;
use PDL;

BEGIN {
  eval "use PDL::Slatec;";
  if ( !$@ ) {
    eval "use PDL::Graphics::Limits;";
    plan tests => 3;
  } else {
    plan skip_all => 'PDL::Slatec not available';
  }
};

$x1 = pdl( 1, 2, 3 );
$x2 = pdl( 2, 3, 4 );

$xn = pdl( 0.5, 0.5, 0.5 );
$xp = $xn / 2;

$y1 = pdl( 10, 3, 4 );
$y2 = pdl( -1, 2, 4 );

@udsets = ( [ [ $x1, $xn ], $y1], [ $x2, $y2 ] );
@range = limits( @udsets, { Bounds => 'MinMax',
			   Clean => 'None',
			   Trans => [ \&log10 ],
			 } );
ok( eq_array( \@range, [ log10(1-0.5), log10(4),
			 -1, 10 ] ), 'x symmetric trans' );

@udsets = ( [ [ $x1, $xn ], $y1], [ [ $x2, undef, $xp ], $y2 ] );
@range = limits( @udsets, { Bounds => 'MinMax',
			   Clean => 'None',
			   Trans => [ \&log10 ],
			 } );
ok( eq_array( \@range, [ log10(1-0.5), log10(4+0.25),
			 -1, 10 ] ), 'x asymmetric trans' );

$y1 = pdl( 0.5, 1, 5 );
$ys = pdl( 0.5, 0.5, 0.5 );
@udsets = ( [ [ $y1, $ys ] ] );
@range = limits( @udsets, { Bounds => 'MinMax',
			   Clean => 'None',
			   Trans => [ \&log10 ],
			 } );
ok( eq_array( \@range, [ log10(0.5), log10(5+0.5) ] ), 
    'illegal errbar lower bounds' );


