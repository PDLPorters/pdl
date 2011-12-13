no warnings qw(misc);

use Test::More;
use PDL;

BEGIN {
  eval "use PDL::Slatec;";
  if ( !$@ ) {
    eval "use PDL::Graphics::Limits;";
    plan tests => 8;
  } else {
    plan skip_all => 'PDL::Slatec not available';
  }
};

$x1 = pdl( 1, 2, 3 );
$x2 = pdl( 2, 3, 4 );

$y1 = pdl( 10, 3, 4 );
$y2 = pdl( -1, 2, 4 );

sub trans { $_[0] * 10 };
sub trans2 { $_[0] * 11 };

@udsets = ( [ $x1, [ $y1, \&trans ]], [ $x2, $y2 ] );
@limits = limits( @udsets, { Bounds => 'MinMax', Clean => 'None' } );
ok( eq_array( \@limits, [ 1, 4, -1, 100 ] ), 'array: y1 trans' );

@udsets = ( [ $x1, $y1], [ $x2, $y2 ] );
@limits = limits( @udsets, { Trans => [ undef, \&trans ],
			   Bounds => 'MinMax',
			   Clean => 'None' } );
ok( eq_array( \@limits, [ 1, 4, -10, 100 ] ), 'array: y* trans' );

@udsets = ( [ $x1, [ $y1, undef, undef, undef ] ], [ $x2, $y2 ] );
@limits = limits( @udsets, { Trans => [ undef, \&trans ],
			   Bounds => 'MinMax',
			   Clean => 'None' } );
ok( eq_array( \@limits, [ 1, 4, -10, 40 ] ), 'array: y* trans, y1 undef' );

@udsets = ( [ $x1, [ $y1, \&trans ]], [ $x2, [ $y2, \&trans2 ]] );
@limits = limits( @udsets, { Bounds => 'MinMax', Clean => 'None' } );
ok( eq_array( \@limits, [ 1, 4, -11, 100 ] ), 'array: y1 trans y2 trans2' );

############################################################

@udsets = ([ { x => $x1, y => $y1, ytrans => \&trans }, { x => $x2, y => $y2 } ]);
@limits = limits( @udsets, { Bounds => 'MinMax', Clean => 'None',
			   VecKeys => [qw( x y&ytrans )], KeyCroak => 0
			 } );
ok( eq_array( \@limits, [ 1, 4, -1, 100 ] ), 'hash: y1 trans' );

@udsets = ([ { x => $x1, y => $y1 }, { x => $x2, y => $y2 } ]);
@limits = limits( @udsets, { Bounds => 'MinMax', Clean => 'None',
			   VecKeys => [qw( x y )], 
			   Trans => [ undef, \&trans ]
			 } );
ok( eq_array( \@limits, [ 1, 4, -10, 100 ] ), 'hash: y* trans' );

@udsets = ([ { x => $x1, y => $y1, ytrans => undef },
	     { x => $x2, y => $y2 } ]);
@limits = limits( @udsets, { Bounds => 'MinMax', Clean => 'None',
			   VecKeys => [qw( x y&ytrans )], KeyCroak => 0,
			   Trans => [ undef, \&trans ],
			 } );
ok( eq_array( \@limits, [ 1, 4, -10, 40 ] ), 'hash: y* trans y1 undef' );

@udsets = ([ { x => $x1, y => $y1, ytrans => \&trans },
	     { x => $x2, y => $y2, ytrans => \&trans2 } ]);
@limits = limits( @udsets, { Bounds => 'MinMax', Clean => 'None',
			   VecKeys => [qw( x y&ytrans )],
			 } );
ok( eq_array( \@limits, [ 1, 4, -11, 100 ] ), 'hash: y1 trans y2 trans2' );

