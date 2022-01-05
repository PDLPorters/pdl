use strict;
use warnings;
use Test::More;
use PDL;
use PDL::Graphics::Limits;

my $x1 = pdl( 1, 2, 3 );
my $x2 = pdl( 2, 3, 4 );

my $xn = pdl( 0.5, 0.5, 0.5 );
my $xp = $xn / 2;

my $y1 = pdl( 10, 3, 4 );
my $y2 = pdl( 0, 2, 4 );

my @udsets = ( [ [ $x1, $xn ], $y1 ], [ $x2, $y2 ] );
my @range = limits( @udsets, { Bounds => 'MinMax', Clean => 'None' } );
ok( eq_array( \@range, [ 0.5, 4, 0, 10 ] ), 'array: xerr symmetric, y none' );

@udsets = ( [ [ $x1, $xn ], $y1 ], [ [ $x2, undef, $xp ], $y2 ] );
@range = limits( @udsets, { Bounds => 'MinMax', Clean => 'None' } );
ok( eq_array( \@range, [ 0.5, 4.25, 0, 10 ] ), 'array: xerr asymmetric, y none' );

@udsets = ( [ [ $x1, $xn, $xp ], $y1 ], [ $x2, $y2 ] );
@range = limits( @udsets, { Bounds => 'MinMax', Clean => 'None' } );
ok( eq_array( \@range, [ 0.5, 4, 0, 10 ] ), 'array: xerr asymmetric, y none' );

@udsets = ( [ { x => $x1, xerr => $xn, y => $y1 }, { x => $x2, y => $y2 } ]);
@range = limits( @udsets, { VecKeys => [ 'x,=xerr', 'y'], 
      		      Bounds => 'MinMax', 
      		      Clean => 'None',
      		      KeyCroak => 0 } );
ok( eq_array( \@range, [ 0.5, 4, 0, 10 ] ), 'hash: xerr symmetric, y none' );

@udsets = ( [ { x => $x1, xerr => $xn, 'y' => $y1 } => ( 'x =xerr', 'y' ) ],
            [ { x => $x2, xp => $xp, 'y' => $y2 } => ( 'x >xp',  'y')  ] );
@range = limits( @udsets, { Bounds => 'MinMax', Clean => 'None' } );
      	    ok( eq_array( \@range, [ 0.5, 4.25, 0, 10 ] ), 'hash: xerr asymmetric, y none' );

@udsets = ( [ { x => $x1, xn => $xn, xp => $xp, y => $y1 },
              { x => $x2, y => $y2 } ] );
@range = limits( @udsets, { VecKeys => [ 'x <xn >xp', 'y' ], 
       		   Bounds => 'MinMax', 
      		   Clean => 'None',
      		   KeyCroak => 0
      		 } );
ok( eq_array( \@range, [ 0.5, 4, 0, 10 ] ), 'hash: xerr asymmetric, y none' );

done_testing;
