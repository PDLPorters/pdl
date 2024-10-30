use strict;
use warnings;
use Test::More;
use Test::PDL;
use PDL;
use PDL::Graphics::Limits;

my $x1 = pdl( 1, 2 );
my $y1 = pdl( 1, 2 );

my $x2 = pdl( 2, 3 );
my $y2 = pdl( 2, 4 );

my @minmax = ( 1, 3, 1, 4 );
my $minmax_hash = { q1 => { min => 1, max => 3 },
	         q2 => { min => 1, max => 4 } };
my @minmax_range = ( 0.9, 3.1, 0.85, 4.15 );
my @minmax_round = ( 0.5, 5, 0.5, 5 );

my @udsets = ( [ $x1, $y1 ], [ $x2, $y2 ] );

my @range = limits( @udsets, { Bounds => 'MinMax', Clean => 'None' } );
is_deeply \@range, \@minmax, "MinMax, None" or diag explain \@range;

my $range = limits( @udsets, {Bounds => 'MinMax', Clean => 'None' } );
is_deeply $range, $minmax_hash, "MinMax, None, hash" or diag explain $range;

@range = limits( @udsets, {Bounds => 'MinMax', Clean => 'RangeFrac' } );
is_deeply \@range, \@minmax_range, "MinMax, Range" or diag explain \@range;

@range = limits( @udsets, {Bounds => 'MinMax', Clean => 'RoundPow' } );
is_deeply \@range, \@minmax_round, "MinMax, Range" or diag explain \@range;

$x1 = pdl( 1, 2, 3, 4 );
$y1 = pdl( 0, 10, 3, 4 );

@range = limits( [ $x1, $y1 ], { Bounds => 'Zscale', Clean => 'None' } );
is_pdl pdl(@range), pdl(1, 4, -0.4, 8.9), 'Zscale, None';

@range = limits( [ 1, 2 ], [ 3, 4 ], { Bounds => 'MinMax', Clean => 'None' } );
is_deeply \@range, [ 1, 3, 2, 4 ], "scalars in the mix " or diag explain \@range;

done_testing;
