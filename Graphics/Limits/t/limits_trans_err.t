use strict;
use warnings;
use Test::More;
use Test::PDL;
use PDL;
use PDL::Graphics::Limits;

my $x1 = pdl( 1, 2, 3 );
my $x2 = pdl( 2, 3, 4 );

my $xn = pdl( 0.5, 0.5, 0.5 );
my $xp = $xn / 2;

my $y1 = pdl( 10, 3, 4 );
my $y2 = pdl( -1, 2, 4 );

my @udsets = ( [ [ $x1, $xn ], $y1], [ $x2, $y2 ] );
my $range = pdl limits( @udsets, { Bounds => 'MinMax', Clean => 'None', Trans => [ \&log10 ], } );

is_pdl $range, pdl([ log10(1-0.5), log10(4), -1, 10 ]), 'x symmetric trans';

@udsets = ( [ [ $x1, $xn ], $y1], [ [ $x2, undef, $xp ], $y2 ] );
$range = pdl limits( @udsets, { Bounds => 'MinMax', Clean => 'None', Trans => [ \&log10 ], } );

is_pdl $range, pdl([ log10(1-0.5), log10(4+0.25), -1, 10 ]), 'x asymmetric trans';

$y1 = pdl( 0.5, 1, 5 );
$y2 = pdl( 0.5, 0.5, 0.5 );
@udsets = ( [ [ $y1, $y2 ] ] );
$range = pdl limits( @udsets, { Bounds => 'MinMax', Clean => 'None', Trans => [ \&log10 ], } );

is_pdl $range, pdl([ log10(0.5), log10(5+0.5) ]), 'illegal errbar lower bounds';

done_testing;
