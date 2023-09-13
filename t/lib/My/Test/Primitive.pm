#! perl

use strict;
use warnings;
use Test2::V0 '!float';
use PDL::LiteF;

use Exporter 'import';
our @EXPORT = qw( tapprox );

sub tapprox {
    my ( $x, $y ) = @_;
    $_ = pdl($_) for $x, $y;
    if ( ( my $dims_x = join( ',', $x->dims ) ) ne
        ( my $dims_y = join( ',', $y->dims ) ) )
    {
        diag "APPROX: $x $y\n";
        diag "UNEQDIM: |$dims_x| |$dims_y|\n";
        return 0;
    }
    return 1 if $x->isempty and $y->isempty;
    my $d = max( abs( $x - $y ) );
    if ( $d >= 0.01 ) {
        diag "got=$x expected=$y\n";
    }
    $d < 0.01;
}

1;
