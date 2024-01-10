use strict;
use warnings;
use Test::More;

use PDL::LiteF;
use lib 't/lib';
use My::Test::Primitive;

subtest 'setops' => sub {
    my $temp = sequence(10);
    my $x    = which( ( $temp % 2 ) == 0 );
    my $y    = which( ( $temp % 3 ) == 0 );

    ok( tapprox( setops( $x, 'AND', $y ), pdl( [ 0, 6 ] ) ), "setops AND" );

    ok( tapprox( intersect( $x, $y ), pdl( [ 0, 6 ] ) ),
        "intersect same as setops AND" );

    ok( tapprox( setops( $x, 'OR', $y ), pdl( [ 0, 2, 3, 4, 6, 8, 9 ] ) ),
        "setops OR" );

    ok( tapprox( setops( $x, 'XOR', $y ), pdl( [ 2, 3, 4, 8, 9 ] ) ),
        "setops XOR" );
};

subtest 'intersect' => sub {
    my $intersect_test = intersect( pdl( 1, -5, 4, 0 ), pdl( 0, 3, -5, 2 ) );
    ok tapprox( $intersect_test, pdl( -5, 0 ) ), 'Intersect test values';
};

subtest 'AND' => sub {

    # based on cases supplied by @jo-37
    my @cases = (
        [ pdl(1),      empty(), empty() ],
        [ ones(1),     empty(), empty() ],
        [ ones(4),     empty(), empty() ],
        [ sequence(4), empty(), empty() ],
        [ pdl(1),      ones(2), ones(1) ],
        [ ones(1),     ones(2), ones(1) ],
        [ ones(4),     ones(2), ones(1) ],
        [ sequence(4), ones(2), ones(1) ],
    );
    ok tapprox( setops( $_->[0], 'AND', $_->[1] ), $_->[2] ),
      "$_->[0] AND $_->[1]"
      for @cases;
};

done_testing;
