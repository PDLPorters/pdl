#! perl

use strict;
use warnings;
use Test2::V0 '!float';

use PDL::LiteF;
use Test::Lib;
use My::Test::Primitive;

# provide indepdent copies of test data.
sub IM {
    PDL->new(
        [
            [ 1,  2,  3,  3,  5 ],
            [ 2,  3,  4,  5,  6 ],
            [ 13, 13, 13, 13, 13 ],
            [ 1,  3,  1,  3,  1 ],
            [ 10, 10, 2,  2,  2, ]
        ]
    );
}


subtest 'default type' => sub {
    my @statsRes = IM->stats;
    ok( tapprox( $statsRes[0], 5.36 ),  "mean" );
    ok( tapprox( $statsRes[1], 4.554 ), "prms" );
    ok( tapprox( $statsRes[2], 3 ),     "median" );
    ok( tapprox( $statsRes[3], 1 ),     "min" );
    ok( tapprox( $statsRes[4], 13 ),    "max" );
    ok( tapprox( $statsRes[6], 4.462 ), "rms" );
};

subtest 'short' => sub {
    my @statsRes =
      IM->short->stats;    # Make sure that stats are promoted to floating-point
    ok( tapprox( $statsRes[0], 5.36 ),  "short mean" );
    ok( tapprox( $statsRes[1], 4.554 ), "short prms" );
    ok( tapprox( $statsRes[2], 3 ),     "short median" );
    ok( tapprox( $statsRes[3], 1 ),     "short min" );
    ok( tapprox( $statsRes[4], 13 ),    "short max" );
    ok( tapprox( $statsRes[6], 4.462 ), "short rms" );

};

subtest 'weights' => sub {
    my $ones     = ones( 5, 5 );
    my @statsRes = IM->stats($ones);
    ok( tapprox( $statsRes[0], 5.36 ),  "trivial weights mean" );
    ok( tapprox( $statsRes[1], 4.554 ), "trivial weights prms" );
    ok( tapprox( $statsRes[2], 3 ),     "trivial weights median" );
    ok( tapprox( $statsRes[3], 1 ),     "trivial weights min" );
    ok( tapprox( $statsRes[4], 13 ),    "trivial weights max" );
    ok( tapprox( $statsRes[6], 4.462 ), "trivial weights rms" );

};

done_testing;
