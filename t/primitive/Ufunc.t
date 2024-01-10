use strict;
use warnings;
use Test2::V0 '!float';

use PDL::LiteF;
use PDL::Types;
use Test::Lib;
use My::Test::Primitive;

diag "These tests should be in Ufunc, not Primitive";

# provide independent copies of test data.
sub X { PDL->pdl( [ [ 5, 4, 3 ], [ 2, 3, 1.5 ] ] ) }

ok( tapprox( X->average(),  PDL->pdl( [ 4,  2.16666 ] ) ), "average" );
ok( tapprox( X->sumover(),  PDL->pdl( [ 12, 6.5 ] ) ),     "sumover" );
ok( tapprox( X->prodover(), PDL->pdl( [ 60, 9 ] ) ),       "prodover" );


# provide independent copies of test data.
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

subtest 'minmax' => sub {
    my @minMax = IM->minmax;
    ok( $minMax[0] == 1,  "minmax min" );
    ok( $minMax[1] == 13, "minmax max" );
};

subtest dsumover => sub {
    my $x = ones( byte, 3000 );
    my $y;
    dsumover( $x, ( $y = null ) );
    is( $y->get_datatype, $PDL_D, "get_datatype" );
    is( $y->at,           3000,   "at" );
};

subtest 'minimum_n_ind' => sub {

    subtest 'usage' => sub {
        my $p = pdl [ 1, 2, 3, 4, 7, 9, 1, 1, 6, 2, 5 ];
        my $q = zeroes 5;
        minimum_n_ind $p, $q;
        ok( tapprox( $q, pdl( 0, 6, 7, 1, 9 ) ), "usage 1" );
        $q = minimum_n_ind( $p, 5 );
        ok( tapprox( $q, pdl( 0, 6, 7, 1, 9 ) ), "usage 2" );
        minimum_n_ind( $p, $q = null, 5 );
        ok( tapprox( $q, pdl( 0, 6, 7, 1, 9 ) ), "usage 3" );
    };

    subtest 'BAD' => sub {
        my $p = pdl '[1 BAD 3 4 7 9 1 1 6 2 5]';
        my $q = zeroes 5;
        minimum_n_ind $p, $q;
        is $q. '', '[0 6 7 9 2]', "BAD";
    };

    subtest 'insufficient good' => sub {
        my $p = pdl '[1 BAD 3 4 BAD BAD]';
        my $q = zeroes 5;
        minimum_n_ind $p, $q;
        is $q. '', '[0 2 3 BAD BAD]', "insufficient good";
    };

    subtest 'bad & good' => sub {
        my $p = pdl '[1 BAD 3 4 BAD BAD 3 1 5 8 9]';
        my $q = zeroes 5;
        minimum_n_ind $p, $q;
        is $q. '', '[0 7 2 6 3]', "some bad, sufficient good";
    }
};

done_testing;
