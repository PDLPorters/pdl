use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use Test::PDL -atol => 1e-3;

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

subtest 'default type' => sub {
    my @statsRes = IM->stats;
    is_pdl $statsRes[0], pdl( 5.36 ),  "mean";
    is_pdl $statsRes[1], pdl( 4.554 ), "prms";
    is_pdl $statsRes[2], pdl( 3 ),     "median";
    is_pdl $statsRes[3], pdl( 1 ),     "min";
    is_pdl $statsRes[4], pdl( 13 ),    "max";
    is_pdl $statsRes[6], pdl( 4.462 ), "rms";
};

subtest 'short' => sub {
    my @statsRes =
      IM->short->stats;    # Make sure that stats are promoted to floating-point
    is_pdl $statsRes[0], float( 5.36 ),  "short mean";
    is_pdl $statsRes[1], float( 4.554 ), "short prms";
    is_pdl $statsRes[2], short( 3 ),     "short median";
    is_pdl $statsRes[3], long( 1 ),      "short min";
    is_pdl $statsRes[4], long( 13 ),     "short max";
    is_pdl $statsRes[6], float( 4.462 ), "short rms";
};

subtest 'weights' => sub {
    my $ones     = ones( 5, 5 );
    my @statsRes = IM->stats($ones);
    is_pdl $statsRes[0], pdl( 5.36 ),  "trivial weights mean";
    is_pdl $statsRes[1], pdl( 4.554 ), "trivial weights prms";
    is_pdl $statsRes[2], pdl( 3 ),     "trivial weights median";
    is_pdl $statsRes[3], pdl( 1 ),     "trivial weights min";
    is_pdl $statsRes[4], pdl( 13 ),    "trivial weights max";
    is_pdl $statsRes[6], pdl( 4.462 ), "trivial weights rms";
};

done_testing;
