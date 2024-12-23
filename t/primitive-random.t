use strict;
use warnings;
use Test::More;
use Test::Exception;
use PDL::LiteF;
use Test::PDL;

subtest 'random' => sub {
    subtest 'random and srandom' => sub {
        srandom 5;
        my $r1 = random 10;
        srandom 5;
        my $r2 = random 10;
        is_pdl $r1, $r2, "random and srandom";
    };

    subtest 'grandom and srandom' => sub {
        srandom 10;
        my $r1 = grandom 10;
        srandom 10;
        my $r2 = grandom 10;
        is_pdl $r1, $r2, "grandom and srandom";
    };
};

subtest 'types' => sub {

    subtest 'random' => sub {
        my $type;
        lives_ok { $type = random()->type } 'random()';
        is( $type, 'double', 'defaults to double' );
    };

    subtest 'randsym' => sub {
        my $type;
        lives_ok { $type = randsym()->type } 'randsym()';
        is( $type, 'double', 'defaults to double' );

    };
};

subtest 'regressions' => sub {

    # Test some operations with empty ndarrays
    lives_ok { random( 1, 1, 0 )->type } 'empty ndarray';    # used to segfault
};

done_testing;
