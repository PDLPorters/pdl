use strict;
use warnings;
use Test::More;
use Test::Exception;
use PDL::LiteF;
use lib 't/lib';
use My::Test::Primitive;

TODO: { local $TODO = 'Some CPAN Testers fails for OpenBSD'; subtest 'random' => sub {

    # check that our random functions work with Perl's srand
    # local $TODO = ;

    subtest 'random and srand' => sub {
        srand 5;
        my $r1 = random 10;
        srand 5;
        my $r2 = random 10;
        ok( tapprox( $r1, $r2 ), "random and srand" );
    };

    subtest 'grandom and srand' => sub {
        srand 10;
        my $r1 = grandom 10;
        srand 10;
        my $r2 = grandom 10;
        ok( tapprox( $r1, $r2 ), "grandom and srand" );
    };
}; }

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
