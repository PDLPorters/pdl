use strict;
use warnings;
use Test2::V0 '!float';

use PDL::LiteF;
use Test::Lib;
use My::Test::Primitive;

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

ok( ( IM() x IM() )->sum == 3429, "matrix multiplication" );

subtest 'complex' => sub {

    # complex matmult
    my $cm1 = pdl('1 1+i 1');
    my $cm2 = pdl('2 3 i')->transpose;
    ok tapprox( $cm1 x $cm2, pdl('[[5+4i]]') ), 'complex matmult';
    like dies { scalar $cm1->transpose x $cm2 },
      qr/mismatch/,
      'good error on mismatch matmult';
};

sub PA { pdl [ [ 1, 2, 3, 0 ], [ 1, -1, 2, 7 ], [ 1, 0, 0, 1 ] ] }
sub PB { pdl [ [ 1, 1 ], [ 0, 2 ], [ 0, 2 ], [ 1, 1 ] ] }
sub PC { pdl [ [ 1, 11 ], [ 8, 10 ], [ 2, 2 ] ] }
sub EQ { float [ [ 1, 1, 1, 1 ] ] }

subtest 'test fiducials: 3x4 x 4x2' => sub {

    ok tapprox( PA() x PB(), PC() );

    matmult( PA, PB, my $res = null );
    ok tapprox( $res, PC ), 'res=null';
};

subtest 'sliced input' => sub {
    my $pa_sliced =
      PA->dummy( 0, 3 )->dummy( -1, 3 )->make_physical->slice('(1),,,(1)');
    ok tapprox( PC, $pa_sliced x PB );
};

subtest 'output = zeroes(2,3)' => sub {
    my $res = zeroes( 2, 3 );
    matmult( PA, PB, $res );
    ok tapprox( PC, $res ), 'res=zeroes';
};

subtest 'output = ones(2,3)' => sub {
    my $res = ones( 2, 3 );
    matmult( PA, PB, $res );
    ok tapprox( PC, $res ), 'res=ones';
};

# Check collapse: output should be a 1x2...
ok tapprox( EQ() x PB(), pdl( [ [ 2, 6 ] ] ) ), '([4x1] x [2x4] -> [1x2])';

# Check dimensional exception: mismatched dims should throw an error
like(
    dies {
        PB() x EQ();
    },
    qr/mismatch in matmult/,
    '[2x4] x [4x1] --> error (2 != 1)'
);

ok tapprox( PB() x 2, PB() * 2, 'ndarray x Perl scalar' );

ok tapprox( pdl(3) x PB(), PB() *3 ), '1D ndarray x ndarray';

done_testing;
