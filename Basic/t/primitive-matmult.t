use strict;
use warnings;
use Test::More;
use Test::Exception;
use Test::PDL;
use PDL::LiteF;

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
    is_pdl $cm1 x $cm2, pdl('[[5+4i]]'), 'complex matmult';
    throws_ok { scalar $cm1->transpose x $cm2 }
      qr/mismatch/,
      'good error on mismatch matmult';
};

sub PA { pdl [ [ 1, 2, 3, 0 ], [ 1, -1, 2, 7 ], [ 1, 0, 0, 1 ] ] }
sub PB { pdl [ [ 1, 1 ], [ 0, 2 ], [ 0, 2 ], [ 1, 1 ] ] }
sub PC { pdl [ [ 1, 11 ], [ 8, 10 ], [ 2, 2 ] ] }
sub EQ { float [ [ 1, 1, 1, 1 ] ] }

subtest 'test fiducials: 3x4 x 4x2' => sub {

    is_pdl PA() x PB(), PC();

    matmult( PA, PB, my $res = null );
    is_pdl $res, PC, 'res=null';
};

subtest 'sliced input' => sub {
    my $pa_sliced =
      PA->dummy( 0, 3 )->dummy( -1, 3 )->make_physical->slice('(1),,,(1)');
    is_pdl $pa_sliced x PB, PC;
};

subtest 'output = zeroes(2,3)' => sub {
    my $res = zeroes( 2, 3 );
    matmult( PA, PB, $res );
    is_pdl $res, PC, 'res=zeroes';
};

subtest 'output = ones(2,3)' => sub {
    my $res = ones( 2, 3 );
    matmult( PA, PB, $res );
    is_pdl $res, PC, 'res=ones';
};

# Check collapse: output should be a 2x1...
is_pdl EQ() x PB(), pdl( [ [ 2, 6 ] ] ), '([4x1] x [2x4] -> [2x1])';

# Check dimensional exception: mismatched dims should throw an error
throws_ok {
        PB() x EQ();
    }
    qr/mismatch in matmult/,
    '[2x4] x [4x1] --> error (2 != 1)';

is_pdl PB() x 2, PB() * 2, 'ndarray x Perl scalar';
is_pdl pdl(3) x PB(), PB() *3, '1D ndarray x ndarray';

subtest 'nans' => sub {
  my $A = pdl '[1 nan 0; 0 1 0; 0 0 1]';
  my $B = PDL->sequence(2,3);
  my $C = $A x $B;
  $C->inplace->setnantobad;
  $C->inplace->setbadtoval(6);
  is_pdl $C, pdl '[6 6; 2 3; 4 5]';
};

subtest 'badvals' => sub {
  my $A = pdl '[1 BAD 0; 0 1 0; 0 0 1]';
  my $B = PDL->sequence(2,3);
  my $C = $A x $B;
  $C->inplace->setbadtoval(6);
  is_pdl $C, pdl '[6 6; 2 3; 4 5]';
};

done_testing;
