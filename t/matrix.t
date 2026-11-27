use strict;
use warnings;

use PDL::LiteF;
use Test::More;
use Test::PDL;

use PDL::Matrix;
use PDL::MatrixOps;

my $m = mpdl([[1,2,1],[2,0,3],[1,1,1]]); # matrix with determinant 1

my $tol = 1e-6;
note "determinant: ",$m->det;
is_pdl $m->det, pdl(1), "det";
is_pdl $m->determinant, pdl(1), "determinant";

isa_ok identity($m), 'PDL::Matrix', 'identity of mpdl right class';
isa_ok my $from_scalar = identity(vpdl 3), 'PDL::Matrix', 'identity of mpdl right class';
is $from_scalar.'', <<EOF, 'from scalar';
\n[
 [1 0 0]
 [0 1 0]
 [0 0 1]
]
EOF

my $v = vpdl [1..3];
my $gotfunc = inv($m);
my $gotfuncmul = $gotfunc x $v;
my $expected = vpdl('[[13 -2 -8]]');
isa_ok $gotfunc, 'PDL::Matrix', 'inv($mpdl) right class';
is_pdl $gotfuncmul, $expected, 'inv($mpdl) mult correct';
my $gotmeth = $m->inv;
my $gotmethmul = $gotmeth x $v;
isa_ok $gotmeth, 'PDL::Matrix', '$mpdl->inv right class';
is_pdl $gotmethmul, $expected, '$mpdl->inv mult correct';

subtest 'PDL::Matrix constructors, arithmetic, and transpose' => sub {
  my $m2 = mpdl([ [ 1, 2, 3 ], [ 4, 5, 6 ] ]);
  isa_ok $m2, 'PDL::Matrix';
  is_deeply [ $m2->dims ], [ 2, 3 ], 'dims returns (rows, cols)';

  my $z = mzeroes(2, 3);
  is_pdl $z, mpdl([ [ 0, 0, 0 ], [ 0, 0, 0 ] ]), { atol => 0, test_name => 'mzeroes' };

  my $o = mones(3, 2);
  is_pdl $o, mpdl([ [ 1, 1 ], [ 1, 1 ], [ 1, 1 ] ]), { atol => 0, test_name => 'mones' };

  my $s = msequence(2, 3);
  is_pdl $s, mpdl([ [ 0, 2, 4 ], [ 1, 3, 5 ] ]), { atol => 0, test_name => 'msequence' };

  my $a = mpdl([ [ 1, 2 ], [ 3, 4 ] ]);
  my $b = mpdl([ [ 5, 6 ], [ 7, 8 ] ]);
  my $c = $a x $b;
  isa_ok $c, 'PDL::Matrix';
  is_pdl $c, mpdl([ [ 19, 22 ], [ 43, 50 ] ]), { atol => 0, test_name => 'matrix product' };
  is_pdl $a->transpose, mpdl([ [ 1, 3 ], [ 2, 4 ] ]), { atol => 0, test_name => 'transpose' };
  is_pdl $a + $b, mpdl([ [ 6, 8 ], [ 10, 12 ] ]), { atol => 0, test_name => 'matrix addition' };
};

done_testing;
