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

done_testing;
