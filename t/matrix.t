use strict;
use warnings;

use PDL::LiteF;
use Test::More;

use PDL::Matrix;
use PDL::MatrixOps;

my $m = mpdl([[1,2,1],[2,0,3],[1,1,1]]); # matrix with determinant 1

my $tol = $^O =~ /win32/i ? 1e-6 : 1e-15;
note "determinant: ",$m->det;
ok approx($m->det, 1, $tol), "det" or diag 'got: ', $m->det;
ok approx($m->determinant, 1, 1e-15), "determinant";

is ref(identity($m)), 'PDL::Matrix', 'identity of mpdl right class';
is ref(my $from_scalar = identity(vpdl 3)), 'PDL::Matrix', 'identity of mpdl right class';
is $from_scalar.'', <<EOF, 'from scalar';
\n[
 [1 0 0]
 [0 1 0]
 [0 0 1]
]
EOF

my $v = vpdl [1..3];
is +(inv($m) x $v).'', my $mult = <<EOF, 'inv($mpdl) right class';
\n[
 [13]
 [-2]
 [-8]
]
EOF
is +($m->inv x $v).'', $mult, '$mpdl->inv right class';

done_testing;
