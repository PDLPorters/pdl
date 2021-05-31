use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::MatrixOps; # for 'x'
use PDL::GSL::LINALG;

my $A = pdl [
  [0.18, 0.60, 0.57, 0.96],
  [0.41, 0.24, 0.99, 0.58],
  [0.14, 0.30, 0.97, 0.66],
  [0.51, 0.13, 0.19, 0.85],
];
my $B = sequence(2,4); # column vectors, but must transpose for GSL

LU_decomp(my $lu=$A->copy, my $p=null, my $signum=null);
LU_solve($lu, $p, $B->transpose, my $x=null);
$x = $x->inplace->transpose;
my $got = $A x $x;
ok all(approx $got, $B) or diag "got:$got\nexpected:$B";

$got = LU_det($lu, $signum);
my $expected = $lu->diagonal(0,1)->prodover * $signum;
ok all(approx $got, $expected) or diag "got:$got\nexpected:$expected";

my $D = 3;
#solve (1 + i)(b_{n + 1} - b_n)=1 - i with homogeneous BCs
my $c=zeroes($D); # subdiag
my $d=-ones($D); # diagonal
my $e=ones($D); $e->slice('(-1)').=0; # superdiag
my $b=ones($D); $b->slice('(-1)').=(1-$D);
($b, my $info) = solve_tridiag($d, $e, $c, $b, $x=null);
my $r=sequence($D);
ok($x->approx($r)->all, "tridiag")
  or diag "got:$x\nexpected:$r";

# now complex
$A = czip($A, 1e-9);
$B = czip($B, 2);
LU_decomp($lu=$A->copy, $p=null, $signum=null);
LU_solve($lu, $p, $B->transpose, $x=null);
$x = $x->inplace->transpose;
$got = $A x $x;
ok all(approx $got, $B) or diag "got:$got\nexpected:$B";

$got = LU_det($lu, $signum);
$expected = $lu->diagonal(0,1)->prodover * $signum;
ok all(approx $got, $expected) or diag "got:$got\nexpected:$expected";

done_testing;
