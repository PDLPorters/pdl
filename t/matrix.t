use PDL::LiteF;
use Test;

BEGIN {
  plan tests => 2;
}

use PDL::Matrix;
my $m = mpdl([[1,2,1],[2,0,3],[1,1,1]]); # matrix with determinant 1

my $tol = $^O =~ /win32/i ? 1e-6 : 1e-15;
print "determinant: ",$m->det,"\n";
ok abs($m->det  - 1) < $tol ;
ok abs($m->determinant - 1) < 1e-15;

