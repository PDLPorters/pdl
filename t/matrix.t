use PDL::LiteF;
use Test;

BEGIN {
  plan tests => 2;
}

use PDL::Matrix;
my $m = mpdl([[1,2,1],[2,0,3],[1,1,1]]); # matrix with determinant 1


ok $m->det==1;
ok $m->determinant==1;

