use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::IO::STL;
use File::Spec::Functions;
use File::Temp qw/ tempfile /;

my $cubev = pdl(
  PDL::float,
  [[0,0,0], [0,1,0], [1,1,0]],
  [[0,0,0], [1,1,0], [1,0,0]],
  [[0,0,0], [0,0,1], [0,1,1]],
  [[0,0,0], [0,1,1], [0,1,0]],
  [[0,0,0], [1,0,0], [1,0,1]],
  [[0,0,0], [1,0,1], [0,0,1]],
  [[0,0,1], [1,0,1], [1,1,1]],
  [[0,0,1], [1,1,1], [0,1,1]],
  [[1,0,0], [1,1,0], [1,1,1]],
  [[1,0,0], [1,1,1], [1,0,1]],
  [[0,1,0], [0,1,1], [1,1,1]],
  [[0,1,0], [1,1,1], [1,1,0]],
);
my ($vertices, $faceidx) = rstl(catfile qw(t cube.stl));
ok all(approx $vertices->dice_axis(1, $faceidx->flat)->splitdim(1,3), $cubev)
  or diag "vertices=$vertices\nfaceidx=$faceidx";

eval {wstl()};
like $@, qr/Usage:/, 'wstl error right';

{
  my $fh = tempfile(CLEANUP => 1);
  wstl $fh, $vertices, $faceidx;
  my ($v2, $f2) = rstl($fh);
  ok all(approx $v2->dice_axis(1, $f2->flat)->splitdim(1,3), $cubev)
    or diag "v2=$v2\nf2=$f2";
}

done_testing;
