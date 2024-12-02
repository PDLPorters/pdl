use strict;
use warnings;
use Test::More;
use Test::PDL;
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
my ($vertices, $faceidx) = rstl(catfile qw(t io-stl-cube.stl));
is_pdl $vertices->dice_axis(1, $faceidx->flat)->splitdim(1,3), $cubev;

eval {wstl()};
like $@, qr/Usage:/, 'wstl error right';

{
  my $fh = tempfile(CLEANUP => 1);
  wstl $fh, $vertices, $faceidx;
  my ($v2, $f2) = rstl($fh);
  is_pdl $v2->dice_axis(1, $f2->flat)->splitdim(1,3), $cubev;
}

my $blender = float '
  [0.0017237 0.102913 -0.00153113; 0.00195483 0.104636 -0.00253355; 0.00243758 0.104636 -0.00153113]
  [0.00138235 0.103254 -0.00253355; 0.000767261 0.103869 -0.00322725; 0.00108497 0.104636 -0.00322725]
  [0.00138235 0.103254 -0.00253355; 0.00108497 0.104636 -0.00322725; 0.00195483 0.104636 -0.00253355]
';
($vertices, $faceidx) = rstl(catfile qw(t io-stl-ascblender1.stl));
is_pdl $vertices->dice_axis(1, $faceidx->flat)->splitdim(1,3), $blender;
($vertices, $faceidx) = rstl(catfile qw(t io-stl-ascblender2.stl));
is_pdl $vertices->dice_axis(1, $faceidx->flat)->splitdim(1,3), $blender;

done_testing;
