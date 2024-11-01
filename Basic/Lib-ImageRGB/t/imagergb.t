use strict;
use warnings;
use Test::More;
use Test::PDL;
use PDL::LiteF;
use PDL::ImageRGB;
use PDL::Dbg;

is_pdl bytescl(float(1..5),100), byte(1..5);
is_pdl bytescl(float(1,2,3,4,5),-100), byte([0,25,50,75,100]);
is_pdl rgbtogr(pdl([1,1,1],[1,0.5,0.7],[0.1,0.2,0.1])), pdl([1,0.67,0.16]), {atol=>1e-2};

{
  my $im = byte('1 2 3;0 3 0');
  my $lut = byte('0 0 0;10 1 10;2 20 20;30 30 3');
  # also works: $lut->indexND(sequence(1,3)->append($im->slice('*1,*3')))
  my $interl = byte('[10 1 10;2 20 20;30 30 3] [0 0 0;30 30 3;0 0 0]');
  is_pdl interlrgb($im,$lut),$interl;
}

done_testing;
