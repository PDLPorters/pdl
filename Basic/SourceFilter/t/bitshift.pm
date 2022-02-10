package trivialbitshift;
use strict;
use warnings;
use PDL::LiteF;
use PDL::NiceSlice;

sub f {
  my ($pa, $pb) = @_;
  $pa <<= 2;
  $pb >>= 1;
}

1;
