use strict;
use Test::More;
use PDL;
use PDL::LiteF;
use Config;
use PDL::Config;

if($Config{cc} eq 'cl') {
  plan skip_all => 'lgamma not implemented for MS compilers';
}

my @x = lgamma(-0.1);
is(approx($x[0], 2.36896133272879), 1);
is($x[1], -1);

@x = lgamma(1.1);
is(approx($x[0], -0.0498724412598397), 1);
is($x[1], 1);

if($PDL::Config{WITH_BADVAL}) {
  my $p = sequence (1);
  $p->badvalue (0);
  $p->badflag (1);
  my @x = lgamma($p->index(0));
  is($x[0]->badflag(), 1);
  is($x[1]->badflag(), 1);
}

done_testing;
