# -*-perl-*-

use strict;
use Test::More;
use PDL;
use PDL::LiteF;
use Config;
use PDL::Config;

$| = 1;

my $test_count = 6;
my $eps = 1e-9;

if($Config{cc} eq 'cl') {
  plan skip_all => 'lgamma not implemented for MS compilers';
  exit 0;
}
elsif ( $PDL::Config{WITH_BADVAL} ) {
  plan tests => $test_count;
}
else {
  # reduced testing
  plan tests => $test_count - 2;
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

sub my_approx {
  if($_[0] + $eps > $_[1] && $_[0] - $eps < $_[1]) {return 1}
  return 0;
}

