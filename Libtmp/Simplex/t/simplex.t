use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::Opt::Simplex;

sub test_simplex {
  local $Test::Builder::Level = $Test::Builder::Level + 1;
  my ($init, $initsize, $dis, $nolog) = @_;
  my $log_called = 0;
  my $logsub = sub {$log_called++};
  my ( $opt, $ssize, $optval ) = simplex(
    $init->copy, $initsize, 1e-4, 1e4, sub {
      # f = x^2 + (y-1)^2 + 1
      sumover( ( $_[0] - $dis )**2 ) + 1;
    }, $nolog ? () : $logsub,
  );
  ok all PDL::Core::approx($opt, $dis, 1e-3), 'optimum' or diag "got=$opt";
  ok PDL::Core::approx($ssize, 0, 1e-3), 'ssize' or diag "got=$ssize";
  ok PDL::Core::approx($optval, 1, 1e-3), 'optval' or diag "got=$optval";
  ok $log_called, 'log called' if !$nolog;
  my @init_dims = $init->dims;
  my @exp_dims = ($init_dims[0], 1, @init_dims[2..$#init_dims]);
  is_deeply [$opt->dims], \@exp_dims, 'dims optimum right';
}

test_simplex(pdl(2,2), 0.01, pdl(0,1));
test_simplex(pdl(2,2), pdl(0.01,0.01), pdl(0,1));
test_simplex(pdl(2,2), pdl(0.01,0.01), pdl(0,1), 1);
test_simplex(pdl(2,2,2), pdl(0.01,0.01,0.01), pdl(0,1,2));
test_simplex(my $p = pdl(q[-1 -1; -1.1 -1; -1.1 -0.9]), pdl(0.01,0.01), pdl(0,1));
test_simplex($p, undef, pdl(-1,1));

my $s = make_simplex(pdl(0,0,0), pdl(0.12,0.12,0.12));
ok all approx $s, pdl '0 -0.06 -0.08; 0.12 -0.06 -0.08; 0 0.06 -0.08; 0 0 0.04'
  or diag "got=$s";

done_testing;
