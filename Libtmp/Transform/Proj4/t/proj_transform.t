use strict;
use warnings;
use PDL::LiteF;
use Test::More;
use PDL::Transform::Proj4;
use PDL::Transform::Cartography;

sub pdl_cmp {
  my ($g, $e, $l, $eps) = @_;
  $_ = PDL->topdl($_) for $g, $e;
  local $Test::Builder::Level = $Test::Builder::Level + 1;
  my $res = all(approx($g, $e, $eps));
  fail("$l: result was BAD value"), return if $res->isbad;
  ok $res, $l or diag "got:\n$g\nexpected:\n$e";
}

my $fwd = pdl '-146.25 -81.5625; -137.8125 -81.5625; -146.25 -73.125; -137.8125 -73.125';
my $inv = pdl '0 -7213503; 4809002 -7213503; 0 -4809002; 4809002 -4809002';
my $t = t_proj( proj_params => "+proj=eqc +lon_0=0 +datum=WGS84" );
pdl_cmp $fwd->apply($t), pdl(<<'EOF'), 'eqc fwd', 10;
-16280476 -9079496; -15341217 -9079496;
-16280476 -8140237.8; -15341217 -8140237.8
EOF
pdl_cmp $inv->apply(!$t), pdl(<<'EOF'), 'eqc inv', 1e-2;
0 -64.8; 43.2 -64.8; 0 -43.2; 43.2 -43.2
EOF

$fwd = pdl '-146.25 81.5625; -137.8125 81.5625; -146.25 73.125; -137.8125 73.125';
$inv = pdl '0 -4558236.1; 1530752.9 -4558236.1; 0 -3031095.5; 1530752.9 -3031095.5';
$t = t_proj( proj_params => "+proj=ortho +ellps=WGS84 +lon_0=-90 +lat_0=40" );
pdl_cmp $fwd->apply($t), pdl(<<'EOF'), 'ortho fwd', 10;
-780706.14 4502242.5; -695714.05 4432238.3;
-1544186 4016381.2; -1376077.2 3877917.4
EOF
pdl_cmp $inv->apply(!$t), pdl(<<'EOF'), 'ortho inv', 1e-2;
-90 -5.9283617; -75.991258 -7.5230948; -90 11.481088; -75.886486 10.220222
EOF

$fwd = pdl '-146.25 81.5625; -137.8125 81.5625; -146.25 73.125; -137.8125 73.125';
$inv = pdl '0 -4558236.1; 1530752.9 -4558236.1; 0 -3031095.5; 1530752.9 -3031095.5';
$t = t_proj_ortho( ellps => 'WGS84', lon_0 => -90, lat_0 => 40 );
pdl_cmp $fwd->apply($t), pdl(<<'EOF'), 'ortho2 fwd', 10;
-780706.14 4502242.5; -695714.05 4432238.3;
-1544186 4016381.2; -1376077.2 3877917.4
EOF
pdl_cmp $inv->apply(!$t), pdl(<<'EOF'), 'ortho2 inv', 1e-2;
-90 -5.9283617; -75.991258 -7.5230948; -90 11.481088; -75.886486 10.220222
EOF

$fwd = pdl '-146.25 -81.5625; -137.8125 -81.5625; -146.25 -73.125; -137.8125 -73.125';
$inv = pdl '0 -6210111.4; 4081400 -6210111.4; 0 -4140074.2; 4081400 -4140074.2';
$t = t_proj_robin( ellps => 'WGS84', over => 1 );
pdl_cmp $fwd->apply($t), pdl(<<'EOF'), 'robin fwd', 10;
-8363042.7 -8211603.9; -7880559.4 -8211603.9;
-9548357.3 -7548659.1; -8997490.5 -7548659.1
EOF
pdl_cmp $inv->apply(!$t), pdl(<<'EOF'), 'robin inv', 1e-2;
0 -58.719767; 53.453033 -58.719767; 0 -38.71881; 46.580505 -38.71881
EOF

done_testing;
