$|=1;
use PDL;
use PDL::Graphics::TriD;
# use PDL::Dbg;

# PDL::Core::set_debugging(1);

$size = 100;

$a = zeroes(float,$size,$size);
$res = $a->copy;
$resc0 = $res->clump(2);
$resc = $resc0;
$resi = xvals $resc;
$inds0 = $resi;

$re00 = 2*(xvals $a)->clump(2)/$size-1.5;
$im00 = 2*(yvals $a)->clump(2)/$size-0.5;

$re0 = $re00;
$im0 = $im00;

$im = $im0; $re = $re0;
$im2 = $im; $re2 = $re;

for(1..60) {
	$rp = ($resc == 0) * ($im2 ** 2 + $re2 ** 2 > 2) * $_;
	$resc += $rp;
	if(1) {
		$inds = ($resc == 0)->which->long->sever;
		$inds1 = $inds0->index($inds)->sever;
		$inds0 = $inds1;
		$re0 = $re00->index($inds1)->sever;
		$im0 = $im00->index($inds1)->sever;
		$re = $re->index($inds)->sever;
		$im = $im->index($inds)->sever;
		$resi = $resi->index($inds)->sever;
	# Use inds1 here so that resc propagates back only one step.
		$resc = $resc0->index($inds1);
	}
	$re2 = $re ** 2 - $im ** 2;
	$im2 = 2 * $re * $im;
	$re2 += $re0;
	$im2 += $im0;
	$re = $re2; $im = $im2;
	nokeeptwiddling3d();
	my $r = $res/max($res);
	imagrgb [$r,1-$r,$r] if $_ % 2 == 0;
}

