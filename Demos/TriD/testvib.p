use blib ;

use PDL;

use PDL::Graphics::TriD;

$PDL::Graphics::TriD::verbose //= 0;

$offs = 0.0;

$tc = 0.2;
$fc = $tc * 0.4;
$sc = $tc * 3.2;;
$fric = 0.02;
$bc = 1-$fric*$tc;

$size = 80 ;

$x = zeroes(float(),$size,$size);

$y = ((rvals $x) < $size/2)->float;

$c = (rvals ($size,$size,{Centre=>[$size/3,$size/3]}))->float ;
$c2 = (rvals ($size,$size,{Centre=>[$size/3,$size/2]}))->float ;

# $sdiv = 12/$size;
$sdiv = 20/$size;

$x .= exp(-($sdiv*$c) ** 2)->float;
$x -= exp(-($sdiv*$c2) ** 2)->float;
$x *= $y;

if(0) {
	$x->set(8,8,0.3);
	$x->set(8,9,0.5);
	$x->set(9,8,0.5);
	$x->set(9,9,1);
	$x->set(10,8,0.3);
	$x->set(10,9,0.5);
	$x->set(8,10,0.3);
	$x->set(9,10,0.5);
	$x->set(10,10,0.3);
}


$asl1 = $x->slice("0:-3,1:-2");
$asl2 = $x->slice("1:-2,0:-3");
$asl3 = $x->slice("2:-1,1:-2");
$asl4 = $x->slice("1:-2,2:-1");

$ach = $x->slice("1:-2,1:-2");
$bch = $x->slice("1:-2,1:-2");

$s = $ach * 0;

$round = 0;

$win = PDL::Graphics::TriD::get_current_window();
# points3d([SURF2D,$x]);
$g = PDL::Graphics::TriD::get_current_graph();

keeptwiddling3d(1);

$surf = new PDL::Graphics::TriD::SLattice_S([$x]);

if(0) {
	$g->add_dataseries($surf,"surf");
	$g->bind_default("surf");
	$g->scalethings();
	$g->PDL::Graphics::TriD::Object::changed();
	$win->add_object($g);
	$win->twiddle();
} else {
	$win->add_object($surf);
}

$a->set(0,0,$offs);
$a->set(1,0,-$offs);
# $as = $a->slice("");
# $as->doflow(1);
while(1) {

	$aav = ($asl1 + $asl2 + $asl3 + $asl4)/4;

	$da = ($aav - $ach) * $fc;
	$da *= $bch;

	$s += $da;
	$s *= $bc;

	$ach += $s * $sc;

	print $round,"\n";

	if($round % 5 == 0) {
		if(0) {
			$surf->data_changed();
	#		$g->data_changed();
			$g->PDL::Graphics::TriD::Object::changed();
			$win->changed();
			$win->twiddle(1);
		} else {
			imag3d([$x],{KeepTwiddling => 0});
		}
	}
	$round++;
}




