use blib;
use Carp;

# $SIG{__DIE__} = sub {die Carp::longmess(@_);};

use PDL;
use PDL::Graphics::TriD;
use PDL::IO::Pic;
use PDL::Graphics::TriD::Polygonize;


$orig = PDL->pdl(0,0,0)->float;

sub func1 {
	my($x,$y,$z) = map {$_[0]->slice("($_)")} 0..2;
	$r = $x**2 + 1.5*$y**2 + 0.3 * $z**2 + 5*($x**2-$y)**2;
	$res = ($r - 1) *  -1;
#	print $res;
	return $res;
}

$a = PDL::Graphics::TriD::StupidPolygonize::stupidpolygonize($orig,
	5, 50, 10,\&func1)  ;

# print $a;
imag3d $a,{Lines => 0, Smooth => 1};
