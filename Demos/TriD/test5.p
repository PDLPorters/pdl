
use blib;
use Carp;

$SIG{__DIE__} = sub {die Carp::longmess(@_);};

use PDL;
use PDL::Graphics::TriD;
use PDL::Graphics::TriD::Image;
use PDL::IO::Pic;

use PDL::Graphics::TriD::Graph;
use PDL::Graphics::OpenGL;

$g = new PDL::Graphics::TriD::Graph();
$g->default_axes();

$x = PDL->zeroes(3,1000);
random($x->inplace);

$g->add_dataseries(new PDL::Graphics::TriD::Points($x,$x),"pts");
$g->bind_default("pts");

$y = PDL->zeroes(3,30,30);
axisvalues($b->slice("(0)"));
axisvalues($b->slice("(1)")->transpose);

$y /= 30;

random($y->slice("(2)")->inplace);

($tmp = $y->slice("(2)")) /= 5; $tmp += 2;

$c = PDL->zeroes(3,30,30);
random($c->inplace);

$g->add_dataseries(new PDL::Graphics::TriD::SLattice($y,$c),"slat");
$g->bind_default("slat");

# $g->add_dataseries(new PDL::Graphics::TriD::Lattice($y,(PDL->pdl(0,0,0)->dummy(1)->dummy(1))),
# 	"blat");
# $g->bind_default("blat");

$g->add_dataseries(new PDL::Graphics::TriD::SCLattice($y+1,$c->slice(":,0:-2,0:-2")),
	"slat2");
$g->bind_default("slat2");

$g->scalethings();

$win = PDL::Graphics::TriD::get_current_window();
$win->clear_objects();
$win->add_object($g);

$win->twiddle();


