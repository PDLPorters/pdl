use Carp;

$SIG{__DIE__} = \&Carp::longmess;

use PDL;
use PDL::Graphics::TriD;
use PDL::Graphics::TriD::Graph;

$g = PDL::Graphics::TriD::Graph->new;
$g->default_axes();

$x = PDL->zeroes(3,1000);
random($x->inplace);

$g->add_dataseries(PDL::Graphics::TriD::Points->new($x,$x),"pts");
$g->bind_default("pts");

$y = PDL->zeroes(3,30,30);
axisvalues($y->slice("(0)")->inplace);
axisvalues($y->slice("(1)")->transpose->inplace);

$y /= 30;

random($y->slice("(2)")->inplace);

($tmp = $y->slice("(2)")) /= 5; $tmp += 2;

$c = PDL->zeroes(3,30,30);
random($c->inplace);

$g->add_dataseries(PDL::Graphics::TriD::SLattice->new($y,$c),"slat");
$g->bind_default("slat");

# $g->add_dataseries(PDL::Graphics::TriD::Lattice->new($y,(PDL->pdl(0,0,0)->dummy(1)->dummy(1))),
# 	"blat");
# $g->bind_default("blat");

$g->add_dataseries(PDL::Graphics::TriD::SCLattice->new($y+1,pdl(1,1,1)->dummy(-1,30)->dummy(-1,30)), "slat2");
$g->bind_default("slat2");

$g->scalethings;

$win = PDL::Graphics::TriD::get_current_window();
$win->clear_objects;
$win->add_object($g);

$win->twiddle;
