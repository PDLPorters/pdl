BEGIN { $PDL::Graphics::TriD::device = "VRML"; }
use PDL::Graphics::TriD;
use PDL::LiteF;
use Carp;
#	$PDL::Graphics::TriD::verbose=1;
$SIG{__DIE__} = sub {print Carp::longmess(@_); die;};

$set = tridsettings();
$set->browser_com('netscape/unix');

$nx = 5;

$t =  (xvals zeroes $nx+1,$nx+1)/$nx;
$u =  (yvals zeroes $nx+1,$nx+1)/$nx;

$x = sin($u*15 + $t * 3)/2+0.5 + 5*($t-0.5)**2;
$cx = PDL->zeroes(3,$nx+1,$nx+1);
random($cx->inplace);
$pdl = PDL->zeroes(3,20);
$pdl->inplace->random;
$cols = PDL->zeroes(3,20);
$cols->inplace->random;

$g = PDL::Graphics::TriD::get_new_graph;
$name = $g->add_dataseries(new PDL::Graphics::TriD::Points($pdl,$cols));
$g->bind_default($name);
$name = $g->add_dataseries(new PDL::Graphics::TriD::Lattice([SURF2D,$x]));
$g->bind_default($name);
$name = $g->add_dataseries(new PDL::Graphics::TriD::SLattice_S([SURF2D,$x+1],$cx,
						     {Smooth=>1,Lines=>0}));
$g->bind_default($name);
$g->scalethings();
describe3d('A simple test of
the current PDL 3D
VRML module');

$win = PDL::Graphics::TriD::get_current_window();

use PDL::Graphics::TriD::Logo;

$win->add_object(new PDL::Graphics::TriD::Logo);

#use Data::Dumper;
#my $out = Dumper($win);
#print $out;


$win->display('netscape');


