BEGIN {
   $PDL::Graphics::TriD::device = "VRML";
   print "====================================\n";
   print " VRML not available...stopping demo \n";
   print "====================================\n";
   exit;
}
use PDL::Graphics::TriD;
use PDL::LiteF;
use Carp;
$PDL::Graphics::TriD::verbose //= 0;
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
$name = $g->add_dataseries(PDL::Graphics::TriD::Points->new($pdl,$cols));
$g->bind_default($name);
$name = $g->add_dataseries(PDL::Graphics::TriD::Lattice->new([SURF2D,$x]));
$g->bind_default($name);
$name = $g->add_dataseries(PDL::Graphics::TriD::SLattice_S->new([SURF2D,$x+1],$cx,
						     {Smooth=>1,Lines=>0}));
$g->bind_default($name);
$g->scalethings();
describe3d('A simple test of
the current PDL 3D
VRML module');

$win = PDL::Graphics::TriD::get_current_window();

use PDL::Graphics::TriD::Logo;

$win->add_object(PDL::Graphics::TriD::Logo->new);

#use Data::Dumper;
#my $out = Dumper($win);
#print $out;


$win->display('netscape');


