use strict;
use warnings;
use PDL;
use PDL::Graphics::TriD;
use PDL::Graphics::TriD::Graph;
use PDL::Graphics::TriD::MathGraph;
use PDL::Graphics::TriD::Labels;

my $g = PDL::Graphics::TriD::get_new_graph();
$g->default_axes;

my @coords = (
 [ 0,-1,0 ],
 [ -1,-1,-2],
 [3,5,2],
 [2,1,-3],
 [1,3,1],
 [1,1,2],
);

my $from = PDL->pdl(indx, [0,1,2,3,4,4,4,5,5,5]);
my $to =   PDL->pdl(indx, [1,2,3,1,0,2,3,0,1,2]);

my @names = map '  '.join(",",@$_), @coords;

my $e = PDL::GraphEvolver->new(pdl(@coords));
$e->set_links($from,$to,PDL->ones(1));
my $c = $e->getcoords;

$g->add_dataseries(my $lab = PDL::Graphics::TriD::Labels->new($c,{Strings => \@names}));
$g->add_dataseries(my $lin = PDL::Graphics::TriD::MathGraph->new(
	$c, {From => $from, To => $to}));
$g->add_dataseries(my $sph = PDL::Graphics::TriD::Spheres->new($c));

$g->scalethings();

nokeeptwiddling3d();
my $ind = 0;
twiddle3d();
while(1) {
	$e->step();
	if(++$ind%2 == 0) {
		$_->data_changed for $lab, $lin, $sph;
		$g->scalethings() if (($ind % 200) == 0 or 1);
		last if twiddle3d();
	}
}
