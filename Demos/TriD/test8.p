use blib;
use Carp;

 $SIG{__DIE__} = sub {die Carp::longmess(@_);};

use PDL;
use PDL::Graphics::TriD;
use PDL::Opt::Simplex;
use PDL::Dbg;

my $asize = 5000;
my $follow = zeroes(3,4,$asize);
my $followc = zeroes(4,$asize);
my $follows = $follow->oneslice(2,0,1,0);
my $followcs = $followc->oneslice(1,0,1,0);
my $folt = $follows->get_trans();
my $folct = $followcs->get_trans();

sub d2c {
	my($mf) = @_;
	$mf = $mf-min($mf); $mf += (100-$mf)*($mf > 100); $mf /= 100;
	return (1-$mf)**6;
}


sub func {
	my($x) = $_[0]->slice("(0)");
	my($y) = $_[0]->slice("(1)");
	my($z) = $_[0]->slice("(2)");
	return $x**2 + ($y-$x**2)**2 + $z**2;
}

die << "EOD";

This example is disabled since the required
'foomethod' has been disabled in recent versions of PDL.

Contact pdl-porters if you feel you need this functionality.

EOD

my $a = zeroes(3,10000);
random $a->inplace;
$a -= 0.5; $a *= 30;
$mf = d2c(func($a));
points3d($a,[$mf]);

PDL::Graphics::OpenGL::glShadeModel (&PDL::Graphics::OpenGL::GL_SMOOTH);

$PDL::debug = 1;
my $win = PDL::Graphics::TriD::get_current_window();
my $g = PDL::Graphics::TriD::get_current_graph();
$fcc = [$followcs,pdl(0.2),$followcs];
PDL::Graphics::TriD::Rout::combcoords(@$fcc,(my $fccs = PDL->null));
my $line = new PDL::Graphics::TriD::LineStrip($follows->px,$fccs->px);

# $win->add_object($line);

$g->add_dataseries($line,"line");
$g->bind_default("line");

my $ndone = 0;

my $nrounds = 0;
my $perround = 1;

($optimum,$simplex) = simplex(
	pdl(10.0,10.0,10.0),
	0.9,
	0.00000001,
	$asize*$perround+100,
	\&func,
	sub {
		$win->twiddle(1,1);
#		print $_[0],$_[1];
#		print "NDONE: $ndone\n";
		if($ndone == $asize)  {
			return;
		}
		$nrounds++;
#		$follow->dump();
#		$followc->dump();
#		$followcs->dump();
		($tmp = $follow->slice(":,:,($ndone)")) .= $_[0];
		($tmp = $followc->slice(":,($ndone)")) .= d2c($_[1]);
		$ndone++;
		if($nrounds % $perround != 0) {return}
#		print "FOLLOW1:\n";
#		$follow->dump();
		$folt->call_trans_foomethod(0,1,$ndone);
		$folct->call_trans_foomethod(0,1,$ndone);
#		$fccs->dump;
#		$followc->dump;
#		$followcs->dump();
#		print $fccs;
#		print "FOLLOW2:\n";
#		$follow->dump();
		$line->data_changed();
		$win->twiddle(1);
#		print "FOLLOWS: \n";
#		$follows->dump();
#		print "NDONE: $ndone\n";
#		print "FOLLOW:\n";
#		$follow->dump();
#		print "FOLSL: ",$follow->slice(":,:,0:6");
#		print "FOLS: ",$follows;
	}
	,0
);


$win->twiddle();


