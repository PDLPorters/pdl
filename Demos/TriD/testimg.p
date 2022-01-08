use blib;
use Carp;

$SIG{__DIE__} = sub {die Carp::longmess(@_);};

use PDL;
use PDL::Graphics::TriD;
use PDL::Graphics::TriD::Image;
use PDL::IO::Pic;

$PDL::Graphics::TriD::verbose //= 0;

my $win = PDL::Graphics::TriD::get_current_window();
my $vp = $win->new_viewport(0,0,1,1);

# Here we show an 8-dimensional (!!!!!) RGB image to test Image.pm

my $r = zeroes(4,5,6,7,2,2,2,2)+0.1;
my $g = zeroes(4,5,6,7,2,2,2,2);
my $b = zeroes(4,5,6,7,2,2,2,2);

(my $tmp = $r->slice(":,:,2,2")) .= 1;
($tmp = $r->slice(":,:,:,1")) .= 0.5;
($tmp = $g->slice("2,:,1,2")) .= 1;
($tmp = $b->slice("2,3,1,:")) .= 1;

$vp->clear_objects();
$vp->add_object(new PDL::Graphics::TriD::Image([$r,$g,$b]));
$win->twiddle();

