
# Test GL + Tk combination.


use blib;
use Carp;

$SIG{__DIE__} = sub {die Carp::longmess(@_);};

use PDL;
use PDL::Graphics::TriD;
use PDL::Graphics::TriD::Image;
use PDL::Graphics::TriD::Tk;
use PDL::IO::Pic;

use PDL::Graphics::TriD::Graph;
use PDL::Graphics::OpenGL;
use FileHandle;

use Tk;

$g = new PDL::Graphics::TriD::Graph();
$g->default_axes();

$a = PDL->zeroes(3,1000);
random($a->inplace);

$g->add_dataseries(new PDL::Graphics::TriD::Points($a,$a),"pts");
$g->bind_default("pts");

$g->scalethings();

my $win = PDL::Graphics::TriD::get_current_window();
$win->clear_objects();
$win->add_object($g);

my $MW = MainWindow->new();
my $but = $MW->Button(-text => 'FOO', -command => sub {print "PRESSED FOO\n";});
$but->pack();

MainLoop;
