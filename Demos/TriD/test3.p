use blib;
use Carp;

$SIG{__DIE__} = sub {die Carp::longmess(@_);};

use PDL;

#$PDL::Graphics::TriD::verbose=1;

use PDL::Graphics::TriD;

use PDL::Graphics::TriD::Image;
use PDL::IO::Pic;
use PDL::Graphics::TriD::GoBoard;



# Calculate some random function


print "START\n";

sub snap {
	if(1) {return}
	my $ppdl = grabpic3d();
	print "GOT PICTURE!\n";
	wpic $ppdl,"$_[0].jpg";
	system("xv $_[0].jpg");
}

$gob = pdl [
[
	[1,	0,	0,	0],
	[0,	1,	0,	0],
	[0,	0,	1,	0],
	[0,	0,	0,	1],
],
[
	[0.5,	0.5,	0,	0],
	[0.5,	0,	0.5,	0],
	[0.5,	0,	0,	0.5],
	[0,	0,	0.5,	0.5],
],
[
	[0.33,	0.33,	0.33,	0],
	[0.33,	0.33,	0,	0.33],
	[0.33,	0,	0.33,	0.33],
	[0,	0.33,	0.33,	0.33],
],
[
	[0.25,	0.25,	0.25,	0.25],
	[0.25,	0.25,	0.25,	0.25],
	[0.25,	0.25,	0.25,	0.25],
	[0.25,	0.25,	0.25,	0.25],
]
];

$gob2 = $gob->slice(":,1:2,1:2");
$gob3 = $gob->slice(":,2:3,2:3");

$b = new PDL::Graphics::TriD::GoBoard({Data => $gob});
$b->add_inlay($gob2,1,1,0.25);
$b->add_inlay($gob3,2,2,0.5);

if(1) {
$win = PDL::Graphics::TriD::get_current_window();
$win->clear_objects();
$win->add_object($b);
$win->twiddle();
}

snap "pic0";

# $f = zeroes(10,10);

# $foo = cos(xvals($f)/1.5) * cos(yvals($f)/1.5)/2;
$t =  xvals zeroes 30,30;
$u =  yvals zeroes 30,30;

$x = sin($u*0.5 + $t * 0.1)/2+0.5;
$y = cos($u*0.3 + $t * 0.27)/2+0.5;
$z = cos($u*0.1 + $t * 0.56)/2+0.5;

PDL::Graphics::TriD::imagrgb([$x,$y,$z]);
snap "pic1.1";

$x .= $t / 30;
$y .= $u / 30;
$z .= 0.5*($t + $u)/30;

$r = zeroes(4,4,4,4)+0.1;
$g = zeroes(4,4,4,4);
$b = zeroes(4,4,4,4);

($tmp = $r->slice(":,:,2,2")) .= 1;
($tmp = $r->slice(":,:,:,1")) .= 0.5;
($tmp = $g->slice("2,:,1,2")) .= 1;
($tmp = $b->slice("2,3,1,:")) .= 1;

$t = 0.1 * xvals zeroes 300;

$x = sin($t * 0.1)/2+0.5;
$y = cos($t * 0.27)/2+0.5;
$z = cos($t * 0.56)/2+0.5;

line3d([$x,$y,$z],[$x,1-$x,0]);
snap "pic4";

 $f = zeroes(3,3);
$foo = ((xvals $f) - 2) ** 2 + ((yvals $f) -2) ** 2;

print $foo;

print "TOIMAG\n";

PDL::Graphics::TriD::imag3d([$foo]);	# Use default values to make a 3D plot.
		# Stops here for rotating until user presses 'q'.
snap "pic5";

print "OUTOFIMAG\n";

