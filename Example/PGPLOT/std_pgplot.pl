=head1 NAME

std_pgplot - Examples of PGPLOT routines.

=head1 SYNOPSIS

std_pgplot.pl

=head1 DESCRIPTION

This file is intended to show the use of PGPLOT routines using the
standard interface. See the C<pgplot.pl> file for an object-oriented
version of the same (the object-oriented interface is strongly recommended.)

=cut

use PDL;
use PDL::Graphics::PGPLOT;


##
## Test all PGPLOT routines.
##
my $random = grandom(1000);
my ($xc, $yc)=hist $random;
my $x=zeroes(100)->xlinvals(-5,5);
my $y=exp(-$x*$x/2);



print "First we will test all functions in PGPLOT.\n";
print "We also will show most of the options\n";
print "After each plot - please press <CR> to proceed - type q<CR> to quit.\n";

function_to_do ('dev(), env(), hold(), release(), bin(), line()');

my $win1 = dev('/xw', {Aspect => 1, WindowWidth => 7});
env(-5, 5, 0, max($yc)*1.1, {Axiscolour => 'Red'});
bin $xc, $yc;
hold;
line $x, $y*max($yc), {LineSty => 'Dashed', Color => 'Blue', LineWidth => 5};
release;
#close_window($win);

next_plot();
function_to_do ('errb() & points()');
my $xd = pdl(1,3,7,10);
my $yd = pdl(2, 7,5,7);
my $dy = sqrt($yd**2/12+0.2**2);
env(0, 15, 0, 10, {Xtitle => 'X-data', Ytitle=>'Y-data',
		   Title => 'An example of errb and points', Font => 'Italic'});
points $xd, $yd;
errb $xd, $yd, $dy;
release;

next_plot();
function_to_do('line() poly(), cont(), label_axes() and text()');
my $im = rvals(100, 100);
cont $im, {NCOn => 4};
hold;
line pdl(0, 50), pdl(0, 50), {Color => 'RED'};
my $px = pdl(20, 40, 40, 20, 20);
my $py = pdl(80, 80, 100, 100, 80);
poly $px, $py, {Fill => 3};
poly $px, $py, {Color=>'Red', Fill => 3, Hatch => {Phase => 0.5}};
label_axes('X-direction', 'Y-direction', 'Title', {Color => 'Yellow'});

text 'Towards the centre', 24, 25, {Justification => 0.5, Angle=>45,
				    Font => 'Italic'};


next_plot();
function_to_do('imag(), ctab(), hi2d and several panels');

$win1= dev('/xw', 2, 2, {Aspect => 0.5, CharSize => 2});
imag $im, {Transform => pdl([0, 0.1, 0, 0, 0, 0.1])};
imag1 $im, {PIX => 1, ITF=>'Sqrt'};
ctab('Fire');
imag $im;
hold;
cont $im, {Color => 'Yellow'};
release;
hi2d $im->slice('0:-1:10,0:-1:10');

next_plot();
function_to_do('Several plot windows. focus_window(), window_list()');

close_window($win1);
$win1 = dev('/xw', {Aspect => 1, AxisColour => 'Blue', WindowName => 'First',
		   WindowWidth => 6});
my $win2 = dev('/xw', {Aspect => 0.618, AxisColour => 'Red',
		       WindowWidth => 6, NewWindow => 1});

focus_window('First');
line $x, $x**2;
hold;
focus_window($win2);
my $ii = which($x>=0);
points $x->index($ii), sqrt($x->index($ii));
hold;
line $x->index($ii), sqrt($x->index($ii)), {Color => 'Red', Linestyle => 'dashed'};
release;
focus_window($win1);
points $x, $x**2+$x->grandom();
release;

my ($nums, $names)=window_list();
print "Window list:\n";
for (my $i=0; $i <= $#$nums; $i++) {
  print "   $$nums[$i]:  $$names[$i]\n";
}


next_plot();
function_to_do('legend(), cursor()');

close_window($win2);

legend ['Parabola', 'Scatter points'], -2, 20,
  {Width => 5, LineStyle => ['Solid', undef], Symbol => [undef, 'Default']};


print "Select a point using the cursor:\n";
my ($xp, $yp)=cursor({Type => 'CrossHair'});
print "(X, Y)=($xp, $yp)\n";

next_plot();

function_to_do('circle(), ellipse(), rectangle() and arrow()');

dev('/xs', {Aspect => 1, WindowWidth => 6});
env(0, 100, 0,100);
circle(50, 50, 10, {Fill => 'Outline'});
ellipse(40, 20, {MajorAxis => 30, MinorAxis=> 10, Theta => 30*3.14/180, Colour => 'Red'});
rectangle(70, 70, {XSide => 10, Angle => 45*3.14/180});

next_plot();

close_window($win1);

$win1 = dev('/xw', 2, 2, {Aspect => 1});
line $x, $y;
bin $xc, $yc, {Panel => 3};
env(0, 1, 0, 1, {Axis => 'Box'});
text "That's all folks!", 0.5, 0.5, {Justification => 0.5, CharSize => 3,
				     Color => 'Yellow'};

next_plot();


sub function_to_do {

  print "\n**************************\n";
  print "* $_[0]\n";
  print "**************************\n\n";

}


sub next_plot {
  my $message = shift;

  $message ||='';

  print $message."\n";
  my $in = <STDIN>;

  if ($in =~ /^q/i) {
    exit;
  }


}
