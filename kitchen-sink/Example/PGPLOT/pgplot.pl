=head1 NAME

std_pgplot - Examples of PGPLOT routines.

=head1 SYNOPSIS

std_pgplot.pl

=head1 DESCRIPTION

This file is intended to show the use of PGPLOT routines using the
object-oriented approach.

=cut

use PDL;
use PDL::Graphics::PGPLOT::Window;


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

#
# Create a window object to which we will subsequently plot.
#
my $w = PDL::Graphics::PGPLOT::Window->new({Device => '/xw', Aspect => 1,
					       WindowWidth => 7});
# Note how we call the functions via the window object. This particular
# call will set the axis colour to Red.
$w->env(-5, 5, 0, max($yc)*1.1, {Axiscolour => 'Red'});

# Plot a histogram.
$w->bin($xc, $yc);

# Hold the plot. You can subsequently test whether the plot is held
# by calling held() - which returns true (1) if the plot is held.
$w->hold();

# This draws a line plot on top of the histogram. Note that you can
# refer to options using only the first part of their name - and case
# doesn't matter either. It is advised however to use the full name
# for options since in some cases you could imagine getting the
# wrong result.
$w->line($x, $y*max($yc),
	 {LineSty => 'Dashed', Color => 'Blue', LineWidth => 5});

# Release your connection so that the next plotting command will
# erase the screen and create a new plot.
$w->release();

# These two are just a convenience functions for this demo..
next_plot();
function_to_do ('errb() & points()');

# Create some more data - this time for plots with errorbars.
my $xd = pdl(1,3,7,10);
my $yd = pdl(2, 7,5,7);
my $dy = sqrt($yd**2/12+0.2**2);

# This is how you set titles on plots. If you forget to you can
# use the label_axes() command (see below) to set them later.
$w->env(0, 15, 0, 10, {Xtitle => 'X-data', Ytitle=>'Y-data',
		       Title => 'An example of errb and points',
		       Font => 'Italic'});

# Plot the data as points
$w->points($xd, $yd);
# Overplot (implicit) the points with error-bars.
$w->errb($xd, $yd, $dy);
$w->release();

next_plot();
function_to_do('line() poly(), cont(), label_axes() and text()');

# Create an image.
my $im = rvals(100, 100);

# Draw contours - and hold the plot because will clutter it some more.
$w->cont($im, {NCOn => 4});
$w->hold();

# Note that the colours can be specified also in upper case. They can
# also be referred to with numbers in keeping with the PGPLOT tradition.
$w->line(pdl(0, 50), pdl(0, 50), {Color => 'RED'});

# The corners of a polygon - note that the last should be equal to the
# first to get the expected results.
my $px = pdl(20, 40, 40, 20, 20);
my $py = pdl(80, 80, 100, 100, 80);

# The poly function draws polygons - note that we set the fill using
# the numerical notation here - this sets a hatched fill.
$w->poly($px, $py, {Fill => 3});
# Pay attention to the hatching command as it sets the properties using
# an anonymous hash again. I would normally construct this separately
# and use a variable for this for readability.
$w->poly($px, $py, {Color=>'Red', Fill => 3, Hatch => {Phase => 0.5}});

# label_axes() is a separate function to set the axis titles. This is
# often clearer although less compact than setting it directly in the
# env() function.
$w->label_axes('X-direction', 'Y-direction', 'Title', {Color => 'Yellow'});

# The text() command puts text on the display and can be displayed using
# different justifications, angles and fonts as well as colours.
$w->text('Towards the centre', 24, 25, {Justification => 0.5, Angle=>45,
					Font => 'Italic'});


next_plot();
function_to_do('imag(), ctab(), hi2d and several panels');

#
# We now create a new window for image display since we want several
# panels in the plot window. This requires a new window to be created
# at present - changes to the code welcome.
#
$w->close();
$w = PDL::Graphics::PGPLOT::Window->new({Device => '/xw',
					 Aspect => 0.5,
					 NX => 2, NY => 2,
					 CharSize => 2});


# Display the image using a transform between the pixel coordinates
# and the display coordinates.
$w->imag($im, {Transform => pdl([0, 0.1, 0, 0, 0, 0.1])});

# This command will go in the next panel  and will display the image using
# a square root transfer function and square pixels (PIX => 1)
$w->imag1($im, {PIX => 1, ITF=>'Sqrt'});

# You set the colour table using ctab()
$w->ctab('Fire');
$w->imag($im);

# A hold command in a multi-panel situation keeps you in the same panel.
$w->hold();
# So that you can overplot a contour plot for instance...
$w->cont($im, {Color => 'Yellow'});
$w->release();

# The hi2d() function draws a 2D histogram of the data and could very
# likely be improved by someone with some extra time on their hands.
$w->hi2d($im->slice('0:-1:10,0:-1:10'));

next_plot();
function_to_do('Several plot windows. focus_window(), window_list()');

#
# Multiple windows - the secret unveiled... Well, it is easy actually
# at least when you use the OO interface!
#
$w->close();
my $w1 = PDL::Graphics::PGPLOT::Window->new({Device => '/xw', Aspect => 1,
					     AxisColour => 'Blue',
					     WindowName => 'First',
					     WindowWidth => 6});
my $w2 = PDL::Graphics::PGPLOT::Window->new({Device => '/xw', Aspect => 0.618,
					     AxisColour => 'Red',
					     WindowWidth => 6});
# First draw something in Window 1
$w1->line($x, $x**2);
$w1->hold();

# Then switch to window 2...
my $ii = which($x>=0);
$w2->points($x->index($ii), sqrt($x->index($ii)));
$w2->hold();

$w2->line($x->index($ii), sqrt($x->index($ii)),
	  {Color => 'Red', Linestyle => 'dashed'});
$w2->release();

# Switch back to window 1 - note how easier it is with the OO interface.
$w1->points($x, $x**2+$x->grandom());
$w1->release();

# In the OO interface there is no built-in way to keep track of different
# windows in the way that the non-OO interface does, but on the other hand
# you don't really need it.

# See the std_pgplot.pl file for an example of this.

next_plot();
function_to_do('legend(), cursor()');

# Let's close Window2 and continue our examples in window 1.
$w2->close();

# The legend function draws legends on plots which can be for different
# symbols, linestyles, widths and mixtures.
$w1->legend(['Parabola', 'Scatter points'], -2, 20,
  {Width => 5, LineStyle => ['Solid', undef], Symbol => [undef, 'Default']});

#
# Now read the cursor - different types of cursor can be chosen
print "Select a point using the cursor:\n";
my ($xp, $yp)=$w1->cursor({Type => 'CrossHair'});
print "(X, Y)=($xp, $yp)\n";

next_plot();
function_to_do('circle(), ellipse(), rectangle() and arrow()');

# The circle, ellipse and rectangle functions do not take note of the
# intrinsic display aspect ratio so if you want a really round circle,
# you must make sure that the aspect ratio is 1.
$w1->close();
$w1 = PDL::Graphics::PGPLOT::Window->new({Device => '/xs', Aspect => 1,
					  WindowWidth => 6});
$w1->env(0, 100, 0,100);

# Draw a circle - at the moment the default fill-style is solid so we need
# to specify the outline fill to get a non-filled circle..
$w1->circle(50, 50, 10, {Fill => 'Outline'});
# The ellipse can be specified with major and minor axis and the rotation
# angle for the ellipse, but note that the angle must be specified in radians..
$w1->ellipse(40, 20, {MajorAxis => 30, MinorAxis=> 10,
		      Theta => 30*3.14/180, Colour => 'Red'});
# The angle must be specified in radians for the rectangle too....
$w1->rectangle(70, 70, {XSide => 10, Angle => 45*3.14/180});

# And finally draw an arrow...
$w1->arrow(40, 20, 70, 20, {Color => 'Green'});

next_plot();
$w1->close();

$w1 = PDL::Graphics::PGPLOT::Window->new({Device => '/xs', Aspect => 1,
					  NX => 2, NY => 2});
$w1->line($x, $y);

# The important thing here is to show that you can jump directly to a given
# panel and start drawing in this..
$w1->bin($xc, $yc, {Panel => 3});
$w1->env(0, 1, 0, 1, {Axis => 'Box'});
$w1->text("That's all folks!", 0.5, 0.5, {Justification => 0.5, CharSize => 5,
					  Color => 'Yellow'});

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
