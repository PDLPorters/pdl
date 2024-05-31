=head1 NAME

PDL::Graphics::TriD - PDL 3D interface

=head1 SYNOPSIS

 use PDL::Graphics::TriD;

 # Generate a somewhat interesting sequence of points:
 $t = sequence(100)/10;
 $x = sin($t); $y = cos($t), $z = $t;
 $coords = cat($x, $y, $z)->transpose;
 my $red = cos(2*$t); my $green = sin($t); my $blue = $t;
 $colors = cat($red, $green, $blue)->transpose;

 # After each graph, let the user rotate and
 # wait for them to press 'q', then make new graph
 line3d($coords);       # $coords = (3,n,...)
 line3d($coords,$colors);  # $colors = (3,n,...)
 line3d([$x,$y,$z]);

 # Generate a somewhat interesting sequence of surfaces
 $surf1 = (rvals(100, 100) / 50)**2 + sin(xvals(100, 100) / 10);
 $surf2 = sqrt(rvals(zeroes(50,50))/2);
 $x = sin($surface); $y = cos($surface), $z = $surface;
 $coords = cat($x, $y, $z)->transpose;
 $red = cos(2*$surface); $green = sin($surface); $blue = $surface;
 $colors = cat($red, $green, $blue)->transpose;
 imagrgb([$red,$green,$blue]);     # 2-d ndarrays
 lattice3d([$surf1]);
 points3d([$x,$y,$z]);
 spheres3d([$x,$y,$z]);  # preliminary implementation

 hold3d(); # the following graphs are on top of each other and the previous
 line3d([$x,$y,$z]);
 line3d([$x,$y,$z+1]);
 $pic = grabpic3d(); # Returns the picture in a (3,$x,$y) float ndarray (0..1).

 release3d(); # the next graph will again wipe out things.

=head1 WARNING

These modules are still in a somewhat unfocused state: don't use them yet
if you don't know how to make them work if they happen to do something
strange.

=head1 DESCRIPTION

This module implements a generic 3D plotting interface for PDL.
Points, lines and surfaces (among other objects) are supported.

With OpenGL, it is easy to manipulate the resulting 3D objects
with the mouse in real time - this helps data visualization a lot.

=for comment
With VRML, you can generate objects for everyone to see with e.g.
Silicon Graphics' Cosmo Player. You can find out more about VRML
at C<http://vrml.sgi.com/> or C<http://www.vrml.org/>

=head1 SELECTING A DEVICE

The default device for TriD is currently OpenGL.
You can specify a different device either in your program
or in the environment variable C<PDL_3D_DEVICE>.
The one specified in the program takes priority.

The currently available devices are

=over 8

=item GL

OpenGL

=item GLpic

OpenGL but off-line (pixmap) rendering and writing to
a graphics file.

=item VRML (I< Not available this release >)

VRML objects rendering. This writes a VRML file describing the
scene. This VRML file can then be read with  a browser.

=back

=head1 ONLINE AND OFFLINE VISUALIZATION

TriD  offers both on- and off-line visualization.
Currently the interface  w.r.t. this division is still much
in motion.

For OpenGL you can select either on- or off-line rendering.
VRML is currently always offline (this may change  later,
if someone bothers to write  the  java(script)  code to  contact
PDL and wait for the next PDL image over the network.

=head1 COORDINATE SPECIFICATIONS

Specifying a set of coordinates is generally a context-dependent operation.
For a traditional 3D surface plot, you'll want two of the coordinates
to have just the xvals and yvals of the ndarray, respectively.
For a line, you would generally want to have one coordinate held
at zero and the other advancing.

This module tries to make a reasonable way of specifying the context
while letting you do whatever you want by overriding the default
interpretation.

The alternative syntaxes for specifying a set of coordinates (or colors) are

   $ndarray                             # MUST have 3 as first dim.
  [$ndarray]
  [$ndarray1,$ndarray2]
  [$ndarray1,$ndarray2,$ndarray3]
  [CONTEXT,$ndarray]
  [CONTEXT,$ndarray1,$ndarray2]
  [CONTEXT,$ndarray1,$ndarray2,$ndarray3]

where C<CONTEXT> is a string describing in which context you wish these
ndarrays to be interpreted. Each routine specifies a default context
which is explained in the routines documentation.
Context is usually used only to understand what the user wants
when they specify less than 3 ndarrays.

The following contexts are currently supported:

=over 8

=item SURF2D

A 2-D lattice. C< [$ndarray] > is interpreted as the Z coordinate over
a lattice over the first dimension. Equivalent to
C<< [$ndarray->xvals, $ndarray->yvals, $ndarray] >>.

=item POLAR2D

A 2-D polar coordinate system. C< [$ndarray] > is interpreted as the
z coordinate over theta and r (theta = the first dimension of the ndarray).

=item COLOR

A set of colors. C< [$ndarray] > is interpreted as grayscale color
(equivalent to C< [$ndarray,$ndarray,$ndarray] >).

=item LINE

A line made of 1 or 2 coordinates. C< [$ndarray] > is interpreted as
C<< [$ndarray->xvals,$ndarray,0] >>. C< [$ndarray1,$ndarray2] > is interpreted as
C<< [$ndarray1,$ndarray2,$ndarray1->xvals] >>.

=back

What makes contexts useful is that if you want to plot points
instead of the full surface you plotted with

  imag3d([$zcoords]);

you don't need to start thinking about where to plot the points:

  points3d([SURF2D,$zcoords]);

will do exactly the same.

=head2 Wrapping your head around 3d surface specifications

Let's begin by thinking about how you might make a 2d data plot.
If you sampled your data at regular intervals, you would have
a time series y(t) = (y0, y1, y2, ...).  You could plot y vs t
by computing t0 = 0, t1 = dt, t2 = 2 * dt, and then plotting
(t0, y0), (t1, y1), etc.

Next suppose that you measured x(t) and y(t).  You can still
plot y vs t, but you can also plot y vs x by plotting (x0, y0),
(x1, y1), etc.  The x-values don't have to increase monotonically:
they could back-track on each other, for example, like the
latitude and longitude of a boat on a lake.  If you use plplot,
you would plot this data using
C<< $pl->xyplot($x, $y, PLOTTYPE => 'POINTS') >>.

Good.  Now let's add a third coordinate, z(t).  If you actually
sampled x and y at regular intervals, so that x and y lie on a
grid, then you can construct a grid for z(x, y), and you would
get a surface.  This is the situation in which you would use
C<mesh3d([$surface])>.

Of course, your data is not required to be regularly gridded.
You could, for example, be measuring the flight path of a bat
flying after mosquitos, which could be wheeling and arching
all over the space.  This is what you might plot using
C<line3d([$x, $y, $z])>.  You could plot the trajectories of
multiple bats, in which case C<$x>, C<$y>, and C<$z> would have
multiple columns, but in general you wouldn't expect them to be
coordinated.

More generally, each coordinate is expected to be arranged in a 3D
fashion, similar to C<3,x,y>. The "3" is the actual 3D coordinates of
each point. The "x,y" help with gridding, because each point at C<x,y>
is expected to have as geographical neighbours C<x+1,y>, C<x-1,y>,
C<x,y+1>, C<x,y-1>, and the grid polygon-building relies on that.
This is how, and why, the 3D earth in C<demo 3d> arranges its data.

 use PDL;
 use PDL::Graphics::TriD;

 # Draw out a trajectory in three-space
 $t = sequence(100)/10;
 $x = sin($t); $y = cos($t); $z = $t;

 # Plot the trajectory as (x(t), y(t), z(t))
 print "using line3d to plot a trajectory (press q when you're done twiddling)\n";
 line3d [$x,$y,$z];

 # If you give it a single ndarray, it expects
 # the data to look like
 # ((x1, y1, z1), (x2, y2, z2), ...)
 # which is why we have to do the exchange:
 $coords = cat($x, $y, $z)->transpose;
 print "again, with a different coordinate syntax (press q when you're done twiddling)\n";
 line3d $coords;

 # Draw a regularly-gridded surface:
 $surface = sqrt(rvals(zeroes(50,50))/2);
 print "draw a mesh of a regularly-gridded surface using mesh3d\n";
 mesh3d [$surface];
 print "draw a regularly-gridded surface using imag3d\n";
 imag3d [$surface], {Lines=>0};

 # Draw a mobius strip:
 $two_pi = 8 * atan2(1,1);
 $t = sequence(51) / 50 * $two_pi;
 # We want three paths:
 $mobius1_x = cos($t) + 0.5 * sin($t/2);
 $mobius2_x = cos($t);
 $mobius3_x = cos($t) - 0.5 * sin($t/2);
 $mobius1_y = sin($t) + 0.5 * sin($t/2);
 $mobius2_y = sin($t);
 $mobius3_y = sin($t) - 0.5 * sin($t/2);
 $mobius1_z = $t - $two_pi/2;
 $mobius2_z = zeroes($t);
 $mobius3_z = $two_pi/2 - $t;

 $mobius_x = cat($mobius1_x, $mobius2_x, $mobius3_x);
 $mobius_y = cat($mobius1_y, $mobius2_y, $mobius3_y);
 $mobius_z = cat($mobius1_z, $mobius2_z, $mobius3_z);

 $mobius_surface = cat($mobius_x, $mobius_y, $mobius_z)->mv(2,0);

 print "A mobius strip using line3d one way\n";
 line3d $mobius_surface;
 print "A mobius strip using line3d the other way\n";
 line3d $mobius_surface->xchg(1,2);
 print "A mobius strip using mesh3d\n";
 mesh3d $mobius_surface;
 print "The same mobius strip using imag3d\n";
 imag3d $mobius_surface, {Lines => 0};

=head1 SIMPLE ROUTINES

Because using the whole object-oriented interface for doing
all your work might be cumbersome, the following shortcut
routines are supported:

=head1 FUNCTIONS

=head2 line3d

=for ref

3D line plot, defined by a variety of contexts.

Implemented by C<PDL::Graphics::TriD::LineStrip>.

=for usage

 line3d ndarray(3,x), {OPTIONS}
 line3d [CONTEXT], {OPTIONS}

=for example

Example:

 pdl> line3d [sqrt(rvals(zeroes(50,50))/2)]
 - Lines on surface
 pdl> line3d [$x,$y,$z]
 - Lines over X, Y, Z
 pdl> line3d $coords
 - Lines over the 3D coordinates in $coords.

Note: line plots differ from mesh plots in that lines
only go in one direction. If this is unclear try both!

See module documentation for more information on
contexts and options

=head2 line3d_segs

=for ref

3D line plot of non-continuous segments, defined by a variety of contexts.

Implemented by C<PDL::Graphics::TriD::Lines>. Handles pairs of vertices
as produced by L<PDL::ImageND/contour_segments>.

=for usage

 line3d_segs ndarray(3,x), {OPTIONS}
 line3d_segs [CONTEXT], {OPTIONS}

=for example

  use PDL::ImageND
  $size = 5;
  $x = xvals($size+1,$size+1) / $size;
  $y = yvals($size+1,$size+1) / $size;
  $z = 0.5 + 0.5 * (sin($x*6.3) * sin($y*6.3)) ** 3;
  $points = cat($x,$y,$z)->mv(-1,0)
  ($segs, $cnt) = contour_segments(pdl(0.203,0.276), $z, $points)
  $segs = $segs->slice(',0:'.$cnt->max)
  line3d_segs $segs

=head2 imag3d

=for ref

3D rendered image plot, defined by a variety of contexts

Implemented by C<PDL::Graphics::TriD::SLattice_S>.

The variant, C<imag3d_ns>, is implemented by C<PDL::Graphics::TriD::SLattice>.

=for usage

 imag3d ndarray(3,x,y), {OPTIONS}
 imag3d [ndarray,...], {OPTIONS}

=for example

Example:

 pdl> imag3d [sqrt(rvals(zeroes(50,50))/2)], {Lines=>0};

 - Rendered image of surface

See module documentation for more information on
contexts and options

=head2 mesh3d

=for ref

3D mesh plot, defined by a variety of contexts

Implemented by C<PDL::Graphics::TriD::Lattice>.

=for usage

 mesh3d ndarray(3,x,y), {OPTIONS}
 mesh3d [ndarray,...], {OPTIONS}

=for example

Example:

 pdl> mesh3d [sqrt(rvals(zeroes(50,50))/2)]

 - mesh of surface

Note: a mesh is defined by two sets of lines at
right-angles (i.e. this is how is differs from
line3d).

See module documentation for more information on
contexts and options

=head2 lattice3d

=for ref

alias for mesh3d

=head2 trigrid3d

Show a triangular mesh, giving C<$vertices> and C<$faceidx> which is
a series of triplets of indices into the vertices, each describing
one triangle. The order of points matters for the shading - the normal
vector points towards the clockface if the points go clockwise.

Options: C<Smooth> (on by default), C<Lines> (off by default),
C<ShowNormals> (off by default, useful for debugging).

Implemented by C<PDL::Graphics::TriD::STrigrid_S>.

=head2 trigrid3d_ns

Like L</trigrid3d>, but without shading or normals.

Implemented by C<PDL::Graphics::TriD::STrigrid>.

=head2 points3d

=for ref

3D points plot, defined by a variety of contexts

Implemented by C<PDL::Graphics::TriD::Points>.

=for usage

 points3d ndarray(3), {OPTIONS}
 points3d [ndarray,...], {OPTIONS}

=for example

Example:

 pdl> points3d [sqrt(rvals(zeroes(50,50))/2)];
 - points on surface

See module documentation for more information on
contexts and options

=head2 spheres3d

=for ref

3D spheres plot (preliminary implementation)

This is a preliminary implementation as a proof of
concept.  It has fixed radii for the spheres being
drawn and no control of color or transparency.

Implemented by C<PDL::Graphics::TriD::Spheres>.

=for usage

 spheres3d ndarray(3), {OPTIONS}
 spheres3d [ndarray,...], {OPTIONS}

=for example

Example:

 pdl> spheres3d ndcoords(10,10,10)->clump(1,2,3)

 - lattice of spheres at coordinates on 10x10x10 grid

=head2 imagrgb

=for ref

2D RGB image plot (see also imag2d)

Implemented by C<PDL::Graphics::TriD::Image>.

=for usage

 imagrgb ndarray(3,x,y), {OPTIONS}
 imagrgb [ndarray,...], {OPTIONS}

This would be used to plot an image, specifying
red, green and blue values at each point. Note:
contexts are very useful here as there are many
ways one might want to do this.

=for example

e.g.

 pdl> $x=sqrt(rvals(zeroes(50,50))/2)
 pdl> imagrgb [0.5*sin(8*$x)+0.5,0.5*cos(8*$x)+0.5,0.5*cos(4*$x)+0.5]

=head2 imagrgb3d

=for ref

2D RGB image plot as an object inside a 3D space

Implemented by C<PDL::Graphics::TriD::Image>.

=for usage

 imagrgb3d ndarray(3,x,y), {OPTIONS}
 imagrgb3d [ndarray,...], {OPTIONS}

The ndarray gives the colors. The option allowed is Points,
which should give 4 3D coordinates for the corners of the polygon,
either as an ndarray or as array ref.
The default is [[0,0,0],[1,0,0],[1,1,0],[0,1,0]].

=for example

e.g.

 pdl> imagrgb3d $colors, {Points => [[0,0,0],[1,0,0],[1,0,1],[0,0,1]]};
 - plot on XZ plane instead of XY.

=head2 grabpic3d

=for ref

Grab a 3D image from the screen.

=for usage

 $pic = grabpic3d();

The returned ndarray has dimensions (3,$x,$y) and is of type float
(currently). XXX This should be altered later.

=head2 contour3d

=for usage

 contour3d $d,[$x,$y,$z],[$r,$g,$b], {OPTIONS}

where C<$d> is a 2D pdl of data to be contoured. C<[$x,$y,$z]> define a 3D
map of C<$d> into the visualization space. C<[$r,$g,$b]> is an optional C<[3,1]>
ndarray specifying the contour color and C<$options> is a hash reference to
a list of options documented below.  Contours can also be coloured by
value using the set_color_table function.

Implemented by L<PDL::Graphics::TriD::Contours>.

=head2 hold3d, release3d

=for ref

Keep / don't keep the previous objects when plotting new 3D objects

=for usage

 hold3d();
 release3d();

or

 hold3d(1);
 hold3d(0);

=head2 keeptwiddling3d, nokeeptwiddling3d

=for ref

Wait / don't wait for 'q' after displaying a 3D image.

Usually, when showing 3D images, the user is given a chance
to rotate it and then press 'q' for the next image. However,
sometimes (for e.g. animation) this is undesirable and it is
more desirable to just run one step of the event loop at
a time.

=for usage

 keeptwiddling3d();
 nokeeptwiddling3d();

or

 keeptwiddling3d(1);
 keeptwiddling3d(0);

When an image is added to the screen, keep twiddling it until
user explicitly presses 'q'.

=for example

 keeptwiddling3d();
 imag3d(..);
 nokeeptwiddling3d();
 $o = imag3d($c);
 do {
	$c .= nextfunc($c);
	$o->data_changed;
 } while(!twiddle3d()); # animate one step, then iterate
 keeptwiddling3d();
 twiddle3d(); # wait one last time

=head2 twiddle3d

=for ref

Wait for the user to rotate the image in 3D space.

Let the user rotate the image in 3D space, either for one step
or until they press 'q', depending on the 'keeptwiddling3d'
setting. If 'keeptwiddling3d' is not set the routine returns
immediately and indicates that a 'q' event was received by
returning 1. If the only events received were mouse events,
returns 0.

=head2 close3d

=for ref

Close the currently-open 3D window.

=head1 CONCEPTS

The key concepts (object types) of TriD are explained in the following:

=head2 Object

In this 3D abstraction, everything that you can "draw"
without using indices is an Object. That is, if you have a surface,
each vertex is not an object and neither is each segment of a long
curve. The whole curve (or a set of curves) is the lowest level Object.

Transformations and groups of Objects are also Objects.

A Window is simply an Object that has subobjects.

=head2 Twiddling

Because there is no eventloop in Perl yet and because it would
be hassleful to do otherwise, it is currently not possible to
e.g. rotate objects with your mouse when the console is expecting
input or the program is doing other things. Therefore, you need
to explicitly say "$window->twiddle()" in order to display anything.

=head1 OBJECTS

The following types of objects are currently supported.
Those that do not have a calling sequence described here should
have their own manual pages.

There are objects that are not mentioned here; they are either internal
to PDL3D or in rapidly changing states. If you use them, you do so at
your own risk.

The syntax C<PDL::Graphics::TriD::Scale(x,y,z)> here means that you create
an object like

	$c = PDL::Graphics::TriD::Scale->new($x,$y,$z);

=head2 PDL::Graphics::TriD::LineStrip

This is just a line or a set of lines. The arguments are 3 1-or-more-D
ndarrays which describe the vertices of a continuous line and an
optional color ndarray (which is 1-D also and simply
defines the color between red and blue. This will probably change).

=head2 PDL::Graphics::TriD::Lines

This is just a line or a set of lines. The arguments are 3 1-or-more-D
ndarrays where each contiguous pair of vertices describe a line segment
and an optional color ndarray (which is 1-D also and simply
defines the color between red and blue. This will probably change).

=head2 PDL::Graphics::TriD::Image

This is a 2-dimensional RGB image consisting of colored
rectangles. With OpenGL, this is implemented by texturing so this should
be relatively memory and execution-time-friendly.

=head2 PDL::Graphics::TriD::Lattice

This is a 2-D set of points connected by lines in 3-space.
The constructor takes as arguments 3 2-dimensional ndarrays.

=head2 PDL::Graphics::TriD::Points

This is simply a set of points in 3-space. Takes as arguments
the x, y and z coordinates of the points as ndarrays.

=head2 PDL::Graphics::TriD::Scale(x,y,z)

Self-explanatory

=head2 PDL::Graphics::TriD::Translation(x,y,z)

Ditto

=head2 PDL::Graphics::TriD::Quaternion(c,x,y,z)

One way of representing rotations is with quaternions. See the appropriate
man page.

=head2 PDL::Graphics::TriD::ViewPort

This is a special class: in order to obtain a new viewport, you
need to have an earlier viewport on hand. The usage is:

  $new_vp = $old_vp->new_viewport($x0,$y0,$x1,$y1);

where $x0 etc are the coordinates of the upper left and lower right
corners of the new viewport inside the previous (relative
to the previous viewport in the (0,1) range.

Every implementation-level window object should implement the new_viewport
method.

=cut

#KGB: NEEDS DOCS ON COMMON OPTIONS!!!!!

# List of global variables
#
# $PDL::Graphics::TriD::offline
# $PDL::Graphics::TriD::Settings
$PDL::Graphics::TriD::verbose //= 0;
# $PDL::Graphics::TriD::keeptwiddling
# $PDL::Graphics::TriD::only_one
# $PDL::Graphics::TriD::create_window_sub
# $PDL::Graphics::TriD::current_window
#
# '

package PDL::Graphics::TriD;

use strict;
use warnings;
use PDL::Exporter;
use PDL::Core '';  # barf
our @ISA = qw/PDL::Exporter/;
our @EXPORT_OK = qw/imag3d_ns imag3d line3d mesh3d lattice3d points3d
  trigrid3d trigrid3d_ns line3d_segs
  contour3d spheres3d describe3d imagrgb imagrgb3d hold3d release3d
  keeptwiddling3d nokeeptwiddling3d close3d
  twiddle3d grabpic3d tridsettings/;
our %EXPORT_TAGS = (Func=>\@EXPORT_OK);
our $verbose;

use PDL::Graphics::TriD::Object;
use PDL::Graphics::TriD::Window;
use PDL::Graphics::TriD::ViewPort;
use PDL::Graphics::TriD::Graph;
use PDL::Graphics::TriD::Quaternion;
use PDL::Graphics::TriD::Objects;
use PDL::Graphics::TriD::Rout;

# Then, see which display method are we using:

$PDL::Graphics::TriD::device = $PDL::Graphics::TriD::device;
BEGIN {
	my $dev = $PDL::Graphics::TriD::device; # First, take it from this variable.
	$dev ||= $::ENV{PDL_3D_DEVICE};
        if(!defined $dev) {
#            warn "Default PDL 3D device is GL (OpenGL):
# Set PDL_3D_DEVICE=GL in your environment in order not to see this warning.
# You must have OpenGL or Mesa installed and the PDL::Graphics::OpenGL extension
# compiled. Otherwise you will get strange warnings.";

           $dev = "GL";  # default GL works on all platforms now
        }
	my $dv;
# The following is just a sanity check.
	for($dev) {
#		(/^OOGL$/  and $dv="PDL::Graphics::TriD::OOGL") or
		(/^GL$/  and $dv="PDL::Graphics::TriD::GL") or
		(/^GLpic$/  and $dv="PDL::Graphics::TriD::GL" and $PDL::Graphics::TriD::offline=1) or
		(/^VRML$/  and $dv="PDL::Graphics::TriD::VRML" and $PDL::Graphics::TriD::offline=1) or
		(barf "Invalid PDL 3D device '$_' specified!");
	}
	my $mod = $dv;
	$mod =~ s|::|/|g;
	print "dev = $dev mod=$mod\n" if($verbose);
	require "$mod.pm";
	$dv->import;
        my $verbose;
}

# currently only used by VRML backend
$PDL::Graphics::TriD::Settings = $PDL::Graphics::TriD::Settings;
sub tridsettings {return $PDL::Graphics::TriD::Settings}

# Allowable forms:
# x(3,..)  [x(..),y(..),z(..)]
sub realcoords {
	my($type,$c) = @_;
	if(ref $c ne "ARRAY") {
		if($c->getdim(0) != 3) {
			barf "If one ndarray given for coordinate, must be (3,...) or have default interpretation";
		}
		return $c ;
	}
	my @c = @$c;
	if(!ref $c[0]) {$type = shift @c}
	if(!@c || @c>3) {
		barf "Must have 1..3 array members for coordinates";
	}
	if(@c == 1 and $type eq "SURF2D") {
		# surf2d -> this is z axis
		@c = ($c[0]->xvals,$c[0]->yvals,$c[0]);
	} elsif(@c == 1 and $type eq "POLAR2D") {
		my $t = 6.283 * $c[0]->xvals / ($c[0]->getdim(0)-1);
		my $r = $c[0]->yvals / ($c[0]->getdim(1)-1);
		@c = ($r * sin($t), $r * cos($t), $c[0]);
	} elsif(@c == 1 and $type eq "COLOR") {
		# color -> 1 ndarray = grayscale
		@c = @c[0,0,0];
	} elsif(@c == 1 and $type eq "LINE") {
		@c = ($c[0]->xvals, $c[0], 0);
	} elsif(@c == 2 and $type eq "LINE") {
		@c = (@c[0,1], $c[0]->xvals);
	}
	# XXX
	if(@c != 3) {
		barf("Must have 3 coordinates if no interpretation (here '$type')");
	}
	# allow a constant (either pdl or not) to be introduced in one dimension
	foreach(0..2) {
	  if(ref($c[$_]) ne "PDL" or $c[$_]->nelem==1){
	    $c[$_] = $c[$_]*(PDL->ones($c[($_+1)%3]->dims));
	  }
	}
	my $g = PDL::Graphics::TriD::Rout::combcoords(@c);
	$g->dump if $PDL::Graphics::TriD::verbose;
	return $g;
}

sub checkargs {
	if(ref $_[$#_] eq "HASH" and $PDL::Graphics::TriD::verbose) {

	  print "enter checkargs \n";
		for(['KeepTwiddling',\&keeptwiddling3d]) {
		  print "checkargs >$_<\n";
			if(defined $_[$#_]{$_->[0]}) {
				&{$_->[1]}(delete $_[$#_]{$_->[0]});
			}
		}
	}
}

*keeptwiddling3d=*keeptwiddling3d=\&PDL::keeptwiddling3d;
sub PDL::keeptwiddling3d {
	$PDL::Graphics::TriD::keeptwiddling = $_[0] // 1;
}
*nokeeptwiddling3d=*nokeeptwiddling3d=\&PDL::nokeeptwiddling3d;
sub PDL::nokeeptwiddling3d {
	$PDL::Graphics::TriD::keeptwiddling = 0 ;
}
keeptwiddling3d();
*twiddle3d = *twiddle3d = *PDL::twiddle3d = \&twiddle_current;

*close3d = *close3d = \&PDL::close3d;
sub PDL::close3d {
  return if !ref $PDL::Graphics::TriD::current_window;
  return if !$PDL::Graphics::TriD::current_window->can('close');
  $PDL::Graphics::TriD::current_window->close;
}

sub graph_object {
	my($obj) = @_;
	if(!defined $obj or !ref $obj) {
		barf("Invalid object to TriD::graph_object");
	}
	print "graph_object: calling get_new_graph\n" if($PDL::Graphics::TriD::verbose);
	my $g = get_new_graph();
	print "graph_object: back from get_new_graph\n" if($PDL::Graphics::TriD::verbose);
	my $name = $g->add_dataseries($obj);
	$g->bind_default($name);
	$g->scalethings();
	print "ADDED TO GRAPH: '$name'\n" if $PDL::Graphics::TriD::verbose;
	twiddle_current();
	return $obj;
}

# Plotting routines that use the whole viewport

*describe3d=*describe3d=\&PDL::describe3d;
sub PDL::describe3d {
	require PDL::Graphics::TriD::TextObjects;
	my ($text) = @_;
	my $win = PDL::Graphics::TriD::get_current_window();
	my $imag = PDL::Graphics::TriD::Description->new($text);
	$win->add_object($imag);
#	$win->twiddle();
}

*imagrgb=*imagrgb=\&PDL::imagrgb;
sub PDL::imagrgb {
	require PDL::Graphics::TriD::Image;
	my (@data) = @_; &checkargs;
	my $win = PDL::Graphics::TriD::get_current_window();
	my $imag = PDL::Graphics::TriD::Image->new(@data);
	$win->clear_viewports();
	$win->current_viewport()->add_object($imag);
	$win->twiddle();
}

# Plotting routines that use the 3D graph

# Call: line3d([$x,$y,$z],[$color]);
*line3d=*line3d=\&PDL::line3d;
sub PDL::line3d {
    &checkargs;
    my $obj = PDL::Graphics::TriD::LineStrip->new(@_);
    print "line3d: object is $obj\n" if($PDL::Graphics::TriD::verbose);
    graph_object($obj);
}

*line3d_segs=*line3d_segs=\&PDL::line3d_segs;
sub PDL::line3d_segs {
    &checkargs;
    my $obj = PDL::Graphics::TriD::Lines->new(@_);
    print "line3d_segs: object is $obj\n" if($PDL::Graphics::TriD::verbose);
    graph_object($obj);
}

*contour3d=*contour3d=\&PDL::contour3d;
sub PDL::contour3d {
  &checkargs;
  require PDL::Graphics::TriD::Contours;
  graph_object(PDL::Graphics::TriD::Contours->new(@_));
}

# XXX Should enable different positioning...
*imagrgb3d=*imagrgb3d=\&PDL::imagrgb3d;
sub PDL::imagrgb3d { &checkargs;
	require PDL::Graphics::TriD::Image;
	graph_object(PDL::Graphics::TriD::Image->new(@_));
}

*imag3d_ns=*imag3d_ns=\&PDL::imag3d_ns;
sub PDL::imag3d_ns {  &checkargs;
	graph_object(PDL::Graphics::TriD::SLattice->new(@_));
}

*imag3d=*imag3d=\&PDL::imag3d;
sub PDL::imag3d { &checkargs;
	graph_object(PDL::Graphics::TriD::SLattice_S->new(@_));
}

*trigrid3d=*trigrid3d=\&PDL::trigrid3d;
sub PDL::trigrid3d { &checkargs;
  graph_object(PDL::Graphics::TriD::STrigrid_S->new(@_)); }

*trigrid3d_ns=*trigrid3d_ns=\&PDL::trigrid3d_ns;
sub PDL::trigrid3d_ns { &checkargs;
  graph_object(PDL::Graphics::TriD::STrigrid->new(@_)); }

*mesh3d=*mesh3d=\&PDL::mesh3d;
*lattice3d=*lattice3d=\&PDL::mesh3d;
*PDL::lattice3d=*PDL::lattice3d=\&PDL::mesh3d;
sub PDL::mesh3d { &checkargs;
	graph_object(PDL::Graphics::TriD::Lattice->new(@_));
}

*points3d=*points3d=\&PDL::points3d;
sub PDL::points3d { &checkargs;
	graph_object(PDL::Graphics::TriD::Points->new(@_));
}

*spheres3d=*spheres3d=\&PDL::spheres3d;
sub PDL::spheres3d { &checkargs;
	graph_object(PDL::Graphics::TriD::Spheres->new(@_));
}

*grabpic3d=*grabpic3d=\&PDL::grabpic3d;
sub PDL::grabpic3d {
	my $win = PDL::Graphics::TriD::get_current_window();
	barf "backend doesn't support grabbing the rendered scene"
	  unless $win->can('read_picture');
	my $pic = $win->read_picture();
	return ($pic->float) / 255;
}

$PDL::Graphics::TriD::only_one = 1;
sub PDL::hold3d {$PDL::Graphics::TriD::only_one = !($_[0] // 1);}
sub PDL::release3d {$PDL::Graphics::TriD::only_one = 1;}

*hold3d=*hold3d=\&PDL::hold3d;
*release3d=*release3d=\&PDL::release3d;

sub get_new_graph {
    print "get_new_graph: calling PDL::Graphics::TriD::get_current_window...\n" if($PDL::Graphics::TriD::verbose);
	my $win = PDL::Graphics::TriD::get_current_window();
    print "get_new_graph: calling get_current_graph...\n" if($PDL::Graphics::TriD::verbose);
	my $g = get_current_graph($win);
    print "get_new_graph: back get_current_graph returned $g...\n" if($PDL::Graphics::TriD::verbose);
	if ($PDL::Graphics::TriD::only_one) {
		$g->clear_data;
		$win->clear_viewport;
	}
	$g->default_axes;
	$win->add_object($g);
	return $g;
}

sub get_current_graph {
   my $win = shift;
	my $g = $win->current_viewport()->graph();
	if(!defined $g) {
		$g = PDL::Graphics::TriD::Graph->new;
		$g->default_axes();
		$win->current_viewport()->graph($g);
	}
	return $g;
}

# $PDL::Graphics::TriD::create_window_sub = undef;
sub get_current_window {
  my $opts = shift @_;
  my $win = $PDL::Graphics::TriD::current_window;

  if(!defined $win) {
	 if(!$PDL::Graphics::TriD::create_window_sub) {
		barf("PDL::Graphics::TriD must be used with a display mechanism: for example PDL::Graphics::TriD::GL!\n");
	 }
	 print "get_current_window - creating window...\n" if($PDL::Graphics::TriD::verbose);
	 $PDL::Graphics::TriD::current_window = $win = PDL::Graphics::TriD::Window->new($opts);

	 print "get_current_window - calling set_material...\n" if($PDL::Graphics::TriD::verbose);
	 $win->set_material(PDL::Graphics::TriD::Material->new);
  }
  return $PDL::Graphics::TriD::current_window;
}

sub twiddle_current { get_current_window()->twiddle() }

###################################
#
#
package PDL::Graphics::TriD::Material;

sub new {
  my ($type,%ops) = @_;
  my $this = bless {}, $type;
  for (['Shine',40],
       ['Specular',[1,1,0.3,0]],
       ['Ambient',[0.3,1,1,0]],
       ['Diffuse',[1,0.3,1,0]],
       ['Emissive',[0,0,0]]) {
    if (!defined $ops{$_->[0]}) {
      $this->{$_->[0]} = $_->[1];
    } else {
      $this->{$_->[0]} = $ops{$_->[0]};
    }
  }
  return $this;
}

package PDL::Graphics::TriD::BoundingBox;
use base qw/PDL::Graphics::TriD::Object/;
use fields qw/Box/;

sub new {
  my($type,$x0,$y0,$z0,$x1,$y1,$z1) = @_;
  my $this = $type->SUPER::new();
  $this->{Box} = [$x0,$y0,$z0,$x1,$y1,$z1];
}

sub normalize {my($this,$x0,$y0,$z0,$x1,$y1,$z1) = @_;
	$this = $this->{Box};
	my $trans = PDL::Graphics::TriD::Transformation->new();
	my $sx = ($x1-$x0)/($this->[3]-$this->[0]);
	my $sy = ($y1-$y0)/($this->[4]-$this->[1]);
	my $sz = ($z1-$z0)/($this->[5]-$this->[2]);
	$trans->add_transformation(
		PDL::Graphics::TriD::Translation->new(
			($x0-$this->[0]*$sx),
			($y0-$this->[1]*$sy),
			($z0-$this->[2]*$sz)
		));
	$trans->add_transformation(PDL::Graphics::TriD::Scale->new($sx,$sy,$sz));
	return $trans;
}

package PDL::Graphics::TriD::OneTransformation;
use fields qw/Args/;

sub new {
  my($type,@args) = @_;
  my $this = fields::new($type);
  $this->{Args} = [@args];
  $this;
}

package PDL::Graphics::TriD::Scale;
use base qw/PDL::Graphics::TriD::OneTransformation/;

package PDL::Graphics::TriD::Translation;
use base qw/PDL::Graphics::TriD::OneTransformation/;


package PDL::Graphics::TriD::Transformation;
use base qw/PDL::Graphics::TriD::Object/;

sub add_transformation {
	my($this,$trans) = @_;
	push @{$this->{Transforms}},$trans;
}

=head1 AUTHOR

Copyright (C) 1997 Tuomas J. Lukka (lukka@husc.harvard.edu). Documentation
contributions from Karl Glazebrook (kgb@aaoepp.aao.gov.au).
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.

=cut

1;
