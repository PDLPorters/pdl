=head1 NAME

PDL::Graphics::TriD -- PDL 3D interface

=head1 SYNOPSIS

use PDL::Graphics::TriD;

# After each graph, let the user rotate is and
# wait for him to press 'q', then make new graph
line3d($coords);       # $coords = (3,n,...)
line3d($coords,$colors);  # $colors = (3,n,...)
line3d([$x,$y,$z]);
imagrgb([$r,$g,$b]);
lattice3d([$x,$y,$z]); # 2-d piddles
points3d([$x,$y,$z]);

hold3d(); # the following graphs are on top of each other and the previous
line3d([$x,$y,$z]);
line3d([$x,$y,$z+1]);
$pic = grabpic3d(); # Returns the picture in a (3,$x,$y) float piddle (0..1).

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

=item VRML

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
to have just the xvals and yvals of the piddle, respectively.
For a line, you would generally want to have one coordinate held
at zero and the other advancing.

This module tries to make a reasonable way of specifying the context
while letting you do whatever you want by overriding the default
interpretation.

The alternative syntaxes for specifying a set of coordinates (or colors) are

$piddle                             # MUST have 3 as first dim.
  [$piddle]
  [$piddle1,$piddle2]
  [$piddle1,$piddle2,$piddle3]
  [CONTEXT,$piddle]
  [CONTEXT,$piddle1,$piddle2]
  [CONTEXT,$piddle1,$piddle2,$piddle3]

where C<CONTEXT> is a string describing in which context you wish these
piddles to be interpreted. Each routine specifies a default context
which is explained in the routines documentation.
Context is usually used only to understand what the user wants
when he/she specifies less than 3 piddles.

The following contexts are currently supported:

=over 8

=item SURF2D

A 2-D lattice. [$piddle] is interpreted as the Z coordinate over
a lattice over the first dimension. Equivalent to
[$piddle->xvals, $piddle->yvals, $piddle].

=item POLAR2D

A 2-D polar coordinate system. [$piddle] is interpreted as the
z coordinate over theta and r (theta = the first dimension of the piddle).

=item COLOR

A set of colors. [$piddle] is interpreted as grayscale color
(equivalent to [$piddle,$piddle,$piddle]).

=item LINE

A line made of 1 or 2 coordinates. [$piddle] is interpreted as
[$piddle->xvals,$piddle,0]. [$piddle1,$piddle2] is interpreted as
[$piddle1,$piddle2,$piddle1->xvals].

=back

What makes contexts useful is that if you want to plot points
instead of the full surface you plotted with

  imag3d([$zcoords]);

you don't need to start thinking about where to plot the points:

	points3d([SURF2D,$zcoords]);

will do exactly the same.

=head1 SIMPLE ROUTINES

Because using the whole object-oriented interface for doing
all your work might be cumbersome, the following shortcut
routines are supported:

=head1 FUNCTIONS

=head2 line3d

=for ref

3D line plot, defined by a variety of contexts.

=for usage

 line3d piddle(3,x), {OPTIONS}
 line3d [CONTEXT], {OPTIONS}

=for example

Example:

 perldl> line3d [sqrt(rvals(zeroes(50,50))/2)]
 - Lines on surface
 perldl> line3d [$x,$y,$z]
 - Lines over X, Y, Z
 perldl> line3d $coords
 - Lines over the 3D coordinates in $coords.

Note: line plots differ from mesh plots in that lines
only go in one direction. If this is unclear try both!

See module documentation for more information on
contexts and options

=head2 imag3d

=for ref

3D rendered image plot, defined by a variety of contexts

=for usage

 imag3d piddle(3,x,y), {OPTIONS}
 imag3d [piddle,...], {OPTIONS}

=for example

Example:

 perldl> imag3d [sqrt(rvals(zeroes(50,50))/2)], {{Lines=>0};

 - Rendered image of surface

See module documentation for more information on
contexts and options

=head2 mesh3d

=for ref

3D mesh plot, defined by a variety of contexts

=for usage

 mesh3d piddle(3,x,y), {OPTIONS}
 mesh3d [piddle,...], {OPTIONS}

=for example

Example:

 perldl> mesh3d [sqrt(rvals(zeroes(50,50))/2)]

 - mesh of surface

Note: a mesh is defined by two sets of lines at
right-angles (i.e. this is how is differs from
line3d).

See module documentation for more information on
contexts and options

=head2 lattice3d

=for ref

alias for mesh3d

=head2 points3d

=for ref

3D points plot, defined by a variety of contexts

=for usage

 points3d piddle(3), {OPTIONS}
 points3d [piddle,...], {OPTIONS}

=for example

Example:

 perldl> points3d [sqrt(rvals(zeroes(50,50))/2)];
 - points on surface

See module documentation for more information on
contexts and options

=head2 imagrgb

=for ref

2D TrueColor Image plot

=for usage

 imagrgb piddle(3,x,y), {OPTIONS}
 imagrgb [piddle,...], {OPTIONS}

This would be used to plot an image, specifying
red, green and blue values at each point. Note:
contexts are very useful here as there are many
ways one might want to do this.

=for example

e.g.

 perldl> $a=sqrt(rvals(zeroes(50,50))/2)
 perldl> imagrgb [0.5*sin(8*$a)+0.5,0.5*cos(8*$a)+0.5,0.5*cos(4*$a)+0.5]

=head2 imagrgb3d

=for ref

2D TrueColor Image plot as an object inside a 3D space

=for usage

 imagrdb3d piddle(3,x,y), {OPTIONS}
 imagrdb3d [piddle,...], {OPTIONS}

The piddle gives the colors. The option allowed is Points,
which should give 4 3D coordinates for the corners of the polygon,
either as a piddle or as array ref.
The default is [[0,0,0],[1,0,0],[1,1,0],[0,1,0]].

=for example

e.g.

 perldl> imagrgb3d $colors, {Points => [[0,0,0],[1,0,0],[1,0,1],[0,0,1]]};
 - plot on XZ plane instead of XY.

=head2 grabpic3d

=for ref

Grab a 3D image from the screen.

=for usage

 $pic = grabpic3d();

The returned piddle has dimensions (3,$x,$y) and is of type float
(currently). XXX This should be altered later.

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
 while(1) {
	$c .= nextfunc($c);
	$o->data_changed();
	twiddle3d();		# animate one step, then return.
 }

=head2 twiddle3d

=for ref

Wait for the user to rotate the image in 3D space.

Let the user rotate the image in 3D space, either for one step
or until (s)he presses 'q', depending on the 'keeptwiddling3d'
setting. If 'keeptwiddling3d' is not set the routine returns
immediately and indicates that a 'q' event was received by
returning 1. If the only events received were mouse events,
returns 0.

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

	$a = new PDL::Graphics::TriD::Scale($x,$y,$z);

=head2 PDL::Graphics::TriD::LineStrip

This is just a line or a set of lines. The arguments are 3 1-or-more-D
piddles which describe the vertices of a continuous line and an 
optional color piddle (which is 1-D also and simply
defines the color between red and blue. This will probably change).

=head2 PDL::Graphics::TriD::Lines

This is just a line or a set of lines. The arguments are 3 1-or-more-D
piddles where each contiguous pair of vertices describe a line segment 
and an optional color piddle (which is 1-D also and simply
defines the color between red and blue. This will probably change).

=head2 PDL::Graphics::TriD::Image

This is a 2-dimensional RGB image consisting of colored
rectangles. With OpenGL, this is implemented by texturing so this should
be relatively memory and execution-time-friendly.

=head2 PDL::Graphics::TriD::Lattice

This is a 2-D set of points connected by lines in 3-space.
The constructor takes as arguments 3 2-dimensional piddles.

=head2 PDL::Graphics::TriD::Points

This is simply a set of points in 3-space. Takes as arguments
the x, y and z coordinates of the points as piddles.

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
# $PDL::Graphics::TriD::verbose
# $PDL::Graphics::TriD::keeptwiddling
# $PDL::Graphics::TriD::hold_on
# $PDL::Graphics::TriD::curgraph
# $PDL::Graphics::TriD::cur
# $PDL::Graphics::TriD::create_window_sub
# $PDL::Graphics::TriD::current_window
# 
# '

package PDL::Graphics::TriD::Basic;
package PDL::Graphics::TriD;

use PDL::Exporter;
use PDL::Core '';  # barf
use vars qw/@ISA @EXPORT_OK %EXPORT_TAGS/;
@ISA = qw/PDL::Exporter/;
@EXPORT_OK = qw/imag3d_ns imag3d line3d mesh3d lattice3d points3d
  describe3d imagrgb imagrgb3d hold3d release3d
  keeptwiddling3d nokeeptwiddling3d
  twiddle3d grabpic3d tridsettings/;
%EXPORT_TAGS = (Func=>[@EXPORT_OK]);

#use strict;
use PDL::Graphics::TriD::Object;
use PDL::Graphics::TriD::Window;
use PDL::Graphics::TriD::ViewPort;
use PDL::Graphics::TriD::Graph;
use PDL::Graphics::TriD::Quaternion;
use PDL::Graphics::TriD::Objects;
use PDL::Graphics::TriD::Rout;


# Then, see which display method are we using:

BEGIN {
	my $dev;
	$dev ||= $::PDL::Graphics::TriD::device; # First, take it from this variable.
	$dev ||= $::ENV{PDL_3D_DEVICE};

	if(!defined $dev) {
#		warn "Default PDL 3D device is GL (OpenGL):
#Set PDL_3D_DEVICE=GL in your environment in order not to see this warning.
#You must have OpenGL or Mesa installed and the PDL::Graphics::OpenGL extension
#compiled. Otherwise you will get strange warnings.";
		$dev = $^O =~ /win32/i ? "VRML" : "GL";
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
	$mod =~ s|::|//|g;
	print "dev = $dev mod=$mod\n" if($verbose);
 
	require "$mod.pm";
	$dv->import;
        my $verbose;

}


# currently only used by VRML backend
sub tridsettings {return $PDL::Graphics::TriD::Settings}

# Allowable forms:
# x(3,..)  [x(..),y(..),z(..)]
sub realcoords {
	my($type,$c) = @_;
	if(ref $c ne "ARRAY") {
		if($c->getdim(0) != 3) {
			barf "If one piddle given for coordinate, must be (3,...) or have default interpretation";
		}
		return $c ;
	}
	if(!ref $c->[0]) {$type = shift @$c}
	if($#$c < 0 || $#$c>2) {
		barf "Must have 1..3 array members for coordinates";
	}
	if($#$c == 0 and $type =~ /^SURF2D$/) {
		# surf2d -> this is z axis
		@$c = ($c->[0]->xvals,$c->[0]->yvals,$c->[0]);
	} elsif($#$c == 0 and $type eq "POLAR2D") {
		my $t = 6.283 * $c->[0]->xvals / ($c->[0]->getdim(0)-1);
		my $r = $c->[0]->yvals / ($c->[0]->getdim(1)-1);
		@$c = ($r * sin($t), $r * cos($t), $c->[0]);
	} elsif($#$c == 0 and $type eq "COLOR") {
		# color -> 1 piddle = grayscale
		@$c = ($c->[0], $c->[0], $c->[0]);
	} elsif($#$c == 0 and $type eq "LINE") {
		@$c = ($c->[0]->xvals, $c->[0], 0);
	} elsif($#$c == 1 and $type eq "LINE") {
		@$c = ($c->[0], $c->[1], $c->[0]->xvals);
	}
	# XXX
	if($#$c != 2) {
		barf("Must have 3 coordinates if no interpretation (here '$type')");
	}
	# allow a constant (either pdl or not) to be introduced in one dimension
        foreach(0..2){  
	  if(ref($c->[$_]) ne "PDL" or $c->[$_]->nelem==1){
	    $c->[$_] = $c->[$_]*(PDL->ones($c->[($_+1)%3]->dims));
	  }
	}
	my $g = PDL->null;
	&PDL::Graphics::TriD::Rout::combcoords(@$c,$g);
	$g->dump if $PDL::Graphics::TriD::verbose;
	return $g;
}

sub objplotcommand {
	my($object) = @_;
	my $win = PDL::Graphics::TriD::get_current_window();
	my $world = $win->world();
}

sub checkargs {
	if(ref $_[$#_] eq "HASH") {

	  print "enter checkargs \n";
		for([KeepTwiddling,\&keeptwiddling3d]) {
		  print "checkargs >$_<\n";
			if(defined $_[$#_]{$_->[0]}) {
				&{$_->[1]}(delete $_[$#_]{$_->[0]});
			}
		}
	}
}

*keeptwiddling3d = \&PDL::keeptwiddling3d;
sub PDL::keeptwiddling3d {
	$PDL::Graphics::TriD::keeptwiddling = (defined $_[0] ? $_[0] : 1);
}
*nokeeptwiddling3d = \&PDL::nokeeptwiddling3d;
sub PDL::nokeeptwiddling3d {
	$PDL::Graphics::TriD::keeptwiddling = 0 ;
}
keeptwiddling3d();
*twiddle3d = \&PDL::twiddle3d;
sub PDL::twiddle3d {
	twiddle_current();
}

sub graph_object {
	my($obj) = @_;
	if(!defined $obj or !ref $obj) {
		barf("Invalid object to TriD::graph_object");
	}
	my $g = get_new_graph();
	my $name = $g->add_dataseries($obj);
	$g->bind_default($name);
	$g->scalethings();
	print "ADDED TO GRAPH: '$name'\n" if $PDL::Graphics::TriD::verbose;

	twiddle_current();
	return $obj;
}

# Plotting routines that use the whole viewport

*describe3d=\&PDL::describe3d;
sub PDL::describe3d {
	require PDL::Graphics::TriD::TextObjects;
	my ($text) = @_;
	my $win = PDL::Graphics::TriD::get_current_window();
	my $imag = new PDL::Graphics::TriD::Description($text);
	$win->add_object($imag);
#	$win->twiddle();
}

*imagrgb=\&PDL::imagrgb;
sub PDL::imagrgb {
	require PDL::Graphics::TriD::Image;
	my (@data) = @_; &checkargs;
	my $win = PDL::Graphics::TriD::get_current_window();
	my $imag = new PDL::Graphics::TriD::Image(@data);

	$win->clear_viewports();

	$win->current_viewport()->add_object($imag);
	$win->twiddle();
}

# Plotting routines that use the 3D graph

# Call: line3d([$x,$y,$z],[$color]);
*line3d=\&PDL::line3d;
sub PDL::line3d { &checkargs;
	&graph_object(new PDL::Graphics::TriD::LineStrip(@_));
}

*contour3d=\&PDL::contour3d;
sub PDL::contour3d { 
#  &checkargs;
  require PDL::Graphics::TriD::Contours;
  &graph_object(new PDL::Graphics::TriD::Contours(@_));
}

# XXX Should enable different positioning...
*imagrgb3d=\&PDL::imagrgb3d;
sub PDL::imagrgb3d { &checkargs;
	require PDL::Graphics::TriD::Image;
	&graph_object(new PDL::Graphics::TriD::Image(@_));
}

*imag3d_ns=\&PDL::imag3d_ns;
sub PDL::imag3d_ns {  &checkargs;
	&graph_object(new PDL::Graphics::TriD::SLattice(@_));
}

*imag3d=\&PDL::imag3d;
sub PDL::imag3d { &checkargs;
	&graph_object(new PDL::Graphics::TriD::SLattice_S(@_));
}

*mesh3d=\&PDL::mesh3d;
*lattice3d=\&PDL::mesh3d;
*PDL::lattice3d=\&PDL::mesh3d;
sub PDL::mesh3d { &checkargs;
	&graph_object(new PDL::Graphics::TriD::Lattice(@_));
}

*points3d=\&PDL::points3d;
sub PDL::points3d { &checkargs;
	&graph_object(new PDL::Graphics::TriD::Points(@_));
}

*grabpic3d=\&PDL::grabpic3d;
sub PDL::grabpic3d {
	my $win = PDL::Graphics::TriD::get_current_window();
	barf "backend doesn't support grabing the rendered scene"
	  unless $win->can('read_picture');
	my $pic = $win->read_picture();
	return ($pic->float) / 255;
}

$PDL::Graphics::TriD::hold_on = 0;

sub PDL::hold3d {$PDL::Graphics::TriD::hold_on =(!defined $_[0] ? 1 : $_[0]);}
sub PDL::release3d {$PDL::Graphics::TriD::hold_on = 0;}

*hold3d=\&PDL::hold3d;
*release3d=\&PDL::release3d;

sub get_new_graph {
	my $win = PDL::Graphics::TriD::get_current_window();

	my $g = get_current_graph($win);

	if(!$PDL::Graphics::TriD::hold_on) {
		$g->clear_data();
		$win->clear_viewport();
	}
	$g->default_axes();

	$win->add_object($g);
	return $g;
}

sub get_current_graph {
   my $win = shift;
	my $g = $win->current_viewport()->graph();

	if(!defined $g) {
		$g = new PDL::Graphics::TriD::Graph();
		$g->default_axes();
		$win->current_viewport()->graph($g);
	}
	return $g;
}


# $PDL::Graphics::TriD::cur = {};
# $PDL::Graphics::TriD::create_window_sub = undef;
sub get_current_window {
  my $opts = shift @_;
  my $win = $PDL::Graphics::TriD::cur;

  if(!defined $win) {
	 if(!$PDL::Graphics::TriD::create_window_sub) {
		barf("PDL::Graphics::TriD must be used with a display mechanism: for example PDL::Graphics::TriD::GL!\n");
	 }
	 $win = new PDL::Graphics::TriD::Window($opts);

	 $win->set_material(new PDL::Graphics::TriD::Material);
	 $PDL::Graphics::TriD::current_window = $win;
	 $PDL::Graphics::TriD::cur = $win
  }
  return $PDL::Graphics::TriD::current_window;
}

# Get the current graphbox
sub get_current_graph {
	my $graph = $PDL::Graphics::TriD::curgraph;
	if(!defined $graph) {
		$graph = new PDL::Graphics::TriD::Graph();
		$graph->default_axes();
		$PDL::Graphics::TriD::curgraph = $graph;
	}
	return $graph;
}

sub twiddle_current {
	my $win = get_current_window();
	$win->twiddle();
}

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


###################################
#
#
package PDL::Graphics::TriD::OneTransformation;
use fields qw/Args/;

sub new {
  my($type,@args) = @_;
  my $this = bless [\%{"$type\::FIELDS"}], $type;

  $this->{Args} = [@args];
  return $this;
}

package PDL::Graphics::TriD::Scale;
use base qw/PDL::Graphics::TriD::OneTransformation/;

package PDL::Graphics::TriD::Translation;
use base qw/PDL::Graphics::TriD::OneTransformation/;


package PDL::Graphics::TriD::Transformation;
use base qw/PDL::Graphics::TriD::Object/;

#sub new {
#	my($type) = @_;
#	bless {},$type;
#}

sub add_transformation {
	my($this,$trans) = @_;
	push @{$this->{Transforms}},$trans;
}



=head1 BUGS

Not enough is there yet.

=head1 AUTHOR

Copyright (C) 1997 Tuomas J. Lukka (lukka@husc.harvard.edu). Documentation
contributions from Karl Glazebrook (kgb@aaoepp.aao.gov.au).
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.


=cut
