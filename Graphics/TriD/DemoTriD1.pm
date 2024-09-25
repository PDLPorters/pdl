# Copyright (C) 1998 Tuomas J. Lukka.
# All rights reserved, except redistribution
# with PDL under the PDL License permitted.

package PDL::Demos::TriD1;

use PDL::Graphics::TriD;
use Carp;
require File::Spec;

my @f = qw(PDL IO STL owl.stl);
our $owlfile = undef;
foreach my $path ( @INC ) {
    my $file = File::Spec->catfile( $path, @f );
    if ( -f $file ) { $owlfile = $file; last; }
}
confess "Unable to find owl.stl within the perl libraries.\n"
  unless defined $owlfile;

sub info {('3d', '3d demo (requires TriD with OpenGL or Mesa)')}
sub init {'
use PDL::Graphics::TriD;
'}

my @demo = (
[comment => q|
	Welcome to a short tour of the capabilities of
	PDL::Graphics::TriD.

	Press 'q' in the graphics window for the next screen.
	Rotate the image by pressing mouse button one and
	dragging in the graphics window.
	Zoom in/out by pressing MB3 and drag up/down.
	Note that a standalone TriD script must start with

		use PDL;
		use PDL::Graphics::TriD;

	to work properly.
|],

[actnw => q|
	# See if we had a 3D window open already
	$|.__PACKAGE__.q|::we_opened = !defined $PDL::Graphics::TriD::current_window;
	$vertices = pdl([ [0,0,-1], [1,0,-1], [0.5,1,-1], [0.5,0.5,0] ]);
	$faceidx = pdl([ [0,2,1], [0,1,3], [0,3,2], [1,2,3] ]);
	# show the vertex and face normal vectors on a triangular grid
	trigrid3d($vertices,$faceidx,{ShowNormals=>1});
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	# Show a PDL logo
	require PDL::Graphics::TriD::Logo;
	$vertices = $PDL::Graphics::TriD::Logo::POINTS;
	$faceidx = $PDL::Graphics::TriD::Logo::FACES;
	$rotate_m = pdl [1,0,0],[0,0,1],[0,-1,0]; # top towards X axis
	$c22 = cos(PI/8); $s22 = sin(PI/8);
	$rot22 = pdl [$c22,$s22,0],[-$s22,$c22,0],[0,0,1]; # +22deg about vert
	$vertices = ($vertices x $rotate_m x $rot22);
	trigrid3d($vertices,$faceidx);
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	# Show an owl loaded from an STL file
	use PDL::IO::STL;
	($vertices, $faceidx) = rstl $|.__PACKAGE__.q|::owlfile;
	trigrid3d($vertices,$faceidx);
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	# Number of subdivisions for lines / surfaces.
	$size = 25;
	$cz = (xvals zeroes $size+1) / $size;  # interval 0..1
	$cx = sin($cz*12.6);	# Corkscrew
	$cy = cos($cz*12.6);
	line3d [$cx,$cy,$cz];	# Draw a line
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	$r = sin($cz*6.3)/2 + 0.5;
	$g = cos($cz*6.3)/2 + 0.5;
	$b = $cz;
	line3d [$cx,$cy,$cz], [$r,$g,$b];    # Draw a colored line
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	$x = (xvals zeroes $size+1,$size+1) / $size;
	$y = (yvals zeroes $size+1,$size+1) / $size;
	$z = 0.5 + 0.5 * (sin($x*6.3) * sin($y*6.3)) ** 3;   # Bumps
	line3d [$x,$y,$z];	# Draw several lines
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	$r = $x;
	$g = $y;
	$b = $z;
	line3d [$x,$y,$z], [$r,$g,$b];	# Draw several colored lines
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	lattice3d [$x,$y,$z], [$r,$g,$b];  # Draw a colored lattice
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	points3d [$x,$y,$z], [$r,$g,$b], {PointSize=>4};  # Draw colored points
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	imag3d_ns [$x,$y,$z], [$r,$g,$b];  # Draw a colored surface
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	imag3d [$x,$y,$z]; # Draw a shaded surface
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	# Draw a shaded, coloured, unsmoothed (default is on) surface
	imag3d [$x,$y,$z], [$x,$y,$z], { Smooth => 0 };
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	# Draw a shaded, coloured, smoothed (the default) surface
	imag3d [$x,$y,$z], [$x,$y,$z];
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	hold3d();	# Leave the previous object in..
	imag3d_ns [$x,$y,$z+1], [$r,$g,$b];
			# ...and draw a colored surface on top of it...
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	lattice3d [$x,$y,$z-1], [$r,$g,$b];
			# ...and draw a colored lattice under it...
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	contour3d($z, [$x,$y,$z-1]);
			# ...and draw contours on that
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	nokeeptwiddling3d(); # Don't wait for user while drawing
	for(-2,-1,0,1,2) {
		line3d [$cx,$cy,$cz+$_]; # ... and corkscrews...
	}
	keeptwiddling3d();   # Do wait for user while drawing...
	twiddle3d();	     # and actually, wait right now.
	release3d();
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	# The reason for the [] around $x,$y,$z:
	# 1. You can give all the coordinates and colors in one ndarray.
	$c = (zeroes 3,$size+1) / $size;
	$coords =
		sin((3+3*xvals $c)*yvals $c);
	$colors = $coords;
	line3d $coords, $colors;        # Draw a curved line, colored
					# (this works also for lattices, etc.)
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	# 2. You can use defaults inside the brackets:
	lattice3d [$z], [$r];  # Note: no $x, $y, and $r is greyscale
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	# 3. You can plot in certain other systems as defaults
	imag3d_ns [POLAR2D, $z], [$r, $g, $b];  # Draw the familiar
						# bumpy surface in polar
						# coordinates
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	# Show graph-evolver
	use PDL::Graphics::TriD::MathGraph;
	use PDL::Graphics::TriD::Labels;
	my @coords = ([0,-1,0], [-1,-1,-2], [3,5,2],
	    [2,1,-3], [1,3,1], [1,1,2]);
	my $from = PDL->pdl(indx, [0,1,2,3,4,4,4,5,5,5]);
	my $to =   PDL->pdl(indx, [1,2,3,1,0,2,3,0,1,2]);
	my @names = map '  '.join(",",@$_), @coords;
	my $e = PDL::GraphEvolver->new(pdl(@coords));
	$e->set_links($from,$to,PDL->ones(1));
	my $c = $e->getcoords;
	my $graph = PDL::Graphics::TriD::get_new_graph(); # also clears
	hold3d();
	nokeeptwiddling3d();
	PDL::Graphics::TriD::graph_object(
	  my $lab = PDL::Graphics::TriD::Labels->new($c,{Strings => \@names}));
	PDL::Graphics::TriD::graph_object(
		my $lin = PDL::Graphics::TriD::MathGraph->new(
		$c, {From => $from, To => $to}));
	PDL::Graphics::TriD::graph_object(
		my $sph = PDL::Graphics::TriD::Spheres->new($c));
	my $ind = 0;
	while(1) {
		$e->step();
		if(++$ind%2 == 0) {
			$_->data_changed for $lab, $lin, $sph;
			$graph->scalethings() if (($ind % 200) == 0 or 1);
			last if twiddle3d();
		}
	}
	keeptwiddling3d();
	release3d();
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	# Show the world!
	use PDL::Transform::Cartography;
	eval { # this is in case no NetPBM, i.e. can't load Earth images
	  $shape = earth_shape();
	  $floats = t_raster2float()->apply($shape->dummy(0,3));
	  $radius = $floats->slice('(2)'); # r g b all same
	  $radius *= float((6377.09863 - 6370.69873) / 6371);
	  $radius += float(6370.69873 / 6371);
	  $e_i = earth_image('day');
	  $earth = t_raster2float()->apply($e_i->mv(2,0));
	  $earth = $earth->append($radius->dummy(0));
	  $shrink = 2.5; # how much to shrink by
	  $new_x = int($e_i->dim(0) / $shrink);
	  $earth2 = $earth->mv(0,2)->match([$new_x,int($new_x/2),6])->mv(2,0); # shrink
	  ($lonlatrad, $rgb) = map $earth2->slice($_), pdl(0,1,5), '2:4';
	  $sph = t_spherical()->inverse()->apply($lonlatrad);
	  imag3d($sph, $rgb, {Lines=>0});
	};
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	return if !defined $earth; # failed to load
	# Show off the world!
	# The Earth's radius doesn't proportionally vary much,
	# but let's exaggerate it to prove we have height information!
	$lonlatrad->slice('2') -= 1;
	$lonlatrad->slice('2') *= 100;
	$lonlatrad->slice('2') += 1;
	$sph = t_spherical()->inverse()->apply($lonlatrad);
	imag3d($sph, $rgb, {Lines=>0});
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	return if !defined $earth; # failed to load
	# Now zoom in over Europe
	($lats, $lons) = map $_ / 180, pdl(22, 72), pdl(-10, 40);
	$lats = indx(($lats + 0.5) * $earth->dim(2));
	$lons = indx((($lons + 1) / 2) * $earth->dim(1));
	$earth3 = $earth->slice(':', map [$_->list], $lons, $lats)->sever; # zoom
	($lonlatrad, $rgb) = map $earth3->slice($_), pdl(0,1,5), '2:4';
	$lonlatrad->slice('2') -= 1;
	$lonlatrad->slice('2') *= 50; # exaggerate terrain but less
	$lonlatrad->slice('2') += 1;
	$sph = t_spherical()->inverse()->apply($lonlatrad);
	imag3d($sph, $rgb, {Lines=>0});
	# [press 'q' in the graphics window when done]
|],

[actnw => q|
	# '3d2' contains some of the more special constructions available
	# in the PDL::Graphics::TriD modules.

	# close 3D window if we opened it
	close3d() if $|.__PACKAGE__.q|::we_opened;
|],
);

sub demo { @demo }

1;
