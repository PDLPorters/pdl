=head1 NAME

PDL::Transform::Cartography - Useful cartographic projections

=head1 SYNOPSIS

 # make a Mercator map of Earth
 use PDL::Transform::Cartography;
 $a = earth_coast();
 $a = graticule(10,2)->glue(1,$a);
 $t = t_mercator;
 $w = pgwin(xs);
 $w->lines($t->apply($a)->clean_lines());

=head1 DESCRIPTION

PDL::Transform::Cartography includes a variety of useful cartographic
and observing projections (mappings of the surface of a sphere),
including reprojected observer coordinates.  See L<PDL::Transform>
for more information about image transforms in general.

Cartographic transformations are used for projecting not just
terrestrial maps, but also any nearly spherical surface including the
Sun, the Celestial sphere, various moons and planets, distant stars,
etc.  They also are useful for interpreting scientific images, which
are themselves generally projections of a sphere onto a flat focal
plane (e.g. the L<t_gnomonic|/t_gnomonic> projection).

Unless otherwise noted, all the transformations in this file convert
from (theta,phi) coordinates on the unit sphere (e.g. (lon,lat) on a
planet or (RA,dec) on the celestial sphere) into some sort of
projected coordinates, and have inverse transformations that convert
back to (theta,phi).  This is equivalent to working from the
equidistant cylindrical (or L<"plate caree"|/t_caree>) projection, if
you are a cartography wonk.

The projected coordinates are generally in units of body radii
(radians), so that multiplying the output by the scale of the map
yields physical units that are correct wherever the scale is correct
for that projection.  For example, areas should be correct everywhere
in the authalic projections; and linear scales are correct along
meridians in the equidistant projections and along the standard
parallels in all the projections.

The transformations that are authalic (equal-area), conformal
(equal-angle), azimuthal (circularly symmetric), or perspective (true
perspective on a focal plane from some viewpoint) are marked.  The
first two categories are mutually exclusive for all but the 
L<"unit sphere"|/t_unit_sphere> 3-D projection.

Extra dimensions tacked on to each point to be transformed are, in
general, ignored.  That is so that you can add on an extra index
to keep track of pen color.  For example, L<earth_coast|/earth_coast>
returns a 3x<n> piddle containing (lon, lat, pen) at each list location.
Transforming the vector list retains the pen value as the first index
after the dimensional directions.

=head1 GENERAL NOTES ON CARTOGRAPHY

Unless otherwise noted, the transformations and miscellaneous
information in this section are taken from Snyder & Voxland 1989: "An
Album of Map Projections", US Geological Survey Professional Paper
1453, US Printing Office (Denver); and from Snyder 1987: "Map
Projections - A Working Manual", US Geological Survey Professional
Paper 1395, US Printing Office (Denver, USA).  You can obtain your own
copy of both by contacting the U.S. Geological Survey, Federal Center,
Box 25425, Denver, CO 80225 USA.

The mathematics of cartography have a long history, and the details
are far trickier than the broad overview.  For terrestrial (and, in
general, planetary) cartography, the best reference datum is not a
sphere but an oblate ellipsoid due to centrifugal force from the
planet's rotation.  Furthermore, because all rocky planets, including
Earth, have randomly placed mass concentrations that affect the
gravitational field, the reference gravitational isosurface (sea level
on Earth) is even more complex than an ellipsoid and, in general,
different ellipsoids have been used for different locations at the
same time and for the same location at different times.

The transformations in this package use a spherical datum and hence
include global distortion at about the 0.5% level for terrestrial maps
(Earth's oblateness is ~1/300).  This is roughly equal to the
dimensional precision of physical maps printed on paper (due to
stretching and warping of the paper) but is significant at larger
scales (e.g. for regional maps).  If you need more precision than
that, you will want to implement and use the ellipsoidal
transformations from Snyder 1987 or another reference work on geodesy.
A good name for that package would be C<...::Cartography::Geodetic>.

=head1 GENERAL NOTES ON PERSPECTIVE AND SCIENTIFIC IMAGES

Cartographic transformations are useful for interpretation of
scientific images, as all cameras produce projections of the celestial
sphere onto the focal plane of the camera.  A simple (single-element)
optical system with a planar focal plane generates
L<gnomonic|/t_gnomonic> images -- that is to say, gnomonic projections
of a portion of the celestial sphere near the paraxial direction.
This is the projection that most consumer grade cameras produce.

Magnification in an optical system changes the angle of incidence
of the rays on the focal plane for a given angle of incidence at the
aperture.  For example, a 10x telescope with a 2 degree field of view
exhibits the same gnomonic distortion as a simple optical system with 
a 20 degree field of view.  Wide-angle optics typically have magnification
less than 1 ('fisheye lenses'), reducing the gnomonic distortion 
considerably but introducing L<"equidistant azimuthal"|/t_az_eqd> distortion --
there's no such thing as a free lunch!

Because many solar-system objects are spherical,
PDL::Transform::Cartography includes perspective projections for
producing maps of spherical bodies from perspective views.  Those
projections are L<"t_vertical"|/t_vertical> and
L<"t_perspective"|/t_perspective>.  They map between (lat,lon) on the
spherical body and planar projected coordinates at the viewpoint.  
L<"t_vertical"|/t_vertical> is the vertical perspective projection 
given by Snyder, but L<"t_perspective"|/t_perspective> is a fully
general perspective projection that also handles magnification correction.

=head1 TRANSVERSE & OBLIQUE PROJECTIONS; STANDARD OPTIONS

Oblique projections rotate the sphere (and graticule) to an arbitrary
angle before generating the projection; transverse projections rotate
the sphere exactly 90 degrees before generating the projection.  

Most of the projections accept the following standard options,
useful for making transverse and oblique projection maps.  

=over 3

=item o, origin, Origin [default (0,0,0)]

The origin of the oblique map coordinate system, in (old-theta, old-phi) 
coordinates.

=item r, roll, Roll [default 0.0]

The roll angle of the sphere about the origin, measured CW from (N = up)
for reasonable values of phi and CW from (S = up) for unreasonable
values of phi.  This is equivalent to observer roll angle CCW from the
same direction.

=item u, unit, Unit [default 'degree']

This is the name of the angular unit to use in the lon/lat coordinate system.

=item b, B

The "B" angle of the body -- used for extraterrestrial maps.  Setting
this parameter is exactly equivalent to setting the phi component
of the origin, and in fact overrides it.

=item l,L

The longitude of the central meridian as observed -- used for extraterrestrial
maps.  Setting this parameter is exactly equivalent to setting the theta
component of the origin, and in fact overrides it.

=item p,P

The "P" (or position) angle of the body -- used for extraterrestrial maps.
This parameter is a synonym for the roll angle, above.

=item bad, Bad, missing, Missing [default nan]

This is the value that missing points get.  Mainly useful for the
inverse transforms.  (This should work fine if set to BAD, if you have
bad-value support compiled in).  The default nan is asin(1.2), calculated
at load time.

=back

=head1 EXAMPLES

Draw a Mercator map of the world on-screen:

   $w = pgwin(xs);
   $w->lines(earth_coast->apply(t_mercator)->clean_lines);

Here, C<earth_coast()> returns a 3xn piddle containing (lon, lat, pen) 
values for the included world coastal outline; C<t_mercator> converts
the values to projected Mercator coordinates, and C<clean_lines> breaks
lines that cross the 180th meridian.

Draw a Mercator map of the world, with lon/lat at 10 degree intervals:

   $w = pgwin(xs)
   $a = earth_coast()->glue(1,graticule(10,1));
   $w->lines($a->apply(t_mercator)->clean_lines);

This works just the same as the first example, except that a map graticule
has been applied with interline spacing of 10 degrees lon/lat and 
inter-vertex spacing of 1 degree (so that each meridian contains 181 points,
and each parallel contains 361 points).

=head1 NOTES

Currently angular conversions are rather simpleminded.  A list of
common conversions is present in the main constructor, which inserts a
conversion constant to radians into the {params} field of the new
transform.  Something like Math::Convert::Units should be used instead
to generate the conversion constant. 

A cleaner higher-level interface is probably needed (see the examples);
for example, earth_coast could return a graticule if asked, instead of 
needing one to be glued on.

The class structure is somewhat messy because of the varying needs of
the different transformations.  PDL::Transform::Cartography is a base
class that interprets the origin options and sets up the basic
machinery of the Transform.  The conic projections have their
own subclass, PDL::Transform::Conic, that interprets the standard
parallels.  Since the cylindrical and azimuthal projections are pretty
simple, they are not subclassed.

The perl 5.6.1 compiler is quite slow at adding new classes to the
structure, so it does not makes sense to subclass new transformations
merely for the sake of pedantry.

=head1 AUTHOR

Copyright 2002, Craig DeForest (deforest@boulder.swri.edu).  This
module may be modified and distributed under the same terms as PDL
itself.  The module comes with NO WARRANTY.

The included digital world map is derived from the 1987 CIA World Map,
translated to ASCII in 1988 by Joe Dellinger (geojoe@freeusp.org) and
simplified in 1995 by Kirk Johnson (tuna@indra.com) for the program
XEarth.  The map comes with NO WARRANTY.  An ASCII version of the map,
and a sample PDL function to read it, may be found in the Demos
subdirectory of the PDL source distribution.

=head1 FUNCTIONS

The module exports both transform constructors ('t_<foo>') and some
auxiliary functions (no leading 't_').

=cut

# Import PDL::Transform into the calling package -- the cartography
# stuff isn't much use without it.
use PDL::Transform;

package PDL::Transform::Cartography;
@ISA = ( 'Exporter','PDL::Transform' );
$VERSION = "0.5";

BEGIN {
  use Exporter ();
  @EXPORT_OK = qw(graticule earth_image earth_coast clean_lines t_unit_sphere t_orthographic t_rot_sphere t_caree t_mercator t_sin_lat t_sinusoidal t_conic t_albers t_lambert t_stereographic t_gnomonic t_az_eqd t_az_eqa t_vertical t_perspective t_hammer t_aitoff);
  @EXPORT = @EXPORT_OK;
  %EXPORT_TAGS = (Func=>[@EXPORT_OK]);
}

use PDL;
use PDL::Transform;
use PDL::MatrixOps;
use PDL::NiceSlice;

use Carp;


##############################
# Steal _opt from PDL::Transform.
*PDL::Transform::Cartography::_opt = \&PDL::Transform::_opt;
use overload '""' => \&_strval;

use strict;

our $PI = $PDL::Transform::PI;
our $DEG2RAD = $PDL::Transform::DEG2RAD;
our $RAD2DEG = $PDL::Transform::RAD2DEG;

sub _strval {
  my($me) = shift;
  $me->stringify();
}

######################################################################

=head2 graticule

=for usage

   $lonlatp     = graticule(<grid-spacing>,<line-segment-size>);   

=for ref

(Cartography) PDL constructor - generate a lat/lon grid.

Returns a grid of meridians and parallels as a list of vectors suitable
for sending to L<PDL::Graphics::PGPLOT::Window::lines|/lines> for plotting.
The grid is in degrees in (theta, phi) coordinates -- this is (E lon, N lat) 
for terrestrial grids or (RA, dec) for celestial ones.  You must then 
transform the graticule in the same way that you transform the map.

You can attach the graticule to a vector map using the syntax:

    $out = graticule(10,2)->glue(1,$map);

In array context you get back a 2-element list containing a piddle of
the (theta,phi) pairs and a piddle of the pen values (1 or 0) suitable for
calling L<PDL::Graphics::PGPLOT::Window::lines|/lines>.  In scalar context
the two elements are combined into a single piddle.

The pen values associated with the graticule are negative, which will
cause L<PDL::Graphics::PGPLOT::Window::lines|/lines> to plot them as
hairlines.

=cut

sub graticule {
    my $grid = shift;
    my $step = shift;
    $grid = 10 unless defined($grid);
    $step = $grid/2 unless defined($step);
    
    my $par_siz = ((floor(180/$grid)) * (floor(360/$step) + 1))->at(0);
    my $mer_siz = ((floor(360/$grid)+ 1) * floor(180/$step + 1))->at(0);
    
    my $out = zeroes(2,$par_siz + $mer_siz);
    my $p = ones($par_siz + $mer_siz);
    
    # First do parallels
    $out->((0),0:$par_siz - 1) .= 
	(xvals($par_siz) * $step) % (360+$step);
    $out->((1),0:$par_siz - 1) .=
	$grid * (1+floor((xvals($par_siz) * $step) / (360+$step)));
    $p->(  ( xvals((floor(180/$grid)-1)->at(0)) + 1) * (floor(360/$step)+1)-1)
	.= 0;

    # Next, meridians.  Duplicate the 180 degree meridian, for cleaner
    # default plots.
    $out->((0),$par_siz:-1) .=
	$grid * floor(xvals($mer_siz) * $step / (180+$step));
    $out->((0),-floor(180/$step + 1):-1) -= 1e-3;

    $out->((1),$par_siz:-1) .=
	(xvals($mer_siz)*$step) % (180+$step) ;
      
    $p->( $par_siz +
	  ( xvals(floor(360/$grid + 1)->at(0))+1) * (floor(180/$step)+1)-1
	  ) .=  0;


    $out->(0) -= 180;
    $out->(1) -= 90;

    $out->append(-$p->dummy(0,1));
}

=head2 earth_coast

=for usage

  $a = earth_coast()

=for ref

(Cartography) PDL constructor - coastline map of Earth

Returns a vector coastline map based on the 1987 CIA World Coastline
database (see author information).  The vector coastline data are in
plate caree format so they can be converted to other projections via
the L<apply|/apply> method and cartographic transforms,
and are suitable for plotting with the
L<lines|/lines> method in the PGPLOT 
output library:  the first dimension is (X,Y,pen) with breaks having 
a pen value of 0 and hairlines having negative pen values.  The second 
dimension threads over all the points in the data set.  

The vector map includes lines that pass through the antipodean
meridian, so if you want to plot it without reprojecting, you should
run it through L<clean_lines|/clean_lines> first:

    $w = pgwin();
    $w->lines(earth_coast->clean_lines);     # plot plate caree map of world
    $w->lines(earth_coast->apply(t_gnomonic))# plot gnomonic map of world

C<earth_coast> is just a quick-and-dirty way of loading the file
"earth_coast.vec.fits" that is part of the normal installation tree.

=cut

sub earth_coast {
    my $fn = "PDL/Transform/Cartography/earth_coast.vec.fits";
    local $_;
    foreach(@INC) {
	my $file = "$_/$fn";
	return rfits($file) if(-e $file);
    }
    barf("earth_coast: $fn not found in \@INC.\n");
}

=head2 earth_image

=for usage

 $rgb = earth_image()

=for ref

(Cartography) PDL constructor - RGB pixel map of Earth 

Returns an RGB image of Earth based on data from the MODIS instrument
on the NASA EOS/Terra satellite.  (You can get a full-resolution 
image from http://earthobservatory.nasa.gov/Newsroom/BlueMarble/).
The image is a plate caree map, so you can convert it to other
projections via the L<map|/map> method and cartographic
transforms.

This is just a quick-and-dirty way of loading the earth-image files that
are distributed along with PDL.

=cut

sub earth_image {
  my($nd) = shift;
  my $f;
  my $dir = "PDL/Transform/Cartography/earth_";
  $f = ($nd =~ m/^n/i) ? "${dir}night.jpg" : "${dir}day.jpg";
  
  local $_;
  my $im;
  foreach(@INC) {
    my $file = "$_/$f";
    $im = rpic($file)->mv(0,-1) if(-e $file);
  }

  barf("earth_image: $f not found in \@INC.\n")
    unless defined($im);

  my $h = $im->fhdr;

  $h->{SIMPLE} = 'T';
  $h->{NAXIS} = 3;
  $h->{NAXIS1}=2048;	      $h->{CRPIX1}=1024.5;    $h->{CRVAL1}=0;
  $h->{NAXIS2}=1024;	      $h->{CRPIX2}=512.5;     $h->{CRVAL2}=0;
  $h->{NAXIS3}=3,             $h->{CRPIX3}=1;         $h->{CRVAL3}=0;
  $h->{CTYPE1}='Longitude';   $h->{CUNIT1}='degrees'; $h->{CDELT1}=180/1024.0;
  $h->{CTYPE2}='Latitude';    $h->{CUNIT2}='degrees'; $h->{CDELT2}=180/1024.0;
  $h->{CTYPE3}='RGB';         $h->{CUNIT3}='index';   $h->{CDELT3}=1.0;
  $h->{COMMENT}='Plate Caree Projection';
  $h->{HISTORY}='PDL Distribution Image, derived from NASA/MODIS data',
  
  $im->hdrcpy(1);
  $im;
}

=head2 clean_lines

=for usage

 $a = clean_lines(t_mercator->apply(scalar(earth_coast())));
 $a = clean_lines($line_pen, [threshold]);
 $a = $lines->clean_lines;

=for ref

(Cartography) PDL method - remove projection irregularities

C<clean_lines> massages vector data to remove jumps due to singularities
in the transform.

In the first (scalar) form, C<$line_pen> contains both (X,Y) points and pen 
values suitable to be fed to L<lines|/lines>;
in the second (list) form, C<$lines> contains the (X,Y) points and C<$pen>
contains the pen values.  

C<clean_lines> assumes that all the outline polylines are local --
that is to say, there are no large jumps.  Any jumps larger than a
threshold size are broken by setting the appropriate pen values to 0.

The C<threshold> parameter sets the relative size of the largest jump, relative
to the map range (as determined by a min/max operation).  The default size is
0.1.

NOTES

This almost never catches stuff near the apex of cylindrical maps,
because the anomalous vectors get arbitrarily small.  This could be 
improved somewhat by looking at individual runs of the pen and using
a relative length scale that is calibrated to the rest of each run.
it is probably not worth the computational overhead.

=cut

*PDL::clean_lines = \&clean_lines;
sub clean_lines {
    my($lines) = shift;
    my($a) = shift;
    my($b) = shift;
    my($l,$p,$th);

    $th = 0.1;

    if(defined($b)) {
	# separate case with thresh
	$l = $lines;
	$p = $a->is_inplace?$a:$a->copy;
	$th = $b;
    } else {
	if(!defined($a)) {
	    # duplex case no thresh
	    $l = $lines->(0:1);
	    $p = $lines->is_inplace ? $lines->((2)) : $lines->((2))->sever;
	} elsif(UNIVERSAL::isa($a,'PDL') && 
		$lines->((0))->nelem == $a->nelem) {
	    # Separate case no thresh
	    $l = $lines;
	    $p = $a->is_inplace ? $a : $a->copy;;
	} else {
	    # duplex case with thresh
	    $l = $lines->(0:1);
	    $p = $lines->is_inplace ? $lines->((2)) : $lines->((2))->sever;
	    $th = $a;
	}
    }

    my $pok = ($p != 0 & isfinite($p));
    # Kludge to work around minmax bug (nans confuse it!)
    my($l0) = $l->((0));
    my($x0,$x1) = $l0->where(isfinite($l0) & $pok)->minmax;
    my($xth) = abs($x1-$x0) * $th;

    my($l1) = $l->((1));
    ($x0,$x1) = $l1->where(isfinite($l1) & $pok)->minmax;
    my($yth) = abs($x1-$x0) * $th;

    my $diff = abs($l->(:,1:-1) - $l->(:,0:-2));

    $diff->where(!isfinite($diff)) .= 2*($xth + $yth); 
    $p->where(($diff->((0)) > $xth) | ($diff->((1)) > $yth)) .= 0;
    if(wantarray){
	return($l,$p);
    } else {
	return $l->append($p->dummy(0,1));
    }
}    



######################################################################

###
# Units parser
# Get unit, return conversion factor to radii, or undef if no match found.
#
sub _uconv{
  ###
  # Replace this with a more general units resolver call!
  ###
  local($_) = shift;
  my($silent) =shift;
  my($a) = 
    ( m/^deg/i    ? $DEG2RAD :
      m/^arcmin/i ? $DEG2RAD / 60 :
      m/^arcsec/i ? $DEG2RAD / 3600 :
      m/^hour/i   ? $DEG2RAD * 15 :    # Right ascension
      m/^min/i    ? $DEG2RAD * 15/60 : # Right ascension
      m/^microrad/i ? 1e-6 :
      m/^millirad/i ? 1e-3 :
      m/^rad(ian)?s?$/i ? 1.0 :
      undef
      );
  print STDERR "Cartography: unrecognized unit '$_'\n"    
    if( (!defined $a) && !$silent && ($PDL::debug || $PDL::verbose));
  $a;
}

###
#
# Cartography general constructor -- called by the individual map
# constructors.  Not underscored because it's certainly OK to call from
# outside -- but the *last* argument is the name of the transform.
#
# The options list is put into the {options} field of the newly constructed
# Transform -- fastidious subclass constructors will want to delete it before 
# returning.
#


sub _new{new('PDL::Transform::Cartography',@_);} # not exported
sub new {
    my($class) = shift;
    my($name) = pop;

    my($o) = $_[0];
    $o = {@_}
      unless(ref $o eq 'HASH');

    my($me) = PDL::Transform::new($class);
    $me->{idim} = $me->{odim} = 2;
    $me->{name} = $name;
 
    ####
    # Parse origin and units arguments
    # 
    my $or = _opt($o,['o','origin','Origin'],zeroes(2));
    if($or->nelem != 2) {
	croak("PDL::Transform::Cartography: origin must have 2 elements\n");
    }

    my($l) = _opt($o,['l','L']);
    my($b) = _opt($o,['b','B']);

    $or->(0) .= pdl($l) if defined($l);
    $or->(1) .= pdl($b) if defined($b);

    my $roll = pdl(_opt($o,['r','roll','Roll','P'],0));
    my $unit = _opt($o,['u','unit','Unit'],'degrees');

    $me->{params}->{conv} = my $conv = _uconv($unit);
    $me->{params}->{u} = $unit;

    $me->{itype} = ['latitude','longitude'];
    $me->{iunit} = [$me->{params}->{u},$me->{params}->{u}];

    $me->{params}->{o} = $or * $conv;
    $me->{params}->{roll} = $roll * $conv;

    $me->{params}->{bad} = _opt($o,['b','bad','Bad','missing','Missing'],
			      asin(pdl(1.2)));

    # Get the standard parallel (in general there's only one; the conics
    # have two but that's handled by _c_new)
    $me->{params}->{std} = pdl(_opt($me->{options},
			 ['s','std','standard','Standard'],
			 0))->at(0) * $me->{params}->{conv};

    $me->{options} = $o;
    $me;
}

# Compose self with t_rot_sphere if necessary -- useful for 
# finishing off the transformations that accept the origin and roll 
# options.
sub PDL::Transform::Cartography::_finish {
  my($me) = shift;
  if( ($me->{params}->{o}->(0) != 0) || 
      ($me->{params}->{o}->(1) != 0) ||
      ($me->{params}->{roll} != 0) 
      ) {
      my $out = t_compose($me,t_rot_sphere($me->{options}));
      $out->{itype} = $me->{itype};
      $out->{iunit} = $me->{iunit};
      $out->{otype} = $me->{otype};
      $out->{ounit} = $me->{ounit};
      $out->{odim} = 2;
      $out->{idim} = 2;
      return $out;
    } 
  return $me;
}


######################################################################

=head2 t_unit_sphere

=for usage

  $t = t_unit_sphere(<options>);

=for ref

(Cartography) 3-D globe projection (conformal; authalic)

This is similar to the inverse of L<t_spherical|/t_spherical>, but the
inverse transform projects 3-D coordinates onto the unit sphere,
yielding only a 2-D (lon/lat) output.  Similarly, the forward
transform deprojects 2-D (lon/lat) coordinates onto the surface of a
unit sphere.

The cartesian system has its Z axis pointing through the pole of the 
(lon,lat) system, and its X axis pointing through the equator at the 
prime meridian.

Unit sphere mapping is unusual in that it is both conformal and authalic.
That is possible because it properly embeds the sphere in 3-space, as a 
notional globe.

This is handy as an intermediate step in lots of transforms, as 
Cartesian 3-space is cleaner to work with than spherical 2-space.

Higher dimensional indices are preserved, so that "rider" indices (such as 
pen value) are propagated.

There is no oblique transform for t_unit_sphere, largely because 
it's so easy to rotate the output using t_linear once it's out into 
Cartesian space.  In fact, the other projections implement oblique
transforms by L<wrapping|/t_wrap> L<t_linear|/t_linear> with
L<t_unit_sphere|/t_unit_sphere>.

OPTIONS:

=over 3

=item radius, Radius (default 1.0) 

The radius of the sphere, for the inverse transform.  (Radius is ignored
in the forward transform).  Defaults to 1.0 so that the resulting Cartesian
coordinates are in units of "body radii".

=back

=cut
    
sub t_unit_sphere {
  my($me) = _new(@_,'Unit Sphere Projection'); 
  $me->{odim} = 3;

  $me->{params}->{otype} = ['X','Y','Z'];
  $me->{params}->{ounit} = ['body radii','body radii','body radii'];

  $me->{params}->{r} = pdl(_opt($me->{options},
				['r','radius','Radius'],
				1.0)
			   )->at(0);
				    
  
  $me->{func} = sub {
    my($d,$o) = @_;
    my(@dims) = $d->dims;
    $dims[0] ++;
    my $out = zeroes(@dims);
    
    my($thetaphi) = ((defined $o->{conv} && $o->{conv} != 1.0) ? 
		     $d->(0:1) * $o->{conv} : $d->(0:1)
		     );

    my $th = $thetaphi->((0));
    my $ph = $thetaphi->((1));

    # use x as a holding tank for the cos-phi multiplier
    $out->((0)) .= $o->{r} * cos($ph) ;
    $out->((1)) .= $out->((0)) * sin($th);
    $out->((0)) *= cos($th);

    $out->((2)) .= $o->{r} * sin($ph);

    if($d->dim(0) > 2) {
	$out->(3:-1) .= $d->(2:-1);
    }

    $out;

  };

  $me->{inv} = sub {
    my($d,$o) = @_;
	
    my($d0,$d1,$d2) = ($d->((0)),$d->((1)),$d->((2)));
    my($r) = sqrt(($d->(0:2)*$d->(0:2))->sumover);
    my(@dims) = $d->dims;
    $dims[0]--;
    my($out) = zeroes(@dims);
    
    $out->((0)) .= atan2($d1,$d0);
    $out->((1)) .= asin($d2/$r);

    if($d->dim(0) > 3) {
	$out->(2:-1) .= $d->(3:-1);
    }

    $out->(0:1) /= $o->{conv}
      if(defined $o->{conv} && $o->{conv} != 1.0);

    $out;
  };

    
  $me;
}

######################################################################

=head2 t_rot_sphere

=for usage

    $t = t_rot_sphere({origin=>[<theta>,<phi>],roll=>[<roll>]});

=for ref

(Cartography) Generate oblique projections

You feed in the origin in (theta,phi) and a roll angle, and you get back 
out (theta', phi') coordinates.  This is useful for making oblique or 
transverse projections:  just compose t_rot_sphere with your favorite
projection and you get an oblique one.

Most of the projections automagically compose themselves with t_rot_sphere
if you feed in an origin or roll angle.

t_rot_sphere converts the base plate caree projection (straight lon, straight
lat) to a Cassini projection.

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=back

=cut

# helper routine for making the rotation matrix
sub _rotmat {
  my($th,$ph,$r) = @_;
  
  pdl( [ cos($th) ,  -sin($th),    0  ],   # apply theta
       [ sin($th) ,   cos($th),    0  ],
       [  0,          0,           1  ] )
    x
    pdl( [ cos($ph),    0,  -sin($ph)  ], # apply phi
	 [ 0,           1,    0       ],
	 [ sin($ph),   0,  cos($ph)  ] )
    x
    pdl( [ 1,         0 ,       0      ], # apply roll last
	 [ 0,    cos($r),   -sin($r)   ], 
	 [ 0,    sin($r),    cos($r)   ])
    ;
}

sub t_rot_sphere {
    my($me) = _new(@_,'Spherical rotation');


    my($th,$ph) = $me->{params}->{o}->list;
    my($r) = $me->{params}->{roll}->at(0);

    my($rotmat) = _rotmat($th,$ph,$r);

    my $out =  t_wrap( t_linear(m=>$rotmat, d=>3), t_unit_sphere());
    $out->{itype} = $me->{itype};
    $out->{iunit} = $me->{iunit};
    $out->{otype} = ['rotated longitude','rotated latitude'];
    $out->{ounit} = $me->{iunit};

    $out;
}


######################################################################

=head2 t_orthographic

=for usage

    $t = t_orthographic(<options>);

=for ref

(Cartography) Ortho. projection (azimuthal; perspective)

This is a perspective view as seen from infinite distance.  You
can specify the sub-viewer point in (lon,lat) coordinates, and a rotation
angle of the map CW from (north=up).  This is equivalent to specify
viewer roll angle CCW from (north=up).

t_orthographic is a convenience interface to t_unit_sphere -- it is implemented
as a composition of a t_unit_sphere call, a rotation, and a slice.

[*] In the default case where the near hemisphere is mapped, the
inverse exists.  There is no single inverse for the whole-sphere case,
so the inverse transform superimposes everything on a single
hemisphere.  If you want an invertible 3-D transform, you want
L<t_unit_sphere|/t_unit_sphere>.

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=item m, mask, Mask, h, hemisphere, Hemisphere [default 'near']

The hemisphere to keep in the projection (see L<PDL::Transform::Cartography>).

=back 

NOTES

Alone of the various projections, this one does not use
L<t_rot_sphere|/t_rot_sphere> to handle the standard options, because
the cartesian coordinates of the rotated sphere are already correctly
projected -- t_rot_sphere would put them back into (theta', phi')
coordinates.

=cut

sub t_orthographic {
    my($me) = _new(@_,'Orthographic Projection');
    
    $me->{otype} = ['projected X','projected Y'];
    $me->{ounit} = ['body radii','body radii'];

    my $m= _opt($me->{options},
		['m','mask','Mask','h','hemi','hemisphere','Hemisphere'],
		1);
    if($m=~m/^b/i) {
	$me->{params}->{m} = 0;
    } elsif($m=~m/^n/i) {
	$me->{params}->{m} = 1;
    } elsif($m=~m/^f/i) {
	$me->{params}->{m} = 2;
    } else {
	$me->{params}->{m} = $m;
    }

    my $origin= $me->{params}->{o} * $RAD2DEG;
    my $roll = $me->{params}->{roll} * $RAD2DEG;

    $me->{params}->{t_int} = 
	t_compose(
		  t_linear(rot=>[90 - $origin->at(1),
				 0,
				 90 + $origin->at(0)],
			   d=>3),
		  t_unit_sphere(u=>$me->{params}->{u})
		  );
    
    $me->{params}->{t_int} = 
	t_compose(
		  t_linear(rot=>[0,0,$roll->at(0)],d=>3),
		  $me->{params}->{t_int}
		  )
	    if($roll->at(0));

    $me->{name} = "orthographic";

    $me->{idim} = 2;
    $me->{odim} = 2;

    $me->{func} = sub {
	my ($d,$o) = @_ ;
	my ($out) = $o->{t_int}->apply($d);
	if($o->{m}) {
	    my $idx;
	    $idx = whichND($out->((2)) < 0) 
		if($o->{m} == 1);
	    $idx = whichND($out->((2)) > 0)
		if($o->{m} == 2);
	    if(defined $idx && ref $idx eq 'PDL' && $idx->nelem){
	      $out->((0))->range($idx) .= $o->{bad};
	      $out->((1))->range($idx) .= $o->{bad};
	    }
	}

	my($d0) = $out->dim(0);

	# Remove the Z direction
	($d0 > 3) ? $out->(pdl(0,1,3..$d0-1)) : $out->(0:1);

    };

    # This is slow to run, quick to code -- could be made better by
    # having its own 2-d inverse instead of calling the internal one.
    $me->{inv} = sub {
	my($d,$o) = @_;
	my($d1) = $d->(0:1);
	my(@dims) = $d->dims;
	$dims[0]++;
	my($out) = zeroes(@dims);
	$out->(0:1) .= $d1;
	$out->(3:-1) .= $d->(2:-1) 
	    if($dims[0] > 3);

	$out->((2)) .= sqrt(1 - ($d1*$d1)->sumover);
	$out->((2)) *= -1 if($o->{m} == 2);

	$o->{t_int}->invert($out);
    };

    $me;
}

######################################################################

=head2 t_caree

=for usage

    $t = t_caree(<options>);

=for ref

(Cartography) Plate Caree projection (cylindrical; equidistant)

This is the simple Plate Caree projection -- also called a "lat/lon plot".
The horizontal axis is theta; the vertical axis is phi.  This is a no-op
if the angular unit is radians; it is a simple scale otherwise. 

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=item s, std, standard, Standard (default 0)

The standard parallel where the transformation is conformal.  Conformality
is achieved by shrinking of the horizontal scale to match the 
vertical scale (which is correct everywhere).

=back

=cut

@PDL::Transform::Cartography::Caree::ISA = ('PDL::Transform::Cartography');

sub t_caree {
    my($me) = _new(@_,'Plate Caree Projection');
    my $p = $me->{params};

    $me->{otype} = ['projected longitude','latitude'];
    $me->{ounit} = ['proj. body radii','body radii'];

    $p->{stretch} = cos($p->{std});

    $me->{func} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;
	$out->(0:1) *= $o->{conv};
	$out->(0) *= $p->{stretch};
	$out;
    };
    
    $me->{inv} = sub {
	my($d,$o)= @_;
	my($out) = $d->is_inplace ? $d : $d->copy;
	$out->(0:1) /= $o->{conv};
	$out->(0) /= $p->{stretch};
	$out;
    };
    
    $me->_finish;
}

######################################################################

=head2 t_mercator

=for usage

    $t = t_mercator(<options>);

=for ref

(Cartography) Mercator projection (cylindrical; conformal)

This is perhaps the most famous of all map projections: meridians are mapped
to parallel vertical lines and parallels are unevenly spaced horizontal lines.
The poles are shifted to +/- infinity.  The output values are in units of 
globe-radii for easy conversion to kilometers; hence the horizontal extent
is -pi to pi.

You can get oblique Mercator projections by specifying the C<origin> or
C<roll> options; this is implemented via L<t_rot_sphere|/t_rot_sphere>.

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=item c, clip, Clip (default 75 [degrees])

The north/south clipping boundary of the transformation.  Because the poles are
displaced to infinity, many applications require a clipping boundary.  The
value is in whatever angular unit you set with the standard 'units' option.
The default roughly matches interesting landforms on Earth.
For no clipping at all, set b=>0.  For asymmetric clipping, use a 2-element
list ref or piddle.

=item s, std, Standard (default 0)

This is the parallel at which the map has correct scale.  The scale
is also correct at the parallel of opposite sign.  

=back

=cut


@PDL::Transform::Cartography::Mercator::ISA = ('PDL::Transform::Cartography');

sub t_mercator {
    my($me) = _new(@_,'Mercator Projection');
    my $p = $me->{params};

# This is a lot of shenanigans just to get the clip parallels, but what the
# heck -- it's not a hot spot and it saves copying the input data (for 
# nondestructive clipping).
    $p->{c} = _opt($me->{options},
		   ['c','clip','Clip'],
		   undef);
    if(defined($p->{c})) {
	$p->{c} = pdl($p->{c});
	$p->{c} *= $p->{conv};
    } else {
	$p->{c} = pdl($DEG2RAD * 75);
    }
    $p->{c} = abs($p->{c}) * pdl(-1,1) if($p->{c}->nelem == 1);
    $p->{c} = log(tan(($p->{c}/2) + $PI/4));       
    $p->{c} = [$p->{c}->list];

    $p->{std} = pdl(_opt($me->{options},
			 ['s','std','standard','Standard'],
			 0))->at(0) * $p->{conv};

    if($p->{std} == 0) {
      $me->{otype} = ['longitude','tan latitude'];
      $me->{ounit} = ['radians',' '];
    } else {
      $me->{otype} = ['proj. longitude','proj. tan latitude'];
      $me->{ounit} = ['radians',' '];
    }

    $p->{stretch} = cos($p->{std});

    $me->{func} = sub {
	my($d,$o) = @_;

	my($out) = $d->is_inplace ? $d : $d->copy;

	$out->(0:1) *= $o->{conv};

	$out->((1)) .= log(tan($out->((1))/2 + $PI/4));
	$out->((1)) .= $out->((1))->clip(@{$o->{c}})
	    unless($o->{c}->[0] == $o->{c}->[1]);

	$out->(0:1) *= $o->{stretch};
			    
	$out;
    };

    $me->{inv} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace? $d : $d->copy;

	$out->(0:1) /= $o->{stretch};
	$out->((1)) .= (atan(exp($out->((1)))) - $PI/4)*2;
	$out->(0:1) /= $o->{conv};

	$out;
    };

    $me->_finish;
}    

######################################################################

=head2 t_sin_lat

=for usage

    $t = t_sin_lat(<options>);

=for ref

(Cartography) Cyl. equal-area projection (cyl.; authalic)

This projection is commonly used in solar Carrington plots; but not much
for terrestrial mapping.

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=item s,std, Standard (default 0)

This is the parallel at which the map is conformal.  It is also conformal
at the parallel of opposite sign.  The conformality is achieved by matched
vertical stretching and horizontal squishing (to achieve constant area).

=back

=cut

@PDL::Transform::Cartography::SinLat::ISA = ('PDL::Transform::Cartography');
sub t_sin_lat {
    my($me) = _new(@_,"Sine-Latitude Projection");

    $me->{params}->{std} = pdl(_opt($me->{options},
				['s','std','standard','Standard'],
				0))->at(0) * $me->{params}->{conv};

    if($me->{params}->{std} == 0) {
      $me->{otype} = ['longitude','sin latitude'];
      $me->{ounit} = ['radians',' ']; # nonzero but blank!
    } else {
      $me->{otype} = ['proj. longitude','proj. sin latitude'];
      $me->{ounit} = ['radians',' '];
    }

    $me->{params}->{stretch} = sqrt(cos($me->{params}->{std}));

    $me->{func} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;

	$out->(0:1) *= $me->{params}->{conv};
	$out->((1)) .= sin($out->((1))) / $o->{stretch};
	$out->((0)) *= $o->{stretch};
	$out;
    };

    $me->{inv} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;
	$out->((1)) .= asin($out->((1)) * $o->{stretch});
	$out->((0)) /= $o->{stretch};
	$out->(0:1) /= $me->{params}->{conv};
	$out;
    };

    $me->_finish;
}

######################################################################

=head2 t_sinusoidal

=for usage

    $t = t_sinusoidal(<options>);

=for ref

(Cartography) Sinusoidal projection (authalic)

Sinusoidal projection preserves the latitude scale but scales
longitude according to sin(lat); in this respect it is the companion to
L<t_sin_lat|/t_sin_lat>, which is also authalic but preserves the
longitude scale instead.  

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=back

=cut

sub t_sinusoidal {
  my($me) = _new(@_,"Sinusoidal Projection");
  $me->{otype} = ['longitude','latitude'];
  $me->{ounit} = [' ','radians'];
  
  $me->{func} = sub {
    my($d,$o) = @_;
    my($out) = $d->is_inplace ? $d : $d->copy;
    $out->(0:1) *= $o->{conv};

    $out->((0)) *= cos($out->((1)));
    $out;
  };

  $me->{inv} = sub {
    my($d,$o) = @_;
    my($out) = $d->is_inplace ? $d : $d->copy;
    my($x) = $out->((0));
    my($y) = $out->((1));
    
    $x /= cos($out->((1)));

    my($rej) = ( (abs($x)>$PI) | (abs($y)>($PI/2)) )->flat;
    $x->flat->($rej) .= $o->{bad};
    $y->flat->($rej) .= $o->{bad};
    
    $out->(0:1) /= $o->{conv};
    $out;
  };

  $me->_finish;
}
   




######################################################################
#
# Conic projections are subclassed for easier stringification and
# parsing of the standard parallels.  The constructor gets copied
# into the current package for ease of hackage.
#
# This is a little kludgy -- it's intended for direct calling
# rather than method calling, and it puts its own class name on the
# front of the argument list.  But, hey, it works...
#
@PDL::Transform::Cartography::Conic::ISA = ('PDL::Transform::Cartography');
sub _c_new {
    my($def_std) = pop;
    my($me) = new('PDL::Transform::Cartography::Conic',@_); 

    my($p) = $me->{params};
    $p->{std} = _opt($me->{options},['s','std','standard','Standard'],
		     $def_std);
    $p->{std} = pdl($p->{std}) * $me->{params}->{conv};
    $p->{std} = pdl([$PI/2 * ($p->{std}<0 ? -1 : 1), $p->{std}->at(0)])
	if($p->{std}->nelem == 1);
    
    $me->{params}->{cylindrical} = 1
	if(approx($p->{std}->(0),-$p->{std}->(1)));

    $me;
}

sub PDL::Transform::Cartography::Conic::stringify {
    my($me) = shift;
    my($out) = $me->SUPER::stringify;

    $out .= sprintf("\tStd parallels: %6.2f,%6.2f %s\n",
		    $me->{params}->{std}->at(0) / $me->{params}->{conv}, 
		    $me->{params}->{std}->at(1) / $me->{params}->{conv}, 
		    $me->{params}->{u});
    $out;
}

######################################################################

=head2 t_conic

=for usage

    $t = t_conic(<options>)

=for ref

(Cartography) Simple conic projection (conic; equidistant)

This is the simplest conic projection, with parallels mapped to
equidistant concentric circles.  It is neither authalic nor conformal.
This transformation is also referred to as the "Modified Transverse
Mercator" projection in several maps of Alaska published by the USGS;
and the American State of New Mexico re-invented the projection in
1936 for an official map of that State.

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=item s, std, Standard (default 29.5, 45.5)

The locations of the standard parallel(s) (where the cone intersects
the surface of the sphere).  If you specify only one then the other is
taken to be the nearest pole.  If you specify both of them to be one
pole then you get an equidistant azimuthal map.  If you specify both
of them to be opposite and equidistant from the equator you get a
Plate Caree projection.

=back

=cut

sub t_conic {
    my($me) = _c_new(@_,"Simple Conic Projection",[29.5,45.5]);

    my($p) = $me->{params};

    if($p->{cylindrical}) {
	print STDERR "Simple conic: degenerate case; using Plate Caree\n"
	    if($PDL::verbose);
	return t_caree($me->{options});
    }

    $p->{n} = ((cos($p->{std}->((0))) - cos($p->{std}->((1)))) 
	       /
	       ($p->{std}->((1)) - $p->{std}->((0))));

    $p->{G} = cos($p->{std}->((0)))/$p->{n} + $p->{std}->((0));

    $me->{otype} = ['Conic X','Conic Y'];
    $me->{ounit} = ['Proj. radians','Proj. radians'];

    $me->{func} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;

	my($rho) = $o->{G} - $d->((1)) * $o->{conv};
	my($theta) = $o->{n} * $d->((0)) * $o->{conv};
	
	$out->((0)) .= $rho * sin($theta);
	$out->((1)) .= $o->{G} - $rho * cos($theta);

	$out;
    };

    $me->{inv} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;

	my($x) = $d->((0));
	my($y) = $o->{G} - $d->((1));
	my($rho) = sqrt($x*$x + $y*$y);
	$rho *= -1 if($o->{n}<0);

	my($theta) = ($o->{n} < 0) ? atan2(-$x,-$y) : atan2($x,$y);
    
	$out->((1)) .= $o->{G} - $rho;
	$out->((1))->where(($out->((1)) < -$PI/2) | ($out->((1)) > $PI/2))
	    .= $o->{bad};

	$out->((0)) .= $theta / $o->{n};
	$out->((0))->where(($out->((0)) < -$PI) | ($out->((0)) > $PI/2))
	    .= $o->{bad};

	$out->(0:1) /= $o->{conv};

	$out;
    };

    $me->_finish;
}




######################################################################

=head2 t_albers

=for usage
    
    $t = t_albers(<options>)

=for ref

(Cartography) Albers conic projection (conic; authalic)

This is the standard projection used by the US Geological Survey for
sectionals of the 50 contiguous United States of America.  

The projection reduces to the Lambert equal-area conic (infrequently
used and not to be confused with the Lambert conformal conic,
L<t_lambert|/t_lambert>!)  if the pole is used as one of the two
standard parallels.

Notionally, this is a conic projection onto a cone that intersects the
sphere at the two standard parallels; it works best when the two parallels
straddle the region of interest.

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=item s, std, standard, Standard (default (29.5,45.5))

The locations of the standard parallel(s).  If you specify only one then 
the other is taken to be the nearest pole and a Lambert Equal-Area Conic
map results.  If you specify both standard parallels to be the same pole,
then the projection reduces to the Lambert Azimuthal Equal-Area map as
aq special case.  (Note that L<t_lambert|/t_lambert> is Lambert's
Conformal Conic, the most commonly used of Lambert's projections.)

The default values for the standard parallels are those chosen by Adams
for maps of the lower 48 US states: (29.5,45.5).  The USGS recommends
(55,65) for maps of Alaska and (8,18) for maps of Hawaii -- these latter
are chosen to also include the Canal Zone and Philippine Islands farther
south, which is why both of those parallels are south of the Hawaiian islands.

The transformation reduces to the cylindrical equal-area (sin-lat)
transformation in the case where the standard parallels are opposite and
equidistant from the equator, and in fact this is implemented by a call to
t_sin_lat.

=back

=cut

sub t_albers  {
    my($me) = _c_new(@_,"Albers Equal-Area Conic Projection",[29.5,45.5]);

    my($p) = $me->{params};

    if($p->{cylindrical}) {
	print STDERR "Albers equal-area conic: degenerate case; using equal-area cylindrical\n"
	    if($PDL::verbose);
	return t_sin_lat($me->{options});
    }

    $p->{n} = sin($p->{std})->sumover / 2;
    $p->{C} = (cos($p->{std}->((1)))*cos($p->{std}->((1))) + 
		     2 * $p->{n} * sin($p->{std}->((1))) );
    $p->{rho0} = sqrt($p->{C}) / $p->{n}; 

    $me->{otype} = ['Conic X','Conic Y'];
    $me->{ounit} = ['Proj. radians','Proj. radians'];

    $me->{func} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;

	my($rho) = sqrt( $o->{C} - 2 * $o->{n} * sin($d->((1)) * $o->{conv}) ) / $o->{n};
	my($theta) = $o->{n} * $d->((0)) * $o->{conv};

	$out->((0)) .= $rho * sin($theta);
	$out->((1)) .= $p->{rho0} - $rho * cos($theta);
	$out;
    };

    $me->{inv} = sub {
	my($d,$o) = @_;

	my($out) = $d->is_inplace ? $d : $d->copy;

	my($x) = $d->((0));
	my($y) = $o->{rho0} - $d->((1));

	my($theta) = ($o->{n} < 0) ? atan2 -$x,-$y : atan2 $x, $y;
	my($rho) = sqrt( $x*$x + $y*$y ) * $o->{n};

	$out->((1)) .= asin( ( $o->{C} - ( $rho * $rho ) ) / (2 * $o->{n}) );

	$out->((0)) .= $theta / $o->{n};
	$out->((0))->where($out->((0))>$PI | $out->((0))<-$PI) .= $o->{bad};

	$out->(0:1) /= $o->{conv};

	$out;
    };

    $me->_finish;
}

######################################################################

=head2 t_lambert

=for usage
    
    $t = t_lambert(<options>);

=for ref

(Cartography) Lambert conic projection (conic; conformal)

Lambert conformal conic projection is widely used in aeronautical
charts and state base maps published by the USA's FAA and USGS.  It's
especially useful for mid-latitude charts.  In particular, straight lines
approximate (but are not exactly) great circle routes of up to ~2 radians.

The default standard parallels are 33 and 45 to match the USGS state
1:500,000 base maps of the United States.  At scales of 1:500,000 and
larger, discrepancies between the spherical and ellipsoidal projections
become important; use care with this projection on spheres.

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=item s, std, standard, Standard (default (33,45))

The locations of the standard parallel(s) for the conic projection.
The transform reduces to the Mercator projection in the case where the
standard parallels are opposite and equidistant from the equator, and
in fact this is implemented by a call to t_mercator.

=item c, clip, Clip (default [-75,75])

Because the transform is conformal, the distant pole is displaced to
infinity.  Many applications require a clipping boundary.  The value
is in whatever angular unit you set with the standard 'unit' option.
For consistency with L<t_mercator|/t_mercator>, clipping works the same
way even though in most cases only one pole needs it.  Set this to 0
for no clipping at all.

=back

=cut

sub t_lambert {
    my($me)= _c_new(@_,"Lambert Conformal Conic Projection",[33,45]);
    my($p) = $me->{params};

    if($p->{cylindrical}){
	print STDERR "Lambert conformal conic: std parallels are opposite & equal; using Mercator\n" 
	    if($PDL::verbose);
	return t_mercator($me->{options});
    }
    
    # Find clipping parallels
    $p->{c} = _opt($me->{options},['c','clip','Clip'],undef);
    if(defined($p->{c})) {
	$p->{c} = pdl($p->{c});
    } else {
	$p->{c} = pdl(-75,75);
    }
    $p->{c} = abs($p->{c}) * pdl(-1,1) if($p->{c}->nelem == 1);
    $p->{c} = [$p->{c}->list];

    # Prefrobnicate
    if(approx($p->{std}->((0)),$p->{std}->((1)))) {
	$p->{n} = sin($p->{std}->((0)));
    } else {
	$p->{n} = (log(cos($p->{std}->((0)))/cos($p->{std}->((1)))) 
		   / 
		   log( tan( $PI/4 + $p->{std}->((1))/2 ) 
			/ 
			tan( $PI/4 + $p->{std}->((0))/2 ) 
			)
		   );
    }

    $p->{F} = ( cos($p->{std}->((0))) 
		*
		( tan( $PI/4 + $p->{std}->((0))/2 ) ** $p->{n} ) / $p->{n}
		);

    $p->{rho0} = $p->{F};

    $me->{otype} = ['Conic X','Conic Y'];
    $me->{ounit} = ['Proj. radians','Proj. radians'];

    $me->{func} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;

	my($cl) = ( ($o->{c}->[0] == $o->{c}->[1]) ? 
		    $d->((1))*$o->{conv} : 
		    ($d->((1))->clip(@{$o->{c}}) * $o->{conv})
		    );

	my($rho) = $o->{F} / ( tan($PI/4 + ($cl)/2 ) ** $o->{n} );
	my($theta) = $o->{n} * $d->((0)) * $o->{conv};

	$out->((0)) .= $rho * sin($theta);
	$out->((1)) .= $o->{rho0} - $rho * cos($theta);
	$out;
    };

    $me->{inv} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;
	
	my($x) = $d->((0));
	my($y) = $o->{rho0} - $d->((1));

	my($rho) = sqrt($x * $x + $y * $y);
	$rho *= -1 if($o->{n} < 0);
	my($theta) = ($o->{n} < 0) ? atan2(-$x,-$y):(atan2 $x,$y);


	$out->((0)) .= $theta / $o->{n};
	$out->((0))->where($out->((0)) > $PI | $out->((0)) < -$PI) .= $o->{bad};


	$out->((1)) .= 2 * atan(($o->{F}/$rho)**(1.0/$o->{n})) - $PI/2;
	$out->((1))->where($out->((1)) > $PI/2 | $out->((1)) < -$PI/2) .= $o->{bad};

	$out->(0:1) /= $o->{conv};

	$out;
    };


    $me->_finish;
}

######################################################################

=head2 t_stereographic

=for usage

    $t = t_stereographic(<options>);

=for ref

(Cartography) Stereographic projection (az.; conf.; persp.)

The stereographic projection is a true perspective (planar) projection
from a point on the spherical surface opposite the origin of the map.  

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=item c, clip, Clip (default 120)

This is the angular distance from the center to the edge of the 
projected map.  The default 120 degrees gives you most of the opposite
hemisphere but avoids the hugely distorted part near the antipodes.

=back

=cut

sub t_stereographic {
    my($me) = _new(@_,"Stereographic Projection");
    
    $me->{params}->{k0} = 1.0;
    $me->{params}->{c} = _opt($me->{options},
			      ['c','clip','Clip'],
			      120) * $me->{params}->{conv};

    $me->{otype} = ['Stereo X','Stereo Y'];
    $me->{ounit} = ['Proj. body radii','Proj. radians'];

    $me->{func} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;

	my($th,$ph) = ($out->((0)) * $o->{conv},
		       $out->((1)) * $o->{conv});

	my($cph) = cos($ph); # gets re-used 
	my($k) = 2 * $o->{k0} / (1 + cos($th) * $cph);
	$out->((0)) .= $k * $cph * sin($th);
	$out->((1)) .= $k * sin($ph);

	my($cl0) = 2*$o->{k0} / (1 + cos($o->{c}));
	$out->((0))->where($k>$cl0) .= $o->{bad};
	$out->((1))->where($k>$cl0) .= $o->{bad};
	$out;
    };

    $me->{inv} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;
	
	my($x) = $d->((0));
	my($y) = $d->((1));
	my($rho) = sqrt($x*$x + $y*$y);
	my($c) = 2 * atan2($rho,2*$o->{k0});
	
	$out->((0)) .= atan2($x * sin($c), $rho * cos($c));
	$out->((1)) .= asin($y * sin($c) / $rho);
	
	$out ->(0:1) /= $o->{conv};
	$out;
    };

    $me->_finish;
}
     
######################################################################

=head2 t_gnomonic

=for usage

    $t = t_gnomonic(<options>);

=for ref

(Cartography) Gnomonic (focal-plane) projection (az.; persp.)

The gnomonic projection projects a hemisphere onto a tangent plane.
It is useful in cartography for the property that straight lines are
great circles; and it is useful in scientific imaging because 
it is the projection generated by a simple optical system with a flat
focal plane.

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=item c, clip, Clip (default 75)

This is the angular distance from the center to the edge of the 
projected map.  The default 75 degrees gives you most of the 
hemisphere but avoids the hugely distorted part near the horizon.

=back

=cut

sub t_gnomonic {
    my($me) = _new(@_,"Gnomonic Projection");
    
    $me->{params}->{k0} = 1.0;  # Useful for standard parallel (TBD: add one)

    $me->{params}->{c} = pdl(_opt($me->{options},
			      ['c','clip','Clip'],
			      75) * $me->{params}->{conv});

    $me->{params}->{c} .= $me->{params}->{c}->clip(undef,(90-1e-6)*$me->{params}->{conv});

    $me->{otype} = ['Tangent-plane X','Tangent-plane Y'];
    $me->{ounit} = ['Proj. radians','Proj. radians'];

    $me->{func} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;

	my($th,$ph) = ($out->((0)) * $o->{conv},
		       $out->((1)) * $o->{conv});

	my($cph) = cos($ph); # gets re-used 

	my($k) = $o->{k0} / (cos($th) * $cph);
	my($cl0) = $o->{k0} / (cos($o->{c}));

	$out->((0)) .= $k * $cph * sin($th);
	$out->((1)) .= $k * sin($ph);

	my $idx = whichND($k > $cl0  | ($k < 0) | (!isfinite($k)));
	if($idx->nelem) {
	  $out->((0))->range($idx) .= $o->{bad};
	  $out->((1))->range($idx) .= $o->{bad};
	}

	$out;
    };

    $me->{inv} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;

	my($x) = $d->((0));
	my($y) = $d->((1));

	my($rho) = sqrt($x*$x + $y*$y);
	my($c) = atan($rho/$o->{k0});

	$out->((0)) .= atan2($x * sin($c), $rho * cos($c));
	$out->((1)) .= asin($y * sin($c) / $rho);

	my $idx = whichND($rho==0);

	if($idx->nelem) {
	  $out->((0))->range($idx) .= 0;
	  $out->((1))->range($idx) .= 0;
	}
	$out->(0:1) /= $o->{conv};
	$out;
    };

    $me->_finish;
}

######################################################################

=head2 t_az_eqd

=for usage

  $t = t_az_eqd(<options>);

=for ref 

(Cartography) Azimuthal equidistant projection (az.; equi.)

Basic azimuthal projection preserving length along radial lines from
the origin (meridians, in the original polar aspect).  Hence, both
azimuth and distance are correct for journeys beginning at the origin.

Applied to the celestial sphere, this is the projection made by
fisheye lenses; it is also the projection into which C<t_vertical>
puts perspective views.

The projected plane scale is normally taken to be planetary radii;
this is useful for cartographers but not so useful for scientific
observers.  Setting the 't=>1' option causes the output scale to shift
to camera angular coordinates (the angular unit is determined by the
standard 'Units' option; default is degrees).

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=item c, clip, Clip (default 180 degrees)

The largest angle relative to the origin.  Default is the whole sphere.

=back

=cut

sub t_az_eqd {
  my($me) = _new(@_,"Equidistant Azimuthal Projection");

  $me->{params}->{c} = pdl(_opt($me->{options},
				['c','clip','Clip'],
				180) * $me->{params}->{conv});

  $me->{otype} = ['X distance','Y distance'];
  $me->{ounit} = ['radians','radians'];

  $me->{func} = sub {
    my($d,$o) = @_;
    my($out) = $d->is_inplace ? $d : $d->copy;
    
    my($ph) = $d->((1)) * $o->{conv};
    my($th) = $d->((0)) * $o->{conv};

    my $cos_c = cos($ph) * cos($th);
    my $c = acos($cos_c);
    my $k = $c / sin($c);
    $k->where($c==0) .= 1;
    
    my($x,$y) = ($out->((0)), $out->((1)));

    $x .= $k * cos($ph) * sin($th);
    $y .= $k * sin($ph);

    my $idx = whichND($c > $o->{c});
    if($idx->nelem) {
      $x->range($idx) .= $o->{bad};
      $y->range($idx) .= $o->{bad};
    }

    $out;
  };

  $me->{inv} = sub {
    my($d,$o) = @_;
    my($out) = $d->is_inplace ? $d : $d->copy;
    my($x) = $d->((0));
    my($y) = $d->((1));

    my $rho = sqrt(($d->(0:1)*$d->(0:1))->sumover);
    # Order is important -- ((0)) overwrites $x if is_inplace!
    $out->((0)) .= atan2( $x * sin($rho), $rho * cos $rho );
    $out->((1)) .= asin( $y * sin($rho) / $rho );

    my $idx = whichND($rho == 0);
    if($idx->nelem) {
      $out->((0))->range($idx) .= 0;
      $out->((1))->range($idx) .= 0;
    }

    $out->(0:1) /= $o->{conv};

    $out;
  };

  $me->_finish;
}


######################################################################

=head2 t_az_eqa

=for usage

  $t = t_az_eqa(<options>);

=for ref

(Cartography) Azimuthal equal-area projection (az.; auth.)

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=item c, clip, Clip (default 180 degrees)

The largest angle relative to the origin.  Default is the whole sphere.

=back

=cut

sub t_az_eqa {
  my($me) = _new(@_,"Equal-Area Azimuthal Projection");
  
  $me->{params}->{c} = pdl(_opt($me->{options},
				['c','clip','Clip'],
				180) * $me->{params}->{conv});

  $me->{otype} = ['Azimuthal X','Azimuthal Y'];
  $me->{ounit} = ['Proj. radians','Proj. radians'];

  $me->{func} = sub {
    my($d,$o) = @_;
    my($out) = $d->is_inplace ? $d : $d->copy;
    
    my($ph) = $d->((1)) * $o->{conv};
    my($th) = $d->((0)) * $o->{conv};
				
    my($c) = acos(cos($ph) * cos($th));
    my($rho) = 2 * sin($c/2);
    my($k) = 1.0/cos($c/2);

    my($x,$y) = ($out->((0)),$out->((1)));
    $x .= $k * cos($ph) * sin($th);
    $y .= $k * sin($ph);

    my $idx = whichND($c > $o->{c});
    if($idx->nelem) {
      $x->range($idx) .= $o->{bad};
      $y->range($idx) .= $o->{bad};
    }

    $out;
  };

  $me->{inv} = sub {
    my($d,$o) = @_;
    my($out) = $d->is_inplace ? $d : $d->copy;

    my($x,$y) = ($d->((0)),$d->((1)));
    my($ph,$th) = ($out->((0)),$out->((1)));
    my($rho) = sqrt($x*$x + $y*$y);
    my($c) = 2 * asin($rho/2);

    $ph .= asin($d->((1)) * sin($c) / $rho);
    $th .= atan2($x * sin($c),$rho * cos($c));

    $ph /= $o->{conv};
    $th /= $o->{conv};
    
    $out;
  };

  $me->_finish;
}


######################################################################

=head2 t_aitoff 

=head2 t_hammer

=for ref

(Cartography) Hammer/Aitoff elliptical projection (az.; auth.)

The Hammer/Aitoff projection is often used to display the Celestial
sphere.  It is mathematically related to the Lambert Azimuthal Equal-Area
projection (L<t_az_eqa|/t_az_eqa>), and maps the sphere to an ellipse of unit 
eccentricity, with vertical radius sqrt(2) and horizontal radius of 
2 sqrt(2).

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=cut

*t_aitoff = \&t_hammer;

sub t_hammer {
  my($me) = _new(@_,"Hammer/Aitoff Projection");
  
  $me->{otype} = ['Longitude','Latitude'];
  $me->{ounit} = [' ',' '];
  $me->{odim} = 2;
  $me->{idim} = 2;

  $me->{func} = sub {
    my($d,$o) = @_;
    my($out) = $d->is_inplace ? $d : $d->copy;
    $out->(0:1) *= $o->{conv};
    my($th) = $out->((0));
    my($ph) = $out->((1));
    my($t) = sqrt( 2 / (1 + cos($ph) * cos($th/2)));
    $th .= 2 * $t * cos($ph) * sin($th/2);
    $ph .= $t * sin($ph);
    $out;
  }
  ;

  $me->{inv} = sub {
    my($d,$o) = @_;
    my($out) = $d->is_inplace ? $d : $d->copy;
    my($x) = $out->((0));
    my($y) = $out->((1));

    my($rej) = which(($x*$x/8 + $y*$y/2)->flat > 1);
    
    my($zz);
    my($z) = sqrt( $zz = (1 - $x*$x/16 - $y*$y/4) );
    $x .= 2 * atan( ($z * $x) / (4 * $zz - 2) );
    $y .= asin($y * $z);
    
    $out->(0:1) /= $o->{conv};

    $x->flat->($rej) .= $o->{bad};
    $y->flat->($rej) .= $o->{bad};

    $out;
  };

  $me->_finish;
}


######################################################################

=head2 t_vertical

=for usage

    $t = t_vertical(<options>);

=for ref

(Cartography) Vertical perspective projection (az.; persp.)

Vertical perspective projection is a generalization of L<gnomonic|/t_gnomonic>
and L<stereographic|/t_stereographic> projection, and a special case of 
L<perspective|/t_perspective> projection.  It is a projection from the 
sphere onto a focal plane at the camera location.  

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=item m, mask, Mask, h, hemisphere, Hemisphere [default 'near']

The hemisphere to keep in the projection (see L<PDL::Transform::Cartography>).

=item r0, R0, radius, d, dist, distance [default 2.0]

The altitude of the focal plane above the center of the sphere.  The default
places the point of view one radius above the surface.

=item t, telescope, Telescope, cam, Camera (default '')

If this is set, then the central scale is in telescope or camera 
angular units rather than in planetary radii.  The angular units are 
parsed as with the normal 'u' option for the lon/lat specification.
If you specify a non-string value (such as 1) then you get telescope-frame
radians, suitable for working on with other transformations.

=item f, fish, fisheye (default '')

If this is set then the output is in azimuthal equidistant coordinates
instead of in tangent-plane coordinates.  This is a convenience function
for '(t_az_eqd) x !(t_gnomonic) x (t_vertical)'.

=back

=cut

sub t_vertical {
    my($me) = _new(@_,'Vertical Perspective');
    my $p = $me->{params};
    
    my $m= _opt($me->{options},
		['m','mask','Mask','h','hemi','hemisphere','Hemisphere'],
		1);

    $me->{otype} = ['Perspective X','Perspective Y'];
    $me->{ounit} = ['Body radii','Body radii'];

    if($m=~m/^b/i) {
	$p->{m} = 0;
    } elsif($m=~m/^n/i) {
	$p->{m} = 1;
    } elsif($m=~m/^f/i) {
	$p->{m} = 2;
    } else {
	$p->{m} = $m;
    }

    $p->{r0} = _opt($me->{options},
		    ['r0','R0','radius','Radius',
		     'd','dist','distance','Distance'],
		    2.0
		    );
    
    if($p->{r0} == 0) {
      print "t_vertical: r0 = 0; using t_gnomonic instead\n"
	if($PDL::verbose);
      return t_gnomonic($me->{options});
    }
    
    if($p->{r0} == 1) {
      print "t_vertical: r0 = 1; using t_stereographic instead\n"
	if($PDL::verbose);
      return t_stereographic($me->{options});
    }
    
    
    $p->{t} = _opt($me->{options},
		   ['t','tele','telescope','Telescope',
		    'cam','camera','Camera'],
		   undef);
    
    $p->{f} = _opt($me->{options},
		   ['f','fish','fisheye','Fisheye'],
		   undef);
    
    $p->{t} = 'rad'
      if($p->{f} && !defined($p->{t}));
      
    $p->{tconv} = _uconv($p->{t},1) || _uconv('rad')
      if(defined $p->{t});

    $me->{func} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;
	my($th) = $d->((0))*$o->{conv};
	my($ph) = $d->((1))*$o->{conv};

	my($cph) = cos($ph);

	my($cos_c) = $cph * cos($th);

	my($k) = (($o->{r0} - 1) / 
		  ($o->{r0} - $cos_c));

	# If it's a telescope perspective, figure the apparent size
	# of the globe and scale accordingly.
	if($o->{t}) {
	  my($theta) = asin(1/$o->{r0});
	}
	
	$out->(0:1) /= ($o->{r0} - 1.0) * ($o->{f} ? 1.0 : $o->{tconv})
  	  if($o->{t});



	$out->((0)) .= $cph * sin($th);
	$out->((1)) .= sin($ph);

	# Handle singularity at the origin
	$k->where(($out->((0)) == 0) & ($out->((1)) == 0)) .= 0;
	$out->(0:1) *= $k->dummy(0,2);

	if($o->{m}) {
	    my $idx;
	    $idx = whichND($cos_c < 1.0/$o->{r0})
		if($o->{m} == 1);
	    $idx = whichND($cos_c > 1.0/$o->{r0})
		if($o->{m} == 2);

	    if(defined $idx && ref $idx eq 'PDL' && $idx->nelem){
	      $out->((0))->range($idx) .= $o->{bad};
	      $out->((1))->range($idx) .= $o->{bad};
	    }
	}


	$out;
    };

    $me->{inv} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;

	# Reverse the hemisphere if the mask is set to 'far'
	my($P) = ($o->{m} == 2) ? -$o->{r0} : $o->{r0};

	$out->(0:1) *= ($P - 1.0) * ($o->{f} ? 1.0 : $o->{tconv})
     	    if($o->{t});

	my($rho) = sqrt(sumover($d->(0:1) * $d->(0:1)));
	my($sin_c) = ( (  $P - sqrt( 1 - ($rho*$rho * ($P+1)/($P-1)) ) ) /
		       ( ($P-1)/$rho + $rho/($P-1) )
		       );

	my($cos_c) = sqrt(1 - $sin_c*$sin_c);

	# Switch c's quadrant where necessary, by inverting cos(c).
	if($P<0) {
	  my $idx = whichND($rho > ($P-1/$P));
	  $cos_c->range($idx) *= -1
	    if($idx->nelem > 0);
	}

	
	$out->((0)) .= atan( $d->((0)) * $sin_c / ($rho * $cos_c) );
	$out->((1)) .= asin( $d->((1)) * $sin_c / $rho );

	$out->(0:1) /= $o->{conv};

	$out;
    };
	      

    # Compose on both front and back as necessary.
    return t_compose( t_scale(1.0/$p->{tconv}), 
		      t_az_eqd, 
		      t_gnomonic->inverse, 
		      $me->_finish )
      if($p->{f}); 

    $me->_finish;
  }

######################################################################

=head2 t_perspective

=for usage

    $t = t_perspective(<options>);

=for ref

(Cartography) Arbitrary perspective projection 

Perspective projection onto a focal plane from an arbitrary location
within or without the sphere, with an arbitary central look direction,
and with correction for magnification within the optical system.

In the forward direction, t_perspective generates perspective views of
a sphere given (lon/lat) mapping or vector information.  In the reverse
direction, t_perspective produces (lon/lat) maps from aerial or distant
photographs of spherical objects.

Viewpoints outside the sphere treat the sphere as opaque by default,
though you can use the 'm' option to specify either the near or far
surface (relative to the origin).  Viewpoints below the surface treat
the sphere as transparent and undergo a mirror reversal for
consistency with projections that are special cases of the perspective
projection (e.g. t_gnomonic for r0=0 or t_stereographic for r0=-1).

Magnification correction handles the extra edge distortion due to
higher angles between the focal plane and focused rays within the
optical system of your camera.  If you do not happen to know the
magnification of your camera, a simple rule of thumb is that the
magnification of a reflective telescope is roughly its focal length
(plate scale) divided by its physical length; and the magnification of 
a compound refractive telescope is roughly twice its physical length divided 
by its focal length.  Simple optical sytems with a single optic have
magnification = 1.  Fisheye lenses have magnification < 1.

This transformation was derived by direct geometrical calculation
rather than being translated from Voxland & Snyder.

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

As always, the 'origin' field specifies the sub-camera point on the
sphere.

The 'roll' option is the roll angle about the sub-camera point, for
consistency with the other projectons.

=item p, ptg, pointing, Pointing (default (0,0,0))

The pointing direction, in (horiz. offset, vert. offset, roll) of the
camera relative to the center of the sphere.  This is a spherical
coordinate system with the origin pointing directly at the sphere and
the pole pointing north in the pre-rolled coordinate system set by the
standard origin.  It's most useful for space-based images taken some distance
from the body in question (e.g. images of other planets or the Sun).

Be careful not to confuse 'p' (pointing) with 'P' (P angle, a standard
synonym for roll).

=item c, cam, camera, Camera (default undef) 

Alternate way of specifying the camera pointing, using a spherical
coordinate system with poles at the zenith (positive) and nadir
(negative) -- this is useful for aerial photographs and such, where
the point of view is near the surface of the sphere.  You specify
(azimuth from N, altitude from horizontal, roll from vertical=up).  If
you specify pointing by this method, it overrides the 'pointing'
option, above.  This coordinate system is most useful for aerial photography
or low-orbit work, where the nadir is not necessarily the most interesting
part of the scene.

=item r0, R0, radius, d, dist, distance [default 2.0] 

The altitude of the point of view above the center of the sphere.
The default places the point of view 1 radius aboove the surface.
Do not confuse this with 'r', the standard origin roll angle!  Setting 
r0 < 1 gives a viewpoint inside the sphere.  In that case, the images are
mirror-reversed to preserve the chiralty of the perspective.  Setting 
r0=0 gives gnomonic projections; setting r0=-1 gives stereographic projections.
Setting r0 < -1 gives strange results.

=item iu, im_unit, image_unit, Image_Unit (default 'degrees')

This is the angular units in which the viewing camera is calibrated
at the center of the image.

=item mag, magnification, Magnification (default 1.0)

This is the magnification factor applied to the optics -- it affects the
amount of tangent-plane distortion within the telescope. 
1.0 yields the view from a simple optical system; higher values are 
telescopic, while lower values are wide-angle (fisheye).  Higher 
magnification leads to higher angles within the optical system, and more 
tangent-plane distortion at the edges of the image.  
The magnification is applied to the incident angles themselves, rather than
to their tangents (simple two-element telescopes magnify tan(theta) rather
than theta itself); this is appropriate because wide-field optics more
often conform to the equidistant azimuthal approximation than to the 
tangent plane approximation.  If you need more detailed control of 
the relationship between incident angle and focal-plane position, 
use mag=1.0 and compose the transform with something else to tweak the
angles.

=item m, mask, Mask, h, hemisphere, Hemisphere [default 'near']

'hemisphere' is by analogy to other cartography methods although the two 
regions to be selected are not really hemispheres.

=item f, fov, field_of_view, Field_Of_View [default 60 degrees]

The field of view of the telescope -- sets the crop radius on the
focal plane.  If you pass in a scalar, you get a circular crop.  If you
pass in a 2-element list ref, you get a rectilinear crop, with the
horizontal 'radius' and vertical 'radius' set separately. 

=back

EXAMPLES

Model a camera looking at the Sun through a 10x telescope from Earth
(~230 solar radii from the Sun), with an 0.5 degree field of view and
a solar P (roll) angle of 30 degrees, in February (sub-Earth solar
latitude is 7 degrees south).  Convert a solar FITS image taken with
that camera to a FITS lon/lat map of the Sun with 20 pixels/degree
latitude:

  # Define map output header (no need if you don't want a FITS output map)
  $maphdr = {NAXIS1=>7200,NAXIS2=>3600,            # Size of image
	     CTYPE1=>longitude,CTYPE2=>latitude,   # Type of axes
	     CUNIT1=>deg,CUNIT2=>deg,              # Unit of axes
	     CDELT1=>0.05,CDELT2=>0.05,            # Scale of axes
	     CRPIX1=>3601,CRPIX2=>1801,            # Center of map
	     CRVAL1=>0,CRVAL2=>0                   # (lon,lat) of center 
	     };

  # Set up the perspective transformation, and apply it.
  $t = t_perspective(r0=>229,fov=>0.5,mag=>10,P=>30,B=>-7);
  $map = $im->map( $t , $maphdr );

Draw an aerial-view map of the Chesapeake Bay, as seen from a sounding
rocket at an altitude of 100km, looking NNE from ~200km south of
Washington (the radius of Earth is 6378 km; Washington D.C. is at
roughly 77W,38N).  Superimpose a linear coastline map on a photographic map.

  $a = graticule(1,0.1)->glue(1,earth_coast());
  $t = t_perspective(r0=>6478/6378.0,fov=>60,cam=>[22.5,-20],o=>[-77,36])
  $w = pgwin(size=>[10,6],J=>1);
  $w->fits_imag(earth_image()->map($t,[800,500],{m=>linear}));
  $w->hold;
  $w->lines($a->apply($t),{xt=>'Degrees',yt=>'Degrees'});
  $w->release;

Model a 5x telescope looking at Betelgeuse with a 10 degree field of view
(since the telescope is looking at the Celestial sphere, r is 0 and this
is just an expensive modified-gnomonic projection).

  $t = t_perspective(r0=>0,fov=>10,mag=>5,o=>[88.79,7.41])
  


=cut

sub t_perspective {
    my($me) = _new(@_,'Focal-Plane Perspective');
    my $p = $me->{params};
    
    my $m= _opt($me->{options},
		['m','mask','Mask','h','hemi','hemisphere','Hemisphere'],
		1);
    $p->{m} = $m;
    $p->{m} = 0 if($m=~m/^b/i);
    $p->{m} = 1 if($m=~m/^n/i);
    $p->{m} = 2 if($m=~m/^f/i);

    $p->{r0} = _opt($me->{options},
		    ['r0','R0','radius','Radius',
		     'd','dist','distance','Distance'],
		    2.0
		    );
    
    $p->{iu} = _opt($me->{options},
		   ['i','iu','image_unit','Image_Unit'],
		   'degrees');
    
    $p->{tconv} = _uconv($p->{iu});

    $p->{mag} = _opt($me->{options},
		     ['mag','magnification','Magnification'],
		     1.0);

    # Regular pointing pseudovector -- make sure there are exactly 3 elements
    $p->{p} = (pdl(_opt($me->{options},
			['p','ptg','pointing','Pointing'],
			[0,0,0])
		   )
	       * $p->{tconv}
	       )->append(zeroes(3))->(0:2);

    $p->{pmat} = _rotmat( (- $p->{p})->list );
    
    # Funky camera pointing pseudovector overrides normal pointing option 
    $p->{c} = _opt($me->{options},
		   ['c','cam','camera','Camera'],
		   undef
		   );

    if(defined($p->{c})) {
      $p->{c} = (pdl($p->{c}) * $p->{tconv})->append(zeroes(3))->(0:2);

      $p->{pmat} = ( _rotmat( 0,-$PI/2,0 ) x 
		     _rotmat( (-$p->{c})->list ) 
		     );
    }

    # Reflect X axis if we're inside the sphere.
    if($p->{r0}<1) {
      $p->{pmat} = pdl([-1,0,0],[0,1,0],[0,0,1]) x $p->{pmat};
    }

    $p->{f} = ( _opt($me->{options},
		     ['f','fov','field_of_view','Field_of_View'],
		     pdl($PI*2/3) / $p->{tconv} / $p->{mag} )
		* $p->{tconv}
		);
    
    $me->{otype} = ['Tan X','Tan Y'];
    $me->{ounit} = [$p->{iu},$p->{iu}];

    # "Prefilter" -- subsidiary transform to convert the 
    # spherical coordinates to 3-D coords in the viewer's 
    # reference frame (Y,Z are more-or-less tangent-plane X and Y,
    # and -X is the direction toward the planet, before rotation 
    # to account for pointing).

    $me->{params}->{prefilt} = 
      t_compose(
                # Offset for the camera pointing.
		t_linear(m=>$p->{pmat},
			 d=>3),

                # Rotate the sphere so the correct origin is at the 
		# maximum-X point, then move the whole thing in the 
		# -X direction  by r0.
		t_linear(m=>(_rotmat($p->{o}->at(0),
				     $p->{o}->at(1),
				     $p->{roll}->at(0))
			     ),
			 d=>3,
			 post=> pdl( - $me->{params}->{r0},0,0)
			 ),

                # Put initial sci. coords into Cartesian space
		t_unit_sphere(u=>'radian')  
		);

    # Store the origin of the sphere -- useful for the inverse function
    $me->{params}->{sph_origin} = (
				   pdl(-$me->{params}->{r0},0,0) x 
				   $p->{pmat}
				   )->(:,(0));

    #
    # Finally, the meat -- the forward function!
    #
    $me->{func} = sub {
      my($d,$o) = @_;

      my($out) = $d->is_inplace ? $d : $d->copy;
      $out->(0:1) *= $o->{conv};
      
      # If we're outside the sphere, do hemisphere filtering
      my $idx;
      if(abs($o->{r0}) < 1 ) {
	$idx = null;
      } else {
	# Great-circle distance to origin
	my($cos_c) = ( sin($o->{o}->((1))) * sin($out->((1)))
		     +
		     cos($o->{o}->((1))) * cos($out->((1))) * 
		     cos($out->((0)) - $o->{o}->((0)))
		     );

	my($thresh) = (1.0/$o->{r0});
	
	if($o->{m}==1) {
	  $idx = whichND($cos_c < $thresh);
	} elsif($o->{m}==2) {
	  $idx = whichND($cos_c > $thresh);
	} else {
	  $idx = null;
	}
      }	

      ### Transform everything -- just chuck out the bad points at the end.

      ## convert to 3-D viewer coordinates (there's a dimension change!)
      my $dc = $out->apply($o->{prefilt});

      ## Apply the tangent-plane transform, and scale by the magnification.
      my $dcyz = $dc->(1:2);

      my $r = ( $dcyz * $dcyz ) -> sumover -> sqrt ;     
      my $rscale;
      if( $o->{mag} == 1.0 ) {
	  $rscale = - 1.0 / $dc->((0));
      } else {
	  print "(using magnification...)\n" if $PDL::verbose;
	  $rscale = - tan( $o->{mag} * atan( $r / $dc->((0)) ) ) / $r;
      }
      $r *= $rscale;
      $out->(0:1) .= $dcyz * $rscale->dummy(0,1);

      # Chuck points that are outside the FOV: glue those points
      # onto the removal list.   The conditional works around a bug 
      # in 2.3.4cvs and earlier: null piddles make append() crash.
      my $w;
      if(ref $o->{f} eq 'ARRAY') {
	$w = whichND( ( abs($dcyz->((0))) > $o->{f}->[0] ) |
		      ( abs($dcyz->((1))) > $o->{f}->[1] ) |
		      ($r < 0)
		      );
      } else {
	$w = whichND( ($r > $o->{f}) | ($r < 0) );
      }
    
      $idx = ($idx->nelem) ?  $idx->glue(1,$w)  : $w
	if($w->nelem);

      if($idx->nelem) {
	$out->((0))->range($idx) .= $o->{bad};
	$out->((1))->range($idx) .= $o->{bad};
      }

      ## Scale by the output conversion factor
      $out->(0:1) /= $o->{tconv};

      $out;
    };

    
    #
    # Inverse function
    #
    $me->{inv} = sub {
	my($d,$o) = @_;

	my($out) = $d->is_inplace ? $d->copy : $d;
	$out->(0:1) *= $o->{tconv};

	my $oyz = $out->(0:1) ;

	## Inverse-magnify if required
	if($o->{mag} != 1.0) {
	  my $r = ($oyz * $oyz)->sumover->sqrt;
	  my $scale = tan( atan( $r ) / $o->{mag} ) / $r;
	  $out->(0:1) *= $scale;
	}
	
	## Solve for the X coordinate of the surface.  
	## This is a quadratic in the tangent-plane coordinates;
	## so here we just figure out the coefficients and plug into
	## the quadratic formula.  $b here is actually -B/2.
	my $a = ($oyz * $oyz)->sumover + 1;
	my $b = ( $o->{sph_origin}->((0)) 
		  - ($o->{sph_origin}->(1:2) * $oyz)->sumover
		  );
	my $c = pdl($o->{r0}*$o->{r0} - 1);
	
	my $x;
	if($o->{m} == 2) { 
	    # Exceptional case: mask asks for the far hemisphere
	    $x = - ( $b - sqrt($b*$b - $a * $c) ) / $a;
	} else {
	    # normal case: mask asks for the near hemisphere
	    $x =   - ( $b + sqrt($b*$b - $a * $c) ) / $a;
	}

	## Assemble the 3-space coordinates of the points
	my $int = $out->(0)->append($out);
	$int->sever;
	$int->((0)) .= -1.0;
	$int->(0:2) *= $x->dummy(0,3);

	## convert back to (lon,lat) coordinates...
	$out .= $int->invert($o->{prefilt});	

	# If we're outside the sphere, do hemisphere filtering
	my $idx;
	if(abs($o->{r0}) < 1 ) {
	    $idx = null;
	} else {
	    # Great-circle distance to origin
	    my($cos_c) = ( sin($o->{o}->((1))) * sin($out->((1)))
			   +
			   cos($o->{o}->((1))) * cos($out->((1))) * 
			   cos($out->((0)) - $o->{o}->((0)))
			   );
	    
	    my($thresh) = (1.0/$o->{r0});
	    
	    if($o->{m}==1) {
		$idx = whichND($cos_c < $thresh);
	    } elsif($o->{m}==2) {
		$idx = whichND($cos_c > $thresh);
	    }
	    else {
		$idx = null;
	    }
	}	

	## Convert to the units the user requested
	$out->(0:1) /= $o->{conv};

	## Mark bad values
	if($idx->nelem) {
	    $out->((0))->range($idx) .= $o->{bad};
	    $out->((1))->range($idx) .= $o->{bad};
	}

	$out;

    };
	      
    $me;
  }

1;
