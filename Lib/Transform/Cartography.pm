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
plane (e.g. the L<t_gnomonic|gnomonic> projection).

Unless otherwise noted, all the transformations in this file convert
from (theta,phi) coordinates on the unit sphere (e.g. (lon,lat) on a
planet or (RA,dec) on the celestial sphere) into some sort of
projected coordinates, and have inverse transformations that convert
back to (theta,phi).  This is equivalent to working from the
equidistant cylindrical (or L<t_caree|"plate caree">) projection, if
you are a cartography wonk.

The projected coordinates are in units of body radii, so that
multiplying the output by the scale of the map yields physical units
that are correct wherever the scale is correct for that projection.
For example, areas should be correct everywhere in the authalic
projections; and linear scales are correct along meridians in the
equidistant projections and along the standard parallels in all the
projections.

The transformations that are authalic (equal-area), conformal
(equal-angle), azimuthal (circularly symmetric), or perspective (true
perspective on a focal plane from some viewpoint) are marked.  These
categories are mutually exclusive for all but the L<t_unit_sphere|unit
sphere> 3-D projection.

Extra dimensions tacked on to each point to be transformed are, in
general, ignored.  That is so that you can add on an extra index
to keep track of pen color.  For example, L<earth_coast|earth_coast>
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
L<t_gnomonic|gnomonic> images -- that is to say, gnomonic projections
of a portion of the celestial sphere near the paraxial direction.
This is the projection that most consumer grade cameras produce.

Wide-angle optical systems are often tuned to produce
L<t_az_equi|"equidistant azimuthal"> projections of a portion of the
celestial sphere, which has the advantage that you can put an entire
celestial hemisphere on a finite region in the focal plane.  If you
want to interpret wide-angle celestial images then you should pick the
projection that is appropriate for your telescope; the two projections
are equivalent for small fields of view.

Because many solar-system objects are spherical,
PDL::Transform::Cartography includes perspective projections for
producing maps of spherical bodies from perspective views.  Those
projections are C<t_vertical|t_vertical> and
C<t_perspective|t_perspective>.  They map between (lat,lon) on the
spherical body and equidistant azimuthal focal plane coordinates at
the camera, so that (e.g.) fisheye aerial views of Earth are
supported.

If you want to use the perspective transformations to interpret wide-angle
views of Earth or other spheres from close up, you must consider whether
your camera is a "fisheye" (equidistant azimuthal) lens or a normal
(gnomonic) lens (typical for consumer cameras up to about a 70 degree field 
of view).  You can tell which you have by examining the distortion around the
outer edges of the image:  are objects stretched radially (gnomonic; simple
lens) or tangentially (equidistant azimuthal; fisheye lens)?  To interpret a 
perspective aerial view from a simple wide-angle lens, you need to use the
compound transform:

    ( t_gnomonic x !t_az_equi x t_perspective(out=>angle,<options>) )

which will map (lon,lat) on the Earth to focal plane parameters in
your camera.

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

This is the name of the angular unit to use.

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

=item b, bad, Bad, missing, Missing [default nan]

This is the value that missing points get.  Mainly useful for the
inverse transforms.  (This should work fine if set to BAD, if you have
bad-value support compiled in).  The default nan is represented as
asin(1.2).

=back

=head1 EXAMPLES

Draw a Mercator map of the world on-screen:

   $w = pgwin(xs);
   $w->lines(t_mercator()->apply(earth_coast())->clean_lines());

Here, C<earth_coast()> returns a 3xn piddle containing (lon, lat, pen) 
values for the included world coastal outline; C<t_mercator> converts
the values to projected Mercator coordinates, and C<clean_lines> breaks
lines that cross the 180th meridian.

Draw a Mercator map of the world, with lon/lat at 10 degree intervals:

   $w = pgwin(xs)
   $a = earth_coast()->glue(1,scalar(graticule(10,1)));
   $w->lines(clean_lines(t_mercator()->apply()));

This works just the same as the first example, except that a map graticule
has been applied with interline spacing of 10 degrees lon/lat and 
inter-vertex spacing of 1 degree (so that each meridian contains 181 points,
and each parallel contains 361 points).

=head1 NOTES

Currently angular conversions are extremely simpleminded.  Everything that
uses angles independently understands degree-to-radian conversions.  That is
really stoopid but quick to code.  Something like Math::Convert::Units should
be used instead.  You can find all the angular conversions by doing a global
search for "/^d/i", which is the regexp that I have used to recognize 
the string 'degrees'.

A cleaner higher-level interface is probably needed (see the examples);
for example, earth_coast could return a graticule if asked.

The class structure is somewhat messy because of the varying needs of
the different transformations.  PDL::Transform::Cartography is a base
class that interprets the origin options and sets up the basic
machinery of the Transform.  The conic projections have their
own subclass, PDL::Transform::Conic, that interprets the standard
parallels.  Since the cylindrical and azimuthal projections are pretty
simple, they are direct subclasses of ::Cartography.

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

Here are some auxiliary functions that are exported by the module.  They 
are not transformations in themselves but are useful for cartographic
applications.

=cut

package PDL::Transform::Cartography;
@ISA = ( 'Exporter','PDL::Transform' );
$VERSION = "0.5";

BEGIN {
  use Exporter ();
  @EXPORT_OK = qw(graticule earth_coast clean_lines t_unit_sphere t_orthographic t_rot_sphere t_caree t_sin_lat t_mercator t_conic t_albers t_lambert t_stereographic t_gnomonic t_vertical);
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

our $PI = 3.14159265358979328462643383279502;
our $DEG2RAD = $PI/180;
our $RAD2DEG = 180/$PI;
our $E = exp(1);


use strict;

our $PI = $PDL::Transform::PI;
our $DEG2RAD = $PDL::Transform::DEG2RAD;
our $RAD2DEG = $PDL::Transform::RAD2DEG;
our $E = $PDL::Transform::E;

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
for sending to L<PDL::Graphics::PGPLOT::Window::lines|lines> for plotting.
The grid is in degrees in (theta, phi) coordinates -- this is (E lon, N lat) 
for terrestrial grids or (RA, dec) for celestial ones.  You must then 
transform the graticule in the same way that you transform the map.

You can attach the graticule to a vector map using the syntax:

    $out = graticule(10,2)->glue(1,$map);

In array context you get back a 2-element list containing a piddle of
the (theta,phi) pairs and a piddle of the pen values (1 or 0) suitable for
calling L<PDL::Graphics::PGPLOT::Window::lines|lines>.  In scalar context
the two elements are combined into a single piddle.

The pen values associated with the graticule are negative, which will
cause L<PDL::Graphics::PGPLOT::Window::lines|lines> to plot them as
hairlines.

=cut

sub graticule {
    my $grid = shift;
    my $step = shift;
    $grid = 10 unless defined($grid);
    $step = $grid/2 unless defined($step);
    
    my $par_siz = ((floor(180/$grid)) * (floor(360/$step) + 1))->at(0);
    my $mer_siz = (floor(360/$grid) * floor(180/$step + 1))->at(0);
    
    my $out = zeroes(2,$par_siz + $mer_siz);
    my $p = ones($par_siz + $mer_siz);
    
    # First do parallels
    $out->((0),0:$par_siz - 1) .= 
	(xvals($par_siz) * $step) % (360+$step);
    $out->((1),0:$par_siz - 1) .=
	$grid * (1+floor((xvals($par_siz) * $step) / (360+$step)));
    $p->(  ( xvals((floor(180/$grid)-1)->at(0)) + 1) * (floor(360/$step)+1)-1)
	.= 0;
    
    
    $out->((0),$par_siz:-1) .=
	$grid * floor(xvals($mer_siz) * $step / (180+$step));
    $out->((1),$par_siz:-1) .=
	(xvals($mer_siz)*$step) % (180+$step) ;
    $p->(  $par_siz + ( xvals(floor(360/$grid)->at(0))+1) * (floor(180/$step)+1)-1)
	.= 0;

    $out->(0) -= 180;
    $out->(1) -= 90;
    
    $out->append(-$p->dummy(0,1));
}

=head2 earth_coast

=for usage

  $a = earth_coast()

=for ref

(Cartography): PDL constructor - coastline map of Earth

Loads the PDL::Transform::Cartography:Earth package and returns a
coastline map based on the 1987 CIA World Coastline database (see
author information).  

The routine loads the package explicitly at run-time in order to 
avoid the overhead of loading it when not needed; this might be 
False Laziness as it is "only" 150k.

=cut

sub earth_coast {
    eval "use PDL::Transform::Cartography::Earth;";
    return PDL::Transform::Cartography::Earth::earth_coast();
    }


=head2 clean_lines

=for usage

 $a = clean_lines(t_mercator->apply(scalar(earth_coast())));
 $a = clean_lines($line_pen, [threshold]);
 $a = $lines->clean_lines;

=for ref

(Cartography): PDL method - remove projection irregularities

C<clean_lines> massages vector data to remove jumps due to singularities
in the transform.

In the first (scalar) form, C<$line_pen> contains both (X,Y) points and pen 
values suitable to be fed to L<PDL::Graphics::PGPLOT::Window::lines|lines>;
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

    my $roll = pdl(_opt($o,['r','roll','Roll','p','P'],0));
    my $unit = _opt($o,['u','unit','Unit'],'degrees');

    ###
    # Replace this with a units resolver call!
    ###
    local($_) = $unit;
    $me->{params}->{conv} = my $conv = 
	( m/^deg/i    ? $DEG2RAD :
	  m/^arcmin/i ? $DEG2RAD / 60 :
	  m/^arcsec/i ? $DEG2RAD / 3600 :
	  m/^hour/i   ? $DEG2RAD * 15 :    # Right ascension
	  m/^min/i    ? $DEG2RAD * 15/60 : # Right ascension
	  m/^microrad/i ? 1e-6 :
	  m/^millirad/i ? 1e-3 :
	  1.0
	  );
    $me->{params}->{u} = $unit;


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
    ( ( ($me->{params}->{o}->(0) != 0) || 
	($me->{params}->{o}->(1) != 0) ||
	($me->{params}->{r} != 0) 
	)
      ?
      t_compose($me,t_rot_sphere($me->{options}))
      :
      $me
     );
}

######################################################################

=head2 t_unit_sphere

=for usage

  $t = t_unit_sphere(<options>);

=for ref

(Cartography) 3-D globe projection (conf; auth)

This is similar to the inverse of L<t_spherical|t_spherical>, but the
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
transforms by L<t_wrap|wrapping> L<t_linear|t_linear> with
L<t_unit_sphere|t_unit_sphere>.

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
		     $d * $o->{conv} : $d
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
    my(@dims) = $d->dims;
    $dims[0]--;
    my($out) = zeroes(@dims);
    
    $out->((0)) .= atan2($d1,$d0);
    $out->((1)) .= asin($d2/$o->{r});

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

sub t_rot_sphere {
    my($me) = _new(@_,'Spherical rotation');

    my($th,$ph) = $me->{params}->{o}->list;
    my($r) = $me->{params}->{roll}->at(0);

    my($rotmat) = (
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
		   );

    return t_wrap( t_linear(m=>$rotmat, d=>3), t_unit_sphere());
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
L<t_unit_sphere|t_unit_sphere>.

OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=item m, mask, Mask, h, hemisphere, Hemisphere [default 'near']

The hemisphere to keep in the projection (see L<PDL::Transform::Cartography>).

=back 

NOTES

Alone of the various projections, this one does not use
L<t_rot_sphere|t_rot_sphere> to handle the standard options, because
the cartesian coordinates of the rotated sphere are already correctly
projected -- t_rot_sphere would put them back into (theta', phi')
coordinates.

=cut

sub t_orthographic {
    my($me) = _new(@_,'Orthographic Projection');

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
				 90+ $origin->at(0)],
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
	    $idx = which($out->((2)) > 0) 
		if($o->{m} == 1);
	    $idx = which($out->((2)) < 0)
		if($o->{m} == 2);
	    $out->(0:1,$idx) .= $o->{bad}
	      if(defined $idx && ref $idx eq 'PDL' && $idx->nelem);
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

(Cartograph) Plate Caree projection (cylindrical; equidistant)

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

    $me->{params}->{stretch} = sqrt(cos($me->{params}->{std}));

    $me->{func} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;

	$out->(0:1) *= $DEG2RAD if($o->{u} =~ m/^d/i);
	$out->((1)) .= sin($out->((1))) / $o->{stretch};
	$out->((0)) *= $o->{stretch};
	$out;
    };

    $me->{inv} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;
	$out->((1)) .= asin($out->((1)) * $o->{stretch});
	$out->((0)) /= $o->{stretch};
	$out->(0:1) *= $DEG2RAD if($o->{u} =~ m/^d/i);
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
C<roll> options; this is implemented via L<t_rot_sphere|t_rot_sphere>.

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
is also correct at the parallel of oppoite sign.  

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
	$p->{c} *= $DEG2RAD if($p->{u} =~ m/^d/i);
    } else {
	$p->{c} = pdl($DEG2RAD * 75);
    }
    $p->{c} = abs($p->{c}) * pdl(-1,1) if($p->{c}->nelem == 1);
    $p->{c} = log(tan(($p->{c}/2) + $PI/4));       
    $p->{c} = [$p->{c}->list];

    $p->{std} = pdl(_opt($me->{options},
			 ['s','std','standard','Standard'],
			 0))->at(0) * $p->{conv};

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
L<t_lambert|t_lambert>!)  if the pole is used as one of the two
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
aq special case.  (Note that L<t_lambert|t_lambert> is Lambert's
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
infinity.  Many applications require a clipping boundary.  The value is
in whatever angular unit you set with the standard 'unit' option.  For
consistency with L<t_mercator>, clipping works the same way even though
in most cases only one pole needs it.  Set this to 0 for no clipping at all.

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
    
    $me->{params}->{k0} = 1.0;

    $me->{params}->{c} = pdl(_opt($me->{options},
			      ['c','clip','Clip'],
			      75) * $me->{params}->{conv});

    $me->{params}->{c} .= $me->{params}->{c}->clip(undef,(90-1e-6)*$me->{params}->{conv});

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
	
	$out->((0))->where($k > $cl0 | $k < 0) .= $o->{bad};
	$out->((1))->where($k > $cl0 | $k < 0) .= $o->{bad};
	
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
	
	$out->(0:1) /= $o->{conv};
	$out;
    };

    $me->_finish;
}
     
######################################################################

=head2 t_vertical

=for usage

    Rt = t_vertical(<options>);

=for ref

(Cartography) Vertical perspective projection (az.; persp.)

Vertical perspective projection is a generalization of L<t_gnomonic|gnomonic>
and L<t_stereographic|stereographic> projection, and a special case of 
L<t_perspective|perspective> projection.  It is a projection from the 
sphere onto a focal sphere (that is to say, equidistant azimuthal projection
at the camera).


OPTIONS

=over 3

=item STANDARD POSITIONAL OPTIONS

=item m, mask, Mask, h, hemisphere, Hemisphere [default 'near']

The hemisphere to keep in the projection (see L<PDL::Transform::Cartography>).

=item r0, R0, radius, d, dist, distance [default 2.0]

The altitude of the focal plane above the center of the sphere.  The default
places the point of view one radius above the surface.

=back

=cut

sub t_vertical {
    my($me) = _new(@_,'Vertical Perspective');
    
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

    $me->{params}->{r0} = _opt($me->{options},
			       ['r0','R0','radius','Radius',
				'd','dist','distance','Distance'],
			       2.0
			       );
    
    if($me->{params}->{r0} == 0) {
	print "t_vertical: r0 = 0; using t_gnomonic instead\n"
	    if($PDL::verbose);
	return t_gnomonic($me->{options});
    }

    if($me->{params}->{r0} == 1) {
	print "t_vertical: r0 = 1; using t_stereographic instead\n"
	    if($PDL::verbose);
	return t_stereographic($me->{options});
    }

    $me->{func} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;
	my($th) = $d->((0))*$o->{conv};
	my($ph) = $d->((1))*$o->{conv};

	my($cph) = cos($ph);

	my($cos_c) = $cph * cos($th);

	my($k) = (($o->{r0} - 1) / 
		  ($o->{r0} - $cos_c));

	$out->((0)) .= $k * $cph * sin($th);
	$out->((1)) .= $k * sin($ph);
	
	if($o->{m}) {
	    my $idx;
	    $idx = which($cos_c < 1.0/$o->{r0})
		if($o->{m} == 1);
	    $idx = which($cos_c > 1.0/$o->{r0})
		if($o->{m} == 2);

	    $out->(0:1,$idx) .= $o->{bad}
	      if(defined $idx && ref $idx eq 'PDL' && $idx->nelem);
	}
	    
	$out;
    };

    $me->{inv} = sub {
	my($d,$o) = @_;
	my($out) = $d->is_inplace ? $d : $d->copy;
	
	# Reverse the hemisphere if the mask is set to 'far'
	my($P) = ($o->{m} == 2) ? -$o->{r0} : $o->{r0};

	my($rho) = sqrt(sumover($d->(0:1) * $d->(0:1)));
	my($sin_c) = ( (  $P - sqrt( 1 - ($rho*$rho * ($P+1)/($P-1)) ) ) /
		       ( ($P-1)/$rho + $rho/($P-1) )
		       );

	my($cos_c) = sqrt(1 - $sin_c*$sin_c);

	# Switch c's quadrant where necessary, by inverting cos(c).
	$cos_c->where($rho > ($P-1/$P)) *= -1
	    if($P<0);

	$out->((0)) .= atan( $d->((0)) * $sin_c / ($rho * $cos_c) );
	$out->((1)) .= asin( $d->((1)) * $sin_c / $rho );

	$out->(0:1) /= $o->{conv};

	$out;
    };
	      

    $me->_finish;
}


1;
