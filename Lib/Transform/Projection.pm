=head1 NAME

PDL::Transform::Projection - Useful cartographic projections

=head1 SYNOPSIS

# make a map from an aerial view
use PDL::Transform::Projection;
$a = rfits('aerial-view.fits')
$t = t_compose(t_obs($a),t_mercator(<params>));
$map = $t->map($a);

=head1 DESCRIPTION

PDL::Transform::Projection includes a variety of useful cartographic
and observing projections (mappings of the surface of a sphere),
including reprojected observer's coordinates.  See L<PDL::Transform>
for more information about image transforms in general.

Cartographic transformations are useful for projecting not just terrestrial
maps, but also any spherical surface including the Sun, the Celestial 
sphere, various moons and planets, distant stars, etc.

Unless otherwise noted, the transformations in this section are taken
from Snyder & Voxland 1989: "An Album of Map Projections", US
Geological Survey Professional Paper 1453, US Printing Office
(Denver).  You can obtain your own copy by writing to the U.S. Geological 
Survey, Federal Center, Box 25425, Denver, CO 80225.

Unless otherwise noted, all the transformations in this file convert
from (theta,phi) coordinates on the unit sphere (e.g. (lon,lat)
on a planet or (RA,dec) on the celestial sphere) into some sort of
projected coordinates, and have inverse transformations that convert
back to (theta,phi).

=head1 CONSTRUCTORS

=cut

package PDL::Transform::Projection;
@ISA = ( 'Exporter','PDL::Transform' );
$VERSION = "0.1";

BEGIN {
  use Exporter ();
  @EXPORT_OK = qw(t_unitsphere t_focalplane);
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
*PDL::Transform::Projection::_opt = \&PDL::Transform::_opt;


our $PI = 3.14159265358979328462643383279502;
our $DEG2RAD = $PI/180;
our $RAD2DEG = 180/$PI;
our $E = exp(1);


use strict;

our $PI = $PDL::Transform::PI;
our $DEG2RAD = $PDL::Transform::DEG2RAD;
our $RAD2DEG = $PDL::Transform::RAD2DEG;
our $E = $PDL::Transform::E;

######################################################################

=head2 t_unitsphere

=for usage

  $t = t_unitsphere(<options>);

=for ref

Deproject into 3-D Cartesian (2-D/3-D; with inverse; conformal; equal-area)

This is similar to the inverse of L<t_spherical|t_spherical>, but the
inverse transform projects 3-D coordinates onto the unit sphere,
yielding only a 2-D (lon/lat) output.  Similarly, the forward
transform deprojects 2-D (lon/lat) coordinates onto the surface of a
unit sphere.

Unit sphere mapping is unusual in that it is both conformal and equal-area.
That is possible because it properly embeds the sphere in 3-space, as a 
notional globe.

This is handy as an intermediate step in lots of transforms, as 
Cartesian 3-space is cleaner to work with than spherical 2-space.

OPTIONS:

=over 3

=item o, origin, Origin [default (0,0,0)]

The center of the sphere in 3-D

=item r, radius, Radius [default 1.0]

The radius of the sphere, for the inverse transform.  (Radius
information is simply discarded by the forward transform).

=item u, unit, Unit [default 'radian']

This is the name of the angular unit to use.

=back

=cut

#BEGIN {push(@$EOK,'t_unitsphere')}
@PDL::Transform::Projection::Unitsphere::ISA = ('PDL::Transform');
sub t_unitsphere {new PDL::Transform::Projection::Unitsphere(@_)};

sub PDL::Transform::Projection::Unitsphere::new {
  my($class) = shift;
  my($o) = $_[0];
  $o = {@_}
    unless(ref $o eq 'HASH');

  my($me) = PDL::Transform::new($class);
  
  $me->{name} = "unit-sphere";

  $me->{params}->{indim} = 3;
  $me->{params}->{outdim} = 2;
  
  $me->{params}->{o} =_opt($o,['o','origin','Origin']);
  $me->{params}->{o} = pdl($me->{params}->{o}) 
    if defined($me->{params}->{o});
			   
  $me->{params}->{r} = pdl(_opt($o,['r','radius','Radius'],1.0));
  $me->{params}->{u} =     _opt($o,['u','unit','Unit'],'radian');

  ## Replace this with a units call!
  $me->{params}->{angunit} = ($me->{params}->{u} =~ m/^d/i) ? $DEG2RAD : undef;
  
  $me->{inv} = sub {
    my($data,$o) = @_;
    my($d) = (defined $o->{origin}) ? $data - $o->{origin} : $data;
    
    my($d0,$d1,$d2) = ($d->((0)),$d->((1)),$d->((2)));
    my($out) = zeroes($data->(0:1));
    
    $out->((0)) .= atan2($d1,$d0);
    $out->((1)) .= asin($d2/$out->((0)));

    $out /= $o->{angunit}
      if(defined $o->{angunit});

    $out;
  };

  $me->{func} = sub {
    my($d,$o) = @_;
    my(@dims) = $d->dims;
    $dims[0] = 3;
    my $out = zeroes(@dims);
    
    my($thetaphi) = $d->sever;
    $thetaphi *= $o->{angunit} 
      if(defined $o->{angunit});

    my $th = $thetaphi->((0));
    my $ph = $thetaphi->((1));

    # use x as a holding tank for the cos-phi multiplier
    $out->((0)) .= $o->{r} * cos($ph) ;
    $out->((1)) .= $out->((0)) * sin($th);
    $out->((0)) *= cos($th);

    $out->((2)) .= $o->{r} * sin($ph);

    $out += $o->{origin}
      if(defined $o->{origin});
    
    $out;
  };
    
  $me;
}


=head2 t_focalplane

=for usage

   $t = t_focalplane(<Options>)

=for ref 

Convert observer (tan-theta) coordinates to spherical coordinates.

Most telescopes and optical systems yield an azimuthally symmetric map
of the angular light distribution coming in.  Azimuth is preserved by
the telescope map, but distance from the origin of the focal plane is
proportional to the tangent of the great-circle angle between the
optic axis and the incoming light ray.  For convenience, you can also
specify the direction of the focal plane center, in spherical
coordinates (theta, phi).  For celestial coordinates this is
equivalent to (RA, dec), except in normal angular units instead of hours.

This works by pointing the pole of a spherical coordinate system
through the middle of the focal plane.  Then azimuth translates
directly to theta, and radius translates to tan-phi.  Then the derived
instrument-centric spherical coordinates are translated into
scientific spherical coordinates using the specified central pointing
vector and roll angle.

OPTIONS:

=over 3

=item c,center,Center [default (0,0)]

This is (theta,phi) coordinates in degrees, indicating which direction
the focal plane is pointing in a spherical universal coordinate system
such as celestial coordinates.  The origin of the focal plane points
in the given direction.

=item r,roll,Roll [default (0)]

The clockwise roll angle of the observer compared to the (+phi = +y)
direction (north=up in celestial coordinates).  If the observer is
looking at a pole of the spherical coordinate system, the roll angle is
determined in the obvious way: it is still relative to the meridian given 
in the C<center> argument.

=item u,unit,Unit [default 'radian']

This is the name of the angular unit to use.

=back

=cut

#BEGIN {push(@$EOK,'t_focalplane')}
@PDL::Transform::Projection::Focalplane::ISA = ('PDL::Transform');
sub t_focalplane {new PDL::Transform::Projection::Focalplane(@_)};

sub PDL::Transform::Projection::Focalplane::new {
  my($class) = shift;
  my($o) = $_[0];
  if(ref $o ne 'HASH') {
    $o = {@_};
  }

  my($me) = PDL::Transform::new($class);

  # Replace this kludge with a units call!
  $me->{params}->{u} = _opt($o,['u','unit','Unit'],'radian');
  $me->{params}->{angunit} = 1.0;
  $me->{params}->{angunit} = $RAD2DEG
    if($me->{params}->{u} =~ m/^d/i);

  $me->{params}->{roll} = _opt($o,['r','roll','roll'],zeroes(1));
  $me->{params}->{c} = _opt($o,['c','center','Center'],zeroes(2));

  $me->{params}->{c}    = pdl($me->{params}->{c});
  $me->{params}->{roll} = pdl($me->{params}->{roll});

  # Figure a rotation matrix in Cartesian space.
			     
  my $th = $me->{params}->{c}->at(0);
  my $ph = $me->{params}->{c}->at(1);
  my $r = $me->{params}->{roll}->at(0);

  $me->{params}->{rotmat} = 
    pdl( [ cos($th) ,  -sin($th),   0 ],   # apply theta last
	 [ sin($th),   cos($th),   0 ],
	 [ 0        ,   0       , 1 ] )
    x
    pdl( [ cos($ph),   0,   -sin($ph) ],   # apply phi
	 [ 0,          0,   0         ],   # 
	 [ sin($ph),   0,   cos($ph)  ] )  # 
    x
    pdl( [ 1,          0,   0         ],   # apply roll
	 [ 0,    cos($r),   -sin($r)  ],
	 [ 0,    sin($r),   cos($r)   ] )
    x
    pdl( [ 0, 0, -1 ],  # Rotate inst system pole to (0,0).
	 [ 0, 1,  0 ],
	 [ 1, 0,  0 ])
    ;


  $me->{name} = "focal plane";

  # The func & inv work on (azimuth,radius) coords in the focal plane --
  # that's achieved with the t_wrap call at the bottom.

  $me->{func} = sub {   
    my($d,$o) = @_;
    my($out) = zeroes($d);
    $out->((0)) .= ($PI/2.0 - atan($out->((1)))) * $o->{angunit}; # r -> cophi
    $out->((1)) .= $d->((0));  # az -> theta
  };
    
  $me->{inv} = sub {
    my($d,$o) = @_;
    my($out) = zeroes($d);
    $out->((1)) .= tan($PI/2.0 - ($d->((0))/$o->{angunit})); # cophi -> r
    $out->((0)) .= $d->((1));                                # theta -> az
  };


  t_compose(t_radial(u=>$me->{params}->{u}),    
	    $me,
	    t_wrap( t_linear(m=>$me->{params}->{rotmat}),
		    t_unitsphere(u=>$me->{params}->{u})
		    )
	    );
}
